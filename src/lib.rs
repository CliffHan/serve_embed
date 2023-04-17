#[cfg(test)]
mod tests;

use axum::response::{IntoResponse, Response as AxumResponse};
use bytes::Bytes;
use fly_accept_encoding::Encoding;
use http::header::*;
use http::{HeaderMap, HeaderValue, Method, Request, StatusCode};
use http_body::{Body, SizeHint};
use log::trace;
use rust_embed::{EmbeddedFile, RustEmbed};
use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::str::FromStr;
use std::task::{Context, Poll};
use tower::Service;

enum ServeEmbedResponse {
    Full { file: EmbeddedFile, encoding: Option<Encoding>, origin: Option<EmbeddedFile>, head_only: bool },
    Partial { file: EmbeddedFile, start: u64, end: u64 },
}

impl Debug for ServeEmbedResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Full { file: _, encoding, origin, head_only } => f
                .debug_struct("Full")
                .field("encoding", encoding)
                .field("origin.is_some()", &origin.is_some())
                .field("head_only", head_only)
                .finish(),
            Self::Partial { file: _, start, end } => {
                f.debug_struct("Partial").field("start", start).field("end", end).finish()
            }
        }
    }
}

#[derive(Debug)]
enum ServeEmbedError {
    NotFound,
    TempRedirect(String),
    NotAllowed,
    BadRequest,
    RangeNotSatisfied,
    // PreconditionFailed,
    // InternalServerError,
}

type PollResult<T> = Poll<Option<std::result::Result<<T as Body>::Data, <T as Body>::Error>>>;
type PollTrailersResult<T> = Poll<std::result::Result<Option<HeaderMap>, <T as Body>::Error>>;

struct PartialBody {
    file: EmbeddedFile,
    start: u64,
    end: u64,
    polled: bool,
}

impl Body for PartialBody {
    type Data = Bytes;
    type Error = axum::Error;

    fn poll_data(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> PollResult<Self> {
        if self.polled {
            return std::task::Poll::Ready(None);
        }
        let start = self.start as usize;
        let end = self.end as usize;
        let data: Bytes = match &self.file.data {
            Cow::Borrowed(d) => Bytes::from_static(&d[start..end]),
            Cow::Owned(d) => Bytes::copy_from_slice(&d[start..end]), // for rust-embed debug
        };
        std::task::Poll::Ready(Some(Ok(data)))
    }

    fn poll_trailers(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> PollTrailersResult<Self> {
        std::task::Poll::Ready(Ok(None))
    }

    fn size_hint(&self) -> http_body::SizeHint {
        let mut size_hint = SizeHint::new();
        size_hint.set_exact(self.end - self.start);
        size_hint
    }
}

impl IntoResponse for PartialBody {
    fn into_response(self) -> AxumResponse {
        http::Response::new(self.boxed_unsync())
    }
}

type Result<T> = std::result::Result<T, ServeEmbedError>;
type ServeEmbedResult = Result<ServeEmbedResponse>;
type Headers = HeaderMap<HeaderValue>;
type FoundFile = Option<(EmbeddedFile, Option<Encoding>)>;

const INDEX_HTML: &str = "index.html";

#[derive(Debug, derivative::Derivative)]
#[derivative(Clone, Default)]
pub struct ServeEmbed<E: RustEmbed> {
    base: Option<PathBuf>,
    #[derivative(Default(value = "true"))]
    append_index_html_on_directories: bool,
    not_found_fallback: Option<PathBuf>,
    // buf_chunk_size: usize,
    // precompressed_variants: Option<PrecompressedVariants>,
    // // This is used to specialise implementation for
    // // single files
    // variant: ServeVariant,
    // fallback: Option<F>,
    // call_fallback_on_method_not_allowed: bool,
    _marker: PhantomData<E>,
}

impl<E: RustEmbed> ServeEmbed<E> {
    pub fn with_base<P>(mut self, path_option: Option<P>) -> Self
    where
        P: AsRef<Path>,
    {
        self.base = path_option.map(|path| PathBuf::from(path.as_ref()));
        self
    }

    pub fn append_index_html_on_directories(mut self, append: bool) -> Self {
        self.append_index_html_on_directories = append;
        self
    }

    pub fn with_not_found_fallback<P>(mut self, path_option: Option<P>) -> Self
    where
        P: AsRef<Path>,
    {
        self.not_found_fallback = path_option.map(|path| PathBuf::from(path.as_ref()));
        self
    }

    fn handle_request(&self, method: &Method, path: &str, headers: &Headers) -> ServeEmbedResult {
        trace!("handle_request(), method={:?}, path={:?}, headers={:?}", method, path, headers);

        // validate if method allowed
        self.validate_method(method)?;

        // transform url path into target embedded file path
        let file_path = self.transform_path(path)?;
        trace!("handle_request(), file_path={:?}", file_path);

        // when range exists and original file exists, return file range as response
        if let Some(result) = self.process_range(method, headers, &file_path)? {
            return Ok(result);
        }

        // negotiate if we have the proper compressed file and return
        self.negotiate_response_content(method, headers, &file_path)

        //TODO: handle ok result and see if we can return 304 only
    }

    fn validate_method(&self, method: &Method) -> Result<()> {
        match *method {
            Method::GET | Method::HEAD => Ok(()),
            _ => Err(ServeEmbedError::NotAllowed),
        }
    }

    fn transform_path(&self, path: &str) -> Result<String> {
        trace!("transform_path(), path={}, base={:?}", path, self.base);
        // handle path ends with '/' as directory
        if path.ends_with('/') {
            return Err(match self.append_index_html_on_directories {
                true => ServeEmbedError::TempRedirect(format!("{}{}", path, INDEX_HTML)),
                false => ServeEmbedError::NotFound,
            });
        }

        // combine path with base and return
        let path = path.trim_start_matches('/');
        let target_path = match &self.base {
            None => PathBuf::from_str(path).unwrap(),
            Some(base) => {
                let mut target = base.clone();
                target.push(path);
                target
            }
        };
        trace!("transform_path(), target_path={:?}", target_path);
        target_path.into_os_string().into_string().map_err(|_| ServeEmbedError::NotFound)
    }

    fn process_range(&self, method: &Method, headers: &Headers, file_path: &str) -> Result<Option<ServeEmbedResponse>> {
        // get range from header, if empty, do nothing
        let range_header = match headers.get(RANGE) {
            Some(v) => v.to_str().map_err(|_| ServeEmbedError::BadRequest)?.to_owned(),
            None => return Ok(None),
        };
        // head method with range not accepted
        if method == Method::HEAD {
            return Err(ServeEmbedError::BadRequest);
        }
        // need original file to return range, compressed file not supported here
        let file = E::get(file_path).ok_or(ServeEmbedError::NotFound)?;
        let parsed_ranges =
            http_range_header::parse_range_header(&range_header).map_err(|_| ServeEmbedError::RangeNotSatisfied)?;
        // validate file size and range count
        let file_size_bytes = file.data.len() as u64;
        let ranges = parsed_ranges.validate(file_size_bytes).map_err(|_| ServeEmbedError::RangeNotSatisfied)?;
        if ranges.len() != 1 {
            // only one range supported
            return Err(ServeEmbedError::RangeNotSatisfied);
        }
        // return file and range
        let (start, end) = (*ranges[0].start(), *ranges[0].end());
        Ok(Some(ServeEmbedResponse::Partial { file, start, end }))
    }

    fn negotiate_response_content(&self, method: &Method, headers: &Headers, file_path: &str) -> ServeEmbedResult {
        // get accept encodings from headers
        let mut encodings = fly_accept_encoding::encodings(headers).map_err(|_| ServeEmbedError::BadRequest)?;
        encodings.sort_by(|(_, q1), (_, q2)| q1.partial_cmp(q2).unwrap());
        // iterate encodings, try to find proper file, and give a last try with original path
        let file_found = match encodings
            .iter()
            .rev()
            .find_map(|(encoding_option, _)| self.find_proper_file(file_path, encoding_option))
        {
            Some(found) => Some(found),
            None => E::get(file_path).map(|f| (f, None)),
        };
        // generate response
        match file_found {
            Some((file, encoding)) => {
                let origin = match encoding {
                    Some(_) => E::get(file_path),
                    None => None,
                };
                Ok(ServeEmbedResponse::Full { file, encoding, origin, head_only: method == &Method::HEAD })
            }
            None => Err(ServeEmbedError::NotFound),
        }
    }

    fn find_proper_file(&self, file_path: &str, encoding_option: &Option<Encoding>) -> FoundFile {
        macro_rules! find_file {
            () => {
                return E::get(file_path).map(|f| (f, encoding_option.clone()))
            };
            ($fmt: expr) => {
                return E::get(&format!($fmt, file_path)).map(|f| (f, encoding_option.clone()))
            };
        }
        macro_rules! try_find_file {
            ($fmt: expr) => {
                if let Some(f) = E::get(&format!($fmt, file_path)) {
                    return Some((f, encoding_option.clone()));
                }
            };
        }
        match encoding_option {
            &Some(Encoding::Gzip) => find_file!("{}.gz"),
            &Some(Encoding::Deflate) => find_file!("{}.zz"),
            &Some(Encoding::Brotli) => find_file!("{}.br"),
            &Some(Encoding::Zstd) => find_file!("{}.zst"),
            &Some(Encoding::Identity) => find_file!(),
            None => {
                try_find_file!("{}.gz");
                try_find_file!("{}.zz");
                try_find_file!("{}.br");
                try_find_file!("{}.zst");
                find_file!();
            }
        }
    }

    fn generate_response(&self, result: ServeEmbedResult) -> AxumResponse {
        use ServeEmbedError::*;
        use ServeEmbedResponse::*;
        match result {
            Err(NotFound) => self.generate_error_response(StatusCode::NOT_FOUND),
            Err(TempRedirect(location)) => self.generate_redirect_response(&location),
            Err(NotAllowed) => self.generate_error_response(StatusCode::METHOD_NOT_ALLOWED),
            Err(BadRequest) => self.generate_error_response(StatusCode::BAD_REQUEST),
            Err(RangeNotSatisfied) => self.generate_error_response(StatusCode::RANGE_NOT_SATISFIABLE),
            Ok(Partial { file, start, end }) => self.generate_partial_content_response(file, start, end),
            Ok(Full { file, encoding, origin, head_only }) => {
                self.generate_full_response(file, &encoding, origin, head_only)
            } // _ => self.generate_error_response(StatusCode::INTERNAL_SERVER_ERROR),
        }
    }

    fn generate_fallback_response(&self, path: &Path) -> AxumResponse {
        // get file name
        let target_path = match self.transform_path(path.to_str().unwrap()) {
            Ok(p) => p,
            Err(_) => return self.generate_error_response(StatusCode::NOT_FOUND),
        };
        // return NOT_FOUND with file data
        if let Some(file) = E::get(&target_path) {
            let mut headers = HeaderMap::new();
            headers.append(CONTENT_TYPE, file.metadata.mimetype().parse().unwrap());
            return (StatusCode::NOT_FOUND, headers, file.data).into_response();
        }
        // when file not found, return status code only
        self.generate_error_response(StatusCode::NOT_FOUND)
    }

    fn generate_full_response(
        &self,
        f: EmbeddedFile,
        encoding_option: &Option<Encoding>,
        origin: Option<EmbeddedFile>,
        head_only: bool,
    ) -> AxumResponse {
        // complete headers
        let mut headers = HeaderMap::new();
        let mimetype = match origin {
            Some(origin_file) => origin_file.metadata.mimetype().parse(),
            None => f.metadata.mimetype().parse(),
        };
        headers.append(CONTENT_TYPE, mimetype.unwrap());
        if let Some(encoding) = encoding_option.filter(|e| *e != Encoding::Identity) {
            headers.append(CONTENT_ENCODING, encoding.to_header_value());
        }
        if let Some(time) = f.metadata.last_modified() {
            use std::{
                ops::Add,
                time::{Duration, SystemTime},
            };
            let last_modified = httpdate::HttpDate::from(SystemTime::UNIX_EPOCH.add(Duration::from_secs(time)));
            headers.append(LAST_MODIFIED, last_modified.to_string().parse().unwrap());
        }
        // when request method is not HEAD (means it's GET), return headers with file data
        if !head_only {
            return (headers, f.data).into_response();
        }
        // return headers only
        headers.append(CONTENT_LENGTH, f.data.len().into());
        headers.into_response()
    }

    fn generate_partial_content_response(&self, f: EmbeddedFile, start: u64, end: u64) -> AxumResponse {
        let mut headers = HeaderMap::new();
        let content_range = format!("bytes {}-{}/{}", start, end, f.data.len());
        headers.append(CONTENT_RANGE, content_range.parse().unwrap());

        if let Some(time) = f.metadata.last_modified() {
            use std::{
                ops::Add,
                time::{Duration, SystemTime},
            };
            let last_modified = httpdate::HttpDate::from(SystemTime::UNIX_EPOCH.add(Duration::from_secs(time)));
            headers.append(LAST_MODIFIED, last_modified.to_string().parse().unwrap());
        }

        let body = PartialBody { file: f, start, end, polled: false }.boxed_unsync();
        (StatusCode::PARTIAL_CONTENT, headers, body).into_response()
    }

    fn generate_redirect_response(&self, location: &str) -> AxumResponse {
        let mut headers = HeaderMap::new();
        headers.append(LOCATION, location.parse().unwrap());
        (StatusCode::TEMPORARY_REDIRECT, headers).into_response()
    }

    fn generate_error_response(&self, status_code: StatusCode) -> AxumResponse {
        (status_code).into_response()
    }

    // fn generate_ranged_content_response()
}

impl<ReqBody, E: RustEmbed> Service<Request<ReqBody>> for ServeEmbed<E> {
    type Response = AxumResponse;
    type Error = Infallible;
    type Future = std::future::Ready<std::result::Result<AxumResponse, Infallible>>;

    fn poll_ready(
        &mut self,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::result::Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: Request<ReqBody>) -> Self::Future {
        let method = req.method();
        let path = req.uri().path();
        let headers = req.headers();
        trace!("path={}", path);

        let result = self.handle_request(method, path, headers);
        trace!("handle_request() result={:?}", result);

        use ServeEmbedError::NotFound;
        let response = match (&result, method, &self.not_found_fallback) {
            (&Err(NotFound), &Method::GET, Some(path)) => self.generate_fallback_response(path.as_ref()),
            (_, _, _) => self.generate_response(result),
        };
        std::future::ready(Ok(response))
    }
}
