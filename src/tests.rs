use super::*;
use axum::Router;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};

const WWW1_INDEX_HTML: &str = include_str!("../test-fixtures/www1/index.html");
const WWW1_INDEX_CSS: &str = include_str!("../test-fixtures/www1/index.css");
const WWW1_404_HTML: &str = include_str!("../test-fixtures/www1/404.html");

#[derive(rust_embed::RustEmbed)]
#[folder = "test-fixtures"]
struct TestFixtures;

fn init_logger() {
    // let _ = env_logger::Builder::from_env("serve_embed").is_test(true).default_format().try_init();
    // let _ = env_logger::builder().is_test(true).try_init();
    let mut builder = env_logger::Builder::new();
    builder.is_test(true);
    // builder.filter_level(log::LevelFilter::Trace);
    builder.filter(Some("serve_embed"), log::LevelFilter::Trace);
    let _ = builder.try_init();
}

async fn start_test_server(addr: &SocketAddr, base: &str) -> tokio::sync::oneshot::Sender<()> {
    trace!("start_test_server()");
    let service = ServeEmbed::<TestFixtures>::default()
        .append_index_html_on_directories(true)
        .with_base(Some(base))
        .with_not_found_fallback(Some("404.html"));
    let app = Router::new().fallback_service(service);
    let (tx, rx) = tokio::sync::oneshot::channel::<()>();
    let addr = addr.clone();
    tokio::spawn(async move {
        axum::Server::bind(&addr)
            .serve(app.into_make_service())
            .with_graceful_shutdown(async {
                rx.await.ok();
            })
            .await
            .unwrap();
    });
    tx
}

async fn test_www1_index(addr: &SocketAddr) {
    // full index.html
    let url = format!("http://{}", addr);
    let result = reqwest::get(&url).await;
    assert!(result.is_ok(), "Failed to get index.html, no append?");
    let content = result.unwrap().text().await.unwrap();
    assert_eq!(&content, WWW1_INDEX_HTML, "Failed to compare full index.html response");

    // with range from 0 to range_end
    let range_end = 5;
    let range_str = format!("bytes=0-{}", range_end);
    let mut headers = reqwest::header::HeaderMap::new();
    headers.insert(reqwest::header::RANGE, reqwest::header::HeaderValue::from_str(&range_str).unwrap());
    let builder = reqwest::Client::new().get(&url).headers(headers);
    let result = builder.send().await;
    assert!(result.is_ok(), "Failed to get index.html with range!");
    let content = result.unwrap().text().await.unwrap();
    assert_eq!(&content, &WWW1_INDEX_HTML[0..range_end], "Failed to compare partial index.html response");
}

async fn test_www1_index_css(addr: &SocketAddr) {
    // gzip version index.css
    let url = format!("http://{}/index.css", addr);
    let mut headers = reqwest::header::HeaderMap::new();
    headers.insert(
        reqwest::header::ACCEPT_ENCODING,
        reqwest::header::HeaderValue::from_str("gzip;q=1.0, *;q=0.5").unwrap(),
    );
    let builder = reqwest::Client::new().get(&url).headers(headers);
    let result = builder.send().await;
    assert!(result.is_ok(), "Failed to get index.css in gzip!");
    // ensure content type to be same with original file
    let result = result.unwrap();
    let content_type = result.headers().get(reqwest::header::CONTENT_TYPE);
    assert_eq!(content_type, Some(&HeaderValue::from_static("text/css")), "Invalid content encoding after gzipped");
    // extract gzipped data
    use flate2::bufread::GzDecoder;
    use std::io::prelude::*;
    let data = result.bytes().await.unwrap().to_vec();
    let mut decoder = GzDecoder::new(&data[..]);
    let mut s = String::new();
    decoder.read_to_string(&mut s).unwrap();
    assert_eq!(&s, WWW1_INDEX_CSS, "Failed to compare extracted index.css with original");
}

async fn test_www1_404_fallback(addr: &SocketAddr) {
    let url = format!("http://{}/not_exists_file.html", addr);
    let result = reqwest::get(&url).await;
    assert!(result.is_ok(), "Failed to get 404 response!");
    let result = result.unwrap();
    assert_eq!(result.status(), StatusCode::NOT_FOUND, "Failed to get status code 404!");
    let content = result.text().await.unwrap();
    assert_eq!(&content, WWW1_404_HTML, "Failed to get 404.html in response content!");
}

#[tokio::test]
async fn test_serve_embed() -> std::result::Result<(), anyhow::Error> {
    init_logger();
    trace!("test_serve_embed()");
    let addr = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8080);
    let tx = start_test_server(&addr, "www1").await;

    test_www1_index(&addr).await;
    test_www1_index_css(&addr).await;
    test_www1_404_fallback(&addr).await;

    // tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
    let _ = tx.send(());
    Ok(())
}
