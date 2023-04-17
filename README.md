# serve_embed

Serve embedded file with axum

```rust
#[derive(rust_embed::RustEmbed)]
#[folder = "target_folder"]
struct EmbedFiles;

let addr: std::net::SocketAddr = "127.0.0.1:8080".parse().unwrap();
let service = serve_embed::ServeEmbed::<EmbedFiles>::default().append_index_html_on_directories(true);
let app = axum::Router::new().fallback_service(service);
axum::Server::bind(&addr).serve(app.into_make_service()).await.unwrap();
```

Please check #[example/hello.rs](./examples/hello.rs) for the complete, working example.