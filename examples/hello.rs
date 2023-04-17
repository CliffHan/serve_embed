extern crate serve_embed;

#[derive(rust_embed::RustEmbed)]
#[folder = "test-fixtures/www1"]
struct TestFixtures;

#[tokio::main]
async fn main() {
    const SOCKET_ADDR: &str = "127.0.0.1:8080";
    let addr: std::net::SocketAddr = SOCKET_ADDR.parse().unwrap();
    let service = serve_embed::ServeEmbed::<TestFixtures>::default().append_index_html_on_directories(true);
    let app = axum::Router::new().fallback_service(service);
    let (tx, mut rx) = tokio::sync::mpsc::channel::<()>(1);
    ctrlc::set_handler(move || {
        let _ = tx.blocking_send(());
    })
    .unwrap();
    println!("serving at http://{}", SOCKET_ADDR);
    println!("press ctrl+c to stop server");
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(async move {
            let _ = rx.recv().await;
        })
        .await
        .unwrap();
}
