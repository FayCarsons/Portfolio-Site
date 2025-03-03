use std::num::NonZero;

use actix_files::Files;
use actix_web::{middleware, App, HttpServer};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("Info"));

    HttpServer::new(move || {
        App::new()
            .wrap(middleware::Compress::default())
            .wrap(middleware::Logger::default())
            .wrap(middleware::DefaultHeaders::new().add(("Cache-Control", "public, max-age=3600")))
            .service(
                Files::new("/var/www/faycarsons", "./")
                    .index_file("index.html")
                    .use_etag(true)
                    .prefer_utf8(true)
                    .disable_content_disposition(),
            )
    })
    .backlog(1024)
    .workers(
        std::thread::available_parallelism()
            .map(|n| n.into())
            .unwrap_or(4usize),
    )
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
