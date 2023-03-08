use std::{ffi::OsStr, io::Write, path::Path};

fn main() {
  env_logger::Builder::from_default_env()
    .format(|buf, record| {
      let file = record.file().unwrap_or("unknown");
      let file = Path::new(file);
      let components = file.iter().collect::<Vec<_>>();
      let file_short =
        components[components.len() - 2 ..].join(OsStr::new("/"));
      writeln!(
        buf,
        "[{} {}:{} {}] - {}",
        record.level(),
        file_short.to_string_lossy(),
        record.line().unwrap_or(0),
        buf.timestamp(),
        record.args()
      )
    })
    .init();

  rustc_plugin::driver_main(aquascope_front::AquascopePlugin);
}
