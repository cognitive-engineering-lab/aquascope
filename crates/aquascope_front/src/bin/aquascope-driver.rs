fn main() {
  env_logger::init();
  rustc_plugin::driver_main(aquascope_front::AquascopePlugin);
}
