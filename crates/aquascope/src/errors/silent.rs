use rustc_driver::DEFAULT_LOCALE_RESOURCES;
use rustc_errors::fallback_fluent_bundle;
use rustc_session::parse::ParseSess;

pub fn silent_session() -> Box<dyn FnOnce(&mut ParseSess) + Send> {
  Box::new(|sess| {
    let fallback_bundle =
      fallback_fluent_bundle(DEFAULT_LOCALE_RESOURCES.to_vec(), false);
    sess.dcx().make_silent(fallback_bundle, None, false);
  })
}
