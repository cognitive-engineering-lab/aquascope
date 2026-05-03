use rustc_session::parse::ParseSess;

pub fn silent_session() -> Box<dyn FnOnce(&mut ParseSess) + Send> {
  Box::new(|sess| {
    sess.dcx().make_silent();
  })
}
