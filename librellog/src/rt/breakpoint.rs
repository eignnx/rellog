use super::Rt;

pub trait Breakpoint {
    fn breakpoint(&mut self, rt: &Rt, title: &str);
}
