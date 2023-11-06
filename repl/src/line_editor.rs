use std::cell::RefCell;

use reedline::{Reedline, Signal};

use crate::line_editor_config::{RellogReplConfigHandle, ReplMode};

pub struct LineEditor {
    repl_config: RefCell<RellogReplConfigHandle>,
    line_editor: RefCell<Reedline>,
}

impl LineEditor {
    pub fn new() -> Self {
        let repl_config = RellogReplConfigHandle::default();
        let line_editor = RefCell::new(repl_config.create_editor());
        Self {
            line_editor,
            repl_config: RefCell::new(repl_config),
        }
    }

    pub fn set_repl_mode(&self, mode: ReplMode) {
        self.repl_config.borrow_mut().set_repl_mode(mode);
    }

    pub fn read_line(&self) -> Result<Signal, impl std::error::Error> {
        let dyn_prompt = self.repl_config.borrow();
        self.line_editor.borrow_mut().read_line(&*dyn_prompt)
    }
}
