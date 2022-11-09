use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use reedline::{FileBackedHistory, PromptEditMode, PromptViMode, Reedline};

mod highlighter;
mod prompt;
mod validator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplMode {
    TopLevel,
    PrintingSolns,
}

pub struct RellogReplConfig {
    repl_mode: ReplMode,
    prompt_edit_mode: PromptEditMode,
}

impl Default for RellogReplConfig {
    fn default() -> Self {
        Self {
            repl_mode: ReplMode::TopLevel,
            prompt_edit_mode: PromptEditMode::Vi(PromptViMode::Insert),
        }
    }
}

#[derive(Clone, Default)]
pub struct RellogReplConfigHandle(Arc<RwLock<RellogReplConfig>>);

impl RellogReplConfigHandle {
    pub fn create_editor(&self) -> reedline::Reedline {
        Reedline::create()
            .with_history(Box::<FileBackedHistory>::default())
            .with_validator(Box::new(self.clone()))
            .with_highlighter(Box::new(self.clone()))
            .with_edit_mode(Box::<reedline::Vi>::default())
    }

    pub fn set_repl_mode(&self, mode: ReplMode) {
        self.write().unwrap().repl_mode = mode;
    }

    fn read(
        &self,
    ) -> Result<
        RwLockReadGuard<'_, RellogReplConfig>,
        PoisonError<RwLockReadGuard<'_, RellogReplConfig>>,
    > {
        self.0.read()
    }

    fn write(
        &self,
    ) -> Result<
        RwLockWriteGuard<'_, RellogReplConfig>,
        PoisonError<RwLockWriteGuard<'_, RellogReplConfig>>,
    > {
        self.0.write()
    }
}

impl From<RellogReplConfig> for RellogReplConfigHandle {
    fn from(cfg: RellogReplConfig) -> Self {
        Self(Arc::new(RwLock::new(cfg)))
    }
}
