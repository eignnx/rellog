use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use reedline::{
    default_vi_insert_keybindings, default_vi_normal_keybindings, DefaultHinter, FileBackedHistory,
    PromptEditMode, PromptViMode, Reedline,
};

mod highlighter;
mod prompt;
mod validator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplMode {
    TopLevel { debug: bool },
    PrintingSolns { debug: bool },
}

impl ReplMode {
    pub fn toggle_debug(&mut self) {
        match self {
            ReplMode::TopLevel { debug } => *debug = !*debug,
            ReplMode::PrintingSolns { debug } => *debug = !*debug,
        }
    }
}

pub struct RellogReplConfig {
    repl_mode: ReplMode,
    prompt_edit_mode: PromptEditMode,
}

impl Default for RellogReplConfig {
    fn default() -> Self {
        Self {
            repl_mode: ReplMode::TopLevel { debug: false },
            prompt_edit_mode: PromptEditMode::Vi(PromptViMode::Insert),
        }
    }
}

#[derive(Clone, Default)]
pub struct RellogReplConfigHandle(Arc<RwLock<RellogReplConfig>>);

const HISTSIZE: usize = 500;

impl RellogReplConfigHandle {
    pub fn create_editor(&self) -> reedline::Reedline {
        let hist_file = directories_next::ProjectDirs::from(
            "io.github",   /*qualifier*/
            "eignnx",      /*organization*/
            "Rellog Repl", /*application*/
        )
        .expect("valid home directory could not be located")
        .cache_dir()
        .to_path_buf()
        .join("rellog_history.txt");

        println!("[histfile_location \"{}\"]", hist_file.to_string_lossy());

        Reedline::create()
            .with_history(Box::new(
                FileBackedHistory::with_file(HISTSIZE, hist_file)
                    .expect("HISTFILE could not be created"),
            ))
            .with_hinter(Box::new(DefaultHinter::default().with_min_chars(2)))
            .with_validator(Box::new(self.clone()))
            .with_highlighter(Box::new(self.clone()))
            .with_edit_mode(Box::new(reedline::Vi::new(
                default_vi_insert_keybindings(),
                default_vi_normal_keybindings(),
            )))
    }

    pub fn set_repl_mode(&self, mode: ReplMode) {
        self.write().unwrap().repl_mode = mode;
    }

    pub fn toggle_debug_mode(&self) {
        self.write().unwrap().repl_mode.toggle_debug();
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
