//! Defines a rellog Knowledge Base ('kb').

use std::{
    collections::{BTreeMap, BTreeSet},
    io::Read,
    path::Path,
};

use crate::{
    ast::{self, Clause, Rel, Sig},
    lex, parse,
};

use super::{err, UnifierSet};

#[derive(Debug, Default)]
pub struct KnowledgeBase {
    pub directives: Vec<Rel>,
    pub relations: BTreeMap<Sig, Vec<Clause>>,
}

impl KnowledgeBase {
    pub fn include(&mut self, path: impl AsRef<Path>) -> Result<(), err::Err> {
        let src = {
            let mut f = std::fs::File::open(path.as_ref())?;
            let mut src = String::new();
            f.read_to_string(&mut src)?;
            src
        };

        let mut tok_buf = Vec::new();
        let tokens = lex::tokenize_into(&mut tok_buf, src.as_ref(), path.as_ref().into())?;
        let m = parse::entire_module(tokens)?;

        self.import(m);

        Ok(())
    }

    pub fn import(&mut self, other: impl Into<KnowledgeBase>) {
        // TODO: detect name clashes
        let mut other = other.into();
        self.directives.append(&mut other.directives);
        self.relations.append(&mut other.relations);
    }

    /// Returns an `ExactSizeIterator` of all the clauses that could match the given Rel.
    /// Eventually this ought to perform smart argument-indexing.
    /// If the relation does not exist, `None` is returned.
    pub fn index_match<'m>(
        &'m self,
        query_head: &Rel,
    ) -> Option<impl ExactSizeIterator<Item = &'m Clause> + 'm> {
        let sig = query_head.keys().cloned().collect();
        let sig_based_index = self.relations.get(&sig).map(|clauses| clauses.iter())?;
        let arg_indexed = sig_based_index
            .filter(|clause| {
                let u = UnifierSet::new();
                let clause_head = clause.head.clone().into();
                let query_head = query_head.clone().into();
                u.unify(&clause_head, &query_head).is_some()
            })
            .collect::<BTreeSet<_>>();

        Some(arg_indexed.into_iter())
    }

    pub fn clear(&mut self) {
        self.directives.clear();
        self.relations.clear();
    }
}

impl From<ast::Module> for KnowledgeBase {
    fn from(m: ast::Module) -> Self {
        Self {
            directives: m.directives,
            relations: m.relations,
        }
    }
}
