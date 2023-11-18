//! Defines a rellog Knowledge Base ('kb').

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::{self, Clause, Rel, Sig};

use super::UnifierSet;

#[derive(Debug, Default)]
pub struct KnowledgeBase {
    pub directives: Vec<Rel>,
    pub relations: BTreeMap<Sig, Vec<Clause>>,
}

impl KnowledgeBase {
    pub fn import(&mut self, other: impl Into<KnowledgeBase>) {
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
        let sig = query_head.clone().into();
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
