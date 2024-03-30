// use char_list::CharListTail;

// use crate::{
//     ast::{txt::Segment, RcTm, Tm},
//     data_structures::Var,
//     rt::{
//         self,
//         soln_stream::{self, SolnStream},
//         UnifierSet,
//     },
// };

// pub fn prefix_suffix_compound(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: &RcTm,
//     suffix: &RcTm,
//     compound: &RcTm,
// ) -> Box<dyn SolnStream> {
//     use VarOrTxt::{Txt, Var};
//     match (prefix.into(), suffix.into(), compound.into()) {
//         (Txt(pre), Txt(suf), Txt(com)) => mode_check(rel, u, pre, suf, com),
//         (Txt(pre), Txt(suf), Var(com)) => mode_concat(rel, u, pre, suf, com),
//         (Txt(pre), Var(suf), Txt(com)) => mode_drop_prefix(rel, u, pre, suf, com),
//         (Txt(pre), Var(suf), Var(com)) => mode_all_suffixes(rel, u, pre, suf, com),
//         (Var(pre), Txt(suf), Txt(com)) => mode_drop_suffix(rel, u, pre, suf, com),
//         (Var(pre), Txt(suf), Var(com)) => mode_all_prefixes(rel, u, pre, suf, com),
//         (Var(pre), Var(suf), Txt(com)) => mode_all_partitions(rel, u, pre, suf, com),
//         (Var(pre), Var(suf), Var(com)) => mode_most_general_query(rel, u, pre, suf, com),

//         _ => rt::err::Err::GenericError {
//             rel: rel.to_string(),
//             msg: "only modes supported for `[txt_prefix][txt_suffix][txt_compound]`:\n\
//                             \t[[mode txt_[prefix in][suffix  in][compound out]]]\n\
//                             \t[[mode txt_[prefix in][suffix out][compound out]]]\n\
//                             "
//             .to_string(),
//         }
//         .into(),
//     }
// }

// fn mode_check(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: Option<&TxtBlock>,
//     suffix: Option<&TxtBlock>,
//     compound: Option<&TxtBlock>,
// ) -> Box<dyn SolnStream> {
//     todo!()
//     // match (prefix, suffix, compound) {
//     //     (None, None, None) => soln_stream::success(u),
//     //     (None, None, Some(com)) => {
//     //         if com.segment_len() > 0 {
//     //             soln_stream::failure()
//     //         } else {
//     //             // mode_check(rel, u, None, None, Some(com.segment_tail().next_char_list()))
//     //         }
//     //     }
//     //     (None, Some(_), None) => todo!(),
//     //     (None, Some(_), Some(_)) => todo!(),
//     //     (Some(_), None, None) => todo!(),
//     //     (Some(_), None, Some(_)) => todo!(),
//     //     (Some(_), Some(_), None) => todo!(),
//     //     (Some(_), Some(_), Some(_)) => todo!(),
//     // }
// }

// fn mode_concat(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: Option<&TxtBlock>,
//     suffix: Option<&TxtBlock>,
//     compound: &Var,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_drop_prefix(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: Option<&TxtBlock>,
//     suffix: &Var,
//     compound: Option<&TxtBlock>,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_all_suffixes(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: Option<&TxtBlock>,
//     suffix: &Var,
//     compound: &Var,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_drop_suffix(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: &Var,
//     suffix: Option<&TxtBlock>,
//     compound: Option<&TxtBlock>,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_all_prefixes(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: &Var,
//     suffix: Option<&TxtBlock>,
//     compound: &Var,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_all_partitions(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: &Var,
//     suffix: &Var,
//     compound: Option<&TxtBlock>,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }

// fn mode_most_general_query(
//     rel: &RcTm,
//     u: UnifierSet,
//     prefix: &Var,
//     suffix: &Var,
//     compound: &Var,
// ) -> Box<dyn SolnStream> {
//     todo!()
// }
