
:- module(rellog_shim, [
    '[eq]'/1,
    '[attrs][rel]'/2,
    '[attr][key][value]'/3,
    '[rel][sig]'/2,
    '[gt][lt]'/2,
    '[gte][lte]'/2,
    '[must_be_var]'/1,
    '[must_be_num]'/1,
    '[must_be_sym]'/1,
    '[must_be_txt]'/1,
    '[must_be_rel]'/1,
    '[key][rel][value]'/3,
    '[term][variables]'/2,
    '[duplicate][original]'/2,
    '[duplicate][original][renamed][renaming]'/4,
    '[pascal_case][snake_case]'/2,
    '[sum][x][y]'/3,
    '[product][x][y]'/3,
    '[difference][minuend][subtrahend]'/3,
    '[denominator][numerator][quotient][remainder]'/4,
    '[pred][succ]'/2,
    '[true]'/1,
    '[false]'/1,
    '[not]'/1,
    '[goal][truth]'/2,
    '[io_write][stream]'/2,
    '[io_writeln][stream]'/2,
    '[term][text]'/2,
    '[block][functor][members]'/3,
    '[cwd]'/1,
    '[cd]'/1,
    '[ls]'/1,
    '[recursion_limit]'/1,
    '[directive]'/1,
    '[directives]'/1,
    '[clause_body][clause_head]'/2,
    '[class][tm]'/2,
    '[reified_tm_out][tm_in]'/2,
    '[clauses][directives][file_path]'/3,
    '[raises]'/1,
    '[builtins]'/1
]).

:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics), []).
:- use_module(library(dcg/high_order), [sequence//5]).

'[eq]'([]).
'[eq]'([X, X | Xs]) :-
    '[eq]'(Xs).

:- begin_tests('[eq]').
test(first) :- '[eq]'([]).
test(second) :- '[eq]'([a, a, a, a, a, a, a, a, a, a]).
test(third, [fail]) :- '[eq]'([a, b, c, d, e, f, g, h, i, j]).
:- end_tests('[eq]').

csym(Name, Head, Tail) :-
    nonvar(Name),
    format(codes(Head, Tail), '~w', [Name]).
csym(Name) -->
    [F], {code_type(F, csymf)},
    csyms(Rest),
    { atom_codes(Name, [F|Rest]) }.

csyms([H|T]) -->
    [H], {code_type(H, csym)},
    !,
    csyms(T).
csyms([]) -->
    "".

'[attrs][rel]'(Attrs, Rel) :-
    (
        Rel =.. [Functor | Args],
        % Parse the Rellog functor:
        atom_chars(Functor, FChars),
        phrase((
            % sequence(:Start, :Element, :Sep, :End, ?List)//
            sequence("[", csym, "][", "]", Keys)
        ), FChars),
        maplist(
            [Key, Value, '[key][value]'(Key, Value)]>>true,
            Keys,
            Args,
            Attrs
        )
    )
    -> true
    ; throw(error(invalid_rellog_functor, Rel)).
    
    
:- begin_tests('[attrs][rel]').
test(first, [error(_)]) :- '[attrs][rel]'([], '[]'()).
test(second) :- '[attrs][rel]'(['[key][value]'(single, 1)], '[single]'(1)).
test(third) :-
        '[attrs][rel]'(
            ['[key][value]'(my, 1), '[key][value]'(cool, 2), '[key][value]'(rel, 3)],
            '[my][cool][rel]'(1,2,3)
        ).
test(fourth, [error(_)]) :- '[attrs][rel]'(_, regular_prolog_predicate(1,2)).
:- end_tests('[attrs][rel]').


'[attr][key][value]'(Attr, Key, Value) :-
    '[attrs][rel]'(SingletonList, Attr),
    (
        SingletonList = ['[key][value]'(Key, Value)]
    ->  true
    ;   throw(error(multi_key_rel_is_not_an_attr, Attr))
    ).



:- begin_tests('[attr][key][value]').
test(first) :- '[attr][key][value]'('[a]'(1), a, 1).
test(second) :- '[attr][key][value]'('[blah]'(1234), blah, 1234).
test(third, [error(_)]) :- '[attr][key][value]'('[too][many][keys]'(1,2,3), _, _).
:- end_tests('[attr][key][value]').


'[rel][sig]'(_Rel, _Sig).


'[gt][lt]'(_Gt, _Lt).


'[gte][lte]'(_Gte, _Lte).


'[must_be_var]'(_MustBeVar).


'[must_be_num]'(_MustBeNum).


'[must_be_sym]'(_MustBeSym).


'[must_be_txt]'(_MustBeTxt).


'[must_be_rel]'(_MustBeRel).


'[key][rel][value]'(_Key, _Rel, _Value).


'[term][variables]'(_Term, _Variables).


'[duplicate][original]'(_Duplicate, _Original).


'[duplicate][original][renamed][renaming]'(_Duplicate, _Original, _Renamed, _Renaming).


'[pascal_case][snake_case]'(_PascalCase, _SnakeCase).


'[sum][x][y]'(_Sum, _X, _Y).


'[product][x][y]'(_Product, _X, _Y).


'[difference][minuend][subtrahend]'(_Difference, _Minuend, _Subtrahend).


'[denominator][numerator][quotient][remainder]'(_Denominator, _Numerator, _Quotient, _Remainder).


'[pred][succ]'(_Pred, _Succ).


'[true]'(_True).


'[false]'(_False).


'[not]'(_Not).


'[goal][truth]'(_Goal, _Truth).


'[io_write][stream]'(_IoWrite, _Stream).


'[io_writeln][stream]'(_IoWriteln, _Stream).


'[term][text]'(_Term, _Text).


'[block][functor][members]'(_Block, _Functor, _Members).


'[cwd]'(_Cwd).


'[cd]'(_Cd).


'[ls]'(_Ls).


'[recursion_limit]'(_RecursionLimit).


'[directive]'(_Directive).


'[directives]'(_Directives).


'[clause_body][clause_head]'(_ClauseBody, _ClauseHead).


'[class][tm]'(_Class, _Tm).


'[reified_tm_out][tm_in]'(_ReifiedTmOut, _TmIn).


'[clauses][directives][file_path]'(_Clauses, _Directives, _FilePath).


'[raises]'(_Raises).


'[builtins]'(_Builtins).


