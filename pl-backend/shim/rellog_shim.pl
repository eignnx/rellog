
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
:- use_module(library(clpfd)).
:- op(50, fx, #).

:- begin_tests('[eq]').
test(first) :- '[eq]'([]).
test(second) :- '[eq]'([a, a, a, a, a, a, a, a, a, a]).
test(third, [fail]) :- '[eq]'([a, b, c, d, e, f, g, h, i, j]).
:- end_tests('[eq]').

'[eq]'([]).
'[eq]'([X, X | Xs]) :-
    '[eq]'(Xs).


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
    

:- begin_tests('[attrs][rel]').
test(first, [error(_)]) :- '[attrs][rel]'([], '[]'()).
test(second) :- '[attrs][rel]'(['[key][value]'(single, 1)], '[single]'(1)).
test(third) :-
        '[attrs][rel]'(
            ['[key][value]'(my, 1), '[key][value]'(cool, 2), '[key][value]'(rel, 3)],
            '[my][cool][rel]'(1,2,3)
        ).
test(fourth, [error(invalid_rellog_functor)]) :- '[attrs][rel]'(_, regular_prolog_predicate(1,2)).
:- end_tests('[attrs][rel]').

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


:- begin_tests('[attr][key][value]').
test(first) :- '[attr][key][value]'('[a]'(1), a, 1).
test(second) :- '[attr][key][value]'('[blah]'(1234), blah, 1234).
test(third, [error(multi_key_rel_is_not_an_attr)]) :- '[attr][key][value]'('[too][many][keys]'(1,2,3), _, _).
:- end_tests('[attr][key][value]').

'[attr][key][value]'(Attr, Key, Value) :-
    '[attrs][rel]'(SingletonList, Attr),
    (
        SingletonList = ['[key][value]'(Key, Value)]
    ->  true
    ;   throw(error(multi_key_rel_is_not_an_attr, Attr))
    ).


:- begin_tests('[rel][sig]').
test(first) :- '[rel][sig]'('[asdf]'(1234), '[asdf]'(_)).
test(second) :- '[rel][sig]'('[asdf][qwerty]'(1234, 2345), '[asdf][qwerty]'(_, _)).
:- end_tests('[rel][sig]').

'[rel][sig]'(Rel, Sig) :-
    Rel =.. [Functor | RelArgs],
    length(RelArgs, Argc),
    length(SigArgs, Argc),
    Sig =.. [Functor | SigArgs].


:- begin_tests('[gt][lt]').
test(first) :- '[gt][lt]'(2, 1).
test(second, [fail]) :- '[gt][lt]'(1, 2).
test(third, [fail]) :- '[gt][lt]'(1, 1).
test(fourth) :-
    '[gt][lt]'(Big, Small),
    Big = 100,
    Small = 99.
test(fifth, [fail]) :-
    '[gt][lt]'(Big, Small),
    Big = 99,
    Small = 100.
:- end_tests('[gt][lt]').

'[gt][lt]'(Gt, Lt) :-
    #Gt #> #Lt.


:- begin_tests('[gte][lte]').
test(first) :- '[gte][lte]'(2, 1).
test(second) :- '[gte][lte]'(1, 1).
test(third, [fail]) :- '[gte][lte]'(1, 2).
test(fourth) :-
    '[gte][lte]'(Big, Small),
    Big = 100,
    Small = 99.
test(fifth) :-
    '[gte][lte]'(Big, Small),
    Big = 99,
    Small = 99.
test(sixth, [fail]) :-
    '[gte][lte]'(Big, Small),
    Big = 99,
    Small = 100.
:- end_tests('[gte][lte]').

'[gte][lte]'(Gte, Lte) :-
    #Gte #>= #Lte.


'[must_be_var]'(Term) :- var(Term).


'[must_be_num]'(Term) :- number(Term).


'[must_be_sym]'(Term) :- atom(Term).

:- begin_tests('[must_be_txt]').
:- set_prolog_flag(double_quotes, chars).
test(empty_txt) :- '[must_be_txt]'("").
test(single_char_txt) :- '[must_be_txt]'("a").
test(longer_txt) :- '[must_be_txt]'("abc").
test(includes_non_char, [fail]) :- '[must_be_txt]'([a,b,3]).
test(partial_string, [fail]) :- '[must_be_txt]'([a, b, c | _]).
:- end_tests('[must_be_txt]').

'[must_be_txt]'(Term) :-
    \+ var(Term),
    (
        Term = []
    -> 
        true
    ;
        Term = [C | Cs],
        '[must_be_sym]'(C),
        '[must_be_txt]'(Cs)
    ).


:- begin_tests('[must_be_rel]').
test(var, [fail]) :- '[must_be_rel]'(X).
test(empty_rel, [error(_)]) :- '[must_be_rel]'('[]'()).
test(single_key_rel) :- '[must_be_rel]'('[a]'(1)).
test(multi_key_rel) :- '[must_be_rel]'('[a][b][c]'(1,2,3)).
test(missing_key, [error(invalid_rellog_functor)]) :- '[must_be_rel]'('[a][b][c]'(1,2)).
test(extra_key, [error(invalid_rellog_functor)]) :- '[must_be_rel]'('[a][b][c]'(1,2,3,4)).
test(non_rel, [error(invalid_rellog_functor)]) :- '[must_be_rel]'(my_predicate(1)).
:- end_tests('[must_be_rel]').

'[must_be_rel]'(Term) :-
    \+ var(Term),
    Term =.. [Functor | Args],
    length(Args, Arity),
    '[attrs][rel]'(Attrs, Term),
    length(Attrs, Arity).



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


