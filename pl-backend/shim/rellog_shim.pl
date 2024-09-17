
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
    ).
    


'[attr][key][value]'(Attr, Key, Value).


'[rel][sig]'(Rel, Sig).


'[gt][lt]'(Gt, Lt).


'[gte][lte]'(Gte, Lte).


'[must_be_var]'(MustBeVar).


'[must_be_num]'(MustBeNum).


'[must_be_sym]'(MustBeSym).


'[must_be_txt]'(MustBeTxt).


'[must_be_rel]'(MustBeRel).


'[key][rel][value]'(Key, Rel, Value).


'[term][variables]'(Term, Variables).


'[duplicate][original]'(Duplicate, Original).


'[duplicate][original][renamed][renaming]'(Duplicate, Original, Renamed, Renaming).


'[pascal_case][snake_case]'(PascalCase, SnakeCase).


'[sum][x][y]'(Sum, X, Y).


'[product][x][y]'(Product, X, Y).


'[difference][minuend][subtrahend]'(Difference, Minuend, Subtrahend).


'[denominator][numerator][quotient][remainder]'(Denominator, Numerator, Quotient, Remainder).


'[pred][succ]'(Pred, Succ).


'[true]'(True).


'[false]'(False).


'[not]'(Not).


'[goal][truth]'(Goal, Truth).


'[io_write][stream]'(IoWrite, Stream).


'[io_writeln][stream]'(IoWriteln, Stream).


'[term][text]'(Term, Text).


'[block][functor][members]'(Block, Functor, Members).


'[cwd]'(Cwd).


'[cd]'(Cd).


'[ls]'(Ls).


'[recursion_limit]'(RecursionLimit).


'[directive]'(Directive).


'[directives]'(Directives).


'[clause_body][clause_head]'(ClauseBody, ClauseHead).


'[class][tm]'(Class, Tm).


'[reified_tm_out][tm_in]'(ReifiedTmOut, TmIn).


'[clauses][directives][file_path]'(Clauses, Directives, FilePath).


'[raises]'(Raises).


'[builtins]'(Builtins).


