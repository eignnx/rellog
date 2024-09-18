:- module(lang_shim, ['$block'/2]).

'$block'('-', []).
'$block'('-', [A|As]) :-
    call(A),
    '$block'('-', As).

'$block'('|', [A|As]) :-
    call(A) ; '$block'('|', As).
