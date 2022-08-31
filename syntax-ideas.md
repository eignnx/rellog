```ruby

_ :: male <<
    james1
    charles1
    charles2
    james2
    george1

_ :: female <<
    catherine
    elizabeth
    sophia

[Parent][Child] <<
    charles1 james1
    elizabeth james1
    charles2 charles1
    catherine charles1
    james2 charles1
    sophia elizabeth
    george1 sophia

[Mother][Child] if
    - [parent Mother][Child]
    - Father :: female

[Father][Child] if
    - [parent Father][Child]
    - Father :: male

[Parent][Child] if
    | [mother Parent][Child]
    | [father Parent][Child]

[Ancestor][Descendent] if
    | [parent Ancestor][child Descendent]
    |   - [parent Parent][child Descendent]
        - [Ancestor][descendent Parent]

[Pred][List][Sublist] if
    [goal include_impl[List][Pred]][initial Sublist][final {}]

[Pred: rel{T}][List: {T}][Sublist: {T}] if
    [goal include_impl[List][Pred]][initial Sublist][final {}]

[Pred :: rel{T}][List :: {T}][Sublist :: {T}] if
    [goal include_impl[List][Pred]][initial Sublist][final {}]

include_impl[List {}][Pred] -> {}

include_impl[list {L ...Ls} :: {T}][Pred :: rel{T}] ->
    [cond [rel Pred][params {L}]][then L][else {}]
    include_impl[Ls][Pred]

[list {}][length 0]
[list {_ ...Xs}][length [incr N]] if
    [list Xs][length N]


[x :: Int][y :: Int][sum :: Int] modes
    - {A, B, C} ins {in, out}
    - [not {A, B, C} are_all {X}^(X != out)]
    - [x :: A][y :: B][sum :: C]

[x 0][Y][sum Y]
[x [incr X]][Y][sum [incr Z]] if
    [X][Y][sum Z]

# maplist/2
[Rel][Rows] if
    | Rows = {}
    |   - Rows = {R ...Rs}
        - [Rel][params {R}]
        - [Rel][rows Rs]

[Rel][rows {}]
[Rel][rows {Row ...Rows}] if
    - [Rel][params {Row}]
    - [Rel][Rows]

[Rel][Elems] if
    | Elems = {}
    |   - Elems = {X ...Xs}
        - [Rel][params {X}]
        - [Rel][elems Xs]

[[operator "are_all"][precedence 123]]

{} are_all Rel
{X ...Xs} are_all Rel if
    - [Rel][params {X}]
    - Xs are_all Rel

# maplist/3
[Rel2][list1 {}][list2 {}]
[Rel2][list1 {A ...As}][list2 {B ...Bs}] if
    - [rel Rel2][params {A, B}]
    - [Rel2][list1 As][list2 Bs]

# maplist/4
[Rel3][list1 {}][list2 {}][list3 {}]
[Rel3][list1 {A ...As}][list2 {B ...Bs}][list3 {C ...Cs}] if
    - [rel Rel3][params {A, B, C}]
    - [Rel3][list1 As][list2 Bs][list3 Cs]

[elementwise Application] if
    - [Application][Functor][Attrs]
    - Functor = [x][y][sum]
    - Attrs = {
        [x {1, 2, 3}],
        [y {4, 5, 6}],
        [Sum],
    }
    - Matrix = {
        [x 1][y 4][sum Sum0],
        [x 2][y 5][sum Sum1],
        [x 3][y 6][sum Sum2],
    }
    - 

local.[elementwise]


>>> [elementwise [x {1, 2, 3}][y {4, 5, 6}][Sum]]
    Sum = {5, 7, 9}

### Destructures/Constructs a "block" from a functor and list of elements
[Functor][Elements][Block]

>>> - [select Nat][as Nats][where [Nat] and [perfect Nat]]
    - [all Nats][satisfy [prime]]

>>> - [select {A, B}][as Pairs][where [nat A] and B in [from a][through z]]
    - Pairs all_sat divides
>>> [rel [x][y][sum]][Arity]
    Arity = 3

>>> [rel3 [x][y][sum]][list1 {1, 2, 3}][list2 {4, 5, 6}][list3 What]
    What = {5, 7, 9}

>>> [rel2 [x][y 999][sum]][list1 {1, 2, 3}][list2 What]
    What = {1000, 1001, 1002}

#  {TcxVars}/[X]>>maplist(\==(X), TcxVars)
?- [params {X}][relation [rel1 (_ != X)][list TcxVars]][captured {TcxVars}]
?- [params {X}][relation [rel1 (_ != X)][list TcxVars]]

#  include({TcxVars}/[X]>>maplist(\==(X), TcxVars), TyVars, Vs0),
?- [pred [params {X}][captured {TcxVars}][relation
        [rel1 (X!=_)][list TcxVars] # Yikes, prolly need another nested lambda
    ]][list TyVars][sublist Vs0]

[prefix {}][Suffix][compound Suffix]
[prefix {A ...As}][Suffix][Compound] if
    [prefix As][suffix {A ...Suffix}][Compound]

# reverse([],[]).
# reverse([H|T], RevList):-
#     reverse(T, RevT), append(RevT, [H], RevList).
[forwards {}][backwards {}]
[forwards {A ...As}][Backwards] if
    - [forwards As][backwards AsBackwards]
    - [prefix AsBackwards][suffix {A}][compound Backwards]


[[operator "in"][precedence 342]]

X in {Y ...Ys} if
    | X = Y
    | X in Ys

[forwards {A ...As}][Backwards]
    , [forwards As][backwards AsBackwards]
    , [prefix AsBackwards][suffix {A}][compound Backwards]

[forwards {A ...As}][Backwards]
    [forwards As][backwards AsBackwards],
    [prefix AsBackwards][suffix {A}][compound Backwards],

[forwards {A ...As}][Backwards] ([forwards As][backwards AsBackwards], [prefix AsBackwards][suffix {A}][compound Backwards])

```

## Block Intro Symbols

Ideas:

1. Any `symbol` that begins with `-` or `|`
    - Examples: `-`, `|`, `-^`, `|^`, `-&`
1. Begin with `-`, `+`, `*`
1. Any punctuation except `:` (and probably `,` and `.`)
    - This allows for infix operators to use `:` (i.e. `::`)
    - Maybe all infix operators must contain `:` or `.`
        * Examples: `::`, `.gt.`, `.gt`, `:gt`

### Block Ideas

What if a block is sugar for:

```
<tm1> <block-functor> "{" ( <tm2>, )+ "}" --> <tm1> ( <block-functor> <tm2> )+
```

No this doesn't make sense.

How much do I care about homoiconicity?

*Just define up-front which functors can be infix and which can be block functors.*
*And have syntax sugar so that the `if` can be omitted*

## Definite Clause Grammars

Current idea: use template strings.

```python
[story]
    """
    When the old sailor finally spoke, he whispered only this: "[sentence]"
    """

[sentence]
    "[noun_phrase] [verb_phrase]."

[noun_phrase]
    "[determiner] [noun]"

[determiner]
    | "the"
    | "my"
    | "our"

[noun]
    | "cabin boy"
    | "first mate"
    | "secret"

[verb_phrase]
    "[verb] [noun_phrase]"

[verb]
    | "knew"
    | "didn't believe"
```

### Translation into DCGs

```python
[sentence]
    "[noun_phrase] [verb_phrase]."
```

in prolog is

```prolog
sentence --> noun_phrase, " ", verb_phrase, ".".
```

which desugars to

```prolog
sentence(S0, S) :-
    noun_phrase(S0, S1),
    append(S1, " ", S2),
    verb_phrase(S2, S3),
    append(S3, ".", S).
```

Translating that into rellog gives

```python
[sentence][before S0][after S]
    - [noun_phrase][before S0][after S1]
    - [prefix S1][suffix " "][compound S2]
    - [verb_phrase][before S2][after S3]
    - [prefix S3][suffix "."][compound S]
```

### Example with Braced Goals

```prolog
positives([]) --> [].
positives([A|As]) --> {A > 0}, [A], positives(As).
```

which gets desugared to

```prolog
positives([], S0, S) :-
    append(S0, [], S).
positives([A|As], S0, S) :-
    A > 0,
    append(S0, [A], S1),
    positives(As, S1, S).
```

```ruby
[positives {}] {}

[positives {A ...As}]
    - [unquoted A > 0]
    - {A}
    - [positives As]
```

which gets desugared to

```ruby
[positives {}][before S0][after S]
    [prefix S0][suffix {}][compound S]

[positives {A ...As}][before S0][after S]
    - A > 0
    - [prefix S0][suffix {A}][compound S1]
    - [positives As][before S1][after S]
```

Alternative names for `unquoted`:
- provided
- when
- condition
- call

If you don't wanna define it in the compiler, define it like this:

```ruby
[unquoted R][before S][after S]
    R
```
