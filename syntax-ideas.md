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
    | Elems = {X ...Xs}
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


> [operator "in"][precedence 342]

X in {Y ...Ys} if
    | X = Y
    | X in Ys

```
