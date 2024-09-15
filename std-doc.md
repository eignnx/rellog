
### Builtin Relations

#### `[Rel][Attrs]`

Relates a relation to a list of its attributes. An attribute is a
`[key Value]` pair.


#### `[Attr][Key][Value]`

Relates an attribute (a `[key Value]` pair) to its key and value.


#### `[Rel][Key][Value]`

Extracts a value from a relation given the key name. Faster than searching through a list of the relation's attributes.


#### `[eq List]`

Unifies together the elements of the provided list.

```
-- [eq {A B C}]

    - A = B = C
```


#### `[Term][Variables]`

Relates a term to the list of variables contained in that term.


#### `[Original][Duplicate]`

Creates a duplicate of `Original` and unifies it with `Duplicate`.


#### `[Original][Duplicate][Renaming][Renamed]`

Creates a duplicate of `Original` and unifies it with `Duplicate`.


The variables in the list `Renaming` that appear in `Original` will be
renamed and their new names will be put in the list `Renamed`.

##### Example

```
-- [original [a A][b B][ab {A B}]
   ][Duplicate
   ][renaming {A Z}
   ][Renamed
   ]

    - Duplicate = [a A_1][B][ab {A_1 B}]
    - Renamed = {A_1 Z}
```


#### `[PascalCase][SnakeCase]`

Convert symbols between *snake_case* and *PascalCase*.


#### `[Gt][Lt]`

True if the number `Gt` is greater than the number `Lt`.


#### `[Gte][Lte]`

True if the number `Gte` is greater than or equal to the number `Lte`.


#### `[MustBeVar]`

Succeeds if its argument is an unbound variable.


#### `[MustBeNum]`

Succeeds if its argument is an instantiated number.


#### `[MustBeSym]`

Succeeds if its argument is an instantiated symbol.


#### `[MustBeTxt]`

Succeeds if its argument is an instantiated text string.


#### `[MustBeRel]`

Succeeds if its argument is an instantiated relation structure.


#### `[TxtPrefix][TxtSuffix][TxtCompound]`

Pushes a suffix onto a text string.


#### `[Pred][Succ]`

Relates two adjacent integers: a predecessor and a successor.


#### `[True]`

Always succeeds.


#### `[False]`

Always fails.


#### `[Not]`

Succeeds if the goal passed to it fails, fails if the goal succeeds at least
once. Note: This is safe because the knowledgebase is immutable.


#### `[Stream][IoWriteln]`

Writes a text string to `Stream`, and then writes a newline character.

`Stream` can be the symbol `stdin` or the symbol `stderr`.


#### `[IoWrite][Stream]`

Writes a text string to `Stream`.

`Stream` can be the symbol `stdin` or the symbol `stderr`.


#### `[Term][Text]`

Relates a term to its text string representation.


#### `[Builtins]`

Lists all builtin relations.


#### `[Cwd]`

Gives the current working directory.


#### `[Cd]`

Changes the current working directory to the path provided as an argument.


#### `[Ls]`

Unifies its argument with a list of file/directory names from the current
working directory.


#### `[Difference][Minuend][Subtrahend]`

Expresses the relationship:
$$
\text{Minuend} - \text{Subtrahend} = \text{Difference}
$$


To perform addition, use mode `[subtrahend][difference][Minuend]`.


Alias: `[Min][Sub][Dif]`

### Module `std`
Automatically included in every program unless the `--no-std-lib` flag is provided.

#### `[List][Member]`
Relates a list to a member of that list.

#### `[List][Len]`
Relates a list to it's length.

##### TODO
Mode `[len in][list out]` is broken.

#### `[List][Last]`
Relates a list to its last element.

#### `[Forwards][Backwards]`
Relates a list to its reversal.

##### TODO:
Mode `[forwards out][backwards in]` loops forever. Does it need to be an intrinsic?

#### `[MustBe]`
Requires the argument to conform to the given type test. It must be an *instantiated* value of the given type.
##### `[must_be [List]]`
`List` must be a ground list.
##### `[must_be [Num]]`
`Num` must be a ground number.

#### `[CanBe]`
Allows the argument to be unbound, but if it's bound, it must conform to the given type test.
##### `[can_be [List]]`
`List` can be a ground list or an unbound variable.
##### `[can_be [Num]]`
`Num` can be a ground number or an unbound variable.

#### `[Numerator][Denominator][Quotient][Remainder]`
Expresses the relationship: `Numerator/Denominator = Quotient (with a remainder of Remainder)`. More precisely, the relationship is `Numerator = Quotient * Denominator + Remainder`.

Can be used to express divisibility (when `Remainder` is set to 0), as well as multiplication (when `Remainder` is 0).

Alias: `[Num][Den][Quo][Rem]`

##### Common Modes
```
[num][den][Quo][Rem]   <=> quo = num / den, rem = num % den

[Num][den][quo][rem]   <=> num = den * quo + rem

[Num][den 1][quo][rem] <=> num = quo + rem

[Num][den][quo 1][rem] <=> num = den + rem

[Num][den][quo][rem 0] <=> num = den * quo

[num][Den][quo][rem]   <=> den = (num - rem) / quo

[num][den][Quo][rem]   <=> quo = (num - rem) / den

[num][Den][quo 1][rem] <=> den = num - rem

[num][den 1][Quo][rem] <=> quo = num - rem
```

#### `[FilePath][Clauses][Directives]`
Relates a rellog source file given by `FilePath` to that file's contained `Clauses` and `Directives`.

#### `[UndocumentedSig]`
Finds relations which have not been documented.

##### TODO
Kinda broken.

#### `[Goal][Truth]`
Runs `Goal`. If successful `Truth` = `true`, if it fails `Truth` = `false`, and if it errors `Truth` = `[Error]` where `Error` is the error generated.

When `Goal` errors, all subsequent solutions will be cut.

#### `[If][Then][Else]`
Runs the goal passed as the value to the `If` key, if the goal fails this relation fails, if the goal succeeds (once or more) this relation succeeds (once or more).

### Module `elementwise`

#### `[Elementwise]`
Fulfills a similar role to a list `map` function. See Prolog's [`maplist`](https://www.swi-prolog.org/pldoc/man?predicate=maplist/2) predicate for a rough idea of this relation's uses.

Use `[Splat]` and `[Scalar]` to wrap direct arguments of the relation passed in.

##### TODO
- Allow deeper nesting.

##### Examples
```
-- [elementwise
        [base
            [splat {1 2 3}]
        ][exponent
            [scalar 3]
        ][power
            [splat Out]
        ]
    ]

    - Out = {1 8 27}
```

```
-- [elementwise [pred [splat {1 2 3}]]][succ [splat Succs]]]

    - Succs = {2 3 4}
```

```
-- [elementwise [pred [splat {1 Y 3}]][succ {X 3 Z}]]

    - X = 2
    - Y = 2
    - Z = 4
```