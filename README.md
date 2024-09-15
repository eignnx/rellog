# Rellog

It means "Relational Prolog".

## Idea

Instead of **predicates**, this implementation focuses on **relations**. A Prolog predicate can be thought of as a name and an ordered tuple of arguments, but a Rellog relation is an unordered collection of name-value pairs (called attributes).

While in Prolog you might write

```prolog
append([1, 2], [3, 4], Compound)
```

in Rellog you could write any of the following:

```ruby
[prefix {1 2}][suffix {3 4}][compound Compound]
[prefix {1 2}][suffix {3 4}][Compound] # (Argument punning for `Compound`)
[suffix {3 4}][prefix {1 2}][Compound]
[suffix {3 4}][Compound][prefix {1 2}]
[prefix {1 2}][Compound][suffix {3 4}]
[Compound][suffix {3 4}][prefix {1 2}]
[Compound][prefix {1 2}][suffix {3 4}]
```

## Example

```ruby
[prefix {}][Suffix][compound Suffix]
[prefix {First ..Rest}][Suffix][compound {First ..Compound}]
    - [prefix Rest][suffix Suffix][Compound]

[forwards {}][backwards {}]
[forwards {First ..Forwards.rest}][Backwards]
    - [Forwards.rest][Backwards.rest]
    - [prefix Backwards.rest][suffix {First}][compound Backwards]
```

Note that conjunctions are expressed as indented `-`-preceded lists.

The equivalent Prolog code would be:

```prolog
append([], Suffix, Suffix).
append([First | Rest], Suffix, [First | Compound]) :-
    append(Rest, Suffix, Compound).

reverse([], []).
reverse([First | Rest], Backwards) :-
    reverse(Rest, RestReversed),
    append(RestReversed, [First], Backwards).
```

## Goals

* Focus on relations between values rather than names of relations.
* Be easy to type, avoid symbols requiring holding the `shift` key.
* Have clean, simple syntax for grammars (DCGs).
* Have efficient string types that still feel like lists.
* Prioritize automatic choicepoint elimination rather than efficiency. This makes irrelevant many common uses of "cut."
* Avoid the trailing period problem that prolog suffers from.

![An example of a definite clause grammar being used in rellog. See examples/dcgs.rellog for code.](./examples/story-lion-bear-dcg.png)

## Installation

First download and build Rellog. You'll need a relatively recent (as of October 2023) Rust compiler. If you have Rust installed, you'll need to make sure you're using the nightly compiler.

```shell
$ git clone https://github.com/eignnx/rellog.git
$ cd rellog
$ rustup override set nightly
$ cargo build
```

It may take a few minutes to build, but afterwards you can open a REPL session:

```shell
$ cargo run
```

## Language Reference

<details>
<summary>Table of Contents</summary>

- [Overview: The Big Ideas](#overview-the-big-ideas)
- [Clauses](#clauses)
- [Rellog Values](#rellog-values)
    - [Symbols](#symbols)
    - [Integers](#integers)
    - [Text Strings](#text-strings)
    - [Variables](#variables)
    - [Relations](#relations)
    - [Operators](#operators)
    - [Lists](#lists)
    - [Blocks](#blocks)
- [Getting Help](#getting-help)
- [The REPL](#the-repl)
- [Relation Documentation](#relation-documentation)
</details>

### Overview: The Big Ideas

Rellog is all about ***relations***. A relation is a set of associations of values (kinda like a set of dictionaries each of which have the same schema).

An example of a relation might be a student-teacher relationship. In Rellog, the sentence "Gideon and Aditya are students of Dr. Blum" could be expressed as two facts about the world:

```perl
# Fact #1:
[student gideon][teacher dr_blum]

# Fact #2:
[student aditya][teacher dr_blum]
```

We refer to this relation by its ***signature*** which is written:

```yaml
[Student][Teacher]
```

A signature is a set of symbols which are the names of the relation's ***keys***.

If you're familiar with Excel, think of a relation as a ***spreadsheet***. A relation's signature then is just the spreadsheet's ***column names***.

Here's a bigger example involving multiple relations.

```clojure
[course cmpsc385][instructor dr_null]
[course cmpsc375][instructor dr_blum]
[course cmpsc439][instructor dr_na]

[course cmpsc385][name "Operating Systems"]
[course cmpsc375][name "App Development"]
[course cmpsc439][name "Compilers"]

[student gideon][course cmpsc361]
[student gideon][course cmpsc439]
[student aditya][course cmpsc375]
[student aditya][course cmpsc439]
```

This is a set of facts about three different relations:
1. The `[Course][Instructor]` relation,
1. the `[Course][Name]` relation, and
1. the `[Student][Course]` relation.

An interesting question to ask might be "who are all the students of Dr. Na?"

We can pose this question as a Rellog query in the REPL:

> Note: lines beginning with two dashes (`--`) are meant to be entered in the REPL.

```perl
-- [instructor dr_na][Course] ; [Course][Student]

    - Course = cmpsc439
    - Student = gideon # Press ENTER to search for additional solutions
|
    - Course = cmpsc439
    - Student = aditya
    # Exactly 1 solution found.
```

The system says there are two solutions to the query. In the first one, `Course` is bound to `cmpsc439` and `Student` is bound to `gideon`. Pressing `ENTER` has rellog go search for more solutions, if there are any. In this case one other solution is found.

We could even define the `[Student][Teacher]` relation in Rellog by writing a fact with a collection of conditions:

```perl
# In a .rellog file:
[Student][Teacher]
    - [instructor Teacher][Course]
    - [Course][Student]
```

This rule says:

> A student called `Student` has a teacher called `Teacher` if:
> 1. `Teacher` is the instructor of a course called `Course`, and
> 2. `Student` is a student in that same course `Course`.

Now, to ask who Gideon's teachers are we could query:

```perl
-- [student gideon][Teacher]

    - Teacher = dr_null
|
    - Teacher = dr_na
```

### Clauses

A clause is a top-level definition. A clause is either:

1. A **fact**:

    The following three facts declare that three symbols (`socrates`, `chomsky`, and `you`) refer to humans.
    ```yaml
    [human socrates]
    [human chomsky]
    [human you]
    ```
    Here's another example of a fact:
    ```yaml
    [nonempty_list {A ..B}]
    ```
1. A **rule**:

    A rule has conditions. Here's an example of a rule:
    ```yaml
    [mortal X]
        - [human X]
    ```
    This rule says "an `X` is `mortal` if that `X` is `human`.

    Lets query the `mortal` rule.
    ```perl
    -- [mortal socrates]
        - [true] # Exactly 1 solution found.
    
    -- [mortal Who]
        - Who = socrates
    
    |   - Who = chomsky
    
    |   - Who = you # Exactly 1 solution found.
    
    -- [mortal somebody_new]
        - [false] # The query has no solutions.
    ```

### Rellog Values

Rellog has the following kinds of values:

#### Symbols
- Identifiers for concepts.
- Have no meaning on their own. 
- Implemented as interned strings.

##### Examples

```yaml
socrates

my_dog

area_51

multiply

'NotAVariable'

'still 1 singular/valid @symbol'
```

#### Integers
Arbitrarily-sized integers (positive or negative whole numbers)

##### Examples
`0`, `17`, `-3`, `93740925370000349251`

#### Text Strings

- Represent textual data.
- Useful when text must be constructed or parsed.
- **Warning: Text is still a work in progress.**

##### Text Templates
Text templates allow manipulation of text in the same way that lists can be manipulated. Compare the following:

| Lists | Text |
|-------|------|
| `{1 2 3}` | `"abc"` |
| `{1 2 ..Rest}` | `"ab[{..Rest}]"` |
| `{1 X 3}` | `"a[{X}]c"` |
| `{1 X Y 4 5}` | `"a[{X Y}]de"` |

Note: You can only have one spread segment (`[{..Variable}]`) per text template, and it must appear at the very end of the literal. You can have as many character interpolation segments (like `[{Char}]`) as you like though.

##### Examples
```perl
"Hello?"

"Text strings may contain spaces and punctuation."

# The following is 1 text string literal:
"""
They can also be multi-line.

Like this!

Single linebreaks at the end of a 
line will combine the two lines into
a single paragraph.

Double linebreaks create a new paragraph.
"""

# The following text templates are equivalent:
"abc"
"a[{b}]c"
"[{a}]b[{c}]"
"[{a b c}]"
"a[{.."bc"}]"
"[{a}][{.."bc"}]"
"a[{.."b[{.."c"}]"}]"
```


#### Variables
- Must start with a capital letter or an underscore (`_`).
- Very similar to variables in *algebra*.
- Less similar to the variables in *imperative programming languages*.
- May be *known* (bound to a specific value) or *unknown* (not yet bound to a value).
- May optionally contain a period (`.`) which adds a distinguishing suffix.
    - The suffix may be a (non-negative) integer, or an (unquoted) [symbol](#symbols).

##### Examples

`X`, `ListReversed`, `Pos2D`, `_123`, `X.0`, `State.final`

###### Suffixes

Two variables with the same prefix but different suffixes (for example `S.0` and `S.1`) are seen as:
1. distinct from the perspective of the runtime, but
2. identical for the purposes of [argument punning](#argument-punning).

In the following example, the variables `Arg.sub` and `Arg.sup` are distinct variables because their suffixes are different (`sub` vs `sup`), but are able to be punned in the same way because they have the same stem (`Arg`).

```perl
[[clause_doc "<:->"]]
[in Tcx.0][sub [Arg.sub][Ret.sub]][sup [Arg.sup][Ret.sup]][out Tcx]
    - [sub Arg.sup][sup Arg.sub][in Tcx.0][out Tcx.1]
    - [fn [subst Tcx.1][tm Ret.sub]][ret Ret.sub1]
    - [fn [subst Tcx.1][tm Ret.sup]][ret Ret.sup1]
    - [sub Ret.sub1][sup Ret.sup1][in Tcx.1][out Tcx]
```

In other words, the following two lists are syntactically equivalent terms:
- `{[arg Arg.sub] blah [arg Arg.sup]}`
- `{[Arg.sub] blah [Arg.sup]}`

#### Relation Values

- Sets of ***key-value pairs***. We call a key-value pair an ***attribute***.
- The set of keys defines the name of a relation e.i. the set `{k1, k2, k3}` corresponds to a relation whose ***signature*** is `[K1][K2][K3]`.
- The following 2 relations are the same because order does not matter.
    - `[List][Member]`
    - `[Member][List]`
- ***Note:*** you cannot put any whitespace between the `]` and the `[` within relation value, i.e. this is two separate relations, not one:
  
    ```yaml
    [List]   [Member]
    ```

##### Argument Punning
If the name of the key of a relation's attribute is the same\* as the name of the variable or symbol which is it's value, the key may be omitted.

For example:
```yaml
[fav_color FavColor] = [FavColor]
[the_name the_name] = [the_name]
```

\* Names are compared by converting out of snake_case/PascalCase first e.g. `FavColor or fav_color --> {fav color}` and `the_name --> {the name}`.

See also: [Variable Suffixes](#suffixes)

##### Examples
```yaml
[list L][member M]

# The following 2 are the same:
[human human]
[human]


[numerator 4][denominator 3][Quotient][Remainder]

# The following 2 are the same due to argument punning:
[Prefix][Suffix][Compound]
[prefix Prefix][suffix Suffix][compound Compound]

# Relation values containing variables whose names match the keys are referred to as *relation signatures*.

# Use the `[Sig][Doc]` relation to get documentation on a relation.
[sig [List][Length]][Doc]
```

##### Uses
Relation literals can be used where `struct`s, `record`s, or `class`es are used in other languages. For example, if a datatype is represented in C like this:
```c
typedef struct Person {
    char* name;
    uint8_t age;
    long bank_balance_cents;
} Person;

(Person) { .name = "casey", .age = 29, .bank_balance_cents = -4400 }
```

Or in Python like this:

```python
from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int
    bank_balance_cents: int
    
Person(name='casey', age=29, bank_balance_cents=-4400)
```

In Rellog, this kind of person is representable by values with signature `[Name][Age][BankBalanceCents]`.

An example value of this type would be:
```yaml
[name casey][age 29][bank_balance_cents -4400]
```

#### Operators

There are only a few operators in rellog (and parsing them is kinda buggy at the moment).

| Operator | Name | Meaning |
| -------- | ---- | ------- |
| `A = B` | Unification. | Succeeds if `A` and `B` are unifiable, and in the process unifies them. |
| `A ~ B` | Unifiability. | Succeeds if `A` and `B` are unifiable, fails if they aren't. |
| `A; B` | Conjunction. | Succeeds if `A` succeeds and if `B` subsequently succeeds. |
| `A::B` | Path separator. | Does nothing right now, will be used for module system. |

#### Lists

Lists are defined inductively by this relation:
```yaml
[list {}]

[list {Element ..Tail}]
    - [list Tail]
```

In words, a list is either:
1. The empty list written: `{}`
2. A pair `{A ..B}` where `A` is an element of the list, and `B` is the ***tail*** of the list.

A list's ***tail*** is itself a list. So a tail is either a pair `{C ..D}` or the empty list `{}`.

The following is an abbreviation used to write out lists:
```yaml
{1 2 3}
# The above is the same as the following:
{1 ..{2 ..{3 ..{}}}}
```


##### Examples
```yaml
{} # The empty list.

{0 1 -1 2 -2} # A list with five integers.

{a b c ..Rest} # A partial list.

{1 ..{2 ..{3 ..{}}}} # Same as `{1 2 3}`.

{{}} # A list containing 1 element: the empty list.

{{1 2 3} {4 5 6} {7 8 9}} # A list of lists.

# A heterogenous list (rarely useful).
{{a b c} -31415 [x 3][y -1]}
```

#### Blocks

- Represent sequences of code.
- Use `-` for conjunction ("and"), and `|` for disjunction ("or").
- Semicolon (`;`) can be used for single-line conjunction blocks (useful in the REPL).

##### Examples

```yaml
- [first_condition "asdf"]
- [second][condition {1 2 3}]

# Another way to write conjunction ("and"):
[first_condition "asdf"]; [second][condition {1 2 3}]

| [either][This]
| [or][that 49]
| [or][even][this]
```

### Getting Help

There are many relations predefined for you to use in your Rellog code. Use the `[Sig][Doc]` relation to query them:

```yaml
-- [Sig][Doc]

    - Sig = [Prefix][Suffix][Compound]
    - Doc =
       """
       Relates a list `Compound` to some partitioning 
       of itself into a `Prefix` and a `Suffix`. Also 
       works for text strings.
       """
```

The `[Sig][Help]` relation is good to use to document your own relations too.

There's also the `[Help]` relation. It performs a side-effect when queried (so it's not a pure relation). It prints out the `Help` text associated with the signature passed in:

```
-- [help [Pred][Succ]]

Relation: [Pred][Succ]
--------------
Relates two adjacent integers: a predecessor and a successor.
```

It's just a little easier on the eyes.



### The REPL

The REPL (Read-Eval-Print Loop) is the interactive environment where Rellog code can be loaded and run.

#### REPL Commands

There are several commands that directly interact with the REPL. Most of them begin with a colon (`:`).

| REPL Command            | Description |
| -------------------------- | ----------- |
| `:h`, `:help`, `help`, `?` | Show the help menu. |
| `:r`, `:reload` | Reload the currently loaded source files. |
| `:l <PATH>`, `:load <PATH>` | Load a source file located at `<path>`. Use in conjunction with `[cd]` to change the current working directory, and `[cwd]` to see the current working directory. |
| `:u <PATH>`, `:unload <PATH>` | Sometimes you want to stop paying attention to a file (maybe it was deleted and is no longer relevant). You can tell the REPL to forget about the file with `:unload`. |
| `:d`, `:debug` | Enters debugging mode. Once in, use `help` to learn how to use the debugger. |
| `:q`, `:quit`, `:e`, `:exit`, `:wq` | Quit the REPL. Can also be done with `CTRL-C` or `CTRL-D`. |

## Relation Documentation

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


#### `[FilePath][Clauses][Directives]`
Relates a rellog source file given by `FilePath` to that file's contained `Clauses` and `Directives`.

#### `[Difference][Minuend][Subtrahend]`

Expresses the relationship:
$$
\text{Minuend} - \text{Subtrahend} = \text{Difference}
$$


To perform addition, use mode `[subtrahend][difference][Minuend]`.


Alias: `[Min][Sub][Dif]`

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

#### `[Goal][Truth]`
Runs `Goal`. If successful `Truth` = `true`, if it fails `Truth` = `false`, and if it errors `Truth` = `[Error]` where `Error` is the error generated.

When `Goal` errors, all subsequent solutions are ***cut***.


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


#### `[UndocumentedSig]`
Finds relations which have not been documented.

##### TODO
Kinda broken.

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