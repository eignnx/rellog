# Rellog
It means "Relational Prolog".

## Idea

Instead of predicates, this implementation focuses on relations. While a Prolog predicate can be thought of a symbol and an ordered tuple of arguments, a relation is an unordered collection of attributes, where an attribute is a name-value pair.

While in Prolog you might write

```prolog
append([1, 2], [3, 4], Compound)
```

in Rellog you could write any of

```toml
[prefix {1, 2}][suffix {3, 4}][compound Compound]
[prefix {1, 2}][suffix {3, 4}][Compound]
[suffix {3, 4}][prefix {1, 2}][Compound]
[suffix {3, 4}][Compound][prefix {1, 2}]
[prefix {1, 2}][Compound][suffix {3, 4}]
[Compound][suffix {3, 4}][prefix {1, 2}]
[Compound][prefix {1, 2}][suffix {3, 4}]
```

## Example
```toml
[prefix {}][Suffix][compound Suffix]
[prefix {A ...As}][Suffix][compound {A ...Compound}]
    - [prefix As][suffix Suffix][Compound]

[forwards {}][backwards {}]
[forwards {A ...As}][Backwards]
    - [forwards As][backwards AsBackwards]
    - [prefix AsBackwards][suffix {A}][compound Backwards]
```

Note that conjunctions are expressed as indented `-`-preceeded lists.

## Use

To open a repl,

```shell
$ git clone https://github.com/eignnx/rellog.git
$ cd rellog
$ cargo run
```

## Language Reference

### Overview: The Big Ideas

Rellog is all about ***relations***. A relation is a set of association of values.

An example of a relation might be a student-teacher relationship. In Rellog, the sentence "Gideon and Aditya are students of Dr. Blum" could be expressed as two facts about the world:

```yaml
# Fact #1:
[student gideon][teacher dr_blum]

# Fact #2:
[student aditya][teacher dr_blum]
```

We refer to this relation by its ***signature*** which is written:

```yaml
[student][teacher]
```

A signature is a set of symbols which are the names of the relation's ***keys***.

If you're familiar with Excel, think of a relation as a ***spreadsheet***. A relation's signature then is just the spreadsheet's ***column names***.

Here's a bigger example involving multiple relations. First I'll write the Rellog code, then I'll write an english summary.

```yaml
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
1. The `[course][instructor]` relation,
1. the `[course][name]` relation, and
1. the `[student][course]` relation.

An interesting question to ask might be "who are all the students of Dr. Na?"

We can pose this question as a Rellog query in the REPL:

```yaml
-- [instructor dr_na][Course];
   [Course][Student]

    - Course = cmpsc439
    - Student = gideon
|
    - Course = cmpsc439
    - Student = aditya
```

We could even define the `[student][teacher]` relation in Rellog by writing a fact with a collection of conditions:

```yaml
[Student][Teacher]
    - [instructor Teacher][Course]
    - [Course][Student]
```

This rule says:

> A student called `Student` has a teacer called `Teacher` if:
> 1. `Teacher` is the instructor of a course called `Course`, and
> 2. `Student` is a student in that same course `Course`.

Now, to ask who Gideon's teachers are we could query:

```yaml
-- [student gideon][Teacher]

    - Teacher = dr_null
|
    - Teacher = dr_na
```

### Getting Help

There are many relations predefined for you to use in your Rellog code. Use the `[sig][help]` relation to query them:

```yaml
-- [Sig][Help]

    - Sig = [prefix][suffix][compound]
    - Help =
        """
       Relates a list `Compound` to some partitioning 
       of itself into a `Prefix` and a `Suffix`. Also 
       works for text strings.
       """
```

The `[sig][help]` relation is good to use to document your own relations too.

There's also the `[help]` relation. It performs a side-effect when queried (so it's not a pure relation). It prints out the `Help` text associated with the signature passed in:


<code>
-- [help [pred][succ]]
<br/>
<br/>
<i>Relation: [pred][succ]<br/>
--------------<br/>
Relates two adjacent integers: a predecessor and a successor.
</i>
</code><br/><br/>


It's just a little easier on the eyes.

### Clauses

A clause is a top-level definition. A clause is either:

1. A **fact**:

    The following three facts declare that three symbols (`socrates`, `chomsky`, and `you`) refer to humans.
    ```toml
    [human socrates]
    [human chomsky]
    [human you]
    ```
    Most facts are unconditionally true. But here's an example of a fact that could fail depending on its argument:
    ```toml
    [nonempty_list {A ..B}]
    ```
    The query `[nonempty_list {}]` would fail.
1. A **rule**:

    A rule has conditions. Heres an example of a rule:
    ```toml
    [mortal X]
        - [human X]
    ```
    This rule says "an `X` is `mortal` if that `X` is `human`.

    Lets query the `mortal` rule.
    ```toml
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
Identifiers for concepts.

Have no meaning on their own. 

Implemented as interned strings.

##### Examples
```yaml
socrates

my_dog

area_51

multiply
```

#### Integers
Arbitrarily-sized integers (positive or negative whole numbers)

##### Examples
`0`, `17`, `-3`, `93740925370000349251`

#### Text Strings
- Represent textual data.
- Useful when text must be constructed or parsed.
- **Warning: Text is still a work in progress.**
##### Examples
```yaml
"Hello?"

"Text strings may contain spaces and punctuation."

# The remainder of this example is 1 text string literal:
"""
They can also be multi-line.

Like this!

Single linebreaks at the end of a 
line will combine the two lines into
a single paragraph.

Double linebreaks create a new paragraph.
"""
```

#### Variables
- Must start with a capital letter.
- Very similar to variables in *algebra*.
- Less similar to the variables in *imperative programming languages*.
- May be *known* (bound to a specific value) or *unknown* (not yet bound to a value).

##### Examples
`X`, `ListReversed`, `State0`

#### Relations
- Sets of ***key-value pairs***. We call a key-value pair an ***attribute***.
- The set of keys defines the name of a relation e.i. the set $\lbrace k1, k2, k3 \rbrace$ corresponds to a relation whose ***signature*** is `[k1][k2][k3]`.
- The following 2 relations are the same because order does not matter.
    - `[list][member]`
    - `[member][list]`
- ***Note:*** you cannot put any whitespace between the `]` and the `[` within relation value, i.e. this is two separate relations, not one:
    ```yaml
    [list]   [member]
    ```
##### Examples
```yaml
[list L][member M]

# The following 2 are the same:
[human Human]
[Human]


[numerator 4][denominator 3][Quotient][Remainder]

# The following 2 are the same:
[prefix][suffix][compound]
[prefix prefix][suffix suffix][compound compound]

# Relations of symbols that match the keys are referred to as *relation signatures*.

# Use the `[sig][help]` relation to get documentation on a relation.
[sig [list][member]][Help]
```

##### Uses
Relation literals can be used where `struct`s, `record`s, or `class`es are used in other languages. For example, if a datatype is represented in C like this:
```c
typedef struct Person {
    char* name;
    uint8_t age;
    long bank_balance_cents;
} Person;
```

Or in Python like this:

```python
from dataclasses import dataclass

@dataclass
class Person:
    name: str
    age: int
    bank_balance_cents: int
```

In Rellog, this kind of person is representable by values with signature `[name][age][bank_balance_cents]`.

An example value of this type would be:
```yaml
[name casey][age 29][bank_balance_cents -4400]
```

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

### The REPL

The REPL (Read-Eval-Print Loop) is the interactive environment where rellog code can be loaded and run.

#### REPL Comands

There are several commands that directly interact with the REPL. Most of them begin with a colon (`:`).

| Repl Command               | Description |
| -------------------------- | ----------- |
| `:h`, `:help`, `help`, `?` | Show the help menu. |
| `:r`, `:reload` | Reload the currently loaded source file (WIP) |
| `:l <path>`, `:load <path>` | Load a source file located at `<path>`. Use in conjunction with `[cd]` to change the current working directory. |
| `:q`, `:quit`, `:e`, `:exit`, `:wq` | Quit the REPL. Can also be done with `CTRL-C` or `CTRL-D`. |
