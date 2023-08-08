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

1. Symbols:
    - Example: `socrates`, `my_dog`
    - Identifiers for concepts.
    - Have no meaning on their own. 
    - Implemented as interned strings.
1. Integers:
    - Examples: `0`, `17`, `-3`, `93740925370000349251`
    - Arbitrarily-sized integers (positive or negative whole numbers)
1. Text Strings:
    - Examples:
        ```toml
        "Hello?"
        "Text strings may contain spaces and punctuation."
        """
        They can also be multi-line.

        Like this!

        Single linebreaks at the end of a 
        line will combine the two lines into
        a single paragraph.

        Double linebreaks create a new paragraph.
        """
        ```
    - Represent textual data.
    - (WORK IN PROGRESS)
1. Variables:
    - Examples: `X`, `ListReversed`, `State0`
    - Must start with a capital letter.
    - Very similar to variables in *algebra*.
    - Less similar to the variables in *imperative programming languages*.
    - May be *known* (bound to a specific value) or *unknown* (not yet bound to a value).
1. Relations:
    - Examples:
        ```toml
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
    - Sets of key-value pairs.
    - The keys define the name of a relation.
    - The following 2 relations are the same because order does not matter.
        - `[list][member]`
        - `[member][list]`
    - Note: you cannot put any whitespace between the `]` and the `[` within relation value, i.e. this is two separate relations, not one:
        - `[list]   [member]`
1. Lists: