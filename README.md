# Rellog
It means "Relational Prolog".

## Idea

Instead of predicates, this implementation focuses on relations. While a Prolog predicate can be thought of a symbol and an ordered tuple of arguments, a relation is an unordered collection of attributes, where an attribute is a name-value pair.

While in Prolog you might write

```prolog
append([1, 2], [3, 4], Compound)
```

in Rellog you could write any of

```ruby
[prefix {1, 2}][suffix {3, 4}][compound Compound]
[prefix {1, 2}][suffix {3, 4}][Compound]
[suffix {3, 4}][prefix {1, 2}][Compound]
[suffix {3, 4}][Compound][prefix {1, 2}]
[prefix {1, 2}][Compound][suffix {3, 4}]
[Compound][suffix {3, 4}][prefix {1, 2}]
[Compound][prefix {1, 2}][suffix {3, 4}]
```

## Example
```ruby
[prefix {}][Suffix][compound Suffix]
[prefix {A ...As}][Suffix][Compound]
    - [prefix As][suffix {A ...Suffix}][Compound]

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
$ cargo run -- examples/test.txt
```
