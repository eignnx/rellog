% A grammar specification for the Rellog language.

:- op(300, fx, *). % Zero or more
:- op(300, fx, +). % One or more
:- op(300, fx, ?). % Zero or one

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module --> *(item, item_end), eoi.

eoi --> "<END-OF-INPUT-MARKER>".
item_end --> "<ITEM-END-MARKER>"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

item --> "[", relation, "]".
       | relation, ?term.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

relation --> "[", attr, *("][", attr), "]".

attr --> symbol, term
       | symbol
       | variable.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term --> symbol
       | variable
       | number
       | text
       | relation
       | list
       | block
       | operator_term
       | "(", term, ")".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symbol --> "some_symbol".
variable --> "SomeVariable" | "_".
number --> "123" | "-123".
text --> "\"", *([C], {C != '"'}), "\"".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list --> "{", "}"
       | "{", term, *(",", term), ?("..", term), "}".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block --> indent, +member, dedent. % If you can get an indent, great.
        % If you're in a context where an indent can't be produced (like inside
        % parenthesis), begin the block on the next like after the block allowed
        % operator (":").
        | block_allowed_operator, indent, +member, dedent.
        % An inline way of specifying an and-block is with commas. Semicolons
        % can be used to make an inline or-block.
        | inline_block.

indent --> "<INDENT-MARKER>".
dedent --> "<DEDENT-MARKER>".

member --> and_block_member
         | or_block_member.

and_block_member --> term, block_item_end.
or_block_member --> "|", term, block_item_end.

block_allowed_operator --> ":".

block_item_end --> "<BLOCK-ITEM-END-MARKER>".

inline_block --> non_block_term, +(inline_block_op, non_block_term).
inline_block_op --> "," | ";".

non_block_term --> symbol
                 | variable
                 | number
                 | text
                 | relation
                 | list
                 | operator_term.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

operator_term --> non_op_term, +(operator, non_op_term).

operator --> +("=" | "+" | "-" | "*" | "/" | "<" | ">" | "^" | "&" | "?" | "!")
           | ".", symbol, ".".

/*
cond = (1 .gt. 12) .or. (5 .lte. 6)
*/

non_op_term --> symbol
              | variable
              | number
              | text
              | relation
              | list
              | block.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

substitution_text --> *([C], {C != '['}), (relation | "{", term, "}" | "{", "}").

/*
[story]
    """
    If I ever set foot on that [Temperature] [Location]{ [real Location] }
    again, I'll [Action]{
        Action .not_in. {"die", "sleep"}
        [possible Action]
    }.
    """

[temperature]
    | "hot"
    | "cold"
    | "musty"
    | "dank"


[action] "play football"
[action] "play football"
*/