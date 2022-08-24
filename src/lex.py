from dataclasses import dataclass

def tag(prefix):
    def p(src):
        remainder = src.removeprefix(prefix)
        if remainder != src:
            return remainder, prefix
        else:
            return None
    return p

def take_while_char_0(predicate):
    def p(src):
        idx = 0
        for ch in src:
            if not predicate(ch):
                break
            idx += 1
        return src[idx:], src[:idx]
    return p

def take_while_char_1(predicate):
    def p(src):
        idx = 0
        for ch in src:
            if not predicate(ch):
                break
            idx += 1
        return (src[idx:], src[:idx]) if idx > 0 else None
    return p

def split_indent_text(line):
    idx = 0
    while idx < len(line):
        if not line[idx].isspace():
            return line[:idx], line[idx:]
        idx += 1
    return None

INDENT_SIZE_IN_SPACES = 4

def indent_count(indent_text):
    if not indent_text:
        return 0

    indent_type = None
    for ch in indent_text:
        match (ch, indent_type):
            case (" ", None):
                indent_type = "SPACES"
            case ("\t", None):
                indent_type = "TABS"
            case (" ", "SPACES"):
                continue
            case ("\t", "TABS"):
                continue
            case _:
                raise Exception("Inconsistent tabs/spaces used for indentation!")
    
    match indent_type:
        case "SPACES":
            if len(indent_text) % INDENT_SIZE_IN_SPACES == 0:
                return len(indent_text) // INDENT_SIZE_IN_SPACES
            else:
                raise Exception(f"Indent is not multiple of {INDENT_SIZE_IN_SPACES} spaces!")
        case "TABS":
            return len(indent_text)

@dataclass
class Tok:
    pass

@dataclass
class Indent(Tok):
    pass

@dataclass
class Dedent(Tok):
    pass

@dataclass
class Sym(Tok):
    value: str

@dataclass
class OBrack(Tok):
    pass

@dataclass
class CBrack(Tok):
    pass

@dataclass
class OParen(Tok):
    pass

@dataclass
class CParen(Tok):
    pass

@dataclass
class Colon(Tok):
    pass

@dataclass
class Equal(Tok):
    pass

@dataclass
class Comma(Tok):
    pass

    
def insert_indent_dedent_tokens(lines):
    prev = 0
    for (curr, line) in lines:
        if curr == prev:
            yield line
        else:
            for _ in range(abs(curr - prev)):
                yield Indent() if curr > prev else Dedent()
            yield line
            prev = curr
    for _ in range(prev):
        yield Dedent()

def tokenize(lines):
    state = "BLOCK" # could alternately be "BRACKETED"
    for thing in lines:
        if type(thing) is str:
            line = thing
            state = yield from tokenize_line(line, state)
        else:
            token = thing
            if state == "BLOCK":
                yield token

def tokenize_line(line, state):
    while line != "":
        if res := take_while_char_1(lambda c: c.isalpha() or c in "-_0123456789")(line):
            (line, sym) = res
            yield Sym(sym)
        elif res := tag("[")(line):
            (line, _) = res
            state = "BRACKETED"
            yield OBrack()
        elif res := tag("]")(line):
            (line, _) = res
            state = "BLOCK"
            yield CBrack()
        elif res := tag("(")(line):
            (line, _) = res
            state = "BRACKETED"
            yield OParen()
        elif res := tag(")")(line):
            (line, _) = res
            state = "BLOCK"
            yield CParen()
        elif res := tag(":")(line):
            (line, _) = res
            yield Colon()
        elif res := tag("=")(line):
            (line, _) = res
            yield Equal()
        elif res := tag(",")(line):
            (line, _) = res
            yield Comma()
        elif res := take_while_char_1(lambda c: c.isspace())(line):
            (line, _) = res
        else:
            raise Exception(f"Unknown input '{line[:5]}...'")
    return state
            
        
        

def lex(src):
    lines = (split_indent_text(line) for line in src.splitlines())
    lines = (pair for pair in lines if pair is not None)
    lines = ((indent_count(indent), text) for (indent, text) in lines)
    lines = insert_indent_dedent_tokens(lines)
    lines = tokenize(lines)
    
    from pprint import pprint
    pprint(list(lines))
    

TEST = """
def take_while_char_0(predicate):
    def p(src):
        for ch in enumerate(src):
        
            if not predicate(ch):
                matrix = [ -2, -1, 0,
                    1, 2, 3,
                    4, 5, 6,
                    7, 8, 9,
    10, 11, 12,
                ]
        return None
    return p
"""

if __name__ == "__main__":
    lex(TEST)