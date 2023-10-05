# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
INTEGER = 'INTEGER'
REAL = 'REAL'
INTEGER_CONST = 'INTEGER_CONST'
REAL_CONST = 'REAL_CONST'
INTEGER_DIV = 'INTEGER_DIV'
REAL_DIV = 'REAL_DIV'
PLUS = 'PLUS'
MINUS = 'MINUS'
MUL = 'MUL'
DIV = 'DIV'
LPAREN = 'LAPREN'
RPAREN = 'RPAREN'
ID = 'ID'
ASSIGN = 'ASSIGN'
SEMI = 'SEMI'
DOT = 'DOT'
COLON = 'COLON'
COMMA = 'COMMA'
VAR = 'VAR'
PROGRAM = 'PROGRAM'
BEGIN = 'BEGIN'
END = 'END'
EOF = 'EOF'

class Token(object):
    def __init__(self, type, value):
        # token type: INTEGER, PLUS, MINUS, or EOF
        self.type = type
        # token value: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
        # '+', '-', '*', '/'
        # '(', ')'
        # or None
        self.value = value

    def __str__(self):
        """String representation of the class instance.

        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
        """
        return 'Token({type}, {value})'.format(
                type = self.type,
                value = repr(self.value)
        )

    def __repr__(self):
        return self.__str__()

RESERVED_KEYWORDS = {
    'PROGRAM': Token(PROGRAM, 'PROGRAM'),
    'VAR': Token(VAR, 'VAR'),
    'DIV': Token(INTEGER_DIV, 'DIV'),
    'INTEGER': Token(INTEGER, 'INTEGER'),
    'REAL': Token(REAL, 'REAL'),
    'BEGIN' : Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
}

class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "3 + 5", '12 - 5', etc
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]

    def error(self):
        raise Exception('invalid character')

    def advance(self):
        """Advance the 'pos' pointer and set the 'current_char' variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None
        else:
            self.current_char = self.text[self.pos]

    def peek(self):
        if self.pos + 1 > len(self.text) - 1:
            return None
        else:
            return self.text[self.pos + 1]

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def number(self):
        """Return a (multidigit) integer or float consumed from the input"""
        result = ''
        while self.current_token is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                self.current_char is not None and
                self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()
            token = Token(REAL_CONST, float(result))
        else:
            token = Token(INTEGER_CONST, int(result))

        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char.isdigit():
                return self.number()

            if self.current_char.isalpha():
                return self._id()

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char == ':':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return Token(ASSIGN, ':=')
                else:
                    return Token(COLON, ':')

            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',')

            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')
            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')
            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')
            if self.current_char == '/':
                self.advance()
                return Token(DIV, '/')
            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(')
            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')')

            self.error()
        return Token(EOF, None)

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()
        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()  # the closing curly brace

class AST(object):
    pass

class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr

class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Compound(AST):
    """Represents a 'BEGIN ... END' block"""
    def __init__(self):
        self.children = []

class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class NoOp(AST):
    pass

class Parser(object):
    """
    program : PROGRAM variable SEMI block DOT

    block: declarations compound_statement

    declarations: VAR (variable_declaration SEMI)+
                    | empty

    variable_declaration: ID (COMMA ID)* COLON type_spec

    type_spec: INTEGER | REAL

    compound_statement : BEGIN statement_list END

    statement_list : statement
                   | statement SEMI statement_list

    statement : compound_statement
              | assignment_statement
              | empty

    assignment_statement : variable ASSIGN expr

    empty :

    expr: term ((PLUS | MINUS) term)*

    term: factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

    factor : PLUS factor
           | MINUS factor
           | INTEGER_CONST
           | FLOAT_CONST
           | LPAREN expr RPAREN
           | variable

    variable: ID
    """
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('invalid syntax')

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        """Program: compound_statement DOT"""
        node = self.compound_statement()
        self.eat(DOT)
        return node

    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)
        return root

    def statement_list(self):
        """
        statement_list: statement
                            | statement SEMI statement_list
        """
        node = self.statement()
        results = [node]
        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement())
        if self.current_token.type == ID:
            self.error()
        return results

    def statement(self):
        """
        statement: compound_statement
                        | assignment_statement()
                        | empty
        """
        if self.current_token.type == BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        """
        assignment_statement: variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self):
        """
        variable: ID
        """
        node = Var(self.current_token)
        self.eat(ID)
        return node

    def empty(self):
        """An empty production"""
        return NoOp()

    def factor(self):
        """
        factor: PLUS factor
                | MINUS factor
                | INTEGER
                | LPAREN expr RPAREN
                | variable
        """
        token = self.current_token
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.factor())
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.factor())
        elif token.type == INTEGER_CONST:
            node = Num(token)
            self.eat(INTEGER)
        elif token.type == REAL_CONST:
            node = Num(token)
            self.eat(REAL_CONST)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
        else:
            node = self.variable()

        return node

    def term(self):
        """
        term := factor ( (*|/) factor )*
        """
        node = self.factor()

        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == DIV:
                self.eat(DIV)

            node = BinOp(left = node, op = token, right = self.factor())

        return node

    def expr(self):
        """
        expr := term ( (+|-) term )*
        """
        node = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token

            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)
            node = BinOp(left = node, op = token, right = self.term())

        return node

    def parse(self):
        node = self.program()
        if self.current_token.type != EOF:
            self.error()
        return node

class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))

class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_UnaryOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.expr)
        elif node.op.type == MINUS:
            return -self.visit(node.expr)

    def visit_Num(self, node):
        return node.value

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val

    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)

class RPNPrinter(NodeVisitor):
    """
    Reverse Polish Notation print
    """
    def __init__(self, parser):
        self.parser = parser
        self.notation = []

    def visit_BinOp(self, node):
        """
        post-order traversal
        """
        if node.op.type == PLUS:
            self.visit(node.left)
            self.visit(node.right)
            self.notation.append('+')
        elif node.op.type == MINUS:
            self.visit(node.left)
            self.visit(node.right)
            self.notation.append('-')
        elif node.op.type == MUL:
            self.visit(node.left)
            self.visit(node.right)
            self.notation.append('*')
        elif node.op.type == DIV:
            self.visit(node.left)
            self.visit(node.right)
            self.notation.append('/')

    def visit_UnaryOp(self, node):
        if node.op.type == PLUS:
            self.visit(node.expr)
            self.notation.append('+')
        elif node.op.type == MINUS:
            self.visit(node.expr)
            self.notation.append('-')

    def visit_Num(self, node):
        self.notation.append(str(node.value))

    def output(self):
        tree = self.parser.parse()
        self.visit(tree)
        return ' '.join(self.notation)

class LispStylePrinter(NodeVisitor):
    """
    Lisp Style Notation print
    """
    def __init__(self, parser):
        self.parser = parser
        self.notation = []

    def visit_BinOp(self, node):
        """
        infix order traversal
        """
        self.notation.append('(')
        if node.op.type == PLUS:
            self.notation.append('+')
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == MINUS:
            self.notation.append('-')
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == MUL:
            self.notation.append('*')
            self.visit(node.left)
            self.visit(node.right)
        elif node.op.type == DIV:
            self.notation.append('/')
            self.visit(node.left)
            self.visit(node.right)

        self.notation.append(')')

    def visit_UnaryOp(self, node):
        self.notation.append('(')
        if node.op.type == PLUS:
            self.notation.append('+')
            self.visit(node.expr)
        elif node.op.type == MINUS:
            self.notation.append('-')
            self.visit(node.expr)

        self.notation.append(')')

    def visit_Num(self, node):
        self.notation.append(str(node.value))

    def output(self):
        tree = self.parser.parse()
        self.visit(tree)
        return ' '.join(self.notation)

def main():
    while True:
        try:
            text = raw_input('spi> ')
        except EOFError:
            break
        if not text:
            continue
        parser = Parser(Lexer(text))
        interpreter = Interpreter(parser)
        result = interpreter.interpret()
        print '=', result

        parser = Parser(Lexer(text))
        rpnprinter = RPNPrinter(parser)
        result = rpnprinter.output()
        print 'RPN: ', result

        parser = Parser(Lexer(text))
        lispstyleprinter = LispStylePrinter(parser)
        result = lispstyleprinter.output()
        print 'Lisp:',  result

if __name__ == '__main__':
    main()
