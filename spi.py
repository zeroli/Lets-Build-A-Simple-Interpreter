# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, EOF = (
    'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV',
    'LPAREN', 'RPAREN',
    'EOF')

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


class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "3 + 5", '12 - 5', etc
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        # current token instance
        self.current_token = None
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

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

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
                return Token(INTEGER, self.integer())
            
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

class AST(object):
    pass

class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class Parser(object):
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

    def factor(self):
        """return an INTEGER token value
        factor := INTEGER | "(" expr ")"
        """
        token = self.current_token
        if token.type == INTEGER:
            node = Num(token)
            self.eat(INTEGER)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr() 
            self.eat(RPAREN)

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
        return self.expr()

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

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)
    
    def visit_Num(self, node):
        return node.value

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

        #rpnprinter = RPNPrinter(parser)
        #result = rpnprinter.output()
        print(result)

if __name__ == '__main__':
    main()


