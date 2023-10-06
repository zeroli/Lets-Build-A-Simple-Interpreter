import os, sys

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
PROCEDURE = 'PROCEDURE'
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
    'program': Token(PROGRAM, 'PROGRAM'),
    'VAR': Token(VAR, 'VAR'),
    'var': Token(VAR, 'VAR'),
    'DIV': Token(INTEGER_DIV, 'DIV'),
    'div': Token(INTEGER_DIV, 'DIV'),
    'INTEGER': Token(INTEGER, 'INTEGER'),
    'integer': Token(INTEGER, 'INTEGER'),
    'REAL': Token(REAL, 'REAL'),
    'real': Token(REAL, 'REAL'),
    'BEGIN' : Token(BEGIN, 'BEGIN'),
    'begin' : Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
    'end': Token(END, 'END'),
    'PROCEDURE': Token(PROCEDURE, 'PROCEDURE'),
    'procedure': Token(PROCEDURE, 'PROCEDURE'),
}

class Symbol(object):
    def __init__(self, name, type = None):
        self.name = name
        self.type = type

class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super(BuiltinTypeSymbol, self).__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}'))>".format(
            class_name = self.__class__.__name__,
            name = self.name
        )

class VarSymbol(Symbol):
    def __init__(self, name, type):
        super(VarSymbol, self).__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}'>".format(
                class_name = self.__class__.__name__,
                name = self.name,
                type = self.type
        )

    __repr__ = __str__

class ProcedureSymbol(Symbol):
    def __init__(self, name, params=None):
        super(ProcedureSymbol, self).__init__(name)
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name = self.__class__.__name__,
            name = self.name,
            params = self.params,
        )

    __repr__ = __str__

class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope
        self._init_builtins()

    def _init_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
            ('Enclosing scope',
                self.enclosing_scope.scope_name if self.enclosing_scope else None
            ),
        ):
            lines.append('%-15s: %s' % (header_name, header_value))

        h2 = 'Scope (Scoped symbol table) content'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def define(self, symbol):
        print('Define: {}'.format(symbol))
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        print('Lookup: {}'.format(name))
        symbol = self._symbols.get(name)
        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)

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
        while self.current_char is not None and self.current_char.isdigit():
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
                return Token(REAL_DIV, '/')
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

class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block

class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement

class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class ProcedureDecl(AST):
    def __init__(self, proc_name, params, block_node):
        self.proc_name = proc_name
        self.params = params # a list of param nodes
        self.block_node = block_node

class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

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
        """Program: PROGRAM variable SEMI block DOT"""
        self.eat(PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(SEMI)

        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(DOT)

        return program_node

    def block(self):
        """block: declarations compound_statement"""
        declaration_nodes = self.declarations()
        compund_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compund_statement_node)
        return node

    def declarations(self):
        """
        declarations: VAR (variable_declaration SEMI)+
                        | (PROCEDURE variable (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
                        | empty
        EXAMPLE:
            VAR
                var1: INTEGER;
                var2: INTEGER;
                var3: REAL;
                ...
            PROCECURE proc1;
                VAR
                    ...
                BEGIN
                    ...
                END;
        """
        declarations = []
        if self.current_token.type == VAR:
            self.eat(VAR)
            while self.current_token.type == ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(SEMI)

        while self.current_token.type == PROCEDURE:
            self.eat(PROCEDURE)
            proc_name = self.current_token.value
            self.eat(ID)
            params = []
            if self.current_token.type == LPAREN:
                self.eat(LPAREN)
                params = self.formal_parameter_list()
                self.eat(RPAREN)

            self.eat(SEMI)

            block_node = self.block()
            proc_decl = ProcedureDecl(proc_name, params, block_node)
            declarations.append(proc_decl)
            self.eat(SEMI)

        return declarations

    def variable_declaration(self):
        """
        variable_declaration: ID (COMMA ID)* COLON type_spec
        EXAMPLE:
            var1: INTEGER;
            var2a, var2b: INTEGER;
            var3: REAL;
        """
        var_nodes = [Var(self.current_token)] # first ID
        self.eat(ID)

        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)

        self.eat(COLON)

        type_node = self.type_spec()
        var_declarations = [
            VarDecl(var_node, type_node)
            for var_node in var_nodes
        ]
        return var_declarations

    def type_spec(self):
        """
        type_spec: INTEGER | REAL
        """
        token = self.current_token
        if self.current_token.type == INTEGER:
            self.eat(INTEGER)
        else:
            self.eat(REAL)
        node = Type(token)
        return node

    def formal_parameter_list(self):
        """
        formal_parameter_list: formal_parameters
                                    | formal_parameters SEMI forma_parameter_list
        """
        params = self.formal_parameters()

        while self.current_token.type == SEMI:
            self.eat(SEMI)
            params.extend(self.formal_parameter_list())
        return params

    def formal_parameters(self):
        """
        forma_parameters: ID (COMMA ID)* COLON type_spec
        """
        param_nodes = self.variable_declaration()
        return param_nodes

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

    def term(self):
        """
        term := factor ( (MUL | INTEGER_DIV | REAL_DIV) factor )*
        """
        node = self.factor()

        while self.current_token.type in (MUL, INTEGER_DIV, REAL_DIV):
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == INTEGER_DIV:
                self.eat(INTEGER_DIV)
            elif token.type == REAL_DIV:
                self.eat(REAL_DIV)

            node = BinOp(left = node, op = token, right = self.factor())

        return node

    def factor(self):
        """
        factor: PLUS factor
                | MINUS factor
                | INTEGER_CONST
                | REAL_CONST
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
            self.eat(INTEGER_CONST)
            node = Num(token)
        elif token.type == REAL_CONST:
            self.eat(REAL_CONST)
            node = Num(token)
        elif token.type == LPAREN:
            self.eat(LPAREN)
            node = self.expr()
            self.eat(RPAREN)
        else:
            node = self.variable()

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
        self.current_scope = None

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        self.current_scope.define(var_symbol)

    def visit_ProcedureDecl(self, node):
        pass

    def visit_Type(self, node):
        pass

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == REAL_DIV:
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
        var = self.current_scope.lookup(var_name)
        node.right

    def visit_Var(self, node):
        var_name = node.value
        var = self.current_scope.lookup(var_name)
        if var is None:
            raise Exception(
                "Error: Symbol(identifier) not found {}".format(var_name)
            )
        else:
            return var

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

class SymbolTableBuilder(NodeVisitor):
    def __init__(self):
        self.symtab = SymbolTable()

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_Num(self, node):
        pass

    def visit_UnaryOp(self, node):
        self.visit(node.expr)

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.symtab.lookup(type_name)
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)
        self.symtab.define(var_symbol)

    def visit_ProcedureDecl(self, node):
        pass

    def visit_Assign(self, node):
        var_name = node.left.value
        var_symbol = self.symtab.lookup(var_name)
        if var_symbol is None:
            raise NameError(repr(var_name))
        self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.symtab.lookup(var_name)
        if var_symbol is None:
            raise NameError(repr(var_name))


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        print('Enter scope: global')
        global_scope = ScopedSymbolTable(
            scope_name = 'global',
            scope_level = 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = global_scope

        self.visit(node.block)

        print(global_scope)
        self.current_scope = self.current_scope.enclosing_scope
        print('Leave scope: global')

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_VarDecl(self, node):
        """for now, manaully create a symbol for the INTEGER built-in
        and insert the type symbol in the symbol table
        """
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        if self.current_scope.lookup(var_name, current_scope_only=True):
                raise Exception(
                    "Error: Duplicate identifier '%s' found" % var_name
                )
        self.current_scope.define(var_symbol)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            raise Exception(
                "Error: Symbol(identifier) not found '{}'".format(var_name)
            )

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        self.current_scope.define(proc_symbol)

        print('Enter scope: {}'.format(proc_name))

        procedure_scope = ScopedSymbolTable(
            scope_name = proc_name,
            scope_level = self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = procedure_scope

        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.define(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)
        print(procedure_scope)
        self.current_scope = self.current_scope.enclosing_scope
        print('Leave scope: {}'.format(proc_name))

    def visit_Num(self, node):
        pass

def main():
    if len(sys.argv) > 1:
        text = open(sys.argv[1], 'r').read()
    else:
        text = raw_input('spi>')

    if not text:
        exit(1)
    parser = Parser(Lexer(text))
    program = parser.parse()
    run = SemanticAnalyzer()
    run.visit(program)


if __name__ == '__main__':
    main()
