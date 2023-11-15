import ply.lex as lex
import ply.yacc as yacc


tokens = (
    'NUMBER',
    'MINUS',
    'PLUS',
    'DIVIDE',
    'MUL',
    'LPAREN',
    'RPAREN',
    'IF',
    'ELSE',
    'SWITCH',
    'CASE',
    'LBRACE',
    'RBRACE',
    'EQUALS',
    'DEQUALS',
    'LESSTHANEQ',
    'GREATERTHANEQ',
    'LESSTHAN',
    'GREATERTHAN',
    'INT',
    'STRING',
    'FLOAT',
    'BOOL',
    'COLON',
    'SEMICOLON',
    'VAR',
    'CONST',
    'FOR',
    'FUNCTION',
    'ID',
    'FLOATINGNUMBER',
    'BOOLEAN',
    'COMMA',
    'LSQBRACE',
    'RSQBRACE',
    'ARRAY',
)

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MUL     = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_COLON = r':'
t_LBRACE = r'\{'
t_RBRACE =  r'\}'
t_BOOLEAN = r"true|false"
t_COMMA = r","
t_EQUALS = r'\='
t_DEQUALS = r'=='
t_LESSTHAN = r"<"
t_GREATERTHAN = r">"
t_GREATERTHANEQ = r'>='
t_LESSTHANEQ = r'<='
t_LSQBRACE = r"\["
t_RSQBRACE = r"\]"

reserved = {
    'while' : 'WHILE',
    'float32':'FLOAT',
    'func' : 'FUNCTION',
    'if' : 'IF',
    'else' : 'ELSE',
    'int' : 'INT',
    'string' : 'STRING',
    'var' : 'VAR',
    'const' : 'CONST',
    'switch' : 'SWITCH',
    'for' : 'FOR',
    'case' : 'CASE',
    'bool' : 'BOOL',
}

def t_SEMICOLON(t):
    r';'
    return t


def t_FLOATINGNUMBER(t):
    r'[-+]?[0-9]*\.[0-9]+'
    t.value = float(t.value[1:-1])
    return t

def t_SWITCH_CASE(t):
    r'case'
    t.type = 'CASE' 
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t\n'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


def p_program(p):
    '''program : statements
    '''
    p[0] = p[1]

def p_type_specifier(p):
    '''type_specifier : INT
                      | BOOL
                      | FLOAT
                      | STRING
    '''
    if len(p) == 2:
        p[0] = p[1]


def p_declaration(p):
    '''declaration : VAR ID type_specifier 
                   | VAR ID ids type_specifier 
                   | CONST ID type_specifier 
                   | CONST ID ids type_specifier
                   | VAR ID EQUALS LSQBRACE NUMBER RSQBRACE type_specifier LBRACE RBRACE
    '''
    if len(p) == 3:
        p[0] = ('declaration', p[1], [p[2]])
    else:
        p[0] = ('declaration', p[1], (p[2], p[3]))

def p_ids(p):
    '''ids : COMMA ids
           | COMMA ID
    '''
    p[0] = p[2]


def p_switch_statement(p):
    '''switch_statement : SWITCH ID LBRACE case_statements RBRACE
                        | SWITCH NUMBER LBRACE case_statements RBRACE
    '''
    p[0] = ('switch', p[2], p[4])


def p_case_statements(p):
    '''case_statements : case_statement
                      | case_statement case_statements 
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_case_statement(p):
    '''case_statement : CASE NUMBER COLON statements
    '''
    p[0] = ('case', p[2], p[4])

def p_statements(p):
    '''statements : declaration
                  | statement
                  | declaration statements
                  | statement statements
                  | function_declaration
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_empty(p):
    'empty :'
    pass
    p[0]

def p_statement(p): 
    '''statement : IF LPAREN expression RPAREN LBRACE program RBRACE
                 | IF LPAREN expression RPAREN LBRACE program RBRACE ELSE LBRACE program RBRACE
                 | FOR assignment SEMICOLON expression SEMICOLON expression LBRACE program RBRACE
                 | assignment
                 | switch_statement
    '''
    if len(p) == 7 and p[1] == "if":
        p[0] = ('if', p[3], p[6]) 
    elif len(p) == 13:
        p[0] = ('if-else', p[3], p[6], p[12])
    elif len(p) == 2 and p[1] == "switch":
        p[0] = ('switch', p[3], p[6])
    else:
        p[0] = p[1]

def p_expression(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression MUL expression
                  | expression DIVIDE expression
                  | LPAREN expression RPAREN
                  | expression LESSTHAN expression
                  | expression GREATERTHAN expression
                  | expression LESSTHANEQ expression
                  | expression GREATERTHANEQ expression
                  | expression DEQUALS expression
                  | ID
                  | NUMBER
                  
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_assignment(p):
    '''assignment : ID EQUALS expression 
                  | ID EQUALS BOOLEAN
                  | ID EQUALS FLOATINGNUMBER
    '''
    if len(p) == 5:
        p[0] = ('assignment', p[1], p[3])
    
def p_function_declaration(p):
    ''' function_declaration : FUNCTION ID LPAREN parameters RPAREN
    '''

    p[0] = ("function_declaration", p[1])

def p_parameters(p):
    ''' parameters : parameter COMMA parameters
                   | parameter
    '''
    p[0] = p[1]

def p_parameter(p):
    ''' parameter : ID type_specifier 
                  | empty
    '''
    p[0] = p[1]

def p_error(p):
    print(f"Syntax error at '{p.value}'")

parser = yacc.yacc()


data = """ 
var x int
const y bool
const x, y string
switch 1 {
    case 1:
        x = 10
}
switch y {
    case 2:
        var x float32
}

if(x>10){
    x = x + 10
}

var x = [2]int{}

func myfunction(x int, y float32, f string)

"""

lexer.input(data)

while True:
    tok = lexer.token()
    if not tok:
        break     
    print(tok)

result = parser.parse(data)
if (result):
    print("Accepted!")
else:
    print("Rejected!")

