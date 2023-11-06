import ply.lex as lex
import ply.yacc as yacc

tokens = [
    'NUMBER',
    'MINUS',
    'PLUS',
    'DIVIDE',
    'MUL',
    'LPAREN',
    'RPAREN',
    'WHILE',
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
    'INT',
    'STRING',
    'FLOAT',
    'BOOL',
    'BREAK',
    'COLON',
    'SEMICOLON',
    'VAR',
    'CONST',
    'FOR',
    'CONTINUE',
    'FUNCTION',
    'ID',
    'COMMA',
    'ARRAY',
    'NEWLINE',
]

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MUL     = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_COLON = r':'
t_LBRACE = r'\{'
t_RBRACE =  r'\}'
t_EQUALS = r'='
t_DEQUALS = r'=='
t_SEMICOLON = r';'
t_COMMA = r","
t_NEWLINE = r"\n"

reserved = {
    'while' : 'WHILE',
    'if' : 'IF',
    'else' : 'ELSE',
    'int' : 'INT',
    'string' : 'STRING',
    'float32' : 'FLOAT',
    'var' : 'VAR',
    'const' : 'CONST',
    'switch' : 'SWITCH',
    'for' : 'FOR',
    'while' : 'WHILE',
    'break' : 'BREAK',
    'continue' : 'CONTINUE',
    'case' : 'CASE',
}

def t_BOOL(t):
    r'true|false'
    return t

def t_ARRAY(t):
    r'\[\d+\]'
    t.value = int(t.value[1:-1]) 
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

def p_type_specification(p):
    ''' type_specification : INT 
                           | STRING
                           | FLOAT
                           | BOOL
    '''
    p[0] = p[1]

def p_var_or_const(p):
    ''' state : VAR
              | CONST
    
    '''
    p[0] = p[1]

def p_variable_declaration(p):
    ''' declaration : state ID type_specification 
                    | state ID ARRAY type_specification 
    '''
    p[0] = ("variable_declared", p[1], p[2])

def p_variable_definition(p):
    ''' defintion : 
    '''

data = ''' var x int 
var y int '''

lexer = lex.lex()


lexer.input(data)

while True:
    tok = lexer.token()
    if not tok:
        break     
    print(tok)

parser = yacc.yacc()
result = parser.parse(data)

if(result):
    print('Accept')
else:
    print("Rejected")
