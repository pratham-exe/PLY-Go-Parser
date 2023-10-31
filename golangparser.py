from ply import lex
from ply import yacc

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'switch' : 'SWITCH',
    'var' : 'VAR',
    'int' : 'INT',
    'float32' : 'FLOAT32',
    'bool' : 'BOOL',
    'for' : 'FOR',
    'func' : 'FUNCTION',
}

tokens = [
    'NUMBER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'LCURLYPAREN',
    'RCURLYPAREN',
    'DOUBLEEQUALS',
    'EQUALS',
    'NOTYPEASSIGNMENT',
    'COLON',
] + list(reserved.values())

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_DOUBLEEQUALS = r'\==' 
t_EQUALS = r'\='
t_NOTYPEASSIGNMENT = r':='
t_LCURLYPAREN = r'\{'
t_RCURLYPAREN = r'\}'
t_COLON = r':'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_NUMBER(t):
     r'\d+'
     t.value = int(t.value)    
     return t

def t_newline(t):
     r'\n+'
     t.lexer.lineno += len(t.value)

def t_error(t):
     print("Illegal character '%s'" % t.value[0])
     t.lexer.skip(1)

data = '''
3 + 4 * 10
  + -20 *2
  var int float32 := switch else else if 
'''

t_ignore  = ' \t'

f = open('input.txt', 'r')

lexer = lex.lex()

lexer.input(f.read())

while True:
     tok = lexer.token()
     if not tok: 
          break      # No more input
     print(tok)
