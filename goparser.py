import ply.lex as lex
import ply.yacc as yacc
#oh ye ye

tokens = (
   'NUMBER',
   'SEMICOLON',
   'PLUS',
   'MINUS',
   'MUL',
   'DIVIDE',
   'LPAREN',
   'RPAREN',
   'WHILE',
   'IF',
   'ELSE',
   'LBRACE',
   'RBRACE',
   'ID',
   'EQUALS',
   'DEQUALS',
   'LESSTHAN',
   'GREATERTHAN',
   'LESSTHANEQ',
   'GREATERTHANEQ',
   'INT',
   'CHAR',
   'FLOAT',
   'DOUBLE',
   'DO',
   'SWITCH',
   'CASE',
   'BREAK',
   'ARRAY',
   'COLON',
   'AMPERCENT'
)


t_PLUS    = r'\+'
t_MINUS   = r'-'
t_MUL     = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_COLON=r':'
t_LBRACE = r'\{'
t_RBRACE =  r'\}'
t_EQUALS = r'='
t_DEQUALS = r'=='
t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_LESSTHANEQ = r'<='
t_GREATERTHANEQ = r'>='

reserved = {
   'while' : 'WHILE',
   'if' : 'IF',
   'else' : 'ELSE',
   'int' : 'INT',
   'char' : 'CHAR',
   'float' : 'FLOAT',
   'double' : 'DOUBLE',
   'do':'DO',
   'switch':'SWITCH',
   'break':'BREAK',
   
}

def t_SEMICOLON(t):
    r';'
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

def t_ARRAY(t):
    r'\[\d+\]'
    t.value = int(t.value[1:-1]) 
    return t    


t_ignore = ' \t\n'



def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


lexer = lex.lex()


#precedence = (
#    ('left', 'PLUS', 'MINUS'),
#    ('left', 'MUL', 'DIVIDE'),
#)

def p_program(p):
    '''program : statements
    '''
    p[0] = p[1]

def p_type_specifier(p):
    '''type_specifier : INT
                      | CHAR
                      | FLOAT
                      | DOUBLE
                      | INT pointer_specifier
                      | CHAR pointer_specifier
                      | FLOAT pointer_specifier
                      | DOUBLE pointer_specifier
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[1], p[2])


def p_declaration(p):
    '''declaration : type_specifier ID SEMICOLON
                  | type_specifier ID ARRAY SEMICOLON
    '''
    if len(p) == 3:
        p[0] = ('declaration', p[1], [p[2]])
    else:
        p[0] = ('declaration', p[1], (p[2], p[3]))


def p_pointer_specifier(p):
    '''pointer_specifier : MUL
                       | pointer_specifier MUL
    '''
    if len(p) == 2:
        p[0] = 1  
    else:
        p[0] = p[1] + 1


def p_switch_statement(p):
    '''switch_statement : SWITCH LPAREN ID RPAREN LBRACE case_statements RBRACE
    '''
    p[0] = ('switch', p[3], p[6])


def p_case_statements(p):
    '''case_statements : case_statement
                      | case_statement case_statements 
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_case_statement(p):
    '''case_statement : CASE NUMBER COLON statements BREAK SEMICOLON
    '''
    p[0] = ('case', p[2], p[4])


def p_statements(p):
    '''statements : declaration
                  | statement
                  | declaration statements
                  | statement statements
                  | SEMICOLON
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_statement(p):
    '''statement : IF LPAREN expression RPAREN LBRACE program RBRACE
                 | IF LPAREN expression RPAREN LBRACE program RBRACE ELSE LBRACE program RBRACE
                 | DO LBRACE program RBRACE WHILE LPAREN expression RPAREN SEMICOLON
                 | assignment
                 | switch_statement
    '''
    if len(p) == 7 and p[1] == "if":
        p[0] = ('if', p[3], p[6])
    elif len(p) == 13:
        p[0] = ('if-else', p[3], p[6], p[12])
    elif len(p) == 9 and p[1] == "do":
        p[0] = ('do-while', p[3], p[6])
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
    '''assignment : ID EQUALS expression SEMICOLON
                 | type_specifier ID EQUALS expression SEMICOLON
                 | type_specifier pointer_specifier ID EQUALS AMPERCENT ID SEMICOLON
                 | ID EQUALS AMPERCENT ID SEMICOLON
    '''
    if len(p) == 5:
        
        p[0] = ('assignment', p[1], p[3])
    elif len(p) == 6:
        
        p[0] = ('assignment', p[2], p[4], p[1])
    elif len(p) == 7:
        
        p[0] = ('pointer_assignment', (p[1], p[2]), p[4], p[6])
    elif len(p) == 6:
        
        p[0] = ('address_assignment', p[1], p[4])


def p_error(p):
    print(f"Syntax error at '{p.value}'")

parser = yacc.yacc()


input_code = """ 
int x;
int a;
int b=5;
b=1;
int *p;
int *x;

char y;
int *p;
float z;
int a[10];

int y; 

if (x < 5) {
   x = x * 5;
} else {
   x = x + 5;
}

do {
    y = y / 2;
} while (y >= 6);

switch (x) {
    case 1:
        z = 2 + 5;
        break;
    case 2:
        y=2+5;
        break;    
}
"""

lexer.input(input_code)

while True:
    tok = lexer.token()
    if not tok:
        break     
    print(tok)

result = parser.parse(input_code)
if (result):
    print("Accepted!")
else:
    print("Rejected!")
print("------------------------------------------------------------------------")