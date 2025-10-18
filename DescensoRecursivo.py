# Francisco Aquino - A00833409
# Descenso Recursivo - Parser / Lexer

import ply.lex as lex
import sys
import os

# =========================================================================
# 1. TOKENIZER / LEXER (CÓDIGO BASE PLY)
# =========================================================================

# Diccionario de palabras reservadas
reserved = {
    'program': 'PROGRAM', 'main': 'MAIN', 'var': 'VAR', 'end': 'END', 
    'int': 'INT', 'float': 'FLOAT', 'string': 'STRING', 'void': 'VOID', 
    'if': 'IF', 'else': 'ELSE', 'do': 'DO', 'while': 'WHILE', 'print': 'PRINT', 
    'return': 'RETURN', 'break': 'BREAK', 'continue': 'CONTINUE'
}

# Diccionario de lexemas
tokens = [
    'IDENTIFIER', 'CONST_INT', 'CONST_FLOAT', 'CONST_STRING',
    'OP_ASSIGN', 'OP_EQ', 'OP_NEQ', 'OP_LEQ', 'OP_GEQ', 'OP_LT', 'OP_GT', 
    'OP_PLUS', 'OP_MINUS', 'OP_MULT', 'OP_DIV', 'OP_AND', 'OP_ANDAND', 'OP_OR', 'OP_OROR', 'OP_NOT',
    'SEMICOLON', 'COMMA', 'COLON', 'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET', 'LPAREN', 'RPAREN',
    'COMMENT', 'INVALID_ID'
] + list(reserved.values())

# Definiciones de t_funciones (regex)
t_OP_EQ = r'==' ; t_OP_NEQ = r'!=' ; t_OP_LEQ = r'<=' ; t_OP_GEQ = r'>=' ; t_OP_ASSIGN = r'='
t_OP_LT = r'<' ; t_OP_GT = r'>' ; t_OP_PLUS = r'\+' ; t_OP_MINUS = r'-' ; t_OP_MULT = r'\*' ; t_OP_DIV = r'/'
t_OP_ANDAND = r'&&' ; t_OP_AND = r'&' ; t_OP_OROR = r'\|\|' ; t_OP_OR = r'\|' ; t_OP_NOT = r'!'
t_SEMICOLON = r';' ; t_COMMA = r',' ; t_COLON = r':' ; t_LBRACE = r'\{' ; t_RBRACE = r'\}'
t_LBRACKET = r'\[' ; t_RBRACKET = r'\]' ; t_LPAREN = r'\(' ; t_RPAREN = r'\)'

# Reglas de reconocimiento de tokens
def t_INVALID_ID(t): r'\d+[a-zA-Z_]\w*' ; print(f"[LEXER ERROR] Identificador inválido '{t.value}'") ; t.lexer.skip(1)
def t_CONST_FLOAT(t): r'\d+\.\d+' ; t.value = float(t.value) ; return t
def t_CONST_INT(t): r'\d+' ; t.value = int(t.value) ; return t
def t_CONST_STRING(t): r'\"([^\\\n]|(\\.))*?\"' ; return t
def t_COMMENT(t): r'\#.*' ; t.value = t.value.rstrip('\n') ; return t
def t_IDENTIFIER(t):
    r'[a-zA-Z_]\w*'
    lower_val = t.value.lower()
    if lower_val in reserved:
        t.type = reserved[lower_val]
    return t

# Métodos librería PLY
t_ignore = ' \t'
def t_newline(t): r'\n+' ; t.lexer.lineno += t.value.count("\n")
def t_error(t): print(f"[LEXER ERROR] Caracter no reconocido '{t.value[0]}'") ; t.lexer.skip(1)

# Construir lexer
lexer = lex.lex()

# =========================================================================
# 2. CLASE TOKENS ADAPTADA Y CORRECCIÓN DE ERRORES (Index)
# =========================================================================

class TokensPLY:
    lexer = None  
    current_token = None 

    def __init__(self, lexer, source_code):
        self.lexer = lexer
        self.lexer.input(source_code)
        self.avanza()

    def current(self):
        if self.current_token and self.current_token.type != 'EOF':
            # Devolvemos el tipo y valor
            return [self.current_token.type, self.current_token.value]
        return ["EOL", ""]

    def current_lexpos(self):
        # Devuelve la posición léxica (índice de carácter) del token actual
        if self.current_token and hasattr(self.current_token, 'lexpos'):
            return self.current_token.lexpos
        return -1

    def avanza(self):
        self.current_token = self.lexer.token()
        if not self.current_token:
            self.current_token = lex.LexToken()
            self.current_token.type = 'EOL'
            self.current_token.value = ''
            self.current_token.lexpos = self.lexer.lexpos # Usar la posición final del lexer
            self.current_token.lineno = -1

# FUNCIÓN DE ERROR CORREGIDA PARA USAR LA POSICIÓN LÉXICA REAL
def addError(errors, expected, token, index):
  token_str = f"'{token[0]}', valor '{token[1]}'"
  # Se reporta index + 1 para mostrar la posición basada en 1 (más intuitivo)
  errors.append( f"ERROR en index {index + 1}: esperaba {expected}, recibio {token_str}"  )

# =========================================================================
# 3. PARSER DE DESCENSO RECURSIVO EXTENDIDO (LÓGICA)
# =========================================================================

# Declaraciones anticipadas
def expresion(tokens, errors): pass
def expr(tokens, errors): pass
def termino(tokens, errors): pass
def factor(tokens, errors): pass

# F -> (+|-)? ( (E) | CONST_FLOAT | CONST_INT | IDENTIFIER )
def factor(tokens, errors):
    token, content = tokens.current()
    current_pos = tokens.current_lexpos()
    
    if token == 'OP_PLUS' or token == 'OP_MINUS':
        tokens.avanza()
        token, content = tokens.current()
    
    if token == 'LPAREN':
        tokens.avanza()
        expresion(tokens, errors)
        
        token, content = tokens.current()
        if token == 'RPAREN':
            tokens.avanza()
        else:
            addError(errors, "')'", [token, content], current_pos)
            
    elif token == 'CONST_INT' or token == 'CONST_FLOAT' or token == 'IDENTIFIER':
        tokens.avanza()
        
    else:
        addError(errors, "'(' , Constante o ID", [token, content], current_pos)


# T' -> * F T' | / F T' | epsilon
def termino_prime(tokens, errors):
  token, content = tokens.current()

  if token == 'OP_MULT' or token == 'OP_DIV':
    tokens.avanza()
    factor(tokens, errors)
    termino_prime(tokens, errors)


# T -> F T'
def termino(tokens, errors):
  factor(tokens, errors)
  termino_prime(tokens, errors)


# E' -> + T E' | - T E' | epsilon
def expr_prime(tokens, errors):
  token, content = tokens.current()

  if token == 'OP_PLUS' or token == 'OP_MINUS':
    tokens.avanza()
    termino(tokens, errors)
    expr_prime(tokens, errors)


# E -> T E' (Expresión Aritmética)
def expr(tokens, errors):
  termino(tokens, errors)
  expr_prime(tokens, errors)


# RELACIONAL -> op_rel E RELACIONAL | epsilon (Opción que permite solo una comparación)
def relacional(tokens, errors):
    op_rel = ['OP_GT', 'OP_LT', 'OP_GEQ', 'OP_LEQ', 'OP_NEQ', 'OP_EQ']
    token, content = tokens.current()
    
    if token in op_rel:
        tokens.avanza()
        expr(tokens, errors)


# EXPRESION -> E RELACIONAL (Regla más alta para expresiones)
def expresion(tokens, errors):
    expr(tokens, errors)
    relacional(tokens, errors)


# ASSIGN -> id = EXPRESION ; (Punto de inicio de la verificación)
def assign(tokens, errors):
    token, content = tokens.current()
    current_pos = tokens.current_lexpos() # Posición del token actual (ID)

    # 1. IDENTIFIER (id)
    if token == 'IDENTIFIER':
        tokens.avanza()
        token, content = tokens.current()
        current_pos = tokens.current_lexpos() # Posición del siguiente token (=)
        
        # 2. OP_ASSIGN (=)
        if token == 'OP_ASSIGN':
            tokens.avanza()
            
            # 3. EXPRESION
            expresion(tokens, errors)
            
            token, content = tokens.current()
            current_pos = tokens.current_lexpos() # Posición del siguiente token (;)
            
            # 4. SEMICOLON (;)
            if token == 'SEMICOLON':
                tokens.avanza()
            else:
                addError(errors, "';'", [token, content], current_pos)
        else:
            addError(errors, "'='", [token, content], current_pos)
    else:
        addError(errors, "ID (Identificador)", [token, content], current_pos)


# =========================================================================
# 4. PRUEBA DE INTEGRACIÓN
# =========================================================================

def integrar_parser_lexer(lexer, source_code):
    print("-" * 60)
    print(f"Analizando Asignación: '{source_code}'")

    token_manager = TokensPLY(lexer, source_code)
    errors = []

    assign(token_manager, errors)

    # Comprobación de fin de línea
    current_token, current_content = token_manager.current()
    current_pos = token_manager.current_lexpos()

    if current_token != "EOL":
        addError( errors, "fin de línea (EOL)", token_manager.current() , current_pos )
    
    print("-" * 60)
    if len(errors) == 0:
        print("✅ ANÁLISIS COMPLETO: La asignación es sintácticamente válida.")
    else:
        print("❌ ERRORES DE SINTAXIS ENCONTRADOS:")
        for e in errors:
            print(e)

# =========================================================================
# 5. EJECUCIÓN
# =========================================================================

if __name__ == "__main__":
    
    # --- Pruebas Anteriores (Ahora con indexación de carácter) ---
    
    # 1. Correcta: resultado = (a + 5.0) / (b - 2);
    integrar_parser_lexer(lexer, "resultado = (a + 5.0) / (b - 2);") 
    
    # 2. ERROR: Falta operador de asignación (El error ahora apuntará al índice 9)
    # Cadena: resultado (a + 5);
    # Índices: 0123456789...
    # Token '(' empieza en índice 9
    integrar_parser_lexer(lexer, "resultado (a + 5);")
    
    # 3. ERROR: Falta punto y coma (El error ahora apuntará al índice 15)
    # Cadena: variable = 100
    # Índices: 0123456789012345
    # El EOL/EOF ocurre después de '100' (índice 15)
    integrar_parser_lexer(lexer, "variable = 100")