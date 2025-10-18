# Francisco Aquino - A00833409
# Descenso Recursivo - Parser / Lexer

# Librería de regex (expresiones regulares)
import ply.lex as lex

# Diccionario de palabras reservadas
reserved = {
    'program': 'PROGRAM',
    'main': 'MAIN',
    'var': 'VAR',
    'end': 'END', 
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'void': 'VOID', 
    'if': 'IF',
    'else': 'ELSE',
    'do': 'DO',
    'while': 'WHILE',
    'print': 'PRINT', 
    'return': 'RETURN',
    'break': 'BREAK',
    'continue': 'CONTINUE'
}

# Diccionario de lexemas
tokens = [
    # Constantes alfanuméricas
    'IDENTIFIER', 'CONST_INT', 'CONST_FLOAT', 'CONST_STRING',
    # Operadores aritméticos y lógicos
    'OP_ASSIGN', 'OP_EQ', 'OP_NEQ', 'OP_LEQ', 'OP_GEQ', 'OP_LT', 'OP_GT', 
    'OP_PLUS', 'OP_MINUS', 'OP_MULT', 'OP_DIV', 'OP_AND', 'OP_ANDAND', 'OP_OR', 'OP_OROR', 'OP_NOT',
    # Delimitadores
    'SEMICOLON', 'COMMA', 'COLON', 'LBRACE', 'RBRACE', 'LBRACKET', 'RBRACKET', 'LPAREN', 'RPAREN',
    # Comentarios
    'COMMENT',
    # Errores
    'INVALID_ID'
] + list(reserved.values())

# Token Regex (PLY)
# Operadores aritméticos y lógicos
t_OP_EQ = r'==' ; t_OP_NEQ = r'!=' ; t_OP_LEQ = r'<=' ; t_OP_GEQ = r'>=' ; t_OP_ASSIGN = r'=' 
t_OP_LT = r'<' ; t_OP_GT = r'>' ; t_OP_PLUS = r'\+' ; t_OP_MINUS = r'-' ; t_OP_MULT = r'\*' 
t_OP_DIV = r'/' ; t_OP_ANDAND = r'&&' ; t_OP_AND = r'&' ; t_OP_OROR = r'\|\|' ; t_OP_OR = r'\|' ; t_OP_NOT = r'!'
# Delimitadores
t_SEMICOLON = r';' ; t_COMMA = r',' ; t_COLON = r':' ; t_LBRACE = r'\{' ; t_RBRACE = r'\}'
t_LBRACKET = r'\[' ; t_RBRACKET = r'\]' ; t_LPAREN = r'\(' ; t_RPAREN = r'\)'

# Reglas de reconocimiento de tokens
# Identificadores inválidos
def t_INVALID_ID(t):
    r'\d+[a-zA-Z_]\w*'
    print(f"[ERROR] Identificador inválido '{t.value}'")
    t.lexer.skip(1)
# Constantes flotantes
def t_CONST_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t
# Constantes enteras
def t_CONST_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t
# Cadenas de caracteres
def t_CONST_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    return t
# Comentarios
def t_COMMENT(t):
    r'\#.*'
    t.value = t.value.rstrip('\n') 
    return t 
# Identificadores y palabras reservadas
def t_IDENTIFIER(t):
    r'[a-zA-Z_]\w*'
    # Reconocer y convertir minúsculas para palabras reservadas
    lower_val = t.value.lower()
    if lower_val in reserved:
        t.type = reserved[lower_val]
    return t

# Métodos librería PLY
# Ignorar espacios en blanco y tabulaciones
t_ignore = ' \t'
# Contar los saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")
# Detectar un error léxico (Caracter fuera del diccionario)
def t_error(t):
    print(f"[ERROR] Caracter no reconocido '{t.value[0]}'")
    t.lexer.skip(1)
# Construir lexer
lexer = lex.lex()

# Clase para el manejo de tokens (Parser - Lexer)
class TokensPLY:
    # Atributo para almacenar la instancia del lexer
    lexer = None  
    # Atributo para almacenar el token actual
    current_token = None 
    # Constructor 
    def __init__(self, lexer, source_code):
        # Instanciar lexer
        self.lexer = lexer
        # Almacenar cadenas de código de prueba
        self.lexer.input(source_code)
        # Llamar a la función avanza
        self.avanza()
    # Método para devolver el token actual
    def current(self):
        # Verificar si el token actual no es EOL (End of Line)
        if self.current_token and self.current_token.type != 'EOL':
            # Devolver el tipo y valor del token actual
            return [self.current_token.type, self.current_token.value]
        # Retornar EOL en caso de cadena de tokens vacía
        return ["EOL", ""]
    # Método para devolver la posición del token actual
    def current_lexpos(self):
        # Verificar la posición del token actual
        if self.current_token and hasattr(self.current_token, 'lexpos'):
            # Retornar índice
            return self.current_token.lexpos
        return -1
    # Método para avanzar al siguiente token
    def avanza(self):
        # Almacenar el siguiente token
        self.current_token = self.lexer.token()
        # Verificar si todavía existen tokens disponibles
        if not self.current_token:
            # Crear objeto para la línea 
            self.current_token = lex.LexToken()
            # Asignar EOL al final de línea
            self.current_token.type = 'EOL'
            self.current_token.value = ''
            # Asignar posición final a EOL
            self.current_token.lexpos = self.lexer.lexpos
            self.current_token.lineno = -1

# Función para agregar errores e identificar su posición en línea
def addError(errors, expected, token, index):
  # Construir mensaje de error
  token_str = f"'{token[0]}', valor '{token[1]}'"
  errors.append( f"ERROR en index {index + 1}: esperaba {expected}, recibio {token_str}"  )

# Reglas gramaticales
def expresion(tokens, errors): pass
def expr(tokens, errors): pass
def termino(tokens, errors): pass
def factor(tokens, errors): pass
# F -> (+|-)? ( (E) | CONST_FLOAT | CONST_INT | IDENTIFIER )
def factor(tokens, errors):
    # Obtener tipo y valor del token actual
    token, content = tokens.current()
    # Obtener posición del token actual
    current_pos = tokens.current_lexpos()
    # Verificar si existen operadores opcionales (+ | -)
    if token == 'OP_PLUS' or token == 'OP_MINUS':
        # Avanzar al siguiente token
        tokens.avanza()
        # Actualizar token actual
        token, content = tokens.current()
    # Verificar si existe una expresión entre paréntesis
    if token == 'LPAREN':
        # Avanzar al siguiente token
        tokens.avanza()
        # Llamar recursivamente a EXPRESION
        expresion(tokens, errors)
        # Actualizar token actual
        token, content = tokens.current()
        # Verificar final de EXPRESION
        if token == 'RPAREN':
            # Avanzar al siguiente token
            tokens.avanza()
        else:
            # Agregar error en cierre
            addError(errors, "')'", [token, content], current_pos)
    # Verificar si existe una constante o identificador    
    elif token == 'CONST_INT' or token == 'CONST_FLOAT' or token == 'IDENTIFIER':
        # Avanzar al siguiente token
        tokens.avanza()
    else:
        # Agregar error de caracter inválido
        addError(errors, "'(' , Constante o ID", [token, content], current_pos)

# T' -> * F T' | / F T' | epsilon
def termino_prime(tokens, errors):
  # Obtener tipo y valor del token actual
  token, content = tokens.current()
  # Verificar si existen operadores (* | /)
  if token == 'OP_MULT' or token == 'OP_DIV':
    # Avanzar al siguiente token
    tokens.avanza()
    # Llamar a la regla FACTOR
    factor(tokens, errors)
    # Llamar recursivamente a T'
    termino_prime(tokens, errors)

# T -> F T'
def termino(tokens, errors):
  # Llamar a la regla FACTOR
  factor(tokens, errors)
  # Llamar a la regla T'
  termino_prime(tokens, errors)

# E' -> + T E' | - T E' | epsilon
def expr_prime(tokens, errors):
  # Obtener tipo y valor del token actual
  token, content = tokens.current()
  # Verificar si existen operadores (+ | -)
  if token == 'OP_PLUS' or token == 'OP_MINUS':
    # Avanzar al siguiente token
    tokens.avanza()
    # Llamar a la regla TERMINO
    termino(tokens, errors)
    # Llamar recursivamente a E'
    expr_prime(tokens, errors)

# E -> T E' (Expresión Aritmética)
def expr(tokens, errors):
  # Llamar a la regla TERMINO
  termino(tokens, errors)
  # Llamar a la regla E'
  expr_prime(tokens, errors)

# RELACIONAL -> op_rel E RELACIONAL | epsilon
def relacional(tokens, errors):
    # Lista válida de operadores relacionales
    op_rel = ['OP_GT', 'OP_LT', 'OP_GEQ', 'OP_LEQ', 'OP_NEQ', 'OP_EQ']
    # Obtener tipo y valor del token actual
    token, content = tokens.current()
    # Verificar si el token actual es un operador válido
    if token in op_rel:
        # Avanzar al siguiente token
        tokens.avanza()
        # Llamar a la regla EXPRESION
        expr(tokens, errors)

# EXPRESION -> E RELACIONAL
def expresion(tokens, errors):
    # Llamar a la regla E
    expr(tokens, errors)
    # Llamar a la regla RELACIONAL
    relacional(tokens, errors)

# ASSIGN -> id = EXPRESION; 
def assign(tokens, errors):
    # Obtener tipo y valor del token actual
    token, content = tokens.current()
    # Obtener posición del token actual
    current_pos = tokens.current_lexpos()
    # Verificar si el primer token es un identificador
    if token == 'IDENTIFIER':
        # Avanzar al siguiente token
        tokens.avanza()
        # Obtener tipo y valor del token actual
        token, content = tokens.current()
        # Obtener posición del token actual
        current_pos = tokens.current_lexpos()
        # Verificar si el siguiente token es un operador de asignación (=)
        if token == 'OP_ASSIGN':
            # Avanzar al siguiente token
            tokens.avanza()
            # Llamar a la regla EXPRESION
            expresion(tokens, errors)
            # Obtener tipo y valor del token actual
            token, content = tokens.current()
            # Obtener posición del token actual
            current_pos = tokens.current_lexpos()
            # Verificar si el siguiente token es un operador válido (;)
            if token == 'SEMICOLON':
                # Avanzar al siguiente token
                tokens.avanza()
            else:
                # Agregar error de caracter inválido
                addError(errors, "';'", [token, content], current_pos)
        else:
            # Agregar error de caracter inválido
            addError(errors, "'='", [token, content], current_pos)
    else:
        # Agregar error de caracter inválido
        addError(errors, "ID (Identificador)", [token, content], current_pos)

# Función para integrar el Parser-Lexer
def integrar_parser_lexer(lexer, source_code):
    # Imprimir separador visual
    print("\n" + "=" * 60)
    # Imprimir código analizado
    print(f"Analizando tokens: '{source_code}'")
    # Inicializar token manager y lista de errores
    token_manager = TokensPLY(lexer, source_code)
    errors = []
    # Ejecutar análisis sintáctico
    assign(token_manager, errors)
    # Obtener token actual y su posición
    current_token, current_content = token_manager.current()
    current_pos = token_manager.current_lexpos()
    # Verificar si existen tokens después de un posible error
    if len(errors) == 0 and current_token != "EOL":
        # Agregar error de EOL (End Of File)
        addError( errors, "fin de línea (EOL)", token_manager.current() , current_pos )
    # Verificar y mostrar resultados del análisis
    if len(errors) == 0:
        print("Análisis Válido.")
    else:
        print("Análisis Inválido - Errores encontrados:")
        # Iterar sobre errores y mostrarlos
        for e in errors:
            print(e)

# Ejecutar programa
if __name__ == "__main__":
    # Casos de prueba
    integrar_parser_lexer(lexer, "suma = (a * b) + c / 2.0;")
    integrar_parser_lexer(lexer, "resultado = 100 * (5 + 3;")
    integrar_parser_lexer(lexer, "estado = (x >= y) && !z;") 
    integrar_parser_lexer(lexer, "valor = - + 5;")
    integrar_parser_lexer(lexer, "final = 1.0 + 2.0 * 3.0 / 4.0;") 
    integrar_parser_lexer(lexer, "a = b - c; d = e + f;")
    integrar_parser_lexer(lexer, "check = (a == b) || (c != d);") 
    integrar_parser_lexer(lexer, "a = 50 + / 2;") 
    integrar_parser_lexer(lexer, "area = -mi_lado * -otro_lado;")
    integrar_parser_lexer(lexer, "asignacion (a + 1);") 
    integrar_parser_lexer(lexer, "total = -1 * (y - x);") 
    integrar_parser_lexer(lexer, "variable = 200") 
    integrar_parser_lexer(lexer, "es_valido = !((x < 0) || (x > 10));") 
    integrar_parser_lexer(lexer, "100 = total_final;") 
    integrar_parser_lexer(lexer, "res = (a + b) / (c + d);")
    integrar_parser_lexer(lexer, "promedio = (n1 + n2) / ;") 
    integrar_parser_lexer(lexer, "r = -7 - 8;")
    integrar_parser_lexer(lexer, "a = b + 5 ) * c;")