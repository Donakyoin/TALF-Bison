%{

#include <stdio.h>
#include "eazy.tab.h"
#include <string.h>

%}

LETRA                   [a-zA-Z]

DIGITO                  [0-9]
HEX                     [0-9a-fA-F]
OCTAL                   [0-7]

EXP                     "exp"[+-]?({DECIMAL}|{OCTAL}|{HEXADECIMAL})+

DECIMAL                 {DIGITO}+
HEXADECIMAL             %[hH]{HEX}+ 
OCTAL_DECIMAL           %{OCTAL}+

REAL_DECIMAL            {DECIMAL}?","{DECIMAL}
REAL_OCTAL              %{OCTAL}*","{OCTAL}+ 
REAL_HEXADECIMAL        %[hH]{HEX}*","{HEX}+

REAL_EXP_DECIMAL        ({DECIMAL}|{REAL_DECIMAL}){EXP}
REAL_EXP_OCTAL          ({OCTAL}|{REAL_OCTAL}){EXP}
REAL_EXP_HEXADECIMAL    ({HEXADECIMAL}|{REAL_HEXADECIMAL}){EXP}

IDENTIFICADOR           ({LETRA}|_)({LETRA}|{DIGITO}|_)*

ESCAPE_COMUN            %[fnrt]
ESCAPE_OCTAL            %({OCTAL}{1,3})
ESCAPE_HEXA             %[hH]{HEX}+

ESCAPE                  ({ESCAPE_COMUN}|{ESCAPE_OCTAL}|{ESCAPE_HEXA})

CARACTER                (%\"|%%|{ESCAPE}|[^%\"\n])
CADENA                  \"({CARACTER}|{ESCAPE}|%%|%\")*\"

DELIMITADOR             "{"|"}"|"("|")"|":"|";"|"."

OP_ARITMETICOS          "+"|"-"|"*"|"/" 
OP_BITS                 "&"|"|"|"@"|"~"
OP_RELACIONAL           "<"|">"
OP_LOGICO               "!"
OP_MEMORIA              "?"|"["|"]"|"^"

OPERADOR_SIMPLE         ({OP_ARITMETICOS}|{OP_BITS}|{OP_RELACIONAL}|{OP_LOGICO}|{OP_MEMORIA})

ESPACIO                 [ \t\r]

%x CADENA_MULTILINEA
%x COMENTARIO

%option yylineno

%%

\n {  }
{ESPACIO}+ { }

{OPERADOR_SIMPLE}       { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return yytext[0]; }

"tamano"                { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return TAMANO;}
"mod"                   { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return MOD; }

":="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return ASIG; }
"mod="                  { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return MOD_ASIG; }
"|="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return OR_ASIG; }
"!="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return NEQ; }
"**"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return POTENCIA; }
"+="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return SUMA_ASIG; }
"<-="                   { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return FI_ASIG; }
"**="                   { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return POT_ASIG; }
"&&"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return AND; }
"-="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return RESTA_ASIG; }
"->="                   { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return FD_ASIG; }
">="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return GE; }
"||"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return OR; }
"^?"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return INDIRECCION; }
"*="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return MULT_ASIG; }
"&="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return AND_ASIG; }
"<="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return LE; }
"/="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return DIV_ASIG; }
"@="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return XOR_ASIG; }
"=="                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return EQ; }
"<-"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return FLECHA_IZDA; }
"->"                    { printf("Línea %d - Operador: '%s'\n", yylineno, yytext); return FLECHA_DCHA; }

{DELIMITADOR}           { printf("Línea %d - Delimitador: '%s'\n", yylineno, yytext); return yytext[0]; }
"::"                    { printf("Línea %d - Delimitador: '%s'\n", yylineno, yytext); return PTOS; }

(?i:"abstracto")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ABSTRACTO; }
(?i:"cada")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CADA; }
(?i:"cadena")           { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CADENA; }
(?i:"caracter")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CARACTER; }
(?i:"clase")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CLASE; }
(?i:"como")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return COMO; }
(?i:"constantes")       { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CONSTANTES; }
(?i:"constructor")      { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CONSTRUCTOR; }
(?i:"continuar")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return CONTINUAR; }
(?i:"de")               { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return DE; }
(?i:"defecto")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return DEFECTO; }
(?i:"destructor")       { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return DESTRUCTOR; }
(?i:"devolver")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return DEVOLVER; }
(?i:"ejecuta")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return EJECUTA; }
(?i:"en")               { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return EN; }
(?i:"encambio")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ENCAMBIO; }
(?i:"entero")           { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ENTERO; }
(?i:"enumeracion")      { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ENUMERACION; }
(?i:"es")               { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ES; }
(?i:"escape")           { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ESCAPE; }
(?i:"especifico")       { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ESPECIFICO; }
(?i:"estructura")       { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ESTRUCTURA; }
(?i:"etiqueta")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ETIQUETA; }
(?i:"excepcion")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return EXCEPCION; }
(?i:"fichero")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return FICHERO; }
(?i:"fin")              { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return FIN; }
(?i:"final")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return FINAL; }
(?i:"funcion")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return FUNCION; }
(?i:"generico")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return GENERICO; }
(?i:"hash")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return HASH; }
(?i:"hacer")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return HACER; }
(?i:"importar")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return IMPORTAR; }
(?i:"lanza")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return LANZA; }
(?i:"mientras")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return MIENTRAS; }
(?i:"nada")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return NADA; }
(?i:"otra")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return OTRA; }
(?i:"para")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PARA; }
(?i:"principio")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PRINCIPIO; }
(?i:"privado")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PRIVADO; }
(?i:"programa")         { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PROGRAMA; }
(?i:"protegido")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PROTEGIDO; }
(?i:"publico")          { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return PUBLICO; }
(?i:"real")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return REAL; }
(?i:"ref")              { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return REF; }
(?i:"saltar")           { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return SALTAR; }
(?i:"si")               { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return SI; }
(?i:"sino")             { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return SINO; }
(?i:"tabla")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return TABLA; }
(?i:"tipos")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return TIPOS; }
(?i:"ultima")           { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return ULTIMA;}
(?i:"union")            { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return UNION; }
(?i:"variables")        { printf("Línea %d - Palabra reservada: '%s'\n", yylineno, yytext); return VARIABLES; }

{REAL_EXP_DECIMAL}      { printf("Línea %d - Constante real exp decimal: '%s'\n", yylineno, yytext); return CTC_REAL; }
{REAL_EXP_OCTAL}        { printf("Línea %d - Constante real exp octal: '%s'\n", yylineno, yytext); return CTC_REAL; }
{REAL_EXP_HEXADECIMAL}  { printf("Línea %d - Constante real exp hexadecimal: '%s'\n", yylineno, yytext); return CTC_REAL; }

{REAL_DECIMAL}          { printf("Línea %d - Constante real decimal: '%s'\n", yylineno, yytext); return CTC_REAL; }
{REAL_OCTAL}            { printf("Línea %d - Constante real octal: '%s'\n", yylineno, yytext); return CTC_REAL; }
{REAL_HEXADECIMAL}      { printf("Línea %d - Constante real hexadecimal: '%s'\n", yylineno, yytext); return CTC_REAL; }

{DECIMAL}               { printf("Línea %d - Constante entera decimal: '%s'\n", yylineno, yytext); return CTC_ENTERA; }
{HEXADECIMAL}           { printf("Línea %d - Constante entera hexadecimal: '%s'\n", yylineno, yytext); return CTC_ENTERA; }
{OCTAL_DECIMAL}         { printf("Línea %d - Constante entera octal: '%s'\n", yylineno, yytext); return CTC_ENTERA; }

{IDENTIFICADOR}         { printf("Línea %d - Identificador: '%s'\n", yylineno, yytext); return IDENTIFICADOR; }

{CARACTER}              { printf("Línea %d - Constante carácter: %s\n", yylineno, yytext); return CTC_CARACTER; }
{CADENA}                { printf("Línea %d - Constante cadena: %s\n", yylineno, yytext); return CTC_CADENA; }

\"({CARACTER}|{ESCAPE})*%\n {
    printf("Línea %d - Constante cadena multilínea: %s", yylineno, yytext);
    BEGIN(CADENA_MULTILINEA);
}

<CADENA_MULTILINEA>({CARACTER}|{ESCAPE})*%\n {
    printf("%s", yytext);
}

<CADENA_MULTILINEA>({CARACTER}|{ESCAPE})*\" {
    printf("%s\n", yytext);
    BEGIN(INITIAL);
    return CTC_CADENA;
}

"%%".*    ; 

"%/"                      BEGIN(COMENTARIO); 

<COMENTARIO>"/%"          BEGIN(INITIAL);  
<COMENTARIO>\n            ;  
<COMENTARIO>.             ;

. { printf("Línea %d - Error: Carácter no reconocido '%s'\n", yylineno, yytext); }


%%



