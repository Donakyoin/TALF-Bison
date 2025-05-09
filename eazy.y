
%{

  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();

  #define YYDEBUG 1

  int yyerror(char *);

%}

%token ABSTRACTO AND ASIG AND_ASIG CADA CADENA CARACTER CLASE COMO CONSTANTES CONSTRUCTOR CONTINUAR CTC_CADENA
%token CTC_CARACTER CTC_ENTERA CTC_REAL DE DEFECTO DESTRUCTOR DEVOLVER DIV_ASIG EJECUTA ENCAMBIO ENTERO
%token ENUMERACION EQ EN ES ESCAPE ESPECIFICO ESTRUCTURA ETIQUETA EXCEPCION FD_ASIG FI_ASIG FICHERO FIN FINAL
%token FLECHA_DCHA FLECHA_IZDA FUNCION GENERICO HACER HASH GE IDENTIFICADOR IMPORTAR INDIRECCION LANZA LE MIENTRAS
%token MOD MOD_ASIG MULT_ASIG NADA NEQ OR OTRA OR_ASIG PARA POT_ASIG POTENCIA PRINCIPIO PRIVADO PROGRAMA PROTEGIDO
%token PTOS PUBLICO REAL REF RESTA_ASIG SALTAR SI SINO SUMA_ASIG TAMANO TABLA TIPOS ULTIMA UNION VARIABLES XOR_ASIG

%start programa

%%

/***************************************/
/* Especificación de un programa Eazy */
/*************************************/

//  programa ::= cabecera_programa bloque_programa
programa: 
 cabecera_programa bloque_programa { printf("\tprograma -> cabecera_programa bloque_programa\n"); }
;

//  cabecera_programa ::= ’programa’ IDENTIFICADOR ’.’ [ libreria ]*
cabecera_programa: 
 PROGRAMA IDENTIFICADOR '.' librerias { printf("\tcabecera_programa -> PROGRAMA IDENTIFICADOR '.' librerias\n"); }
;

librerias: 
 librerias libreria { printf("\tlibrerias -> librerias libreria\n"); }
 | { printf("\tlibrerias -> \n"); }
;

//  libreria ::= ’importar’ ( nombre )+ ’.’ | ’importar’ nombre ’como’ IDENTIFICADOR ’.’
libreria: 
 IMPORTAR lista_nombres '.' { printf("\tlibreria -> IMPORTAR lista_nombres '.'\n"); }
 | IMPORTAR nombre COMO IDENTIFICADOR '.' { printf("\tliberia -> IMPORTAR nombre COMO IDENTIFICADOR '.'\n"); }
;

lista_nombres: 
 lista_nombres ';' nombre { printf("\tlista_nombres -> lista_nombres ';' nombre\n"); }
 | nombre { printf("\tlista_nombres -> nombre\n"); }
;

//  nombre ::= (IDENTIFICADOR)::+  
nombre: 
 nombre PTOS IDENTIFICADOR { printf("\tnombre -> nombre '::' IDENTIFICADOR\n"); }
 | IDENTIFICADOR { printf("\tnombre -> IDENTIFICADOR\n"); }
;

//  bloque_programa ::= [ declaraciones_tipos ]? [ declaraciones_constantes ]? [ declaraciones_variables ]? [ declaracion_funcion ]* bloque_instrucciones
bloque_programa: 
 opcion_dec_tipos opcion_dec_constantes opcion_dec_variables funciones bloque_instrucciones  { printf("\tbloque_programa -> opcion_dec_tipos opcion_dec_constantes opcion_dec_variables funciones bloque_instrucciones\n"); }
;

opcion_dec_tipos: 
 declaraciones_tipos { printf("\topcion_dec_tipos -> declaraciones_tipos\n"); }
 | { printf("\topcion_dec_tipos -> \n"); }
;

opcion_dec_constantes: 
 declaraciones_constantes  { printf("\topcion_dec_constantes -> declaraciones_constantes\n"); }
 | { printf("\topcion_dec_constantes -> \n"); }
;

opcion_dec_variables: 
 declaraciones_variables { printf("\topcion_dec_variables -> declaraciones_variables\n"); }
 | { printf("\topcion_dec_variables -> \n"); }
;

funciones: 
 funciones declaracion_funcion { printf("\tfunciones -> funciones declaracion_funcion\n"); }
 | { printf("\tfunciones -> \n"); }
;

/**********/
/* Tipos */
/********/

//  declaraciones_tipos ::= ’tipos’ [ declaracion_tipo ]+ ’fin’
declaraciones_tipos: 
 TIPOS tipos FIN { printf("\tdeclaraciones_tipos -> TIPOS tipos FIN\n"); }
;

tipos: 
 tipos declaracion_tipo  { printf("\ttipos -> tipos declaracion_tipo\n"); }
 | declaracion_tipo  { printf("\ttipos -> declaracion_tipo\n"); }
;

//  declaracion_tipo ::= [ visibilidad ]? IDENTIFICADOR ’es’ [ ’ref’ ]* tipo_basico ’.’ | [ visibilidad ]? IDENTIFICADOR ’es’ [ ’ref’ ]* tipo_estructurado
declaracion_tipo: 
 opcion_visibilidad IDENTIFICADOR ES refs tipo_basico '.'  { printf("\tdeclaracion_tipo -> opcion_visibilidad IDENTIFICADOR ES refs tipo_basico '.'\n"); }
 | opcion_visibilidad IDENTIFICADOR ES refs tipo_estructurado  { printf("\tdeclaracion_tipo -> opcion_visibilidad IDENTIFICADOR ES refs tipo_estructurado\n"); }
 | error '.' { printf("\tError en la declaración de tipos\n"); yyerrok; }
;

opcion_visibilidad: 
 visibilidad { printf("\topcion_visibilidad -> visibilidad\n"); }
 | { printf("\topcion_visibilidad -> \n"); }
;

refs: 
 refs REF  { printf("\trefs -> refs REF\n"); }
 | { printf("\trefs -> \n"); }
;

//  visibilidad ::= ’publico’ | ’privado’ | ’protegido’
visibilidad: 
 PUBLICO { printf("\tvisibilidad -> PUBLICO\n"); }
 | PRIVADO { printf("\tvisibilidad -> PRIVADO\n"); }
 | PROTEGIDO { printf("\tvisibilidad -> PROTEGIDO\n"); }
;

//  tipo_basico ::= nombre | tipo_escalar | tipo_tabla
tipo_basico: 
 nombre  { printf("\ttipo_basico -> nombre\n"); }
 | tipo_escalar  { printf("\ttipo_basico -> tipo_escalar\n"); }
 | tipo_tabla  { printf("\ttipo_basico -> tipo_tabla\n"); }
;

//  tipo_escalar ::= ENTERO | REAL | CARACTER | CADENA | FICHERO | EXCEPCION
tipo_escalar: 
 ENTERO  { printf("\ttipo_escalar -> ENTERO\n"); }
 | REAL  { printf("\ttipo_escalar -> REAL\n"); }
 | CARACTER  { printf("\ttipo_escalar -> CARACTER\n"); }
 | CADENA  { printf("\ttipo_escalar -> CADENA\n"); }
 | FICHERO { printf("\ttipo_escalar -> FICHERO\n"); }
 | EXCEPCION { printf("\ttipo_escalar -> EXCEPCION\n"); }
;

//  tipo_tabla ::= ’tabla’ ’de’ especificacion_tipo | ’tabla’ ’hash’ ’de’ especificacion_tipo
tipo_tabla: 
 TABLA DE especificacion_tipo  { printf("\ttipo_tabla -> TABLA DE especificacion_tipo\n"); }
 | TABLA HASH DE especificacion_tipo { printf("\ttipo_tabla -> TABLA HASH DE especificacion_tipo\n"); }
;

//  especificacion_tipo ::= [ ’ref’ ]* tipo_basico | [ ’ref’ ]* tipo_estructurado
especificacion_tipo: 
 refs tipo_basico { printf("\tespecificacion_tipo -> refs tipo_basico\n"); }
 | refs tipo_estructurado  { printf("\tespecificacion_tipo -> refs tipo_estructurado\n"); }
;

//  tipo_estructurado ::= ’enumeracion’ ’de’ tipo_escalar ( elemento_enum )+ ’fin’ | ’estructura’ ( linea_campo )+ ’fin’ | ’union’ ( linea_campo )+ ’fin’ | ’clase’ [ ’ultima’ ]? [ ’(’ ( nombre )+ ’)’ ]? componentes ’fin’
tipo_estructurado: 
 ENUMERACION DE tipo_escalar lista_enums FIN { printf("\ttipo_estructurado -> ENUMERACION DE tipo_escalar lista_enums FIN\n"); }
 | ESTRUCTURA lineas_campo FIN { printf("\ttipo_estructurado -> ESTRUCTURA lineas_campo FIN\n"); } 
 | UNION lineas_campo FIN  { printf("\ttipo_estructurado -> UNION lineas_campo FIN\n"); } 
 | CLASE opcion_ultima opcion_nombres componentes FIN  { printf("\ttipo_estructurado -> CLASE opcion_ultima opcion_nombres componentes FIN\n"); } 
;

lista_enums: 
 lista_enums ';' elemento_enum { printf("\tlista_enums -> lista_enums ';' elemento_enum\n"); }
 | elemento_enum { printf("\tlista_enums -> elemento_enum\n"); }
;

lineas_campo: 
 lineas_campo ';' linea_campo { printf("\tlineas_campo -> lineas_campo ';' linea_campo\n"); }
 | linea_campo { printf("\tlineas_campo -> linea_campo\n"); }
;

opcion_ultima: 
 ULTIMA { printf("\topcion_ultima -> ULTIMA\n"); }
 | { printf("\topcion_ultima -> \n"); }
;

opcion_nombres: 
 '(' lista_nombres ')' { printf("\topcion_nombres -> (lista_nombres)\n"); }
 | { printf("\topcion_nombres -> \n"); }
 ;

//  elemento_enum ::= IDENTIFICADOR ’:=’ expresion
elemento_enum: 
 IDENTIFICADOR ASIG expresion { printf("\telemento_enum -> IDENTIFICADOR ':=' expresion\n"); }
;

//  linea_campo ::= ( IDENTIFICADOR )+ ’es’ especificacion_tipo
linea_campo: 
 identificadores ES especificacion_tipo  { printf("\topcion_dec_tipos -> identificadores ES especificacion_tipo\n"); }
;

identificadores: 
 identificadores ';' IDENTIFICADOR { printf("\tidentificadores -> identificadores ';' IDENTIFICADOR\n"); }
 | IDENTIFICADOR { printf("\tidentificadores -> IDENTIFICADOR\n"); }
;

//  componentes ::= [ declaraciones_tipos ]? [ declaraciones_constantes ]? [ declaraciones_variables ]? [ declaracion_metodo ]+
componentes: 
 opcion_dec_tipos opcion_dec_constantes opcion_dec_variables metodos { printf("\tcomponentes -> opcion_dec_tipos opcion_dec_constantes opcion_dec_variables metodos\n"); }
;

metodos: 
 metodos declaracion_metodo  { printf("\tmetodos -> metodos declaracion_metodo\n"); }
 | declaracion_metodo  { printf("\tmetodos -> declaracion_metodo\n"); }
;

//  declaracion_metodo ::= [ visibilidad ]? [ modificador ]? firma_funcion cuerpo_funcion
declaracion_metodo: 
 opcion_visibilidad opcion_modificador firma_funcion cuerpo_funcion  { printf("\tdeclaracion_metodo -> opcion_visibilidad opcion_modificador firma_funcion cuerpo_funcion\n"); }
;

opcion_modificador: 
 modificador { printf("\topcion_modificador -> modificador\n"); }
 | { printf("\topcion_modificador -> \n"); }
;

//  modificador ::= ’constructor’ | ’destructor’ | ’generico’ | ’abstracto’ | ’específico’ | ’final’
modificador: 
 CONSTRUCTOR { printf("\tmodificador -> CONSTRUCTOR\n"); }
 | DESTRUCTOR  { printf("\tmodificador -> DESTRUCTOR\n"); }
 | GENERICO  { printf("\tmodificador -> GENERICO\n"); }
 | ABSTRACTO { printf("\tmodificador -> ABSTRACTO\n"); }
 | ESPECIFICO  { printf("\tmodificador -> ESPECIFICO\n"); }
 | FINAL { printf("\tmodificador -> FINAL\n"); }
;

/***************/
/* Constantes */
/*************/

//  declaraciones_constantes ::= ’constantes’ [ declaracion_constante ]+ ’fin’
declaraciones_constantes: 
 CONSTANTES dec_constantes FIN  { printf("\tdeclaraciones_constantes -> CONSTANTES dec_constantes FIN\n"); }  
;

dec_constantes: 
 dec_constantes declaracion_constante { printf("\tdec_constantes -> dec_constantes declaracion_constante\n"); }
 | declaracion_constante { printf("\tdec_constantes -> declaracion_constante\n"); }
;

//  declaracion_constante ::= [ visibilidad ]? IDENTIFICADOR ’es’ tipo_basico ’:=’ constante ’.’
declaracion_constante: 
 opcion_visibilidad IDENTIFICADOR ES tipo_basico ASIG constante '.' { printf("\tdeclaracion_constante -> IDENTIFICADOR ES tipo_basico ':=' constante '.'\n"); } 
 | error '.' { printf("\tError en la declaración de constantes\n"); yyerrok; }
;

//  constante ::= CTC_ENTERA | CTC_REAL | CTC_CARACTER | CTC_CADENA | constante_tabla | constante_estructurada
constante: 
 CTC_ENTERA  { printf("\tconstante -> CTC_ENTERA\n"); }
 | CTC_REAL  { printf("\tconstante -> CTC_REAL\n"); }
 | CTC_CARACTER  { printf("\tconstante -> CTC_CARACTER\n"); }
 | CTC_CADENA  { printf("\tconstante -> CTC_CADENA\n"); }
 | constante_tabla { printf("\tconstante -> constante_tabla\n"); }
 | constante_estructurada  { printf("\tconstante -> constante_estructurada\n"); }
;

//  constante_tabla ::= ’(’ ( constante )* ’)’ | ’(’ ( elemento_hash )* ’)’
constante_tabla: 
 '(' lista_constantes ')'  { printf("\tconstante_tabla -> (lista_constantes)\n"); }
 | '(' lista_elem_hash ')'  { printf("\tconstante_tabla -> (lista_elem_hash)\n"); }
 | '('   ')' { printf("\tconstante_tabla -> ( )\n"); }
;

lista_constantes: 
 lista_constantes ';' constante  { printf("\tlista_constantes -> lista_constantes ';' constante\n"); }
 | constante { printf("\tlista_constantes -> constante\n"); } 
;
    
lista_elem_hash: 
 lista_elem_hash ';' elemento_hash  { printf("\tlista_elem_hash -> lista_elem_hash ';' elemento_hash\n"); }
 | elemento_hash { printf("\tlista_elem_hash -> elemento_hash\n"); }
;

//  elemento_hash::= CTC_CADENA ’->’ constante
elemento_hash: 
 CTC_CADENA FLECHA_DCHA constante  { printf("\telemento_hash -> CTC_CADENA '->' constante\n"); }
;

//  constante_estructurada ::= ’(’ ( campo_constante )+ ’)’
constante_estructurada: 
 '(' lista_campos_constantes ')'  { printf("\tconstante_estructurada -> (lista_campos_constantes)\n"); }
;

lista_campos_constantes: 
 lista_campos_constantes ';' campo_constante  { printf("\tlista_campos_constantes -> lista_campos_constantes ';' campo_constante\n"); }
 | campo_constante { printf("\tlista_campos_constantes -> campo_constante\n"); }
;

//  campo_constante ::= IDENTIFICADOR ’:=’ constante
campo_constante: 
 IDENTIFICADOR ASIG constante  { printf("\tcampo_constante -> IDENTIFICADOR ':=' constante\n"); }
;

/**************/
/* Variables */
/************/

//  declaraciones_variables ::= ’variables’ [ declaracion_variables ]+ ’fin’
declaraciones_variables: 
 VARIABLES dec_variables FIN { printf("\tdeclaraciones_variables -> VARIABLES dec_variables FIN\n"); }
;

dec_variables: 
 dec_variables declaracion_variables { printf("\tdec_variables -> dec_variables declaracion_variables\n"); }
 | declaracion_variables  { printf("\tdec_variables -> declaracion_variables\n"); }
;

//  declaracion_variables ::= [ visibilidad ]? ( IDENTIFICADOR )+ ’es’ especificacion_tipo [ ’:=’ ( expresion )+ ]? ’.’ 
declaracion_variables: 
 opcion_visibilidad identificadores ES especificacion_tipo opcion_expresion_variables '.'  { printf("\tdeclaracion_variables -> opcion_visibilidad identificadores ES especificacion_tipo opcion_expresion_variables '.'\n"); }
 | error '.' { printf("\tError en la declaración de variables\n"); yyerrok; }
;

opcion_expresion_variables: 
 ASIG expresiones { printf("\topcion_expresion_variables -> ':=' expresiones\n"); }
 | { printf("\topcion_expresion_variables -> \n"); }
;

expresiones: 
 expresiones ';' expresion { printf("\texpresiones -> expresiones ';' expresion\n"); }
 | expresion { printf("\texpresiones -> expresion\n"); }
;

/**************/
/* Funciones */
/************/

//  declaracion_funcion ::= [ visibilidad ]? firma_funcion cuerpo_funcion
declaracion_funcion: 
 opcion_visibilidad firma_funcion cuerpo_funcion { printf("\tdeclaracion_funcion -> opcion_visibilidad firma_funcion cuerpo_funcion\n"); }
;

//  firma_funcion ::= ’funcion’ IDENTIFICADOR [ ’(’ ( parametros ):+ ’)’ ]? ’->’ tipo_salida
firma_funcion: 
 FUNCION IDENTIFICADOR opcion_parametros_funcion FLECHA_DCHA tipo_salida { printf("\tfirma_funcion -> FUNCION IDENTIFICADOR opcion_parametros_funcion '->' tipo_salida\n"); }
;

opcion_parametros_funcion: 
 '(' lista_parametros ')' { printf("\topcion_parametros_funcion -> (lista_parametros)\n"); }
 | { printf("\topcion_parametros_funcion -> \n"); }
;

lista_parametros: 
 lista_parametros ':' parametros { printf("\tlista_parametros -> lista_parametros ':' parametros\n"); }
 | parametros  { printf("\tlista_parametros -> parametros\n"); }
;

//  parametros ::= ( IDENTIFICADOR )+ ’es’ especificacion_tipo [ ’:=’ ( expresion_constante )+ ]?
parametros: 
 identificadores ES especificacion_tipo opcion_expresion_constante  { printf("\tparametros -> identificadores ES especificacion_tipo opcion_expresion_constante\n"); }
;

opcion_expresion_constante: 
 ASIG constantes { printf("\topcion_expresion_constante -> ':=' constantes\n"); }
 | { printf("\topcion_expresion_constante -> \n"); }
; 

constantes: 
 constantes ';' expresion_constante { printf("\tconstantes -> constantes ';' expresion_constante\n"); }
 | expresion_constante { printf("\tconstantes -> expresion_constante\n"); }
;

//  tipo_salida ::= especificacion_tipo | ’nada’ 
tipo_salida: 
 especificacion_tipo { printf("\ttipo_salida -> especificacion_tipo\n"); }
 | NADA { printf("\ttipo_salida -> NADA\n"); }
;

//  cuerpo_funcion ::= [ declaraciones_constantes ]? [ declaraciones_variables ]? [ declaracion_funcion ]* bloque_instrucciones
cuerpo_funcion: 
 opcion_dec_constantes opcion_dec_variables funciones bloque_instrucciones { printf("\tcuerpo_funcion -> opcion_dec_constantes opcion_dec_variables funciones bloque_instrucciones\n"); }
;

//  bloque_instrucciones ::= ’principio’ [ instruccion ]+ ’fin’
bloque_instrucciones: 
 PRINCIPIO instrucciones FIN { printf("\tbloque_instrucciones -> PRINCIPIO instrucciones FIN\n"); }
;

instrucciones: 
 instrucciones instruccion { printf("\tinstrucciones-> instrucciones instruccion\n"); }
 | instruccion { printf("\tinstrucciones-> instruccion\n"); }
;

/******************/
/* Instrucciones */
/****************/

//  instruccion ::= instruccion_expresion | instruccion_bifurcacion | instruccion_bucle | instruccion_salto | instruccion_destino_salto | instruccion_devolver | instruccion_lanzamiento_excepcion | instruccion_captura_excepcion | instruccion_vacia
instruccion: 
 instruccion_expresion { printf("\tinstruccion -> instruccion_expresion\n"); }
 | instruccion_bifurcacion { printf("\tinstruccion -> instruccion_bifurcacion\n"); }
 | instruccion_bucle { printf("\tinstruccion -> instruccion_bucle\n"); }
 | instruccion_salto { printf("\tinstruccion -> instruccion_salto\n"); }
 | instruccion_destino_salto { printf("\tinstruccion -> instruccion_destino_salto\n"); }
 | instruccion_devolver { printf("\tinstruccion -> instruccion_devolver\n"); }
 | instruccion_lanzamiento_excepcion { printf("\tinstruccion -> instruccion_lanzamiento_excepcion\n"); }
 | instruccion_captura_excepcion { printf("\tinstruccion -> instruccion_captura_excepcion\n"); }
 | instruccion_vacia { printf("\tinstruccion -> instruccion_vacia\n"); }
 | error '.' { printf("\tError en las instrucciones\n"); yyerrok; }
;

//  instruccion_expresion ::= expresion_funcional ’.’ | asignacion ’.’
instruccion_expresion: 
 expresion_funcional '.' { printf("\tinstruccion_expresion -> expresion_funcional '.'\n"); }
 | asignacion '.' { printf("\tinstruccion_expresion -> asignacion '.'\n"); }
;

//  asignacion ::= expresion_indexada operador_asignacion expresion
asignacion: 
 expresion_indexada operador_asignacion expresion { printf("\tasignacion -> expresion_indexada operador_asignacion expresion\n"); }
;

//  operador_asignacion ::= ’:=’ | ’+=’ | ’-=’ | ’*=’ | ’/=’ | ’mod=’ | ’**=’ | ’<-=’ | ’->=’ | ’&=’ | ’@=’ | ’|=’
operador_asignacion: 
 ASIG { printf("\toperador_asignacion -> ':='\n"); }
 | SUMA_ASIG { printf("\toperador_asignacion -> '+='\n"); }
 | RESTA_ASIG { printf("\toperador_asignacion -> '-='\n"); }
 | MULT_ASIG { printf("\toperador_asignacion -> '*='\n"); }
 | DIV_ASIG { printf("\toperador_asignacion -> '/='\n"); }
 | MOD_ASIG { printf("\toperador_asignacion -> 'mod='\n"); }
 | POT_ASIG { printf("\toperador_asignacion -> '**='\n"); }
 | FI_ASIG { printf("\toperador_asignacion -> '<-='\n"); }
 | FD_ASIG { printf("\toperador_asignacion -> '->='\n"); }
 | AND_ASIG { printf("\toperador_asignacion -> '&='\n"); }
 | XOR_ASIG { printf("\toperador_asignacion -> '@='\n"); }
 | OR_ASIG { printf("\toperador_asignacion -> '|='\n"); }
;

//  instruccion_bifurcacion ::= ’si’ ’(’ expresion ’)’ bloque_instrucciones [ otro_caso ]* [ ’sino’ bloque_instrucciones ]?
instruccion_bifurcacion: 
 SI '(' expresion ')' bloque_instrucciones otros_casos opcion_condicion_bifurcacion { printf("\tinstruccion_bifurcacion -> SI (expresion) bloque_instrucciones otros_casos opcion_condicion_bifurcacion\n"); }
;

otros_casos: 
 otros_casos otro_caso { printf("\totro_casos -> otros_casos otro_caso\n"); }
 | { printf("\totro_casos -> \n"); }
;

opcion_condicion_bifurcacion: 
 SINO bloque_instrucciones { printf("\topcion_condicion_bifurcacion -> SINO bloque_instrucciones\n"); }
 | { printf("\topcion_condicion_bifurcacion -> \n"); }
;

//  otro_caso ::= ’encambio’ ’(’ expresion ’)’ bloque_instrucciones
otro_caso: 
 ENCAMBIO '(' expresion ')' bloque_instrucciones { printf("\totro_caso -> ENCAMBIO (expresion) bloque_instrucciones\n"); }
;

//  instruccion_bucle ::= ’mientras’ ’(’ expresion ’)’ bloque_instrucciones | ’hacer’ bloque_instrucciones ’mientras’ ’(’ expresion ’)’ ’.’ | ’para’ ’(’ ( asignacion )+ ’:’ expresion ’:’ ( asignacion )+ ’)’ bloque_instrucciones | ’para’ ’cada’ IDENTIFICADOR ’en’ ’(’ expresion ’)’ bloque_instrucciones
instruccion_bucle: 
 MIENTRAS '(' expresion ')' bloque_instrucciones { printf("\tinstruccion_bucle -> MIENTRAS (expresion) bloque_instrucciones\n"); }
 | HACER bloque_instrucciones MIENTRAS '(' expresion ')' '.' { printf("\tinstruccion_bucle -> HACER bloque_instrucciones MIENTRAS (expresion) '.'\n"); }
 | PARA '(' asignaciones ':' expresion ':' asignaciones ')' bloque_instrucciones { printf("\tinstruccion_bucle -> PARA (asignaciones ':' expresion ':' asignaciones) bloque_instrucciones\n"); }
 | PARA CADA IDENTIFICADOR EN '(' expresion ')' bloque_instrucciones { printf("\tinstruccion_bucle -> PARA CADA IDENTIFICADOR EN (expresion) bloque_instrucciones\n"); }
;

asignaciones: 
 asignaciones ';' asignacion { printf("\tasignaciones -> asignaciones ';' asignacion\n"); }
 | asignacion { printf("\tasignaciones -> asignacion\n"); }
;

//  instruccion_salto ::= ’saltar’ IDENTIFICADOR ’.’ | ’continuar’ ’.’ | ’escape’ ’.’
instruccion_salto: 
 SALTAR IDENTIFICADOR '.' { printf("\tinstruccion_salto -> SALTAR IDENTIFICADOR '.'\n"); }
 | CONTINUAR '.' { printf("\tinstruccion_salto -> CONTINUAR '.'\n"); }
 | ESCAPE '.' { printf("\tinstruccion_salto -> ESCAPE '.'\n"); }
;

//  instruccion_destino_salto ::= ’etiqueta’ IDENTIFICADOR ’.’
instruccion_destino_salto: 
 ETIQUETA IDENTIFICADOR '.' { printf("\tinstruccion_destino_salto -> ETIQUETA IDENTIFICADOR '.'\n"); }
;

//  instruccion_devolver ::= ’devolver’ [ expresion ]? ’.’
instruccion_devolver: 
 DEVOLVER opcion_expresion '.' { printf("\tinstruccion_devolver -> DEVOLVER opcion_expresion '.'\n"); }
;

opcion_expresion: 
 expresion { printf("\topcion_expresion -> expresion\n"); }
 | { printf("\topcion_expresion -> \n"); }
;

//  instruccion_lanzamiento_excepcion ::= ’lanza’ ’excepcion’ IDENTIFICADOR ’.’
instruccion_lanzamiento_excepcion: 
 LANZA EXCEPCION IDENTIFICADOR '.' { printf("\tinstruccion_lanzamiento_excepcion -> LANZA EXCEPCION IDENTIFICADOR '.'\n"); }
;

//  instruccion_captura_excepcion ::= ’ejecuta’ bloque_instrucciones clausulas
instruccion_captura_excepcion: 
 EJECUTA bloque_instrucciones clausulas { printf("\tinstruccion_captura_excepcion -> EJECUTA bloque_instrucciones clausulas\n"); }
;

//  clausulas ::= clausulas_excepcion [ clausula_defecto ]? | clausula_defecto
clausulas: 
 clausulas_excepcion opcion_clausula_defecto { printf("\tclausulas -> clausula_excepcion\n"); }
 | clausula_defecto { printf("\tclausulas -> clausula_defecto\n"); }
;

opcion_clausula_defecto: 
 clausula_defecto { printf("\topcion_clausula_defecto -> clausula_defecto\n"); }
 | { printf("\topcion_clausula_defecto -> \n"); }
;

//  clausulas_excepcion ::= [ clausula_excepcion_especifica ]* clausula_excepcion_general
clausulas_excepcion: 
 excepciones_especificas clausula_excepcion_general { printf("\tclausulas_excepcion -> excepciones_especificas clausula_excepcion_general\n"); }
;

excepciones_especificas: 
 excepciones_especificas clausula_excepcion_especifica { printf("\texcepciones_especificas -> excepciones_especificas clausula_excepcion_especifica\n"); }
 | { printf("\texcepciones_especificas -> \n"); }
;

//  clausula_excepcion_especifica ::= ’excepcion’ nombre bloque_instrucciones
clausula_excepcion_especifica: 
 EXCEPCION nombre bloque_instrucciones { printf("\tclausula_excepcion_especifica -> EXCEPCION nombre bloque_instrucciones\n"); }
;

//  clausula_excepcion_general ::= ’otra’ ’excepcion’ bloque_instrucciones
clausula_excepcion_general: 
 OTRA EXCEPCION bloque_instrucciones { printf("\tclausula_excepcion_general -> OTRA EXCEPCION bloque_instrucciones\n"); }
;

//  clausula_defecto ::= ’defecto’ bloque_instrucciones
clausula_defecto: 
 DEFECTO bloque_instrucciones { printf("\tclausula_defecto -> DEFECTO bloque_instrucciones\n"); }
;

//  instruccion_vacia ::= ’.’
instruccion_vacia: 
 '.' { printf("\tinstruccion_vacia -> '.'\n"); }
;

/****************/
/* Expresiones */
/**************/

//  expresion_constante ::= CTC_ENTERA | CTC_REAL | CTC_CADENA | CTC_CARACTER
expresion_constante: 
 CTC_ENTERA  { printf("\texpresion_constante -> CTC_ENTERA\n"); }
 | CTC_REAL  { printf("\texpresion_constante -> CTC_REAL\n"); }
 | CTC_CADENA  { printf("\texpresion_constante -> CTC_CADENA\n"); }
 | CTC_CARACTER  { printf("\texpresion_constante -> CTC_CARACTER\n"); }
; 

//  expresion_indexada ::= expresion_basica | expresion_indexada ’?’ expresion_basica | expresion_indexada ’^?’ expresion_basica | expresion_indexada [ ’^?’ ]? indice
expresion_indexada: 
 expresion_basica  { printf("\texpresion_indexada -> expresion_basica\n"); }
 | expresion_indexada '?' expresion_basica { printf("\texpresion_indexada -> expresion_indexada '?' expresion_basica\n"); }
 | expresion_indexada INDIRECCION expresion_basica { printf("\texpresion_indexada -> expresion_indexada '^?' expresion_basica\n"); }
 | expresion_indexada opcion_indireccion indice  { printf("\texpresion_indexada -> expresion_indexada opcion_indireccion indice\n"); }
;

opcion_indireccion: 
 INDIRECCION { printf("\topcion_indireccion -> '^?'\n"); }
 | { printf("\topcion_indireccion -> \n"); }
;

//  expresion_basica ::= nombre | ’(’ expresion ’)’ | ’^’ expresion_basica | ’ref’ expresion_basica
expresion_basica: 
 nombre  { printf("\texpresion_basica -> nombre\n"); }
 | '(' expresion ')' { printf("\texpresion_basica -> (expresion)\n"); }
 | '^' expresion_basica  { printf("\texpresion_basica -> ^expresion_basica\n"); }
 | REF expresion_basica  { printf("\texpresion_basica -> REF expresion_basica\n"); }
 | expresion_constante   { printf("\texpresion_basica -> expresion_constante\n"); }   
;

//  indice ::= ’[’ expresion ’]’ | ’{’ expresion ’}’
indice: 
 '[' expresion ']' { printf("\tindice -> [expresion]\n"); }
 | '{' expresion '}' { printf("\tindice -> {expresion}\n"); }
;

//  expresion_funcional ::= IDENTIFICADOR ’(’ ( expresion )* ’)’
expresion_funcional: 
 IDENTIFICADOR '(' opcion_expresiones_expfun ')' { printf("\texpresion_funcional -> IDENTIFICADOR (opcion_expresiones_expfun)\n"); }
;

opcion_expresiones_expfun: 
 expresiones_expfun { printf("\topciones_expresiones_expfun -> expresiones_expfun\n"); }
 | { printf("\topciones_expresiones_expfun -> \n"); }
;

expresiones_expfun: 
 expresiones_expfun ';' expresion { printf("\texpresiones_expfun -> expresiones_expfun ';' expresion\n"); }
 | expresion { printf("\texpresiones_expfun -> expresion\n"); }
;

//  expresion_logica y resto de expresiones
expresion_logica: 
 or_logico { printf("\texpresion_logica -> or_logico\n"); }
;

or_logico: 
 and_logico  { printf("\tor_logico -> and_logico\n"); }
 | or_logico OR and_logico  { printf("\tor_logico -> or_logico '||' and_logico \n"); }
;

and_logico: 
 expr_igualdad { printf("\tand_logico -> expr_igualdad\n"); }
 | and_logico AND expr_igualdad { printf("\tand_logico -> and_logico '&&' expr_igualdad\n"); }
;

expr_igualdad: 
 expr_relacional { printf("\texpr_igualdad -> expr_relacional\n"); }
 | expr_igualdad EQ expr_relacional { printf("\texpr_igualdad -> expr_igualdad '==' expr_relacional\n"); }
 | expr_igualdad NEQ expr_relacional { printf("\texpr_igualdad -> expr_igualdad '!=' expr_relacional\n"); }
;

expr_relacional: 
 or_binario  { printf("\texpr_relacional -> or_binario\n"); }
 | expr_relacional '<' or_binario { printf("\texpr_relacional -> expr_relacional '<' or_binario\n"); }
 | expr_relacional '>' or_binario { printf("\texpr_relacional -> expr_relacional '>' or_binario\n"); }
 | expr_relacional LE or_binario  { printf("\texpr_relacional -> expr_relacional '<=' or_binario\n"); }
 | expr_relacional GE or_binario  { printf("\texpr_relacional -> expr_relacional '>=' or_binario\n"); }
;

or_binario: 
 xor_binario { printf("\tor_binario -> xor_binario\n"); }
 | or_binario '|' xor_binario { printf("\tor_binario -> or_binario '|' xor_binario\n"); }
;

xor_binario: 
 and_binario { printf("\txor_binario -> and_binario\n"); }
 | xor_binario '@' and_binario { printf("\txor_binario -> xor_binario '@' and_binario\n"); }
;

and_binario: 
 expr_desplazamiento { printf("\tand_binario -> expr_desplazamiento\n"); }
 | and_binario '&' expr_desplazamiento { printf("\tand_binario -> and_binario '&' expr_desplazamiento\n"); }
;

expr_desplazamiento: 
 expr_aditiva  { printf("\texpr_desplazamiento -> expr_aditiva\n"); }
 | expr_desplazamiento FLECHA_IZDA expr_aditiva { printf("\texpr_desplazamiento -> expr_desplazamiento '<-' expr_aditiva\n"); }
 | expr_desplazamiento FLECHA_DCHA expr_aditiva { printf("\texpr_desplazamiento -> expr_desplazamiento '->' expr_aditiva\n"); }
;

expr_aditiva: 
 expr_multiplicativa  { printf("\texpr_aditiva -> expr_multiplicativa\n"); }
 | expr_aditiva '+' expr_multiplicativa { printf("\texpr_aditiva -> expr_aditiva '+' expr_multiplicativa\n"); }
 | expr_aditiva '-' expr_multiplicativa { printf("\texpr_aditiva -> expr_aditiva '-' expr_multiplicativa\n"); }
;

expr_multiplicativa: 
 expr_potencia { printf("\texpr_multiplicativa -> expr_potencia\n"); }
 | expr_multiplicativa '*' expr_potencia { printf("\texpr_multiplicativa -> expr_multiplicativa '*' expr_potencia\n"); }
 | expr_multiplicativa '/' expr_potencia { printf("\texpr_multiplicativa -> expr_multiplicativa '/' expr_potencia\n"); }
 | expr_multiplicativa MOD expr_potencia { printf("\texpr_multiplicativa -> expr_multiplicativa mod expr_potencia\n"); }
;

expr_potencia: 
 expr_unaria { printf("\texpr_potencia -> expr_unaria\n"); }
 | expr_potencia POTENCIA expr_unaria  { printf("\texpr_potencia -> expr_potencia '**' expr_unaria\n"); }
;

expr_unaria: 
 expr_primaria { printf("\texpr_unaria -> expr_primaria\n"); } 
 | '-' expr_primaria { printf("\texpr_unaria -> -expr_primaria\n"); }
 | '~' expr_primaria { printf("\texpr_unaria -> ~expr_primaria\n"); }
 | '!' expr_primaria { printf("\texpr_unaria -> !expr_primaria\n"); }
 | TAMANO expr_primaria { printf("\texpr_unaria -> tamano expr_primaria\n"); } 
;

expr_primaria : 
 expresion_indexada  { printf("\texpr_primaria -> expresion_indexada\n"); }
 | expresion_funcional { printf("\texpr_primaria -> expresion_funcional\n"); }
;

//  expresion ::= expresion_logica [ ’si’ expresion ’sino’ expresion ]? | expresion_logica ’para’ ’cada’ IDENTIFICADOR ’en’ expresion
expresion: 
 expresion_logica opcion_condicion_expresion { printf("\texpresion -> expresion_logica opcion_condicion_expresion\n"); }
 | expresion_logica PARA CADA IDENTIFICADOR EN expresion { printf("\texpresion -> expresion_logica PARA CADA IDENTIFICADOR EN expresion\n"); }
 | error '.' { printf("\tError en las expresiones\n"); yyerrok; }
;

opcion_condicion_expresion: 
 SI expresion SINO expresion { printf("\topcion_condicion_expresion -> SI expresion NO expresion\n"); }
 | { printf("\topcion_condicion_expresion -> \n"); }
;

%%

int yyerror(char *s) {
  fflush(stdout);
  printf("*****************, %s\n",s);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./eazy NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
