%{
/*definitions*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helpers.h"
FILE *p_inputFile,*p_outputFile,*p_stringStream;
extern FILE *yyin;
void writeList(int *list);
%}

%union{
    int operand;
    int *operands;
    char expr[30];
}
 /*tokens of operators*/
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_DBLMULT
%token OP_OC
%token OP_CC
%token OP_COMMA
 /*tokens for keywords in g++ language*/
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token COMMENT
/*token for value*/
%token <operand> VALUE
/* token for identifier*/
%token <expr> IDENTIFIER

/*Types*/
%type <operand> EXPI
%type <operand> EXPB
%type <operands> VALUES
%type <operands> EXPLIST
%type <operands> ELEMENTS

%start START
%%
/*gpp Concrete Syntax*/
/*START -> INPUT*/
/*INPUT -> EXPI | EXPLISTI*/
START:
    EXPI {fprintf(p_outputFile,"SYNTAX OK. \nResult: %d\n", $1);}
    |
    EXPLIST {fprintf(p_outputFile,"SYNTAX OK. \nResult: ");writeList($1);}
    /*|error { yyerrok; yyclearin;}*/
    ;

START:
    EXPB {fprintf(p_outputFile,"SYNTAX OK. \nResult: %s\n", $1 == 1 ? "T" : "NIL");}
    |
    START EXPI {fprintf(p_outputFile,"SYNTAX OK. \nResult: %d\n", $2);}
    |
    START EXPLIST {fprintf(p_outputFile,"SYNTAX OK. \nResult:"); writeList($2); }
    |
    START EXPB {fprintf(p_outputFile,"SYNTAX OK. \nResult: %s\n", $2 == 1 ? "T" : "NIL");}
    |
    COMMENT {}
    |
    START COMMENT {}
    |
    OP_OP KW_EXIT OP_CP { printf("File has created!\n"); exit(-1);}
    |
    START OP_OP KW_EXIT OP_CP { printf("File has created!\n"); exit(-1); }
    ;
EXPI:
    /* (+ EXPI EXPI) G++ Syntax*/
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;}
    |
    /* (- EXPI EXPI) */
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;}
    |
    /* (* EXPI EXPI) */
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;}
    |
    /* (/ EXPI EXPI) */
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;}
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = recursive_pow($3, $4);}
    |
    IDENTIFIER {$$ = getDataOfVariable($1);}
    |
    VALUE {$$ = $1;}
    |
    OP_OP KW_SET IDENTIFIER EXPI OP_CP {$$ = $4; addNewVariable($3, $4);}
    |
    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;}
    |
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; }
    |
    OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; fprintf(p_outputFile,"Display : %d\n", $3);}
    ;

EXPB:
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}
    |
    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}
    |
    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}
    |
    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; }
    |
    KW_TRUE  { $$ = 1; }
    |
    KW_FALSE   { $$ = 0; }
    |
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; fprintf(p_outputFile,"Display : %s\n", ($3 ? "T":"NIL"));}
    ;

EXPLIST:
    OP_OP KW_CONCAT EXPLIST EXPLIST OP_CP {$$ = concatTwoList($3, $4);}
    |
    OP_OP KW_APPEND EXPI EXPLIST OP_CP {$$ = appendElementToList($4, $3);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
    |
    ELEMENTS  {$$ = $1;}
    |
    OP_OP KW_DISP ELEMENTS OP_CP { $$ = $3; fprintf(p_outputFile,"Display : "); writeList($3);}
    ;

ELEMENTS:
    OP_OP VALUES OP_CP {$$ = $2;}
    |
    OP_OP OP_CP { $$= NULL; }
    |
    KW_NIL { $$ = NULL;}
    ;

VALUES:
    VALUES VALUE  {$$ = appendElementToList($1, $2);}
    |
    VALUE {$$ = NULL; $$ = appendElementToList($$, $1);}
    ;


%%

int yyerror(char *error) {
    fprintf(p_outputFile, "SYNTAX ERROR \n");
}

void writeList(int *list){
    fprintf(p_outputFile, "(");
    for(int i=0;list[i]!=-999; ++i){
      if(list[i+1]!=-999){
        fprintf(p_outputFile,"%d ", list[i]);
      }
      else{
        fprintf(p_outputFile,"%d", list[i]);
      }

    }
    fprintf(p_outputFile, ")\n");
}
int main(int argc, char **argv){
  p_outputFile = fopen("parsed_cpp.txt","w");
  if(p_outputFile == NULL)
  {
    printf("Error parsed_cpp.txt doesn't exist!\n");
    exit(1);
  }
  if(argc > 1){
    //read input from file
    p_inputFile = fopen(argv[1],"r");
    if(p_inputFile == NULL)
    {
      printf("Error File doesn't exist!\n");
      exit(1);
    }
    yyin = p_inputFile;
    yyparse();
    printf("File has created!\n");
  }
  else{
    //read input from terminal in repl mode
    printf("Welcome to G++ shell made by Akif with Flex and Yacc, to exit enter empty string\n> ");
    char* newLine = NULL;
    char* line = (char*)malloc(20*sizeof(char));;
    int is_empty_string;
    size_t size = 0;
    //read until empty string and store them
    do{
      is_empty_string = getline(&newLine, &size, stdin);
      if(is_empty_string != 1){
        line = (char *) realloc(line, (strlen(line)+size+2)*sizeof(char));
        strcat(line,newLine);
      }
    }while(is_empty_string != 1);
    //after reading, tokenize by using string stream fuction fmemopen
    p_stringStream = fmemopen (line, strlen (line) - 1, "r");
    yyin = p_stringStream;
    yyparse();
    printf("File has created!\n");
  }
  exit(-1);
}
