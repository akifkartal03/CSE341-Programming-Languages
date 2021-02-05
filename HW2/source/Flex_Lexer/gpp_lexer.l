%{
  /*definitions*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
FILE *p_inputFile,*p_outputFile,*p_stringStream;
%}
 /*Rule and DFA part of lexer*/
%%

 /*rules of operators*/
\+ { fprintf(p_outputFile,"%s\n","OP_PLUS"); }
\- { fprintf(p_outputFile,"%s\n","OP_MINUS"); }
\/ { fprintf(p_outputFile,"%s\n","OP_DIV"); }
\* { fprintf(p_outputFile,"%s\n","OP_MULT"); }
\( { fprintf(p_outputFile,"%s\n","OP_OP"); }
\) { fprintf(p_outputFile,"%s\n","OP_CP"); }
\*\* { fprintf(p_outputFile,"%s\n","OP_DBMULT"); }
\“ { fprintf(p_outputFile,"%s\n","OP_OC"); }
\” { fprintf(p_outputFile,"%s\n","OP_CC"); }
, { fprintf(p_outputFile,"%s\n","OP_COMMA"); }

 /*rules for keywords in g++ language*/
and { fprintf(p_outputFile,"%s\n","KW_AND"); }
or { fprintf(p_outputFile,"%s\n","KW_OR"); }
not { fprintf(p_outputFile,"%s\n","KW_NOT"); }
equal { fprintf(p_outputFile,"%s\n","KW_EQUAL"); }
less { fprintf(p_outputFile,"%s\n","KW_LESS"); }
nil { fprintf(p_outputFile,"%s\n","KW_NIL"); }
list { fprintf(p_outputFile,"%s\n","KW_LIST"); }
append { fprintf(p_outputFile,"%s\n","KW_APPEND"); }
concat { fprintf(p_outputFile,"%s\n","KW_CONCAT"); }
set { fprintf(p_outputFile,"%s\n","KW_SET"); }
deffun { fprintf(p_outputFile,"%s\n","KW_DEFFUN"); }
for { fprintf(p_outputFile,"%s\n","KW_FOR"); }
if { fprintf(p_outputFile,"%s\n","KW_IF"); }
exit { fprintf(p_outputFile,"%s\n","KW_EXIT"); }
load { fprintf(p_outputFile,"%s\n","KW_LOAD"); }
disp { fprintf(p_outputFile,"%s\n","KW_DISP"); }
true { fprintf(p_outputFile,"%s\n","KW_TRUE"); }
false { fprintf(p_outputFile,"%s\n","KW_FALSE"); }

 /*rule for comment*/
;;(.*) { fprintf(p_outputFile,"%s\n","COMMENT"); }
 /*rule for value*/
(0)|([1-9][0-9]*) { fprintf(p_outputFile,"%s\n","VALUE"); }
 /* rule for identifier*/
[a-zA-Z][0-9a-zA-Z]* { fprintf(p_outputFile,"%s\n","IDENTIFIER");}
[\n\f\r\t ] {} /*ignorable tokens*/
 /*
  * violation part -any stament nothing but
  * different than these rules will be caught.-
  */   
. {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");} 
([a-zA-Z]*[,+-/*])|([0-9][0-9a-zA-Z]+) {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}
[+][0-9A-zA-Z]* {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}
[-][0-9A-zA-Z]*	{fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}  
[/][0-9A-zA-Z]* {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}	  
[*][0-9A-zA-Z]* {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}
[,][0-9A-zA-Z]* {fprintf(p_outputFile,"%s %s %s\n","SYNTAX ERROR",yytext,"cannot be tokenized");}
%%
 /*C code part*/
int yywrap(){} 
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
    yylex();
    printf("File has created!\n");  
  }
  else{
    //read input from terminal in repl mode
    printf("Welcome to G++ shell made by Akif with Flex, to exit enter empty string\n> ");
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
    yylex();
    printf("File has created!\n");
  }
  return 0;
}