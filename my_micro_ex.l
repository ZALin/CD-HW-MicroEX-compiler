%{
#include "y.tab.h"

extern "C"{
int yywrap(void){
        return 1;
}
int yylex(void);
}
extern YYSTYPE yylval;
extern symtab *look_for_symbol(const char *str, const char *str2);


/*
not complete
"(" { return 10; }
")" { return 11; }
";" { return 12; }
"," { return 13; }
":=" { return 14; }
"+" { return 15; }
"-" { return 16; }
"*" { return 17; }
"/" { return 18; }
"!=" { return 19; }
">" { return 20; }
"<" { return 21; }
">=" { return 22; }
"<=" { return 23; }
"==" { return 24; }
*/

%}

Digit [0-9]

%%

[Pp][Rr][Oo][Gg][Rr][Aa][Mm]	{return PROGRAM;}
[Bb][Ee][Gg][Ii][Nn]	{return BEGIN_T;}
[Ee][Nn][Dd]	{return END;}
[Ii][Ff]	{return IF;}
[Tt][Hh][Ee][Nn]	{return THEN;}
[Ee][Ll][Ss][Ee]	{return ELSE;}
[Ee][Nn][Dd][Ii][Ff]	{return ENDIF;}
[Ff][Oo][Rr]	{return FOR;}
[Tt][Oo]	{return TO;}
[Dd][Oo][Ww][Nn][Tt][Oo]	{return DOWNTO;}
[Ee][Nn][Dd][Ff][Oo][Rr]	{return ENDFOR;}
[Ss][Tt][Ee][Pp]	{return STEP;}
[Ww][Hh][Ii][Ll][Ee]	{return WHILE;}
[Ee][Nn][Dd][Ww][Hh][Ii][Ll][Ee]	{return ENDWHILE;}
[Dd][Ee][Cc][Ll][Aa][Rr][Ee]	{return DECLARE;}
[Aa][Ss]	{return AS;}
[Ii][Nn][Tt][Ee][Gg][Ee][Rr] { yylval.opterminal = look_for_symbol("Integer",""); return INTEGER;}
[Ff][Ll][Oo][Aa][Tt]	{yylval.opterminal = look_for_symbol("Float",""); return FLOAT;} 
\"[^"\n]*\"          {yylval.symp = look_for_symbol(yytext,"STR_LITERAL");return STRING_LITERAL;}


[a-zA-Z][0-9a-zA-Z_]*    {yylval.symp = look_for_symbol(yytext,"");return ID;}
{Digit}+              {yylval.symp = look_for_symbol(yytext,"Integer");return INT_LITERAL;}
{Digit}+\.{Digit}*   |
{Digit}*\.{Digit}*\+     {yylval.symp = look_for_symbol(yytext,"Float");return FLOAT_LITERAL;}
{Digit}+\.{Digit}+[Ee][+-]?{Digit}+    {yylval.symp = look_for_symbol(yytext,"EXP_FLOAT_LITERAL");return EXP_FLOAT_LITERAL;}
[\t\r\n ]+		/* ignore */ ;
\%\%.*\n     /* ignore Comment => %% …….. \n */ ;
.	{return yytext[0];}
%%