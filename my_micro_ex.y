%{
#include <iostream>
#include <sstream>
#include <cstring>
#include <deque>

struct symtab {
	char *name;  /* sometime as value */
	char *type;
};
using namespace std;

extern "C"{
void yyerror(const char* message){ cerr << message << endl;}
extern int yylex(void);
}

deque <string> result; // Result
symtab SYMBOL_TABLE[1000];
symtab *look_for_symbol(const char *str, const char *str2);

%}

%union{
	struct symtab *symp;
	struct symtab *opterminal;
}
%token<opterminal> PROGRAM BEGIN_T END IF THEN ELSE ENDIF FOR TO DOWNTO ENDFOR STEP WHILE ENDWHILE DECLARE AS INTEGER FLOAT    
%token<symp> ID INT_LITERAL FLOAT_LITERAL STRING_LITERAL EXP_FLOAT_LITERAL
%type<symp> declare_stmt var_list type expression term factor

%%

start: PROGRAM ID program 
		{
			result.push_front("\tSTART " + string($2->name));
			result.push_back("\tHALT " + string($2->name));
		}
	;	

program: BEGIN_T stmt_list END 			
	|	BEGIN_T END 						
	;

stmt_list: stmt				
	|	stmt stmt_list							
	;

stmt: declare_stmt ';'
	;

declare_stmt: DECLARE var_list AS type 
		{
			char * tmp = strtok($2->name, ",");
			while (tmp != NULL) 
			{
				result.push_back("\tDeclare " + string(tmp) + ", " + string($4->name));
				look_for_symbol(tmp, $4->name); // change type
				
				tmp = strtok(NULL, ",");
				if (tmp != NULL && tmp[0] >= '0' && tmp[0] <= '9') /*next is number => array */
				{
					result.back() = result.back() + "_array," + string(tmp);
					tmp = strtok(NULL, ",");
				}
			}
		}
	;


var_list: var_list ',' ID '[' expression ']'
		{
			$$ = new symtab(); 
			$$->name = strdup( (string($1->name) + "," + string($3->name) + "," + string($5->name) ).data() );
		}
	| ID '[' expression ']' 
		{
			$$ = new symtab();
			$$->name = strdup( (string($1->name) + "," + string($3->name)).data() );
		}
	| var_list ',' ID
		{
			$$ = new symtab();
			$$->name = strdup( (string($1->name) + "," + string($3->name)).data() );
		}
	| ID  
	;	
	
	
type: INTEGER  { $$ = $1; }
	| FLOAT   { $$ = $1; }
	;	

expression:	expression '+' term 
	|	expression '-' term 
	|	term  /* $$ = $1 */ 
	;

term : term '*' factor 
	| term '/' factor
	| factor /* $$ = $1 */
	;

factor: '-' factor 
	|	'(' expression ')'	{ $$ = $2; }
	|	ID    
	|	INT_LITERAL 
	|	FLOAT_LITERAL 	
	;
	
%%

int main() {
	yyparse();
	for (int i = 0; i < result.size(); i++) 
	{
		cout << result.at(i) << endl;
	}
	return 0;
}	

symtab *look_for_symbol(const char *str, const char *str2) 
{
	struct symtab *sp;
	for (sp = SYMBOL_TABLE; sp < &SYMBOL_TABLE[1000]; sp++) 
	{
		if (sp->name && !strcmp(sp->name, str)) 
		{
			if ( strcmp(str2,"") != 0 ) 
			{
				sp->type = strdup(str2);
			}
			return sp;
		}
		if ( !sp->name ) 
		{
			sp->name = strdup(str);
			sp->type = strdup(str2);
			return sp;
		}
	}
	yyerror("Symbol can not found!");
	exit(1);
}
