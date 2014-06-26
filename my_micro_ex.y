%{
#include <iostream>
#include <sstream>
#include <cstring>
#include <deque>
int numline = 1;
struct symtab {
	char *name;  /* sometime as value */
	char *type;
};
using namespace std;

extern "C"{
void yyerror(const char* message);
extern int yylex(void);
}

deque <string> result; // Result
symtab SYMBOL_TABLE[1000];
symtab *look_for_symbol(const char *str, const char *str2);
int temp_variable_counter = 0;
string IntToString(int &i);
%}


%union{
	struct symtab *symp;
	struct symtab *opterminal;
}
%token<opterminal> PROGRAM BEGIN_T END IF THEN ELSE ENDIF FOR TO DOWNTO ENDFOR STEP WHILE ENDWHILE DECLARE AS INTEGER FLOAT ASSIGN GE NE EQ LE JL JG   
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
                /*symtab* tmp_sym =  look_for_symbol(tmp, "");
                cerr << tmp_sym->name;
                cerr << tmp_sym->type;*/
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
            if( strcmp($5->type,"Integer") == 0 )
            {
                $$->name = strdup( (string($1->name) + "," + string($3->name) + "," + string($5->name) ).data() );
            }
            else
            {
                yyerror("array[N], N only accept a Integer");
            }
			
		}
	| ID '[' expression ']' 
		{
			$$ = new symtab();
            if( strcmp($3->type,"Integer") == 0 )
            {
                $$->name = strdup( (string($1->name) + "," + string($3->name)).data() );
            }
            else
            {
                yyerror("array[N], N only accept a Integer");
            }
			
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
            {
                symtab* id = look_for_symbol($1->name,$1->type);
                ++temp_variable_counter;
                look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
                if( strcmp(id->type,"Integer") == 0 ) 
                {
                    result.push_back("\tI_ADD " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else if( strcmp(id->type,"Float") == 0 ) 
                {
                    result.push_back("\tF_ADD " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else 
                {
                    result.push_back("\tI_ADD " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                $$ = new symtab();
                $$->name = strdup( string(string("T&")+IntToString(temp_variable_counter)).data() );		
                $$->type = $1->type;
            }
	|	expression '-' term 
            {
                symtab* id = look_for_symbol($1->name,$1->type);
                ++temp_variable_counter;
                look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
                if( strcmp(id->type,"Integer") == 0 )
                {
                    result.push_back("\tI_SUB " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else if( strcmp(id->type,"Float") == 0 )
                {
                    result.push_back("\tF_SUB " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else
                {
                    result.push_back("\tI_SUB " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                $$ = new symtab();
                $$->name = strdup( string(string("T&")+IntToString(temp_variable_counter)).data() );		
                $$->type = $1->type;
            }
	|	term 
            {
                $$ = $1; 
            }
	;

term : term '*' factor 
        {
            symtab* id = look_for_symbol($1->name,$1->type);
			++temp_variable_counter;
			look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data() , (const char*)$1->type);
			if( strcmp(id->type,"Integer") == 0 ) 
            {
				result.push_back("\tI_MUL " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			else if( strcmp(id->type,"Float") == 0 ) 
            {
				result.push_back("\tF_MUL " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			else 
            {
				result.push_back("\tI_MUL " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			$$ = new symtab();
			$$->name = strdup( string(string("T&")+IntToString(temp_variable_counter)).data() );		
			$$->type = $1->type;
        }
	| term '/' factor
        {
            symtab* id = look_for_symbol($1->name,$1->type);
			++temp_variable_counter;
			look_for_symbol(string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
			if( strcmp(id->type,"Integer") == 0 ) 
            {
				result.push_back("\tI_DIV " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			else if( strcmp(id->type,"Float") == 0  )
            {
				result.push_back("\tF_DIV " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			else 
            {
				result.push_back("\tI_DIV " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
			}
			$$ = new symtab();
			$$->name = strdup( string(string("T&")+IntToString(temp_variable_counter)).data() );		
			$$->type = $1->type;
        }
	| factor 
        {
            $$ = $1;
        }
	;

factor: '-' factor 
            {
                ++temp_variable_counter;
                look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data() , (const char*)$2->type);
                result.push_back("\tUMINUS " + string($2->name) + ",T&" + IntToString(temp_variable_counter));
                $$ = new symtab(); 
                $$->name = strdup( string(string("T&")+IntToString(temp_variable_counter)).data() );
                $$->type = $2->type;
            }
	|	'(' expression ')'	
            { 
                $$ = $2; 
            }
	|	ID  
            {
                $$ = look_for_symbol($1->name,"");
            }
	|	INT_LITERAL 
            {
                $$ = look_for_symbol($1->name,"Integer");
            }
	|	FLOAT_LITERAL 	
            {
                $$ = look_for_symbol($1->name,"Float");
            }
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

void yyerror(const char* message)
{
    cerr << "In line " << numline << " : " << message << endl;
    exit(1);
}

symtab *look_for_symbol(const char *str, const char *str2) 
{
	struct symtab *sp;
	for (sp = SYMBOL_TABLE; sp < &SYMBOL_TABLE[1000]; sp++) 
	{
		if (sp->name && !strcmp(sp->name, str)) 
		{
            // change type
			if ( strcmp(str2,"") != 0 ) 
			{
				sp->type = strdup(str2);
			}
			return sp;
		}
        // add new
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

string IntToString(int &i)
{
    string s;
	stringstream ss(s);
	ss << i;
	return ss.str();
}