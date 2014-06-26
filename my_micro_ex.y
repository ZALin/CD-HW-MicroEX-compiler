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
int jump_counter = 0; 
deque <int> tmp_jump_counter;
%}


%union{
    struct symtab *symp;
    struct symtab *opterminal;
}
%token<opterminal> PROGRAM BEGIN_T END IF THEN ELSE ENDIF FOR TO DOWNTO ENDFOR STEP WHILE ENDWHILE DECLARE AS INTEGER FLOAT ASSIGN GE NE EQ LE G L   
%token<symp> ID INT_LITERAL FLOAT_LITERAL STRING_LITERAL EXP_FLOAT_LITERAL
%type<symp> declare_stmt var_list type expression term factor assign_stmt assigned_var loop_stmt forloop forloop_type if_stmt bool_exp bool_op else_t



%%

start: PROGRAM ID program 
        {
            result.push_front("\tSTART " + string($2->name));
            result.push_back("\tHALT " + string($2->name));
        }
    ;    

program: BEGIN_T stmt_list END             
    |    BEGIN_T END                         
    ;

stmt_list: stmt                
    |    stmt stmt_list                            
    ;

stmt: declare_stmt ';'
    | assign_stmt ';' 
    | loop_stmt 
    | if_stmt
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

    
assign_stmt:  assigned_var ASSIGN expression   
                {
                    symtab* pre_ID = look_for_symbol($1->name, $1->type);
                    if( strcmp(pre_ID->type, "Integer") == 0 )
                    {
                        result.push_back("\tI_STORE " + string($3->name) + "," + string($1->name));
                    }
                    else if( strcmp(pre_ID->type, "Float") == 0 )
                    {
                        result.push_back("\tF_STORE " + string($3->name) + "," + string($1->name));
                    } 
                    else  
                    {
                        //cerr << "In assign_stmt... Something is wrong: assigned_var " << $1->name << " 's type is " << $1->type << endl;
                        result.push_back("\tI_STORE " + string($3->name) + "," + string($1->name));
                    } 
                    $$ = $1;
                }
    ;

    
assigned_var : ID 
                {
                    $$ = look_for_symbol($1->name, "");
                }
                
    |   ID '[' expression ']'
                {
                    $$ = new symtab();
                    if( strcmp($3->type,"Integer") == 0 )
                    {
                        $$->type = look_for_symbol($1->name, "")->type;
                        $$->name = strdup( (string($1->name)+"["+string($3->name)+"]").data() );
                    }
                    else
                    {
                        yyerror("array[N], N only accept a Integer");
                    }
                    
                }
                

                
loop_stmt : forloop forloop_type expression ')' stmt_list ENDFOR
                {
                    if( strcmp($2->name, "TO") == 0) 
                    {
                        result.push_back("\tINC " + string($1->name));
                    }
                    else if( strcmp($2->name, "DOWNTO") == 0 )
                    {
                        result.push_back("\tDEC " + string($1->name));
                    }
                    
                    // $1->name:assigned_var(in assign_stmt)  
                    // $1->type: lb& jump_counter : 
                    // CMP assigned_var->name , expression_result
                    if( strcmp( look_for_symbol($1->name, "")->type , "Integer" ) == 0 ) 
                    {
                        result.push_back("\tI_CMP " + string($1->name) + "," + string($3->name));
                    }
                    else if( strcmp( look_for_symbol($1->name, "")->type , "Float" ) == 0 )
                    {
                        result.push_back("\tF_CMP " + string($1->name) + "," + string($3->name));
                    }

                    if( strcmp($2->name, "TO") == 0 ) // INC => lower than => do
                    {
                        result.push_back("\tJL lb&" + string($1->type));
                    }
                    else if( strcmp($2->name, "DOWNTO") == 0 ) // DEC => greater than => do
                    {
                        result.push_back("\tJG lb&" + string($1->type));
                    }
                }
                
    | forloop forloop_type expression STEP expression ')' stmt_list ENDFOR
                {
                    ++temp_variable_counter;
                    look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data() , (const char*)$2->type);
                    
                    // $1->name:assigned_var(in assign_stmt)  
                    // $1->type: lb&jump_counter : 
                    if( strcmp($2->name, "TO") == 0 ) 
                    {
                        result.push_back("\tI_ADD " + string($1->name) + "," + string($5->name) + ",T&" + IntToString(temp_variable_counter) ); // + STEP
                        result.push_back("\tI_STORE T&" + IntToString(temp_variable_counter) + "," + string($1->name)); 
                    }
                    else if( strcmp($2->name, "DOWNTO") == 0) 
                    {
                        result.push_back("\tI_SUB " + string($1->name) + "," + string($5->name) + ",T&" + IntToString(temp_variable_counter) ); // - STEP
                        result.push_back("\tI_STORE T&" + IntToString(temp_variable_counter) + "," + string($1->name));
                    }
                    
                    // CMP fixed_assigned_var , $3 expression_result
                    if( strcmp( look_for_symbol($1->name, "")->type , "Integer" ) == 0)
                    {
                        result.push_back("\tI_CMP " + string($1->name) + "," + string($3->name));
                    }
                    else if( strcmp( look_for_symbol($1->name, "")->type , "Float" ) == 0)
                    {
                        result.push_back("\tF_CMP " + string($1->name) + "," + string($3->name));
                    }
                    
                    if( strcmp($2->name, "TO") == 0 )
                    {
                        result.push_back("\tJL lb&" + string($1->type));
                    }
                    else if( strcmp($2->name, "DOWNTO" ) == 0 )
                    {
                        result.push_back("\tJG lb&" + string($1->type));
                    }
                }
    ;


                
forloop : FOR '(' assign_stmt
            {
                ++jump_counter;
                result.push_back("lb&" + IntToString(jump_counter) + ":");
                $$ = new symtab();
                $$->name = $3->name;
                $$->type = strdup( IntToString(jump_counter).data() );
            }
    ;

forloop_type: TO 
            {
                $$ = $1; 
                $$->name = strdup( "TO" );
            }
            
	| DOWNTO 
            {
                $$ = $1; 
                $$->name = strdup( "DOWNTO" );
            }
	;


if_stmt : IF '(' bool_exp ')' THEN stmt_list else_t stmt_list ENDIF 
            {
                ++jump_counter;
                result.push_back("lb&" + string($7->name) + ":");
            }
	| IF '(' bool_exp ')' THEN stmt_list ENDIF 
            {
                result.push_back("lb&" + string($3->name) + ":");
                tmp_jump_counter.pop_front();
            }
	;


    
else_t: ELSE
        {
			++jump_counter;
			result.push_back("\tJ lb&" + IntToString(jump_counter));
			$$ = new symtab();
			$$->name = strdup(IntToString(jump_counter).data());
			result.push_back("lb&" + IntToString(tmp_jump_counter.front()) + ": ");
			tmp_jump_counter.pop_front();
		}
	;


bool_exp: expression bool_op expression
            {
                symtab* left_ID = look_for_symbol($1->name, $1->type);
                ++jump_counter;
                if( strcmp(left_ID->type, "Integer") == 0 ) 
                {
                    result.push_back("\tI_CMP " + string($1->name) + "," + string($3->name));
                }
                else if( strcmp(left_ID->type, "Float") == 0 )
                {
                    result.push_back("\tF_CMP " + string($1->name) + "," + string($3->name));
                }
                else 
                {
                    result.push_back("\tI_CMP " + string($1->name) + "," + string($3->name));
                }
                //exit or else
                result.push_back("\t" + string($2->name) + " lb&" + IntToString(jump_counter));
                $$ = new symtab();
                $$->name = strdup(IntToString(jump_counter).data());
                tmp_jump_counter.push_front(jump_counter);
            }
	;


bool_op: GE 
        {
            $$ = new symtab(); 
            $$->name = (char *)"JL";
        }

	| LE 
        {
            $$ = new symtab(); 
            $$->name = (char *)"JG";
        }
	
	| G 
        {
            $$ = new symtab();
            $$->name = (char *)"JLE";
        }
	
	| L 
        {
            $$ = new symtab(); 
            $$->name = (char *)"JGE";
        }	

	| EQ 
        {
			$$ = new symtab(); 
			$$->name = (char *)"JNE";
		}	

	| NE 
        {
			$$ = new symtab(); 
			$$->name = (char *)"JE";
		}
	;

    
expression: expression '+' term 
            {
                symtab* pre_ID = look_for_symbol($1->name, $1->type);
                ++temp_variable_counter;
                look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
                if( strcmp(pre_ID->type, "Integer") == 0 ) 
                {
                    result.push_back("\tI_ADD " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else if( strcmp(pre_ID->type, "Float") == 0 ) 
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
            
    |    expression '-' term 
            {
                symtab* pre_ID = look_for_symbol($1->name, $1->type);
                ++temp_variable_counter;
                look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
                if( strcmp(pre_ID->type, "Integer") == 0 )
                {
                    result.push_back("\tI_SUB " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
                }
                else if( strcmp(pre_ID->type, "Float") == 0 )
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
            
    |    term 
            {
                $$ = $1; 
            }
    ;

term : term '*' factor 
        {
            symtab* pre_ID = look_for_symbol($1->name, $1->type);
            ++temp_variable_counter;
            look_for_symbol( string("T&"+IntToString(temp_variable_counter)).data() , (const char*)$1->type);
            if( strcmp(pre_ID->type, "Integer") == 0 ) 
            {
                result.push_back("\tI_MUL " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
            }
            else if( strcmp(pre_ID->type, "Float") == 0 ) 
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
            symtab* pre_ID = look_for_symbol($1->name, $1->type);
            ++temp_variable_counter;
            look_for_symbol(string("T&"+IntToString(temp_variable_counter)).data(), (const char*)$1->type);
            if( strcmp(pre_ID->type, "Integer") == 0 ) 
            {
                result.push_back("\tI_DIV " + string($1->name) + "," + string($3->name) + ",T&" + IntToString(temp_variable_counter));
            }
            else if( strcmp(pre_ID->type, "Float") == 0  )
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
            
    |    '(' expression ')'    
            { 
                $$ = $2; 
            }
            
    |    ID  
            {
                $$ = look_for_symbol($1->name, "");
            }
            
    |	ID '[' expression ']' 
            { 
                $$ = new symtab(); 
                if( strcmp($3->type,"Integer") == 0 )
                {
                    $$->name = strdup( (string($1->name)+"["+string($3->name)+"]").data() );
                    $$->type = look_for_symbol($1->name, "")->type; 
                }
                else
                {
                    yyerror("array[N], N only accept a Integer");
                }
                
                
            }
            
    |    INT_LITERAL 
            {
                $$ = look_for_symbol($1->name, "Integer");
            }
            
    |    FLOAT_LITERAL     
            {
                $$ = look_for_symbol($1->name, "Float");
            }
    ;
    
%%

int main() {
    yyparse();
    for(int i=0 ; i < result.size() ; ++i) 
    {
        cout << result.at(i) << endl;
    }
    for(int i=1 ; i <= temp_variable_counter ; ++i) 
    {
		cout<< "\tDeclare T&" << i << ", " << look_for_symbol( string("T&"+IntToString(i)).data(), "")->type << endl;
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