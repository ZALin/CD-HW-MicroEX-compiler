lex my_micro_ex.l
yacc -d my_micro_ex.y
g++ lex.yy.c y.tab.c -ly -lfl