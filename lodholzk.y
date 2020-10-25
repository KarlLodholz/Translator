/*
    minir.y

    flex minir.l
    bison minir.y
    g++ minir.tab.c -o parser
    ./parser < inputFileName

	Andy Simphaly
	Karl Lodholz

*/

%{
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <stack>
#include "SymbolTable.h"
using namespace std;

int line_num = 1;
bool assignment_statement = false;

stack<SYMBOL_TABLE> scopeStack; // stack of scope hashtables

void beginScope();
void endScope();
void cleanUp();
bool findEntryInAnyScope(const string the_name);

void printTokenInfo(const char* token_type, const char* lexeme);

void printRule(const char *, const char *);

int yyerror(const char *s)
{
    printf("Line %d: %s\n", line_num, s);
    cleanUp();
    exit(1);
}

bool exprListEps = false;
bool indexEp = false;
bool identAdded = false;
bool arithOp = false;
bool multOp = false;


extern "C"
{
    int yyparse(void);
    int yylex(void);
    int yywrap() { return 1; }
}

%}

%union {
    char* text;
    TYPE_INFO typeInfo;
};

%token T_IDENT T_INTCONST T_FLOATCONST T_UNKNOWN T_STRCONST
%token T_IF T_ELSE
%token T_WHILE T_FUNCTION T_FOR T_IN
%token T_TRUE T_FALSE T_QUIT
%token T_PRINT T_CAT T_READ T_LPAREN T_RPAREN T_LBRACE
%token T_RBRACE T_LBRACKET
%token T_RBRACKET T_SEMICOLON T_COMMA T_ADD T_SUB
%token T_MULT T_DIV T_MOD
%token T_POW T_LT T_LE T_GT T_GE T_EQ T_NE T_NOT T_AND
%token T_OR T_ASSIGN T_LIST

%type <text> T_IDENT
%type <typeInfo> N_CONST N_EXPR N_IF_EXPR N_LIST_EXPR N_QUIT_EXPR N_OUTPUT_EXPR
%type <typeInfo> N_FACTOR N_VAR N_EXPR_LIST N_INDEX N_TERM N_ENTIRE_VAR N_PARAMS
%type <typeInfo> N_WHILE_EXPR N_FOR_EXPR N_COMPOUND_EXPR N_ARITHLOGIC_EXPR
%type <typeInfo> N_ASSIGNMENT_EXPR N_INPUT_EXPR N_FUNCTION_CALL N_FUNCTION_DEF N_MULT_OP
%type <typeInfo> N_SIMPLE_ARITHLOGIC N_ADD_OP_LIST N_MULT_OP_LIST N_SINGLE_ELEMENT N_ADD_OP

/*
 *  To eliminate ambiguity in if/else
 */
%nonassoc   T_RPAREN
%nonassoc   T_ELSE


%start N_START

%%

N_START         : N_EXPR
                {
                    printRule("START", "EXPR");
                    endScope();

                    string exprType = "";
                    int n = $1.type;
                    switch(n)
                    {
                        case -1:
                            exprType = "UNDEFINED";
                            break;

                        case -2:
                            exprType = "NOT_APPLICABLE";
                            break;

                        case 0:
                            exprType = "FUNCTION";
                            break;

                        case 1:
                            exprType = "INT";
                            break;

                        case 2:
                            exprType = "STR";
                            break;

                        case 3:
                            exprType = "FLOAT";
                            break;

                        case 4:
                            exprType = "BOOL";
                            break;

                        case 5:
                            exprType = "LIST";
                            break;

                        case 6:
                            exprType = "INT_OR_STR_OR_FLOAT_OR_BOOL";
                            break;

                        case 7:
                            exprType = "NULL";
                            break;

                        default:
                            exprType = "ERROR";
                    }

                    printf("EXPR type is: %s",exprType.c_str());
                    printf("\n---- Completed parsing ----\n\n");
                    return 0;
                }
                ;
N_EXPR          : N_IF_EXPR
                {
                    printRule("EXPR", "IF_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_WHILE_EXPR
                {
                    printRule("EXPR", "WHILE_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FOR_EXPR
                {
                    printRule("EXPR", "FOR_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_COMPOUND_EXPR
                {
                    printRule("EXPR", "COMPOUND_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_ARITHLOGIC_EXPR
                {
                    printRule("EXPR", "ARITHLOGIC_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_ASSIGNMENT_EXPR
                {
                    printRule("EXPR", "ASSIGNMENT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_OUTPUT_EXPR
                {
                    printRule("EXPR", "OUTPUT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_INPUT_EXPR
                {
                    printRule("EXPR", "INPUT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_LIST_EXPR
                {
                    printRule("EXPR", "LIST_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FUNCTION_DEF
                {
                    printRule("EXPR", "FUNCTION_DEF");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FUNCTION_CALL
                {
                    printRule("EXPR", "FUNCTION_CALL");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_QUIT_EXPR
                {
                    printRule("EXPR", "QUIT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                    exit(1);
                }
                ;

N_CONST         : T_INTCONST
                {
                    printRule("CONST", "INTCONST");
                    $$.type = INT;
                }
                | T_STRCONST
                {
                    printRule("CONST", "STRCONST");
                    $$.type = STR;

                }
                | T_FLOATCONST
                {
                    printRule("CONST", "FLOATCONST");
                    $$.type = FLOAT;
                }
                | T_TRUE
                {
                    printRule("CONST", "TRUE");
                    $$.type = BOOL;

                }
                | T_FALSE
                {
                    printRule("CONST", "FALSE");
                    $$.type = BOOL;
                }
                ;

N_ARITHLOGIC_EXPR : N_SIMPLE_ARITHLOGIC
                {
                    printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC");
                    $$.type = $1.type;
                }
                | N_SIMPLE_ARITHLOGIC N_REL_OP
                  N_SIMPLE_ARITHLOGIC
                {
                    printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC REL_OP SIMPLE_ARITHLOGIC");
                    if ($1.type != INT && $1.type != FLOAT && $1.type != BOOL && $1.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                        yyerror("Arg 1 must be integer or float or bool\n");
                    }

                    if ($3.type != INT && $3.type != FLOAT && $3.type != BOOL && $3.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                        yyerror("Arg 2 must be integer or float or bool\n");
                    }

                    $$.type = BOOL;
                }
                ;

N_SIMPLE_ARITHLOGIC : N_TERM N_ADD_OP_LIST
                {
                    printRule("SIMPLE_ARITHLOGIC", "TERM ADD_OP_LIST");

                    if ($2.type != NOT_APPLICABLE) {
                            if ($1.type != INT && $1.type != FLOAT && $1.type != BOOL && $1.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                                yyerror("Arg 1 must be integer or float or bool\n");
                            }

                            if ($2.type != INT && $2.type != FLOAT && $2.type != BOOL && $2.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                                yyerror("Arg 2 must be integer or float or bool\n");
                            }
                        }

                    if ($1.type == FLOAT || $2.type == FLOAT) {
                        $$.type = FLOAT;
                    }
                    else if ($1.type == BOOL && ($2.type == BOOL || $2.type == NOT_APPLICABLE)) {
                        $$.type = BOOL;
                    }
                    else {
                        $$.type = $1.type;
                    }
                }
                ;

N_ADD_OP_LIST	: N_ADD_OP N_TERM N_ADD_OP_LIST
                {
                    if ($3.type != NOT_APPLICABLE) {
                        if ($2.type != INT && $2.type != FLOAT && $2.type != BOOL && $2.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 1 must be integer or float or bool\n");
                        }

                        if ($3.type != INT && $3.type != FLOAT && $3.type != BOOL && $3.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 2 must be integer or float or bool\n");
                        }
                    }


                    printRule("ADD_OP_LIST", "ADD_OP TERM ADD_OP_LIST");
                    if (arithOp) {
                        if ($2.type == FLOAT || $3.type == FLOAT) {
                            $$.type = FLOAT;
                        } else {
                            $$.type = INT;
                        }
                    } else {
                        $$.type = $2.type;
                    }
                }
                | /* epsilon */
                {
                    printRule("ADD_OP_LIST", "epsilon");
                    $$.type = NOT_APPLICABLE;
                }
                ;

N_TERM		: N_FACTOR N_MULT_OP_LIST
                {
                    printRule("TERM", "FACTOR MULT_OP_LIST");

                    if ($2.type != NOT_APPLICABLE) {
                        if ($1.type != INT && $1.type != FLOAT && $1.type != BOOL && $1.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 1 must be integer or float or bool\n");
                        }

                        if ($2.type != INT && $2.type != FLOAT && $2.type != BOOL && $2.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 2 must be integer or float or bool\n");
                        }
                    }

                    if ($1.type == FLOAT || $2.type == FLOAT) {
                        $$.type = FLOAT;
                    } else if ($1.type == BOOL && ($2.type == BOOL || $2.type == NOT_APPLICABLE)) {
                        $$.type = BOOL;
                    } else {
                        $$.type = $1.type;
                    }

                }
                ;

N_MULT_OP_LIST	: N_MULT_OP N_FACTOR N_MULT_OP_LIST
                {
                    printRule("MULT_OP_LIST", "MULT_OP FACTOR MULT_OP_LIST");

                    if ($3.type != NOT_APPLICABLE) {
                        if ($2.type != INT && $2.type != FLOAT && $2.type != BOOL && $2.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 1 must be integer or float or bool\n");
                        }

                        if ($3.type != INT && $3.type != FLOAT && $3.type != BOOL && $3.type != INT_OR_STR_OR_FLOAT_OR_BOOL) {
                            yyerror("Arg 2 must be integer or float or bool\n");
                        }
                    }

                    if (multOp) {
                        if ($2.type == FLOAT || $3.type == FLOAT) {
                            $$.type = FLOAT;
                        } else {
                            $$.type = INT;
                        }
                    } else {
                        $$.type = $2.type;
                    }
                }
                | /* epsilon */
                {
                    printRule("MULT_OP_LIST", "epsilon");
                    $$.type = NOT_APPLICABLE;
                }
                ;

N_FACTOR		: N_VAR
                {
                    printRule("FACTOR", "VAR");
                    $$.type = $1.type;
                }
                | N_CONST
                {
                    printRule("FACTOR", "CONST");
                    $$.type = $1.type;
                }
                | T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("FACTOR", "( EXPR )");
                    $$.type = $2.type;
                }
                | T_NOT N_FACTOR
                {
                    printRule("FACTOR", "! FACTOR");
                    $$.type = $2.type;
                }
                ;

N_COMPOUND_EXPR : T_LBRACE N_EXPR N_EXPR_LIST T_RBRACE
                {
                    printRule("COMPOUND_EXPR","{ EXPR EXPR_LIST }");

                    $$.type = $3.type == NOT_APPLICABLE ? $2.type : $3.type;
                    // $$.numParams = exprListEps? $2.numParams : $3.numParams;
                    // $$.returnType = exprListEps? $2.returnType : $3.returnType;

                }
                ;

N_EXPR_LIST     : T_SEMICOLON N_EXPR N_EXPR_LIST
                {
                    printRule("EXPR_LIST", "; EXPR EXPR_LIST");
                    $$.type = $3.type == NOT_APPLICABLE ? $2.type : $3.type;
                    // $$.numParams = exprListEps? $2.numParams : $3.numParams;
                    // $$.returnType = exprListEps? $2.returnType : $3.returnType;
                }
                | /* epsilon */
                {
                    printRule("EXPR_LIST", "epsilon");

                    $$.type = NOT_APPLICABLE;
                    exprListEps = true;
                }
                ;

N_IF_EXPR       : N_COND_IF T_RPAREN N_THEN_EXPR
                {
                    printRule("IF_EXPR", "COND_IF ) THEN_EXPR");
                }
                | N_COND_IF T_RPAREN N_THEN_EXPR T_ELSE
                  N_EXPR
                {
                    printRule("IF_EXPR", "COND_IF ) THEN_EXPR ELSE EXPR");
                }
                ;

N_COND_IF		: T_IF T_LPAREN N_EXPR
                {
                    printRule("COND_IF", "IF ( EXPR");
                }
                ;

N_THEN_EXPR    : N_EXPR
                {
                    printRule("THEN_EXPR", "EXPR");
                }
                ;

N_WHILE_EXPR    : T_WHILE T_LPAREN N_EXPR
                {
                    //cout<< "type: " << $3.type <<endl;
                    if($3.type == FUNCTION || $3.type == LIST || $3.type == NULL_TYPE || $3.type == STR) {
                        yyerror("Arg 1 cannot be function or null or list or string");
                    }
                }
                T_RPAREN N_EXPR
                {
                    printRule("WHILE_EXPR", "WHILE ( EXPR ) EXPR");
                    $$.type = $6.type;
                }
                ;

N_FOR_EXPR      : T_FOR T_LPAREN T_IDENT
                {
                    printRule("FOR_EXPR", "FOR ( IDENT IN EXPR ) EXPR");
                    if(!(scopeStack.top().findEntry($3))) {
                        //printf("___Adding %s to symbol table\n", $3);
                        bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY($3, UNDEFINED, NOT_APPLICABLE, NOT_APPLICABLE));
                    }
                }
                T_IN N_EXPR T_RPAREN N_EXPR
                {
                    $$.type = $6.type;
                }
                ;

N_LIST_EXPR     : T_LIST T_LPAREN N_CONST_LIST T_RPAREN
                {
                    printRule("LIST_EXPR", "LIST ( CONST_LIST )");
                    $$.type = LIST;
                }
                ;

N_CONST_LIST    : N_CONST T_COMMA N_CONST_LIST
                {
                    printRule("CONST_LIST", "CONST, CONST_LIST");
                }
                | N_CONST
                {
                    printRule("CONST_LIST", "CONST");
                }
                ;

N_ASSIGNMENT_EXPR : T_IDENT N_INDEX {
                    printRule("ASSIGNMENT_EXPR", "IDENT INDEX = EXPR");

                    // N_INDEX ! epsilon
                    if ($2.type != NOT_APPLICABLE) {
                        if (!(scopeStack.top().findEntry($1))) {
                            yyerror("Undefined identifier\n");
                        }
                        else {
                            if (scopeStack.top().getEntryTypeST($1) != LIST) {
                                yyerror("Arg 1 must be list\n");
                            }
                        }
                    }
                    // epsilon
                    else {
                        if (!(scopeStack.top().findEntry($1))) {
                            scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY($1, UNDEFINED, NOT_APPLICABLE, NOT_APPLICABLE));
                        }
                    }
                }
                T_ASSIGN N_EXPR {
                    // N_INDEX ! epsilon
                    if ($2.type != NOT_APPLICABLE) {
                        if ($5.type == LIST) {
                            yyerror("Arg 1 cannot be list\n");
                        }
                    }
                    // epsilon
                    else {
                        if (!(scopeStack.top().findEntry($1))) {
                            // printf("___Adding %s to symbol table\n", $1);
                            scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY($1, $5.type, NOT_APPLICABLE, NOT_APPLICABLE));
                        }
                        else {
                            SYMBOL_TABLE_ENTRY* entry = scopeStack.top().getEntry($1);
                            entry->setTheType($5.type);
                        }
                    }
                    $$.type = $5.type;
                }
                ;

N_INDEX :       T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
			    {
                    printRule("INDEX", " [[ EXPR ]]");
                    $$.type = $3.type;
			    }
			    | /* epsilon */
                {
                    printRule("INDEX", " epsilon");
                    $$.type = NOT_APPLICABLE;
                    indexEp = true;
                }
                ;

N_QUIT_EXPR     : T_QUIT T_LPAREN T_RPAREN
                {
                    printRule("QUIT_EXPR", "QUIT()");
                    $$.type = NULL_TYPE;
                }
                ;

N_OUTPUT_EXPR   : T_PRINT T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("OUTPUT_EXPR", "PRINT ( EXPR )");
                    if (($3.type == FUNCTION) || ($3.type == NULL_TYPE)) {
                        yyerror("Arg 1 cannot be function or null");
                    }
                    $$.type = $3.type;
                    $$.numParams = $3.numParams;
                    $$.returnType = $3.returnType;
                }
                | T_CAT T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("OUTPUT_EXPR", "CAT ( EXPR )");
                    if (($3.type == FUNCTION) || ($3.type == NULL_TYPE)) {
                        yyerror("Arg 1 cannot be function or null");
                    }
                    $$.type = NULL_TYPE;
                }
                ;

N_INPUT_EXPR    : T_READ T_LPAREN T_RPAREN
                {
                    printRule("INPUT_EXPR", "READ ( )");
                    $$.type = NOT_APPLICABLE;
                }
                ;

N_FUNCTION_DEF  : T_FUNCTION
                {
                    beginScope();
                }
                T_LPAREN N_PARAM_LIST T_RPAREN N_COMPOUND_EXPR
                {
                    printRule("FUNCTION_DEF","FUNCTION ( PARAM_LIST ) COMPOUND_EXPR");

                    endScope();

                    $$.type = FUNCTION;
                }
                ;

N_PARAM_LIST    : N_PARAMS
                {
                    printRule("PARAM_LIST", "PARAMS");
                }
                | N_NO_PARAMS
                {
                    printRule("PARAM_LIST", "NO PARAMS");
                }
                ;

N_NO_PARAMS     : /* epsilon */
                {
                    printRule("NO_PARAMS", "epsilon");
                }
                ;

N_PARAMS        : T_IDENT
                {
                    printRule("PARAMS", "IDENT");
                    //printf("___Adding %s to symbol table\n", $1);
                    bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY($1, INT, NOT_APPLICABLE, NOT_APPLICABLE));
                    if(!success) {
                        yyerror("Multiply defined identifier");
                        return(0);
                    }
                }
                | T_IDENT T_COMMA N_PARAMS
                {
                    printRule("PARAMS", "IDENT, PARAMS");
                    //printf("___Adding %s to symbol table\n", $1);
                    bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY($1, INT, NOT_APPLICABLE, NOT_APPLICABLE));
                    if(!success) {
                        yyerror("Multiply defined identifier");
                        return(0);
                    }
                }
                ;

N_FUNCTION_CALL : T_IDENT T_LPAREN N_ARG_LIST T_RPAREN
                {
                    printRule("FUNCTION_CALL", "IDENT" " ( ARG_LIST )");
                    if (!findEntryInAnyScope($1)) {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                }
                ;

N_ARG_LIST      : N_ARGS
                {
                    printRule("ARG_LIST", "ARGS");
                }
                | N_NO_ARGS
                {
                    printRule("ARG_LIST", "NO_ARGS");
                }
                ;

N_NO_ARGS       : /* epsilon */
                {
                    printRule("NO_ARGS", "epsilon");
                }
                ;

N_ARGS          : N_EXPR
                {
                    printRule("ARGS", "EXPR");
                }
                | N_EXPR T_COMMA N_ARGS
                {
                    printRule("ARGS", "EXPR, ARGS");
                }
                ;

N_ADD_OP	     : T_ADD
                {
                    printRule("ADD_OP", "+");
                    arithOp = true;
                }
                | T_SUB
                {
                    printRule("ADD_OP", "-");
                    arithOp = true;
                }
                | T_OR
                {
                    printRule("ADD_OP", "|");
                }
                ;

N_MULT_OP      : T_MULT
                {
                    printRule("MULT_OP", "*");
                    multOp = true;
                }
                | T_DIV
                {
                    printRule("MULT_OP", "/");
                    multOp = true;
                }
                | T_AND
                {
                    printRule("MULT_OP", "&");
                }
                | T_MOD
                {
                    printRule("MULT_OP", "\%\%");
                    multOp = true;
                }
                | T_POW
                {
                    printRule("MULT_OP", "^");
                    multOp = true;
                }
                ;

N_REL_OP        : T_LT
                {
                    printRule("REL_OP", "<");
                }
                | T_GT
                {
                    printRule("REL_OP", ">");
                }
                | T_LE
                {
                    printRule("REL_OP", "<=");
                }
                | T_GE
                {
                    printRule("REL_OP", ">=");
                }
                | T_EQ
                {
                    printRule("REL_OP", "==");
                }
                | T_NE
                {
                    printRule("REL_OP", "!=");
                }
                ;

N_VAR           : N_ENTIRE_VAR
                {
                    printRule("VAR", "ENTIRE_VAR");
                    $$.type = $1.type;
                }
                | N_SINGLE_ELEMENT
                {
                    printRule("VAR", "SINGLE_ELEMENT");
                    $$.type = $1.type;
                }
                ;

N_SINGLE_ELEMENT : T_IDENT T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
                {
                    printRule("SINGLE_ELEMENT", "IDENT"" [[ EXPR ]]");
                        if (!(findEntryInAnyScope($1))) {
                            yyerror("Undefined identifier\n");
                        }
                        else {
                            if (scopeStack.top().getEntry($1)->getTheType() != LIST) {
                                yyerror("Arg 1 must be list\n");
                            }

                            $$.type = INT_OR_STR_OR_FLOAT_OR_BOOL;
                        }


                }
                ;

N_ENTIRE_VAR    : T_IDENT
                {
                    printRule("ENTIRE_VAR", "IDENT");
                    if(!findEntryInAnyScope($1)) {
                        yyerror("Undefined identifier");
                        return(0);
                    }

                    $$.type = scopeStack.top().getEntryTypeST($1);

                }
                ;

%%

#include "lex.yy.c"
extern FILE *yyin;

void printTokenInfo(const char* token_type, const char* lexeme)
{
    //printf("TOKEN: %-20s LEXEME: %s\n", token_type, lexeme);
}

void printRule(const char *lhs, const char *rhs)
{
    //printf("%s -> %s\n", lhs, rhs);
    return;
}

void beginScope() {
    scopeStack.push(SYMBOL_TABLE());
    //printf("\n___Entering new scope...\n\n");
}

void endScope() {
    scopeStack.pop();
    //printf("\n___Exiting scope...\n\n");
}

void cleanUp() {
    if (scopeStack.empty())
        return;
    else {
        scopeStack.pop();
        cleanUp();
    }
}

int getNumParams() {
	int size = scopeStack.top().getHashTable().size();
	return size;
}

bool findEntryInAnyScope(const string the_name) {
    if (scopeStack.empty()) return(false);
    bool found = scopeStack.top().findEntry(the_name);
    if (found)
        return(true);
    else {
        SYMBOL_TABLE symbolTable = scopeStack.top();
        scopeStack.pop();
        found = findEntryInAnyScope(the_name);
        scopeStack.push(symbolTable); // restore stack
                                      // to original state
        return(found);
    }
}

int main()
{
    beginScope();
    do {
        yyparse();
    } while (!feof(yyin));

    cleanUp();

    return 0;
}
