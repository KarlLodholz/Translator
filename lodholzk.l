%{
/*
Definitions of constants, variables, & function prototypes go here
*/

#define T_IDENT          	1
#define T_INTCONST			2
#define T_FLOATCONST		3
#define T_STRCONST			4
#define T_IF				5
#define T_ELSE				6
#define T_WHILE				7
#define T_FUNCTION			8
#define T_FOR				9
#define T_IN				10
#define T_TRUE				11
#define T_FALSE	    		12
#define T_QUIT				13
#define T_PRINT				14
#define T_CAT				15
#define T_READ				16
#define T_LIST				17
#define T_ADD				18
#define T_SUB				19
#define T_MULT				20
#define T_DIV				21
#define T_MOD				22
#define T_POWER				23
#define T_LT				24
#define T_GT				25
#define T_LTE				26
#define T_GTE				27
#define T_EQUAL				28
#define T_NOTEQUAL	       	29
#define T_NOT				30
#define T_AND		    	31
#define T_OR				32
#define T_ASSIGN			33
#define T_SEMICOLON		    34
#define T_COMMA				35
#define T_LPAREN			36
#define T_RPAREN			37
#define T_LBRAKET			38
#define T_RBRAKET			39
#define T_LBRACE			40
#define T_RBRACE			41
#define T_COMMENT			42
#define T_UNKNOWN			99

int numLines = 0;

void printTokenInfo(const char* tokenType, const char* lexeme);

%}

/* Named definitions of regular expressions go here */

WSPACE		    [ \t\v\r]+
NEWLINE		    \n

DIGIT			[0-9]
LETTER		    [a-zA-Z]

IDENT			{LETTER}({LETTER}|{DIGIT}|_)*
INTCONST	 	(\+|-)?{DIGIT}+
FLOATCONST	 	(\+|-)?{DIGIT}+?[.]{DIGIT}+
STRCONST		["][^"\n]+["]

COMMENT 		[#].+{NEWLINE}
%%

"if"	{
		printTokenInfo("IF", yytext);
		return T_IF;
}
"else"	{
		printTokenInfo("ELSE", yytext);
		return T_ELSE;
		}
"while"	{
		printTokenInfo("WHILE", yytext);
		return T_WHILE;
		}
"function"	{
		printTokenInfo("FUNCTION", yytext);
		return T_FUNCTION;
		}
"for"	{
		printTokenInfo("FOR", yytext);
		return T_FOR;
		}
"in"	{
		printTokenInfo("IN", yytext);
		return T_IN;
}
"TRUE"	{
		printTokenInfo("TRUE", yytext);
		return T_TRUE;
}
"FALSE"	{
		printTokenInfo("FALSE", yytext);
		return T_FALSE;
}
"quit"	{
		printTokenInfo("QUIT", yytext);
		return T_QUIT;
}
"print"	{
		printTokenInfo("PRINT", yytext);
		return T_PRINT;
}
"cat"	{
		printTokenInfo("CAT", yytext);
		return T_CAT;
}
"read"	{
		printTokenInfo("READ", yytext);
		return T_READ;
}
"list"	{
		printTokenInfo("LIST", yytext);
		return T_LIST;
}
"+"		{
		printTokenInfo("ADD", yytext);
		return T_ADD;
		}
"-"		{
		printTokenInfo("SUB", yytext);
		return T_SUB;
		}
"*"		{
		printTokenInfo("MULT", yytext);
		return T_MULT;
		}
"\\"		{
		printTokenInfo("DIV", yytext);
		return T_DIV;
		}
"%%"	{
		printTokenInfo("MOD", yytext);
		return T_MOD;
}
"^"	{
		printTokenInfo("POWER", yytext);
		return T_POWER;
}
"<"	{
		printTokenInfo("LT", yytext);
		return T_LT;
}
">"	{
		printTokenInfo("GT", yytext);
		return T_GT;
}
"<="	{
		printTokenInfo("LTE", yytext);
		return T_LTE;
}
">="	{
		printTokenInfo("GTE", yytext);
		return T_GTE;
}
"=="	{
		printTokenInfo("EQUAL", yytext);
		return T_EQUAL;
}
"!="	{
		printTokenInfo("NOTEQUAL", yytext);
		return T_NOTEQUAL;
}
"!"	{
		printTokenInfo("NOT", yytext);
		return T_NOT;
}
"&"	{
		printTokenInfo("AND", yytext);
		return T_AND;
}
"|"	{
		printTokenInfo("OR", yytext);
		return T_OR;
}
"="	{
		printTokenInfo("ASSIGN", yytext);
		return T_ASSIGN;
}
";"	{
		printTokenInfo("SEMICOLON", yytext);
		return T_SEMICOLON;
}
","	{
		printTokenInfo("COMMA", yytext);
		return T_COMMA;
}
"("	{
		printTokenInfo("LPAREN", yytext);
		return T_LPAREN;
}
")"	{
		printTokenInfo("RPAREN", yytext);
		return T_RPAREN;
}
"["	{
		printTokenInfo("LBRAKET", yytext);
		return T_LBRAKET;
}
"]"	{
		printTokenInfo("RBRAKET", yytext);
		return T_RBRAKET;
}
"{"	{
		printTokenInfo("LBRACE", yytext);
		return T_LBRACE;
}
"}"	{
		printTokenInfo("RBRACE", yytext);
		return T_RBRACE;
}

{NEWLINE}	{
		numLines++;
		}

{IDENT}	{
		printTokenInfo("IDENT", yytext);
		return T_IDENT;
		}
{INTCONST}	{
		printTokenInfo("INTCONST", yytext);
		return T_INTCONST;
		}
{FLOATCONST}  {
		printTokenInfo("FLOATCONST", yytext);
		return T_FLOATCONST;
}
{STRCONST}	{
		printTokenInfo("STRCONST", yytext);
		return T_STRCONST;
}
{COMMENT}  {
		return T_COMMENT;
}
{WSPACE}	{ }
{UNKNOWN}	{
		printTokenInfo("UNKNOWN", yytext);
		return T_UNKNOWN;
}

%%

void printTokenInfo(const char* tokenType, const char* lexeme) {
  printf("TOKEN: %-20s LEXEME: %s\n", tokenType, lexeme);
}

int yywrap(void) { return 1; }

int main(void) {
  while (yylex());
  return 0;
}