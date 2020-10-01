/* Lex specification for TINY */

%{
#include "globals.h"
#include "util.h"
//#include "scan.h"

/* lexeme of identifier or reserved word */

#define MAXTOKENLEN 40

char tokenString[MAXTOKENLEN+1];
%}

digit		[0-9]
number		{digit}+
letter		[a-zA-Z]
identifier	{letter}+
newline		\n
whitespace	[ \t]+
idnum			{identifier}{number}
numid			{number}{identifier}
nonid		{idnum}*{identifier}*|{numid}*{number}*
%%

"if"		{return IF;}
"else"		{return ELSE;}
"int"		{return INT;}
"return"	{return RETURN;}
"void"		{return VOID;}
"while"		{return WHILE;}

"=="	{return EQ;}
"!="	{return NE;}
"="		{return ASSIGN;}
"<"		{return LT;}
"<="	{return LE;}
">"		{return GT;}
">="	{return GE;}
"+"		{return PLUS;}
"-"		{return MINUS;}
"*"		{return TIMES;}
"/"		{return OVER;}
"("		{return LPAREN;}
")"		{return RPAREN;}
"["		{return LBRACKET;}
"]"		{return RBRACKET;}
"{"		{return LCURLY;}
"}"		{return RCURLY;}
";"		{return SEMI;}
","		{return COMMA;}

{number}	{return NUM;}
{identifier}	{return ID;}
{newline}	{lineno++;}
{whitespace}	{/*skip whitespace */}
{nonid} {return ERROR;}
.		{return ERROR;}
"/*"		{	
			register int c;
			for(;;)
			{
				while((c = input()) != '*' && c != EOF)
				{
				  	if(c == '\n'){
						lineno++;
					}
				}

				if(c == '*')
				{
					while((c = input()) == '*'){}
					if(c == '/')
						break; /* found end of comment */
					if(c == '\n') lineno++;
				}

				if(c == EOF)
				{
					return COMMENT_ERROR;
				}
			}
		}


%%

TokenType getToken(void)
{
	static int firstTime = TRUE;
	TokenType currentToken;
	if(firstTime)
	{
		firstTime = FALSE;
		lineno++;
		yyin = source;
		yyout = listing;
	}
	currentToken = yylex();
	strncpy(tokenString,yytext,MAXTOKENLEN);
	if(TraceScan)
	{
		fprintf(listing,"\t%d\t ",lineno);
		printToken(currentToken, tokenString);
	}
	return currentToken;
}
		 
