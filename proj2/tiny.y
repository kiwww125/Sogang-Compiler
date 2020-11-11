%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"

//#define YYDEBUG 1

static int yylex(void);

#define YYSTYPE TreeNode *

typedef struct savedData {
    char * savedName; /* for use in saving Lexeme */
    int savedLineNo;  /* ditto */
    struct savedData * next;
} SavedData;
typedef SavedData* SDptr;

static SavedData savedInfo;
static SDptr savedInfoptr = &savedInfo;

void push(SDptr sdptr, char * sndata, int sldata) {
    SDptr new = (SDptr)malloc(sizeof(SavedData));
    new->savedName = sndata;
    new->savedLineNo = sldata;
    new->next = savedInfoptr;
    savedInfoptr = new;
}
void pop(SDptr sdptr) {
    savedInfoptr = sdptr->next;
    free(sdptr);
}


static int savedValue;  /* for use in saving size of array */
static TreeNode * savedTree; /* stores syntax tree for later return */

%}

%right IF ELSE

%token INT RETURN VOID WHILE

%token ID NUM

%token PLUS MINUS TIMES OVER
%token LEST LOET GRET GOET
%token EQ NEQ ASSIGN SEMI LPAREN RPAREN LMPAR RMPAR
%token LCPAR RCPAR LDOCU COMMA

%token ERROR COMTERR

%% /* Grammar for C MINUS */

program     : decl_list
                 { savedTree = $1;}
            ;

decl_list   : decl_list decl
                {
                  YYSTYPE t = $1;
                  while (t->sibling != NULL)
                      t = t->sibling;
                  t->sibling = $2;
                  $$ = $1;
                }

            | decl { $$ = $1; }

            ;

decl        : var_decl { $$ = $1; }

            | fun_decl { $$ = $1; }

            ;

var_decl    : type_speci
                ID { push(savedInfoptr, copyString(tokenString), lineno); }
                  SEMI
                  {
                    $$ = newDeclNode( VarK );
                    $$-> lineno = savedInfoptr->savedLineNo;
                    $$-> child[0] = $1; 
                    $$-> child[1] = newExpNode( IdK );
                    $$-> child[1]-> attr.name = savedInfoptr->savedName;
                    pop(savedInfoptr);
                  }

            | type_speci ID { push(savedInfoptr, copyString(tokenString), lineno); }
                LMPAR NUM { savedValue = atoi(tokenString2); }
                 RMPAR SEMI
                  {
                    $$ = newDeclNode( VarK );
                    $$-> lineno = savedInfoptr->savedLineNo;
                    $$-> child[0] = $1;
                    $$-> child[1] = newExpNode( SubsK );
                    $$-> child[1]-> attr.name = savedInfoptr->savedName;
                    $$-> child[2] = newExpNode( ConstK );
                    $$-> child[2]-> attr.val = savedValue;
                    pop(savedInfoptr);
                  }
            ;

type_speci  : INT { 
                    $$ = newExpNode( TypeK );
                    $$-> type = Integer; 
                    $$-> attr.name = "int"; 
                  }

            | VOID { 
                    $$ = newExpNode( TypeK );
                    $$-> type = Void;
                    $$-> attr.name = "void"; 
                   }

            ;

fun_decl    : type_speci 
                ID { push(savedInfoptr, copyString(tokenString), lineno); }

                LPAREN params RPAREN comp_stmt
                {
                   $$ = newDeclNode( FuncK );
                   $$-> lineno = savedInfoptr-> savedLineNo;
                                                         
                   $$-> child[0] = $1;
                   $$-> child[1] = newExpNode( IdK ); 
                   $$-> child[1]-> attr.name = savedInfoptr-> savedName;
                   $$-> child[1]-> lineno = savedInfoptr-> savedLineNo;
                   
                   $$-> child[2] = $5;
                   $$-> child[3] = $7;

                   pop(savedInfoptr);
                }

            ;

params      : param_list  { $$ = $1; }

            | VOID  {
                      $$ = newDeclNode( ParamK );
                      $$-> type = VOID;
                    }

            ;

param_list  : param_list COMMA param
                {
                  YYSTYPE t = $1;
                  while (t->sibling != NULL)
                      t = t->sibling;
                  t->sibling = $3;
                  $$ = $1;
                }

            | param { $$ = $1; }

            ;

param       : type_speci ID
                {
                    $$ = newDeclNode( ParamK );       
                    $$-> child[0] = $1;
                    $$-> child[1] = newExpNode( IdK ); 
                    $$-> child[1]-> attr.name = copyString(tokenString);    
                }

            | type_speci ID { push(savedInfoptr, copyString(tokenString), lineno); }
                 LMPAR RMPAR
                {
                    $$ = newDeclNode( ParamK );
                    $$-> lineno = savedInfoptr-> savedLineNo;
                    $$-> child[0] = $1;
                    $$-> child[1] = newExpNode( SubsK ); 
                    $$-> child[1]-> attr.name = savedInfoptr-> savedName;
                    pop(savedInfoptr);
                }

            ;


comp_stmt   : LCPAR local_decls stmt_list RCPAR
               {
                 $$ = newStmtNode( CompK );
                 $$-> child[0] = $2;
                 $$-> child[1] = $3;
               }

local_decls : local_decls var_decl
                {
                  YYSTYPE t = $1;
                   if (t != NULL) {
                      while (t->sibling != NULL)
                          t = t->sibling;
                      t->sibling = $2;
                      $$ = $1;
                   }
                   else $$ = $2;
                }

            | /* empty */ {$$ = NULL;}

            ;

stmt_list   : stmt_list stmt
                {
                   YYSTYPE t = $1;
                   if (t != NULL) {
                      while (t->sibling != NULL)
                          t = t->sibling;
                      t->sibling = $2;
                      $$ = $1;
                   }
                   else $$ = $2;
                }

            | /* empty */ {$$ = NULL;}

            ;

stmt        : exp_stmt { $$ = $1; }

            | comp_stmt { $$ = $1; }

            | selec_stmt { $$ = $1; }

            | iter_stmt { $$ = $1; }

            | ret_stmt { $$ = $1; }

//            | error

            ;

exp_stmt    : exp SEMI { $$ = $1; }

            | SEMI {$$ = NULL;}

            ;

selec_stmt  : IF LPAREN exp RPAREN stmt
                  {
                    $$ = newStmtNode( SelecK );
                    $$-> child[0] = $3;
                    $$-> child[1] = $5;
                  }

            | IF LPAREN exp RPAREN stmt ELSE stmt
                  {
                    $$ = newStmtNode( SelecK );
                    $$-> child[0] = $3;
                    $$-> child[1] = $5;
                    $$-> child[2] = $7;
                  }

            ;

iter_stmt   : WHILE LPAREN exp RPAREN stmt
                  {
                    $$ = newStmtNode( IterK );
                    $$-> child[0] = $3;
                    $$-> child[1] = $5;
                  }

            ;

ret_stmt    : RETURN SEMI { $$ = newStmtNode( RetK ); }

            | RETURN exp SEMI
                {
                  $$ = newStmtNode( RetK );
                  $$-> child[0] = $2;
                }

            ;

exp         : var ASSIGN exp
                {
                  $$ = newExpNode( AssignK );
                  
                  $$-> attr.op = ASSIGN;
                  $$-> child[0] = $1;
                  $$-> child[1] = $3;
                }

            | simple_exp { $$ = $1; }

            ;

var         : ID
                { $$ = newExpNode( IdK );
                  $$-> attr.name = copyString(tokenString);
                }

            | ID { push(savedInfoptr, copyString(tokenString), lineno); }
                    LMPAR exp RMPAR
                { $$ = newExpNode( SubsK );
                  $$-> lineno = savedInfoptr-> savedLineNo;
                  $$-> attr.name = savedInfoptr-> savedName;
                  $$-> child[2] = $4;
                  pop(savedInfoptr);
                }

            ;

simple_exp  : add_exp relop add_exp
                {
                   $$ = newExpNode( OpK );
                   
                   $$->attr.op = $2->attr.op;
                   free($2);
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                }

            | add_exp { $$ = $1; }

            ;

relop       : LEST { $$ = newExpNode( OpK ); $$-> attr.op = LEST; }

            | LOET { $$ = newExpNode( OpK ); $$-> attr.op = LOET; }

            | GRET { $$ = newExpNode( OpK ); $$-> attr.op = GRET; }

            | GOET { $$ = newExpNode( OpK ); $$-> attr.op = GOET; }

            | EQ { $$ = newExpNode( OpK ); $$-> attr.op = EQ; }

            | NEQ { $$ = newExpNode( OpK ); $$-> attr.op = NEQ; }

            ;

add_exp     : add_exp addop term
                {
                   $$ = newExpNode( OpK );
                   
                   $$->attr.op = $2->attr.op;
                   free($2);
                   
                   $$->child[0] = $1;
                   $$->child[1] = $3;
                }

            | term { $$ = $1; }

            ;

addop       : PLUS { $$ = newExpNode( OpK ); $$-> attr.op = PLUS; }

            | MINUS { $$ = newExpNode( OpK ); $$-> attr.op = MINUS; }

            ;

term        : term mulop factor
                {
                   $$ = newExpNode( OpK );
                   
                   $$->attr.op = $2->attr.op;
                   free($2);
                   
                   $$-> child[0] = $1;
                   $$-> child[1] = $3;
                }

            | factor { $$ = $1; }

            ;

mulop       : TIMES { $$ = newExpNode( OpK ); $$-> attr.op = TIMES; }

            | OVER { $$ = newExpNode( OpK ); $$-> attr.op = OVER; }

            ;

factor      : LPAREN exp RPAREN
                { $$ = $2; }

            | var
                { $$ = $1; }

            | call
                { $$ = $1; }

            | NUM
                {   $$ = newExpNode( ConstK );
                    $$-> attr.val = atoi(tokenString2);
                }

            | error
            
            ;

call        : ID { push(savedInfoptr, copyString(tokenString), lineno); }
                 LPAREN args RPAREN
                  {
                      $$ = newExpNode( CallK );
                      $$-> lineno = savedInfoptr-> savedLineNo;
                      $$-> attr.name = savedInfoptr-> savedName;
                      $$-> child[0] = $4;
                      pop(savedInfoptr);
                  }

            ;

args        : arg_list { $$ = $1; }

            | /* empty */ {$$ = NULL;}

            ;

arg_list    : arg_list COMMA exp
                {
                    YYSTYPE t = $1;
                    if (t != NULL) {
                      while (t->sibling != NULL)
                          t = t->sibling;
                      t->sibling = $3;
                      $$ = $1;
                    }
                    else $$ = $3;
                }

            | exp { $$ = $1; }

            ;


%%

int yyerror(char * message)
{
  fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString2);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */


static int yylex(void)
{ return getToken(); }


TreeNode * parse(void)
{ yyparse();
  return savedTree;
}
