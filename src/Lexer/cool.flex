/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%option noyywrap

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_depth = 0;
int string_const_len = 0;

%}


%x COMMENT
%x NESTED_COMMENT
%x STRING ESCAPE

/*
 * Define names for regular expressions here.
 */

DARROW          =>
LE              <=
ASSIGN          <-

DIGIT           [0-9]
LETTER          [a-zA-Z]
BLANK           [ \f\r\t\v]
OPERATOR        ("+"|"-"|"*"|\/)
SINGLE_CHAR_TOKEN     ("~"|"<"|"="|"("|")"|"{"|"}"|";"|":"|"."|","|"@")

SINGLE_LINE_COMMENT       "--"
NESTED_COMMENT_START      \(\*
NESTED_COMMENT_END        \*\)

CLASS       (?i:class)
ELSE        (?i:else)
FI          (?i:fi)
IF          (?i:if)
IN          (?i:in)
INHERITS    (?i:inherits)
LET         (?i:let)
LOOP        (?i:loop)
POOL        (?i:pool)
THEN        (?i:then)
WHILE       (?i:while)
CASE        (?i:case)
ESAC        (?i:esac)
OF          (?i:of)
NEW         (?i:new)
ISVOID      (?i:isvoid)
NOT         (?i:not)

BOOL_CONST_TRUE     (t)(?i:rue)
BOOL_CONST_FALSE    (f)(?i:alse)
INT_CONST           {DIGIT}+
TYPEID              ("SELF_TYPE"|[A-Z]({LETTER}|{DIGIT}|"_")*)
OBJECTID            ("self"|{LETTER}({LETTER}|{DIGIT}|"_")*)


%%

 /*
  *  Nested comments
  */

{SINGLE_LINE_COMMENT} { BEGIN(COMMENT); }
<COMMENT>\n {
  curr_lineno++;
  BEGIN(INITIAL);
}
<COMMENT>. { }

{NESTED_COMMENT_START} {
  BEGIN(NESTED_COMMENT);
}
<NESTED_COMMENT>\n {  curr_lineno++; }
<NESTED_COMMENT>{NESTED_COMMENT_START}  {  comment_depth++;  }
<NESTED_COMMENT>{NESTED_COMMENT_END}    {
  comment_depth--;
  if (comment_depth == 0) {
    BEGIN(INITIAL);
  }
}
<NESTED_COMMENT><<EOF>>   {
  cool_yylval.error_msg = "EOF in comment";
  BEGIN(INITIAL);
  return (ERROR);
}
<NESTED_COMMENT>. { }

{NESTED_COMMENT_END} {
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}


 /*
  *  The multiple-character operators
  */
  
{DARROW}		return (DARROW); 
{LE}        return (LE);  
{ASSIGN}    return (ASSIGN);

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}     return (CLASS);
{ELSE}      return (ELSE);
{FI}        return (FI);
{IF}        return (IF);
{IN}        return (IN);
{INHERITS}  return (INHERITS);
{LET}       return (LET);
{LOOP}      return (LOOP);
{POOL}      return (POOL);
{THEN}      return (THEN);
{WHILE}     return (WHILE);
{CASE}      return (CASE);
{ESAC}      return (ESAC);
{OF}        return (OF);
{NEW}       return (NEW);
{ISVOID}    return (ISVOID);
{NOT}       return (NOT);

{BOOL_CONST_TRUE}   {
  cool_yylval.boolean = true;
  return (BOOL_CONST);
}
{BOOL_CONST_FALSE}  {
  cool_yylval.boolean = false;
  return (BOOL_CONST);
}

{INT_CONST} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}

{SINGLE_CHAR_TOKEN} { return (int)(yytext[0]);  }
{OPERATOR} {  return (int)(yytext[0]);  }

{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}
{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}



 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" {
  BEGIN(STRING);
  string_const_len = 0;
}

<STRING>\"  {
  string_buf_ptr = (char*) &string_buf;
  cool_yylval.symbol = stringtable.add_string(string_buf_ptr, string_const_len);
  // string_const_len = 0;
  BEGIN(INITIAL);
  return (STR_CONST);
}

<STRING><<EOF>> {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  return (ERROR);
}

<STRING>\n { 
  curr_lineno++;
  cool_yylval.error_msg = "Unterminated string constant";
  BEGIN(INITIAL);
  return (ERROR);
}

<STRING>\\n {
  if (string_const_len + 1 >= MAX_STR_CONST) {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
  else {
    string_buf[string_const_len++] = '\n';
  }
}

<STRING>\\t {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = '\t';
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
}

<STRING>\\f {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = '\f';
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
}

<STRING>\\b {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = '\b';
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
}

<STRING>\\0 {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = yytext[1];
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
}

<STRING>\\\n {
  curr_lineno++;
  
  // if (string_const_len + 1 < MAX_STR_CONST) {
  //   string_buf[string_const_len++] = yytext[1];
  //   curr_lineno++;
  // }
  // else {
  //   cool_yylval.error_msg = "String constant too long";
  //   // string_const_len = 0;
  //   BEGIN(INITIAL);
  //   return (ERROR);
  // }
}

<STRING>\\[^\0\n] {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = yytext[1];
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }

}

<STRING>\0 {
  cool_yylval.error_msg = "String contains null character";
  BEGIN(ESCAPE);
  return (ERROR);
}

<ESCAPE>[^\n\"] { }
<ESCAPE>[\n\"] { BEGIN(INITIAL);  }

<STRING>. {
  if (string_const_len + 1 < MAX_STR_CONST) {
    string_buf[string_const_len++] = yytext[0];
  }
  else {
    cool_yylval.error_msg = "String constant too long";
    // string_const_len = 0;
    BEGIN(INITIAL);
    return (ERROR);
  }
}



 /*
  * Line tracking
  */

\n        {  curr_lineno++;  }
{BLANK}+  { }

 /*
  * Everything else - error
  */

. {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}


%%
