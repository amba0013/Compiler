/* *******************************************************************
* File Name:		parser.h
* Compiler:			Visual Studio Premium 2013
* Author:			Charles Ambaiowei & Patrick Rowlee
* Course:			CST8152 - Compilers,
* Lab Section:		302
* Assignment:		Assignment 4 - Parser
* Date:				8th December 2016
* Professor:		Svillen Ranev
* Purpose:			Funtion Prototypes
* Function list:	void parser(void);
					void match(void);
					void syn_eh(void);
					void syn_printe(void);
					void gen_incode(void);
* *******************************************************************/

#ifndef  PARSER_H_
#define	 PARSER_H_

#define NO_ATTR -2			/*No attribute required in the token*/

#include "buffer.h"
#include "token.h"
#include "stable.h"
#include <stdlib.h>

static Token lookahead;
static Buffer * sc_buf;

int synerrno;

extern char * kw_table[];
extern Token mlwpar_next_token(Buffer * sc_buf);
extern int line;
extern STD sym_table;
extern Buffer* str_LTBL;

enum { ELSE, IF, INPUT, OUTPUT, PLATYPUS, REPEAT, THEN, USING };

/*Function prototypes*/
void parser(Buffer * in_buf);
void program(void);
void match(int , int );
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char*);
void program(void);
void opt_statements(void);
void input_statement(void);
void statements(void);
void statement(void);
void statements_p(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void opt_variable_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);
void relational_expression(void);
void relational_operator(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);




#endif


