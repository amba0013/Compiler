/**********************************************************************************************
Filename:           passer.c
Compiler:           Visual Studio Premium 2013
Author:             Ambaiowei Charles &Patrick Rowle, 040753059
Course:             CST8152-Compilers
Lab Section:		302
Assignment:         Assignment 4 - The Parser
Date:               December 8th, 2016
Professor:          Svillen Ranev
Purpose:            Implementing a Parser and more…
Functions list:		void parser(Buffer * in_buf);
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
*********************************************************************************************/

#define _CRT_SECURE_NO_WARNINGS
#include "parser.h"


/* ************************************************************************
* Purpose:		To initialize the first lookahead used by the parser
				and start parsing the PLATYPUS program.
* Author:		Professor Svillen Ranev
* Version:		1.0
* Date:			8th December 2016
* ***********************************************************************/
void parser(Buffer * in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}
/* ***************************************************************************
* Purpose:			The match() function matches two tokens: the current input token (lookahead) and the
					token required by the parser. The token required by the parser is represented by two
					integers - the token code (pr_token_code), and the token attribute
					(pr_token_attribute). The attribute code is used only when the token code is one of
					the following codes: KW_T, LOG_OP_T, ART_OP_T, REL_OP_T. In all other cases
					the token code is matched only.
* Author:			Charles Ambaiowei &Patrick Rowle
* History/Version:	1.0 - December 12, 2016
* Called Function:	syn_eh(),syn_printe(),mlwpar_next_token()
* Parameters:		pr_token_code, pr_token_attribute
* Return Value:     void	
* ***************************************************************************/


void match(int pr_token_code, int pr_token_attribute){
	//printf("Lookahead: %d , %d\n", lookahead.code, lookahead.attribute.get_int);
	//printf("Parser: %d , %d\n", pr_token_code, pr_token_attribute);

	if (lookahead.code == pr_token_code){
		/*attribute code is used only if token codes is one of the following */
		if (pr_token_code == KW_T || pr_token_code == LOG_OP_T || pr_token_code == ART_OP_T || pr_token_code == REL_OP_T){
			switch (pr_token_code)
			{
			case KW_T:
				/*If attribute doesnt match go to syn_eh() and return*/
				if (lookahead.attribute.kwt_idx != pr_token_attribute){
					syn_eh(pr_token_code);
					return;
				}
				break;
			case LOG_OP_T:
				/*attribute doesnt match go to syn_eh() and return*/
				if (lookahead.attribute.log_op != pr_token_attribute){
					syn_eh(pr_token_code);
					return;
				}
				break;
			case ART_OP_T:
				/*attribute doesnt match go to syn_eh() and return*/
				if (lookahead.attribute.arr_op != pr_token_attribute){
					syn_eh(pr_token_code);
					return;
				}
				break;
			case REL_OP_T:
				/*attribute doesnt match go to syn_eh() and return*/
				if (lookahead.attribute.rel_op != pr_token_attribute){
					syn_eh(pr_token_code);
					return;
				}
				break;
			
			}

		}
		/*if match is  == SEOF_T return*/
		if (lookahead.code != SEOF_T){
			/*advance to nect input token*/
			lookahead = mlwpar_next_token(sc_buf);
			/*if the new look ahead is == ERR_T*/
			if (lookahead.code == ERR_T){
				/*call print function*/
				syn_printe();
					/*then advance to next token again*/
					lookahead = mlwpar_next_token(sc_buf);
				/*increment error counter no and return*/
				++synerrno;

			}
		}
		return;
	}
	/*if match unsuccesful*/
	syn_eh(pr_token_code);
	return;
}
/* ***************************************************************************
* Purpose:			This function implements a simple panic
					mode error recovery.
* Author:			Charles Ambaiowei &Patrick Rowle
* History/Version:	1.0 - December 12, 2016
* Called Function:	exit(),syn_printe(),mlwpar_next_token()
* Parameters:		sync_token_code
* Return Value:     void
* Algorithm:
* ***************************************************************************/
void syn_eh(sync_token_code){
	/*first call print function*/
	syn_printe();
	/*then increment error counter*/
	++synerrno;
	/*panic mode error recovery implementation:*/
		/*advances the lookahead till token code is found*/
		while(lookahead.code != sync_token_code){	
			/*check to prevent overrunnign the buffer checkS EOF*/
			if (lookahead.code == SEOF_T){
				/*want to  exit the function*/
				exit(synerrno);
			}
			/*advance the token using*/
			lookahead = mlwpar_next_token(sc_buf);
			
			/*if required token is found*/
			if (lookahead.code == sync_token_code){
				/*if case SEOF_T*/
				if (lookahead.code == SEOF_T){
					return;
				}
				else
				{	/*advance using*/
					lookahead = mlwpar_next_token(sc_buf);
					return;
				}
			
			}
		}

}
/* ************************************************************************
* Purpose:		Print
* Author:		provided by Svillen Ranev
* Version:		1.0
* Date:			12 December 2016
* ***********************************************************************/
/* Parser error printing function, Assignmet 4, F16*/
void syn_printe(){
	Token t = lookahead;
	//printf("%d\n", lookahead.code);
	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/* end switch*/
}/* end syn_printe()*/

/* ***************************************************************************
* Purpose:			Takes a string and prints it .
* Author:			Charles Ambaiowei &Patrick Rowle
* History/Version:	1.0 - December 12, 2016
* Called Function:	printf()
* Parameters:		char*
* Return Value:     void
* Algorithm:		print
* ***************************************************************************/
void gen_incode(char * string){
	printf("%s\n", string);
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<program>->
	PLATYPUS {<opt_statements>}

FIRST SET:
FIRST(program) = {KW_T (PLATYPUS)}

*/
void program(void){
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<opt_statements>-> 
	<statements> | E

FIRST SET:
FIRST(opt_statements) = {FIRST(statements), E}

*/
void opt_statements(){
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT){
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:

<statements> ->
	<statement><statements_p>

FIRST SET:
FIRST(STATEMENTS) = {FIRST(statement)}

*/
void statements(void){
	statement(); 
	statements_p();
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:

<statements_p> ->
<statement><statements_p> | E

FIRST SET:
FIRST(statements_p) = {FIRST (statement),E}

*/
void statements_p(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:
		
		if (lookahead.attribute.kwt_idx != PLATYPUS && lookahead.attribute.kwt_idx != ELSE
			&& lookahead.attribute.kwt_idx != THEN
			&& lookahead.attribute.kwt_idx != REPEAT){
			statements();
			statements_p();
		}
		break;
	}

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:

<statement> ->
  <assignment statement>
	| <selection statement>
	| <iteration statement>
	| <input statement>
	| <output statement>

FIRST SET:
FIRST(statement)= 
	FIRST set: { AVID_T, SVID_T, KW_T (only IF, USING, INPUT, OUTPUT) }

*/
void statement(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.kwt_idx){
		case IF:
			selection_statement();
			return;
		case USING:
			iteration_statement();
			return;
		case INPUT:
			input_statement();
			return;
		case OUTPUT:
			output_statement();
			return;
		default:
			syn_printe();
			return;
		}
	default:	 /* If the keyword token does not match the above, print an error and return. */
		syn_printe();
		return;
	}/*end switch*/
}/*end function*/


/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<assignment statement> ->
	<assignment expression>;

FIRST SET:
FIRST(assignment statement)
= {FIRST (assignment expression)}
= {AVID, SVID}

*/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
< assignment expression> ->
		AVID = <arithmetic expression>
	  | SVID = <string expression>

FIRST SET:
FIRST(assignment expression) = {AVID, SVID}

*/

void assignment_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<selection statement> ->
IF (<conditional expression>)  THEN  <opt_statements>
ELSE { <opt_statements> } ;

FIRST SET:
FIRST(<selection statement>)={KW_T(IF)}

*/
void selection_statement(void){
	match(KW_T, IF); 
	match(LPR_T, NO_ATTR); 
	conditional_expression(); 
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);	
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR); 
	opt_statements(); 
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<iteration statement> ->
USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
REPEAT {
< opt_statements>
};

FIRST SET:
FIRST(<iteration statement>)={KW_T(USING)}
*/
void iteration_statement(void){
	match(KW_T, USING); 
	match(LPR_T, NO_ATTR); 
	assignment_expression();
	match(COM_T, NO_ATTR); 
	conditional_expression();
	match(COM_T, NO_ATTR); 
	assignment_expression(); 
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT); 
	match(LBR_T, NO_ATTR); 
	opt_statements(); 
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<input statement> ->
INPUT (<variable list>);

FIRST SET:
FIRST(<input statement>)={KW_T(INPUT)}
*/

void input_statement(void){
	match(KW_T, INPUT); 
	match(LPR_T, NO_ATTR); 
	variable_list(); 
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
* Production: <variable list> 
* FIRST set: { AVID_T, SVID_T }
*/
void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<variable list_p>->
,<variable identifier><variable list_p>|E

FIRST SET:
FIRST(<variable list_p>)={,,E}

*/
void variable_list_p(void){
	if (lookahead.code != COM_T)
		return;

	match(COM_T, NO_ATTR);
	variable_identifier();
	variable_list_p();

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<variable identifier>-> AVID_T|SVID_T

FIRST SET:
FIRST(<variable identifier>)={AVID_T,SVID_T}
*/
void variable_identifier(void){
	switch (lookahead.code){
	case SVID_T:
	case AVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<output statement> ->
OUTPUT (<output list>);


FIRST SET:
FIRST(<output statement>)={KW_T(OUTPUT)}

*/
void output_statement(void){
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	opt_variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
FIRST SET:
FIRST(<output statement>)={SVID_T,AVID_T,STR_T, E}

*/

void opt_variable_list(void){
	switch (lookahead.code){
	case SVID_T: 
	case AVID_T: variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<arithmetic expression> - >
<unary arithmetic expression>
| <additive arithmetic expression>

FIRST SET:
FIRST(< arithmetic expression>)
= { FIRST(<unary arithmetic expression>, FIRST(<additive arithmetic expression>}
= {AVID_T, FPL_T, INL_T,(,+,-, }
*/
void arithmetic_expression(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		default:
			syn_printe();
			return;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed");

}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<unary arithmetic expression> ->
	-  <primary arithmetic expression>
  | +  <primary arithmetic expression>

FIRST SET:
FIRST(<unary arithmetic expression>)={+,-}

*/
void unary_arithmetic_expression(void){
	switch (lookahead.code)
	{
	case ART_OP_T: /* Arithmetic operator token */
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			return;
		default:	/*if not PLUS or MINUS*/
			syn_printe();
			return;
		}
	default:/*if not ART_OP_T*/
		syn_printe();
		return;
	}
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<additive arithmetic expression>->
	<multiplicative arithmetic expression><additive arithmetic expression_p>

FIRST SET:
FIRST(<additive arithmetic expression>)
={FIRST(<multiplicative arithmetic expression>)}
={FIRST(<primary arithmetic expression>)}
={AVID_T,FPL_T,INL_T,(}

*/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<additive arithmetic expression_p>->
	+ <multiplicative arithmetic expression><additive arithmetic expression_p>
  | - <multiplicative arithmetic expression><additive arithmetic expression_p>
  | E

FIRST SET:
FIRST(<additive arithmetic expression_p>) =
{+,-,E}
*/
void additive_arithmetic_expression_p(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		}
	default:;
	}
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<multiplicative arithmetic expression>->
	<primary arithmetic expression><multiplicative arithmetic expression_p>

FIRST SET:
FIRST(<multiplicative arithmetic expression>)
={FIRST(<primary arithmetic expression>)}
={AVID_T,FPL_T,INL_T,(}
*/
void multiplicative_arithmetic_expression(void){
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_p();
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<multiplicative arithmetic expression_p>
* <primary arithmetic expression><multiplicative arithmetic expression_p>
| / <primary arithmetic expression><multiplicative arithmetic expression_p>
| E

FIRST SET:
FIRST SET:
FIRST(<multiplicative arithmetic expression_p>) =
{*,/,E}

*/
void multiplicative_arithmetic_expression_p(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op){
		case DIV:
		case MULT:
			match(lookahead.code, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			return;
		default:;
		}
	default:;
	}

}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<primary arithmetic expression>->
	AVID_T | FPL_T | INL_T | (<arithmetic expression>)

FIRST SET:
FIRST(<primary arithmetic expression>)={AVID_T,FPL_T,INL_T,(}

*/
void primary_arithmetic_expression(void){
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
<string expression> ->
<primary string expression> <string expression_p>

FIRST SET:
FIRST FIRST(<string expression>)={FIRST(<primary string expression>)}
={SVID_T,STR_T}

*/
void string_expression(void){
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
<string expression_p>->
#<primary string expression> <string expression_p> | E

FIRST SET:
FIRST(<primary string expression>)={#,E}

*/
void string_expression_p(void){
	if (lookahead.code != SCC_OP_T)
		return;

		match(lookahead.code, NO_ATTR);
		primary_string_expression();
		string_expression_p();
	
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
<primary string expression> ->
SVID_T
| STR_T

FIRST SET:
FIRST(<primary string expression>)={SVID_T,STR_T}
*/
void primary_string_expression(void){
	switch (lookahead.code){
	case SVID_T:
	case STR_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<conditional expression> ->
	<logical OR  expression>

FIRST SET:
First (<conditional expression>)={ AVID_T, FPL_T,INL_T, SVID_T,STR_T}

*/
void conditional_expression(void){
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
<logical OR expression> ->
<logical AND expression> <logical OR expression_p>

FIRST SET:
FIRST(<logical  OR expression>) = {FIRST( <logical AND expression> )}
={ AVID_T, FPL_T,INL_T, SVID_T,STR_T}

*/
void logical_or_expression(void){
	logical_and_expression();
	logical_or_expression_p();
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
<logical OR expression_p>->
.OR. <logical AND expression><logical  OR expression_p> | E

FIRST SET :
FIRST(<logical OR expression_p>) = {.OR., E}

*/
void logical_or_expression_p(void){
	switch (lookahead.code){
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR){
			match(lookahead.code, lookahead.attribute.log_op);
			logical_and_expression();
			logical_or_expression_p();
			gen_incode("PLATY: Logical OR expression parsed");
		}
	default:;
	}

}
/*
Author: Charles Ambaiowei & Patrick Rowlee
<logical AND expression> ->
<relational expression> <logical AND expression_p>

FIRST SET:
FIRST( <logical AND expression> ) = { <relational expression>}
={ AVID_T, FPL_T,INL_T, SVID_T,STR_T}

*/
void logical_and_expression(void){
	relational_expression();
	logical_and_expression_p();
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<logical AND expression_p>->
	.AND. <relational expression><logical AND expression>_p> | E

FIRST_SET:
FIRST( <logical AND expression> ) = { <relational expression>}
={ AVID_T, FPL_T,INL_T, SVID_T,STR_T}

FIRST( <logical AND expression'> ) = {.AND.,e}

*/
void logical_and_expression_p(void){
	switch (lookahead.code){
	case LOG_OP_T:
		if (lookahead.attribute.log_op==AND){
			match(LOG_OP_T, AND);
			relational_expression();
			logical_and_expression_p();
			gen_incode("PLATY: Logical AND expression parsed");
		}
	default:;
	}

}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<relational expression> ->
	<primary a_relational expression> <relational operator> <primary a_relational expression> 
	|<primary s_relational expression> <relational operator> <primary s_relational expression>

FIRST SET:


*/
void relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		relational_operator();
		primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		relational_operator();
		primary_s_relational_expression();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed");

}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<primary a_relational expression> ->
AVID_T
| FPL_T
| INL_T

FIRST SET:
FIRST(<primary a_relational expression>)={AVID_T,FPL_T,INL_T}

*/
void primary_a_relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}
/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<primary s_relational expression> ->
<primary string expression>

FIRST SET:
FIRST(<primary s_relational expression>)
={FIRST(<primary string expression>)}
={SVID_T,STR_T}

*/
void primary_s_relational_expression(void){
	switch (lookahead.code){
	case SVID_T:
	case STR_T:
		primary_string_expression();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
Author: Charles Ambaiowei & Patrick Rowlee
PRODUCTION:
<relational operator>->
 >,<,==,<>

FIRST SET:
FIRST(<relational operator>)
= {REL_OP_T }
= { >,<,==,<> }

*/
void relational_operator(void){
	switch (lookahead.code){
	case REL_OP_T:
		match(lookahead.code, lookahead.attribute.rel_op);
		break;
	default:
		syn_printe();
		return;
	}

}





