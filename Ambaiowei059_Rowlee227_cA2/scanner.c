/**********************************************************************************************
Filename:           Scanner.c
Compiler:           Visual Studio Premium 2013
Authors:            Ambaiowei Charles & Patrick Rowlee, 040753059 & 040750227
Course:             CST8152-Compilers
Lab Section:		312
Assignment:         Assignment 2 - The Scanner
Date:				0ctober 31st,2016
Professor:          Svillen Ranev
Purpose:            Implementing a Lexical Analyzer (Scanner)
                    as required for CST8152, Assignment #2
                    scanner_init() must be called before using the scanner.
Functions list:     Token aa_func02(char *lexeme);
                    Token aa_func03(char *lexeme);
                    Token aa_func05(char *lexeme);
                    Token aa_func08(char *lexeme);
                    Token aa_func10(char *lexeme);
                    Token aa_func12(char *lexeme);
                    scanner_init(), char_class(), get_next_state(),
                    iskeyword(), atool(), mlwpar_next_token(),
*********************************************************************************************/


/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */
#include <math.h>
/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"


#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/* ************************************************************************
* Purpose:		To initialize the scanner and buffer program by reseting all
                data member to 0 and to the begining of the buffer
* Author:		Svillen Ranev
* Version:		1.0
* Date:			2016-11-03
* ***********************************************************************/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
	/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/* ***************************************************************************
* Purpose:			The purpose of this function is to be able to identify and
                    the return the corrected token from the source, buffer.
* Author:           Charles Ambaiowei & Patrick Rowlee
* History/Version:	1.0
* Called Function:	b_getc(), b_retract(), b_setmark(), b_retract_to_mark()
*					b_getc_offset(), b_addc(), isalnum(), b_create(), b_free()
* Parameters:       sc_buf,
* Return Value:     Token
* Algorithm:		Read the buffer, sc_buf character by character to identify
                    corrected token and set corresponding attribute based on
                    the token and return the token to calling function
* ***************************************************************************/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */

	/*
	lexstart is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the first character of the current lexeme,
	which is being processed by the scanner.
	lexend is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the last character of the current lexeme,
	which is being processed by the scanner.
	*/

	int i = 0;/* iteration variable*/

	if (sc_buf == NULL){
		t.code = ERR_T;
		return t;
	}


	/* SPECIAL CASES OR TOKEN DRIVEN PROCESSING */

	/* endless loop broken by token returns it will generate a warning */
	while (1){
		c = b_getc(sc_buf); /*Get the char*/
		//printf("c: %c | %d\n", c,c);
		switch (c){
			/*Check for SOURCE END OF FILE TOKEN*/
		case SEOF:
		case SEOF_255:
			t.code = SEOF_T;
			return t;
			/* equals token. */
		case '=':
			c = b_getc(sc_buf);
			if (c == '='){					/*If first = is found, check the following character, if next char is = then its "==", assigned with REL_OPT*/
				t.attribute.rel_op = EQ;
				t.code = REL_OP_T;
				return t;
			}
			/* if its not = next its an assignment operator*/
			t.code = ASS_OP_T;
			b_retract(sc_buf);							/*Retract the buffer*/
			return t;
			break;
			/* white space */
		case ' ':
			continue;
			/*check for tab*/
		case'\t':
			continue;
		case '\n':
			++line;
			//printf("new line \n");
			continue;
			/* ******************************
			* Case for concatenation Operation
			* ******************************/
		case '#':
			t.code = SCC_OP_T;
			return t;									/*Set concatenation token*/
			break;

			/* ******************************
			* Cases for Arithmectic Operation
			* ******************************/
			/* plus token. */
		case '+':
			t.attribute.arr_op = PLUS;
			t.code = ART_OP_T;
			return t;
			break;
			/* minus token. */
		case '-':
			t.attribute.arr_op = MINUS;
			t.code = ART_OP_T;
			return t;
			break;
			/* multiplication token. */
		case '*':
			t.attribute.arr_op = MULT;
			t.code = ART_OP_T;
			return t;
			break;
			/* division token. */
		case '/':
			t.attribute.arr_op = DIV;
			t.code = ART_OP_T;
			return t;
			break;

			/* ******************************
			* Cases for Relational Operation
			* ******************************/
		case '<':
			c = b_getc(sc_buf);
			if (c == '>'){												
				t.attribute.rel_op = NE;
			}
			else{
				t.attribute.rel_op = LT;
				b_retract(sc_buf);										
			}
			t.code = REL_OP_T;
			return t;
			break;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
			break;

			/* ******************************
			* Cases for Separators
			* ******************************/
		case ';':
			t.code = EOS_T;				/*Set end of statement token*/
			return t;
			break;

		case ',':
			t.code = COM_T;				/*Set comma token*/
			return t;
			break;

		case ')':						/*Set right parenthesis token*/
			t.code = RPR_T;
			return t;
			break;

		case '(':						/*Set left parenthesis token*/
			t.code = LPR_T;
			return t;
			break;

		case '}':						/*Set right brace token*/
			t.code = RBR_T;
			return t;
			break;

		case '{':
			t.code = LBR_T;				/*Set left brace token*/
			return t;
			break;


			/* ******************************
			* Check for Logical Operation
			*      .AND.    .OR.
			* ******************************/

		case '.':  /* Set Logical operator token */
			b_setmark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			if (c == 'A') {
				c = b_getc(sc_buf);

				if (c == 'N') {
					c = b_getc(sc_buf);

					if (c == 'D') {
						c = b_getc(sc_buf);

						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}
			else if (c == 'O') {
				c = b_getc(sc_buf);

				if (c == 'R') {
					c = b_getc(sc_buf);

					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}

			t.code = ERR_T;
			sprintf(t.attribute.err_lex, ".");
			b_retract_to_mark(sc_buf);
			return t;

			/* Case Commments*/
		case '!':
			c = b_getc(sc_buf);
			if (c == '<'){
				while (c != '\n') {
					c = b_getc(sc_buf);
				}
				b_retract(sc_buf);
				continue;
			}
			else{ /*If there's a comment assume the line is still a comment*/
				t.code = ERR_T;
				sprintf(t.attribute.err_lex, "!%c", c);
				t.attribute.err_lex[2] = '\0';
				while (c != '\n') {
					c = b_getc(sc_buf);
				}
				b_retract(sc_buf);
				return t;
			}

		case '"':  /* Set  String literal token */

			b_setmark(sc_buf, b_getcoffset(sc_buf));	/*SET MARK TO MARK THE BEGINNING OF THE STRING*/
			lexstart = b_getcoffset(sc_buf);	/* lexstart is set to the  the mark */

			c = b_getc(sc_buf);
			while (c != '"'){
				if (c == '\0' || c == 255){

					t.code = ERR_T;
					lexend = b_getcoffset(sc_buf);/*set lexend to the end of file */

					b_retract_to_mark(sc_buf);	/* retract buffer back to start */
					b_retract(sc_buf);	/* retract to exclude '"' in error token err_lex */
					lexstart--;	/*  lexstart must be decremented to match the retracted */

					while (lexstart != lexend){		/* loops until has not been reached */
						c = b_getc(sc_buf);
						if (i < ERR_LEN){		/* If the string is less than 20 characters keep adding characters*/
							t.attribute.err_lex[i++] = c;	/* add character to err_lex && increment count i */
						}
						lexstart++;
					}
					
					t.attribute.err_lex[ERR_LEN - 3] = '.';
					t.attribute.err_lex[ERR_LEN - 2] = '.';
					t.attribute.err_lex[ERR_LEN - 1] = '.';
					t.attribute.err_lex[ERR_LEN] = '\0';
					return t;
				}
				c = b_getc(sc_buf);
			}
			lexend = b_getcoffset(sc_buf) - 1;	/* decrement lexend */

			t.code = STR_T;
			t.attribute.str_offset = b_size(str_LTBL);

			b_retract_to_mark(sc_buf);	/* retracts to begin */

			while (lexstart != lexend) {	/* reads until lexend */
				c = b_getc(sc_buf);

				b_addc(str_LTBL, c);

				++lexstart;
			}
			b_addc(str_LTBL, '\0');
			c = b_getc(sc_buf);
			return t;



		default:

			/* PROCESS STATE TRANSITION TABLE */

			/*IF (c is a digit OR c is a letter)*/
			if (isalnum(c)){
				lexstart = b_getcoffset(sc_buf) - 1;
				/*SET THE MARK AT THE BEGINING OF THE LEXEME - by getting  the getc_offset of the next character -1 */
				b_setmark(sc_buf, lexstart);

				/*Get the next state from the transition table*/
				state = get_next_state(state, c, &accept);
				/*check if state is NOAS*/
				while (accept == NOAS) {
					/*Get the next character*/
					c = b_getc(sc_buf);

					/*Get the next state from the transition table*/
					state = get_next_state(state, c, &accept);

				}

				/*RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE
				GET THE BEGINNING AND THE END OF THE LEXEME*/
				if (accept == ASWR) {
					b_retract(sc_buf);
				}
				/*SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION CREATE  A TEMPORRARY LEXEME BUFFER HERE;*/
				lexend = b_getcoffset(sc_buf);
				//lex_buf = b_create(lexend - lexstart, 1, 'f');

				lex_buf = b_create(1000, 1, 'a');
				b_retract_to_mark(sc_buf);
				/*USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc()*/
				while (b_getcoffset(sc_buf) < lexend){
					c = b_getc(sc_buf);
					//printf("ca %c | %d \n", c, c);
					b_addc(lex_buf, c);
				}
				b_addc(lex_buf, '\0'); /*add the trailing \0 to make c type string*/
				//printf("head |%s|\n", b_cbhead(sc_buf));
				t = aa_table[state](b_cbhead(lex_buf)); /* call the accepting function */

				b_free(lex_buf); /* free lex_buf*/
				return t;

			}
			else{ /* if char does not match isalnum(),return error token for character */
				t.code = ERR_T; /* set error code*/
				t.attribute.err_lex[0] = c; /*set to token attribute*/
				t.attribute.err_lex[1] = '\0'; /*add termination for c type string*/
				return t;
			}
			break;
		}/*End of switch statement*/

	}/* End of while(1) */
}/*End of function*/


/* *************************************************************************
* Purpose:			The purpose of this function is to find the corresponding
state from function arguments via transition table
* Author:			Svillen Ranev
* History/Version:	1.0
* Called Function:	char_class()
* Parameter:		t
* Algorithm:		Validate the input and calling char_class function to
					get the column number corressponding to the char and
					use that number to find the next state of input using
					transition table
* ************************************************************************/
int get_next_state(int state, char c, int *accept){
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif

	*accept = as_table[next];
	return next;
}
/* ************************************************************************
* Purpose:			This function is used to return the column of the character
					on the transistion table
* Author:			Ambaiowei Charles,Patrick Rowlee
* History/Version:	1.0
* Called Function:	isalpha()
* Parameters:
* Return value:		integer
* Algorithm:		Check the condition of table column and return the col
					that matches with corresponding character
* ***********************************************************************/
int char_class(char c){
	int val;

	/*THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
	TABLE st_table FOR THE INPUT CHARACTER c.
	SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
	FOR EXAMPLE IF COLUMN 1 REPRESENTS[A - Z]
	THE FUNCTION RETURNS 1 EVERY TIME c IS ONE
	OF THE LETTERS A, B, ..., Z.
	/* checks if the passed character is alphabetic */
	if (isalpha(c)) {
		val = 0;
	}
	else if (c == '0') {
		val = 1;
	}
	else if (c >= '1' && c < '8') {
		val = 2;
	}
	else if (c == '8' || c == '9') {
		val = 3;
	}
	else if (c == '.') {
		val = 4;
	}
	else if (c == '%') {
		val = 5;
	}
	else {
		val = 6;
	}
	return val;
}

/**************************************************************************
Purpose:		  Accepting function for arithmetic variable (VID-AVID/KW_T)
Author:			  Patrick Rowlee
History/Versions: 1.0
Called functions: isKeyword(), strlen()
Parameters:		  char lexeme[]
Return value:	  Token
Algorithm:		  Setting token attribute with content of lexeme buffer.
				  Set and and return VID/KW_T token whenever the function is
				  called
***************************************************************************/
Token aa_func02(char lexeme[]){
	//printf("lexeme: |%s| \n", lexeme);
	Token t;/* Token to return */
	int i;
	int findKeyword = iskeyword(lexeme);
	//printf("is: |%d| \n", findKeyword);
	int lexLength = strlen(lexeme);
	//PROCESSING ERROR WRONG
	if (findKeyword != NEG){
		t.attribute.kwt_idx = findKeyword;
		t.code = KW_T;
		return t;
	}
	if (lexLength > VID_LEN){
		for (i = 0; i < VID_LEN; i++){
			t.attribute.vid_lex[i] = lexeme[i];
		}
	}
	else{
		for (i = 0; i <lexLength; i++){
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[i] = '\0';
	}
	t.code = AVID_T;
	t.attribute.vid_lex[VID_LEN] = '\0';
	return t;
}

/***************************************************************************
Purpose:		   Accepting function for (VID-SVID)
Author:			   Charles Ambaiowei
History/Versions:  1.0
Called functions:  strlen()
Parameters:		   char lexeme[]
Return value:	   Token t
Algorithm:		   Setting token attribute with content of lexeme buffer.
				   Set and and return SVID token whenever the function is
				   called
***************************************************************************/
Token aa_func03(char lexeme[]){

	Token t;/* Token to return */
	int i = 0, lexLength = strlen(lexeme);
	t.code = SVID_T;
	if (lexLength < (VID_LEN - 1)){
		for (i = 0; i < lexLength; i++){
			t.attribute.vid_lex[i] = lexeme[i];
			if (lexeme[i] == '%')
			{
				break;
			}
		}
	}

	if (lexLength > VID_LEN){
		for (i = 0; i < (VID_LEN - 1); i++){
			t.attribute.vid_lex[i] = lexeme[i];
			if (lexeme[i] == '%' && i < (VID_LEN - 2))
			{
				break;
			}
		}
	}
	t.attribute.vid_lex[i] = '%';
	t.attribute.vid_lex[i + 1] = '\0';
	return t;
}

/***************************************************************************
Purpose:		   Accepting function for the integer literal (IL) - decimal constant (DIL) and zero (0).
Author:			   Patrick Rowlee
History/Versions:  1.0
Called functions:
Parameters:		   char lexeme[]
Return value:	   Token t
Algorithm:
* **************************************************************************/
Token aa_func05(char lexeme[]){
	//printf("in func 5\n");
	//printf("lexeme: |%s| \n", lexeme);
	Token t;
	/* Convert string to integer */
	//int value = atoi(lexeme);
	long value = atol(lexeme);

	/* making sure function is in the SAME RANGE as the value of 2-byte int in C */
	if (value < 0 || value >SHRT_MAX){
		t.code = ERR_T;
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
		t.attribute.vid_lex[ERR_LEN] = '\0';
	}
	else{
		/* THE FUNCTION MUST RETURN ERROR TOKEN THE ERROR TOKEN ATTRIBUTE */
		t.code = INL_T;
		t.attribute.int_value = value;
	}
	return t = aa_table[ES](lexeme);
}

/***************************************************************************
Purpose:		   The purpose of this function is to return the floating point literal token
Author:			   Charles Ambaiowei
History/Versions:  1.0
Called functions:  atof(),aa_func12()
Parameters:		   char lexeme[]
Return value:	   Token t
Algorithm:
***************************************************************************/
Token aa_func08(char lexeme[]){
	//printf("in func 8\n");
	Token t;
	//int lexLength = strlen(lexeme);
	double fpl;

	fpl = atof(lexeme);

	if (fpl > FLT_MAX || (fpl < FLT_MIN && fpl != 0.0)){
		return t = aa_table[ES](lexeme);
	}
	t.code = FPL_T;
	t.attribute.flt_value = (float)fpl;
	return t;
}

/* *************************************************************************
Purpose:		   Accepting function for the integer literal (IL) - octal constant (OIL).
Author:			   Charles Ambaiowei
History/Versions:  1.0
Called functions:  strlen(),atool(),aa_function12()
Parameters:		   char lexeme[]
Return value:	   Token t
Algorithm:
**************************************************************************/
Token aa_func10(char lexeme[]){
	//printf("in func 10\n");
	Token t;
	long octToLong;

	if (strlen(lexeme)>(INL_LEN + 1)){
		return t = aa_table[ES](lexeme);
	}

	octToLong = atool(lexeme);
	if (octToLong >= SHRT_MIN && octToLong <= INT_MAX){
		t.code = INL_T;
		t.attribute.int_value = (int)octToLong;
		return t;
	}
	return t = aa_table[ES](lexeme);
}

/*ACCEPTING FUNCTION FOR THE ERROR TOKEN*/
/** *************************************************************************
Purpose:		   ACCEPTING FUNCTION FOR THE ERROR TOKEN
Author:			   Patrick Rowlee
History/Versions:  1.0
Called functions:  strlen()
Parameters:		   char lexeme[]
Return value:	   Token t
Algorithm:
* **************************************************************************/
Token aa_func12(char lexeme[]){
	//printf("in func 12\n");
	Token t;
	int rangeOfLexeme, i;

	if (strlen(lexeme) > ERR_LEN){
		rangeOfLexeme = ERR_LEN;
	}
	else{
		rangeOfLexeme = strlen(lexeme);
	}

	for (i = 0; i<rangeOfLexeme; i++){
		t.attribute.err_lex[i] = lexeme[i];
	}
	t.attribute.err_lex[rangeOfLexeme] = '\0';
	t.code = ERR_T;
	return t;

}

/*CONVERSION FUNCTION*/
/*** *************************************************************************
* Purpose: Convert an octal number, in a string format, to an integer.
* Author:  Charles Ambaiowei
* History/Versions: 1.0
* Called functions: strtol()
* Parameters: char[]lexeme
* Return value: long int
*** *************************************************************************/
long atool(char * lexeme){
	//printf("in func conversion");
	int octalValue = atoi(lexeme);								/*Convert a string represent into octal number in in base 10; decimal*/
	int powerOfNumber = 0;
	int remainder = 0;
	long intValueOfOctal = 0;

	while (octalValue != 0){
		remainder = octalValue % 10;							/*Get remainder from octalValue*/
		octalValue = octalValue / 10;							/*Update the octalValue*/
		intValueOfOctal += remainder * (int)pow((double)KWT_SIZE, powerOfNumber);
		powerOfNumber++;
	}
	return intValueOfOctal;
	/*THE FUNCTION CONVERTS AN ASCII STRING
	REPRESENTING AN OCTAL INTEGER CONSTANT TO INTEGER VALUE*/
}

/*HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY).
FOR EXAMPLE*/

/* *************************************************************************
* Purpose:			The purpose of this function is to return an integer
					as an indication of the keyword in the kw_table.
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	strcmp()
* Parameter:        char*lexeme
* Return value:	    Token
* Algorithm:		Validate the input with the keyword table
* *************************************************************************/
int iskeyword(char * lexeme){
	//printf("lexeme: |%s| \n", lexeme);
	int i, temp;
	for (i = 0; i < KWT_SIZE; i++) {
		temp = strcmp(lexeme, kw_table[i]);
		/* checks if lexeme a keyword from table */
		if (temp == 0) {
			return i;
		}
	}
	return NEG;
}
