/**********************************************************************************************
Filename:           table.h
Compiler:           Visual Studio Premium 2013
Authors:            Ambaiowei Charles & Patrick Rowlee, 040753059 & 040750227
Course:             CST8152-Compilers
Lab Section:		312
Assignment:         Assignment 2 - The Scanner
Date:				0ctober 31st,2016
Professor:          Svillen Ranev
Purpose:            Transition Table and function declarations necessary for the scanner implementation
					as required for CST8152 - Assignment #2.
Functions list:		
*********************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/
#define TWO_BYTE_MAX 32767 /*two byte max\0*/
#define FOUR_BYTE_MAX 2147483647/*four byte max*/
#define NEG -1 /* my error no*/
#define SEOF '\0'/*source end of file \0*/
#define SEOF_HEX 0xFF/*source end of file in hex*/
#define SEOF_255 255 /*source end of file 255*/
#define ES 12 /* Error state */
#define IS -1    /* Inavalid state */
#define ASWR     2  /* accepting state with retract */
#define ASNR     3  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */
#define KWT_SIZE  8 /*size of the Keyword Table*/
#define TABLE_COLUMNS 7 /* State transition table definition */


/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */{ 1, 6, 4, 4, IS, IS, IS },
	/* State 1 */{ 1, 1, 1, 1, 2, 3, 2 },
	/* State 2 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 4 */{ ES, 4, 4, 4, 7, 5, 5 },
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 6 */{ ES, ES, 9, ES, 7, ES, 5 },
	/* State 7 */{ 8, 7, 7, 7, 8, 8, 8 },
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 9 */{ 10, 9, 9, ES, ES, 10, 10 },
	/* State 10 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 11 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 12 */{ IS, IS, IS, IS, IS, IS, IS },
	/* State 13*/{ IS, IS, IS, IS, IS, IS, IS },
};
	/* Accepting state table definition */
int as_table[] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASWR, ASNR, ASNR };

	/* Accepting action function declarations */
		Token aa_func02(char *lexeme);/*Returns an AVID_T or KW_T*/
		Token aa_func03(char *lexeme);/*Returns an SVID_T*/
		Token aa_func05(char *lexeme);/*Returns an INL_T*/
		Token aa_func08(char *lexeme);/*Returns an FPL_T*/
		Token aa_func10(char *lexeme);/*Returns an INL_T(octal number)*/
		Token aa_func12(char *lexeme);/*Returns ERR_T*/

		/* defining a new type: pointer to function (of one char * argument)
		returning Token
		*/
		typedef Token(*PTR_AAF)(char *lexeme);

	/* Accepting function (action) callback table (array) definition */
	/* If you do not want to use the typedef, the equvalent declaration is:
	* Token (*aa_table[])(char lexeme[]) = {
	*/
	PTR_AAF aa_table[] = {NULL,NULL,aa_func02,aa_func03,NULL,aa_func05,NULL,NULL,aa_func08,NULL,aa_func10,NULL,aa_func12};

	/* Keyword lookup table (.AND. and .OR. are not keywords) */
	char * kw_table[] = {
		"ELSE",
		"IF",
		"INPUT",
		"OUTPUT",
		"PLATYPUS",
		"REPEAT",
		"THEN",
		"USING"
	};

#endif



