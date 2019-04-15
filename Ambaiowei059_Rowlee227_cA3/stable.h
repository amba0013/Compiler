/* *******************************************************************
* File Name:		stable.h
* Compiler:			Visual Studio Premium 2013
* Author:			Charles Ambaiowei & Patrick Rowlee
* Course:			CST8152 - Compilers,
* Lab Section:		302
* Assignment:		Assignment 3 - Symbol Table
* Date:				24th November 2016
* Professor:		Svillen Ranev
* Purpose:			
* Function list:	st_create(), st_install(), st_lookup(),
					st_update_type(), st_update_value(), st_get_type(),
					st_destroy(), st_print(),st_setsize()
					st_incoffset(), st_store(), st_sort(),
* *******************************************************************/

#ifndef  ST_H_
#define  ST_H_

#include <stdlib.h>
#include "buffer.h"

#define FAILURE -1				/*Value of failure*/
#define SUCCESS 1				/*Value of success*/
#define EXIT 1					/*Value used to exit*/
#define DEFAULT_MASK 0xFFF8		/*Default value of the status field*/
#define RESET	0xFFF9			/*Reset*/
#define UPDATE	0x0001			/*update flag*/
#define FLOATING_POINT 	0x0002	/*Mask value to set the status field to a float*/
#define INTEGER 0x0004		/*Mask value to set the status field to an int*/
#define STRING 	0x0006		/*Mask value to set the status field to a string*/



typedef union InitialValue {
	int int_val; /* integer variable initial value */
	float fpl_val; /* floating-point variable initial value */
	int str_offset; /* string variable initial value (offset) */
} InitialValue;
typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field*/
	char * plex; /* pointer to lexeme (VID name) in CA */
	int o_line; /* line of first occurrence */
	InitialValue i_value; /* variable initial value */
	size_t reserved; /*reserved for future use*/
}STVR;
typedef struct SymbolTableDescriptor {
	STVR *pstvr; /* pointer to array of STVR */
	int st_size; /* size in number of STVR elements */
	int st_offset; /*offset in number of STVR elements */
	Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;


/*Function prototypes*/
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
static void st_setsize(void);
static void st_incoffset(void);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);

/*bonus function declarations*/
static int ascending(const void*,const void*);
static int descending(const void*, const void*);

#endif


