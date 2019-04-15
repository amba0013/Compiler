/**********************************************************************************************
Filename:           buffer.h
Compiler:           Visual Studio Premium 2013
Author:             Ambaiowei Charles, 040753059
Course:             CST8152  Compilers, Lab Section:302
Assignment:         Assignment 1 - The Buffer
Date:               September 29, 2016
Professor:          Svillen Ranev
Purpose:            Programming and Using Dynamic Structures (buffers) with C
Function list:      b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(), b_size(),
b_capacity(), b_setmark(), b_mark(), b_mode(), b_inc_factor(), b_load(),
b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(),
b_retract(), b_retract_to_mark(), b_getc_offset()
*********************************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL1 -1         /* fail return value */
#define R_FAIL2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */

#define FIXED_MODE 0					/*fixed mode*/
#define ADDITIVE_MODE 1					/*additive mode*/
#define MULTIPLICATIVE_MODE -1			/*multiplicative mode*/
#define MAXIMUM_BUFFER_SIZE SHRT_MAX	/*maximum buffer size(defined by the data type of the capacity)*/
#define INC_FACTOR_FAIL 256				/*Value returned if the Increment factor is invalid*/
#define R_SUCCESS1 1					/*Value returned if the buffer was reset correctly*/
#define BUFFER_EMPTY 1					/*Value returned if the buffer is empty*/
#define BUFFER_NOT_EMPTY 0				/*Value returned if the buffer is not empty*/
#define BUFFER_FULL 1					/*Value returned if the buffer is full*/
#define BUFFER_NOT_FULL 0				/*Value returned if the buffer is not full*/
#define SET_DEFAULT 0					/*Set the members of the buffer structure to its default value*/
#define INC_ADDCOFFSET 1				/*Increment the addc_offset by 1*/
#define EOB_REACHED 1					/*Value given if the end of buffer is reached*/
#define	MULTIPLICATIVE_MAX 100			
#define	ADDITIVE_MAX 255


/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short mark_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer * b_create(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_reset(Buffer * const);
void b_free(Buffer * const);
int b_isfull(Buffer * const);
short b_size(Buffer * const);
short b_capacity(Buffer * const);
short b_setmark(Buffer * const, short);
short b_mark(Buffer * const);
int b_mode(Buffer * const);
size_t b_incfactor(Buffer * const);
int b_load(FILE * const, Buffer * const);
int b_isempty(Buffer * const);
int b_eob(Buffer * const);
char b_getc(Buffer * const);
int b_print(Buffer * const);
Buffer *b_pack(Buffer * const);
char b_rflag(Buffer * const);
short b_retract(Buffer * const);
short b_retract_to_mark(Buffer * const);
short b_getcoffset(Buffer * const);
char * b_cbhead(Buffer * const);





#endif





