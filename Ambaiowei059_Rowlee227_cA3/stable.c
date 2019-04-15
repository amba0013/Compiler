/* *******************************************************************
* File Name:		stable.c
* Compiler:			Visual Studio Premium 2013
* Author:			Charles Ambaiowei & Patrick Rowlee
* Course:			CST8152 - Compilers, 
* Lab Section:		302
* Assignment:		Assignment 3 - Symbol Table
* Date:				24th November 2016
* Professor:		Svillen Ranev
* Purpose:			To implement and incorporate a symbol table 
* Function list:	st_create(), st_install(), st_lookup(),
					st_update_type(), st_update_value(), st_get_type(),
					st_destroy(), st_print(),st_setsize()
					st_incoffset(), st_store(), st_sort(),
* *******************************************************************/
#include "stable.h"
#include "buffer.h"
#include <stdlib.h>
#include <string.h>  

/*Global Variable*/
extern STD sym_table;

/* *************************************************************************
* Purpose:			To create a symbol table 
* Author:			Patrick Rowlee
* History/Version:  1.0
* Called Function:  b_create()
* Parameter:		st_size
* Return value:		symbol table descriptor
* Algorithm:		Validate the existence of the symbol table by checking
					the passing argument. Return immediately, if the received
					argument is invalid. Otherwise, create a symbol table.
* ************************************************************************/
STD st_create(int st_size){
	/*Local Variable of type STD*/
	STD sym_table_descriptor ;

	/*Allocate dynamic memory for an array of STVR with st_size, number of element*/
	sym_table_descriptor.pstvr = (STVR*)malloc(sizeof(STVR) * st_size);

	/*Check if the symbol table exists*/
	if (st_size <= 0){
		return sym_table_descriptor;
	}

	/*creates a self- incrementing buffer using the corresponding buffer function*/
	sym_table_descriptor.plsBD = b_create(100, 50, 'f');

	
	/*Check if it successfully allocates the memory*/
	if (sym_table_descriptor.pstvr == NULL){
		return sym_table_descriptor;
	}
	
	/*Check if the buffer has successfully created*/
	if (sym_table_descriptor.plsBD == NULL){
		/*Delete allocated memory for array of STVR*/
		free(sym_table_descriptor.pstvr);
		return sym_table_descriptor;
	}
	/*initializes the st_offset to zero*/
	sym_table_descriptor.st_offset = 0;

	/*sets the STD st_size to st_size;*/
	sym_table_descriptor.st_size = st_size;

	return sym_table_descriptor;
}

/* ************************************************************************
* Purpose:			This function is used to install a new entry of VID
					record in the symbol table.
* Author:			Charles Ambaiowei
* History/Version:	1.0
* Called Function:	st_lookup(),st_incoffset(),b_setmark()
* Parameters:		STD sym_table, char *lexeme, char type, int line
* Algorithm:		Register a new entry of VID record in the symbol table. 
					It must first call the st_lookup to search for the lexeme 
					in the symbol table.
* ***********************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){
	
	int i = 0;
	char r_flag = 0;
	short buffer_start = 0;

	/*Check that there is a symbol table*/
	if (sym_table.st_size == 0){
		return FAILURE;
	}
	/*search for the lexeme (variable name) in the symbol table.*/
	if (st_lookup(sym_table, lexeme) != -1){
		return sym_table.st_offset;
	}
	/*if (sym_table.st_offset >= sym_table.st_size){
		return FAILURE;
	}*/

	/*OTHERWISE INSTALL THE VID IN THE SYMBOL TABLE*/
	/*Set the o_line*/
	sym_table.pstvr[sym_table.st_offset].o_line = line;
	/*The location of the lexeme in the corresponding buffer*/
	b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));
	sym_table.pstvr[sym_table.st_offset].plex = &sym_table.plsBD->cb_head[b_mark(sym_table.plsBD)];
	/*set default value of the status field*/
	sym_table.pstvr[sym_table.st_offset].status_field = DEFAULT_MASK;
	/*Reset it*/
	sym_table.pstvr[sym_table.st_offset].status_field &= RESET;

	/*Put the lexeme in the buffer*/
	for (; i<(int)strlen(lexeme) + 1; i++){
		if (!b_addc(sym_table.plsBD, lexeme[i]))
			return FAILURE;
		/*If the buffer has been reallocated set r_flag to 1*/
		if (b_rflag(sym_table.plsBD))
			r_flag = 1;
	}
	/*In case The buffer has been reallocated reset plex*/
	if (r_flag){
		for (; i <= sym_table.st_offset; i++)
		{
			/*Use buffer_start as an offset*/
			/*gives a warning here when i try to cast to a short */
			sym_table.pstvr[i].plex = b_setmark(sym_table.plsBD, buffer_start);
			buffer_start += (short)strlen(sym_table.pstvr[i].plex) + 1;
		}
	}

	/*set datatype Indicator*/
	switch (type){
	case'I':
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | INTEGER;
		sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
		break;
	case'F':
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | FLOATING_POINT;
		sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0;
		break;
	case'S':
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | STRING;
		sym_table.pstvr[sym_table.st_offset].status_field = sym_table.pstvr[sym_table.st_offset].status_field | UPDATE;
		sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
		break;
	}
	
	st_incoffset();
	return sym_table.st_offset;
}
/* ************************************************************************
* Purpose:			This function searches for a lexeme (variable name) in the symbol table.
* Author:			Charles Ambaiowei
* History/Version:	1.0
* Called Function:	strcmp()
* Parameters:		STD symbol table, char *lexeme
* Return value:		lexeme offset if successful, FAILURE if unsuccessful
* ***********************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	int i;
	/*Check if the sym_table exists*/
	if (sym_table.st_size <= 0){
		return FAILURE;
	}
	/*Iterate through array of STVR object, check for lexeme*/
	for (i = sym_table.st_offset - 1; i >= 0; i--){
		if (strcmp(sym_table.pstvr[i].plex, lexeme)== 0){
			return sym_table.st_offset;
		}
	}
	return FAILURE;
}
/* ************************************************************************
* Purpose:			This function is used to update the data type indicator;
					status_field for different type of VID, including "Float",
					"Integer" string cant be updated.
* Author:			Charles Ambaiowei	
* History/Version:	1.0
* Called Function:	none
* Parameters:		STD symbol table, int VID offset, char type
* Return value:		int
* Algorithm:		function  updates the data type indicator;
					status_field using the bitwise operation based
					on the v_type value and setting a corresponding value
					for the status_field
* ***********************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	/*Check that there is a symbol table and that the type has not been changed */
	if (sym_table.st_size == 0 || (sym_table.pstvr[vid_offset].status_field & UPDATE) == UPDATE || vid_offset<0){
		return FAILURE;
	}
	/*Reset the data type*/
	sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field & RESET;

	/*Reset the type according to the v_type*/
	switch (v_type){
	case 'I':
		sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field | INTEGER;
		break;
	case 'F':
		sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field | FLOATING_POINT;
		break;
	default:
		return FAILURE;
		break;
	}
	/*Update flag*/
	sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field | UPDATE;

	return vid_offset;
}
/* ************************************************************************
* Purpose:			Update the InitialValue of the variable specified by the VID offset.
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	none 
* Parameters:		STD sym_table, int vid_offset, InitialValue i_value
* Return value:		vid_offset if successful, FAILURE if unsuccessful
* ***********************************************************************/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	/*Check function arguements*/
	if (sym_table.st_size <= 0 || vid_offset  < 0 || vid_offset > sym_table.st_offset){
		/*on failure*/
		return FAILURE;
	}
	/*Update the i_value of STVR object using pstvr and vid_offset*/
	sym_table.pstvr[vid_offset].i_value = i_value;
	/*on success*/
	return vid_offset;
}
/* ************************************************************************
* Purpose:			The function returns the type of the variable specified by vid_offset
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	none
* Parameters:		STD symbol table, int VID offset, InitialValue
* Return value:		vid_offset if successful, FAILURE if unsuccessful
* ***********************************************************************/
char st_get_type(STD sym_table, int vid_offset){
	/*Check that there is an existing symbol table*/
	if (sym_table.st_size == 0 || vid_offset<0){
		return FAILURE;
	}
	/*Bitwise status_field with the STRING MASK*/
	if ((sym_table.pstvr[vid_offset].status_field & STRING) == STRING){
		return 'S';
	}
	/*Bitwise status_field with the INTEGER MASK */
	else if((sym_table.pstvr[vid_offset].status_field & INTEGER) == INTEGER){
		return 'I';
	}
	/*Bitwise status_field with the FLOAT MASK*/
	if ((sym_table.pstvr[vid_offset].status_field & FLOATING_POINT) == FLOATING_POINT){
		return 'F';
	}
	return FAILURE;
	
}
/* ************************************************************************
* Purpose:			This function frees the memory occupied by the symbol table dynamic areas and sets st_size to 0.
* Author:			Charles Ambaiowei
* History/Version:	1.0
* Called Function:	b_free(), free(), st_setsize()
* Parameters:		STD symbol table
* ***********************************************************************/
void st_destroy(STD sym_table){
	/*check symbol table*/
	if (sym_table.plsBD == NULL || sym_table.pstvr == NULL || sym_table.st_size <= 0){
		return;
	}
	/*free allocated memory of array of STVR object*/
	free(sym_table.pstvr);
	/*Calling the b_free to release buffer memory*/
	b_free(sym_table.plsBD);
	st_setsize();
}
/* ************************************************************************
* Purpose:			function prints the contents of the symbol table to the standard output (screen)
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	printf()
* Parameters:		STD symbol table
* Return value:		number of entries printed if successful, FAILURE if unsuccessful
* ***********************************************************************/
int st_print(STD sym_table){
	int i;
	/*Check that there is an existing symbol table*/
	if (sym_table.st_size == 0){
		return FAILURE;
	}

	printf("\nSymbol Table\n");
	printf("____________\n");
	printf("\nLine Number Variable Identifier\n");

	/*Print every lexeme in the symbol table along with the line where it is found*/
	for (i = 0; i<sym_table.st_offset; i++){
		printf("%2d          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
	}

	return sym_table.st_offset;
}
/* ************************************************************************
* Purpose:			The purpose of this function is to allow other functions,
					which  are not member functions to call and reset the symbol
					table attribute; st_size
					to 0
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	none
* Parameters:		none
* Return value:		none
* Algorithm:		This function is declared as static to allow other 
					non-member function to call and reset the st_size 
* ***********************************************************************/
static void st_setsize(void){
	/*set the st_size = 0, for global use*/
	sym_table.st_size = 0;
}
/* ************************************************************************
* Purpose:			The purpose of this function is to allow other functions,
					which are not member function to increment symbol table
					attribute; st_offset by 1
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	none	
* Parameters:		none
* Return value:		none		
* Algorithm:		This function is declared as static to allow other 
					non-member function to call, incrementing the st_offset
					by 1
* ***********************************************************************/
static void st_incoffset(void){
	/*increase the st_offset of the sym_table, for global use*/
	++(sym_table.st_offset);
}
/* ************************************************************************
* Purpose:				This function is used to store the symbol table and its
						attribute values into a file, $stable.ste
* Author:				Charles Ambaiowei
* History/Version:		1.0
* Called Function:		fopen(), fclose(),
* Parameters:			STD sym_table
* Algorithm:			The purpose of this function is to store the symbol
						table and its attribute values into a file, called 
						$stable.ste.
* ***********************************************************************/
int st_store(STD sym_table){
	/* Define a file handle */
	FILE *fp;
	/* define a file name */
	char *fname = "$stable.ste";
	int record_counter = 0;
	char type;
	/*Check if the sym_table exists*/
	if (sym_table.st_size <= 0){
		return FAILURE;
	}
	/* Open a text file for writing ("wt")* If the file already exists, it will be overwritten*/
	if ((fp = fopen("$stable.ste", "wt")) == NULL){
		printf("Cannot create file: %s/n", fname);
		return FAILURE;
	}

	/*Print to the file; st_size of symbol table*/
	fprintf(fp, "%d", sym_table.st_size);

	for (record_counter = 0; record_counter<sym_table.st_offset; record_counter++){
		fprintf(fp, " %X ", sym_table.pstvr[record_counter].status_field);	/*Print hex value of status_field for each symbol*/
		fprintf(fp, "%d ", strlen(sym_table.pstvr[record_counter].plex));   /*Print the length of lexeme*/
		fprintf(fp, "%s ", sym_table.pstvr[record_counter].plex);
		fprintf(fp, "%d ", sym_table.pstvr[record_counter].o_line);

		/*Print corresponding int_value by checking type of i_value*/
		type = st_get_type(sym_table, record_counter);
		switch (type){
		case 'I': case 'S':
			fprintf(fp, "%d", sym_table.pstvr[record_counter].i_value, type);
			break;

		case 'F':
			fprintf(fp, "%.2f", sym_table.pstvr[record_counter].i_value);
			break;
		}
	}
	printf("\nSymbol Table stored.\n");
	/* Close the file */
	fclose(fp);
	return (record_counter + 1);
}
/* ************************************************************************
* Purpose:			This function is used to sort the symbol table in different
					order based on argument of function; s_order
* Author:			Charles Ambaiowei
* History/Version:	1.0
* Called Function:	qsort(), ascending_compare(), descending_compare();
* Parameters:		STD sym_table, char s_order
* Return value:		int
* Algorithm:		uses qsort()
* ***********************************************************************/
int st_sort(STD sym_table, char s_order){
	/*Check that there is an existing symbol table and that no other character appart from A or D is passed*/
	if (sym_table.st_size <= 0 || (s_order != 'A' && s_order != 'D')){
		return FAILURE;
	}
	/*Sort the symbol table(ascending('A') or descending('D'))*/
	qsort(sym_table.pstvr, sym_table.st_offset, sizeof(STVR), (s_order == 'A') ? ascending:descending);
	return SUCCESS;
}
/* ************************************************************************
* Purpose:			Sort in ascending order
* Author:			Charles Ambaiowei
* History/Version:	1.0
* Called Function:	strcmp()	
* ***********************************************************************/
static int ascending(const void* first,const void* second){
	/*Compare the strings pointed by plex*/
	return strcmp(((STVR *)first)->plex, ((STVR *)second)->plex);
}
/* ************************************************************************
* Purpose:			Sort in descending order
* Author:			Patrick Rowlee
* History/Version:	1.0
* Called Function:	strcmp()
* ***********************************************************************/
static int descending(const void* first,const void* second){
	/*Compare the strings pointed by plex*/
	return strcmp(((STVR *)second)->plex, ((STVR *)first)->plex);
}
