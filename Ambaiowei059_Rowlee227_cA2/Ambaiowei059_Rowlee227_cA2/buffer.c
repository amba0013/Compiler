/**********************************************************************************************
Filename:           buffer.c
Compiler:           Visual Studio Premium 2013
Author:             Ambaiowei Charles, 040753059
Course:             CST8152-Compilers
Lab Section:		312
Assignment:         Assignment 1 - The Buffer
Date:               September 29, 2016
Professor:          Svillen Ranev
Purpose:            Programming and Using Dynamic Structures (buffers) with C
Functions list:     b_create(), b_addc(), b_reset(), b_destroy(), b_isfull(), b_size(),
					b_capacity(), b_setmark(), b_mark(), b_mode(), b_inc_factor(), b_load(),
					b_isempty(), b_eob(), b_getc(), b_print(), b_pack(), b_rflag(),
					b_retract(), b_retract_to_mark(), b_getcoffset(),b_cbhead()
*********************************************************************************************/
#include "buffer.h"

/********************************************************************************************
Purpose:            To create and to initialize the Buffer and its members respectively
Author:             Ambaiowei Charles
Called Functions:   calloc(), malloc()
Parameters:         init_capacity,inc_factor,o_mode
Return Value(s):    Pointer to a Buffer structure or NULL
Algorithm:          Check the validity of the input,allocate memory for the Buffer
					and the array of characters. If memory cannot be allocated NULL is
					returned,otherwise it returns a pointer to the buffer.
					NB : All functions return NULL.
*********************************************************************************************/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode){
		/*Used to allocate memory for the buffer structure and the cb_head array*/
		pBuffer buffer;
		/*Test for invalid Parameters*/
		if (init_capacity < 0){
			return NULL;
		}
		switch (o_mode) {
		case 'f':
			if (init_capacity == 0){
				return NULL;
			}
			break;
		case 'a':
			if ((unsigned char)inc_factor < 0 || (unsigned char)inc_factor > ADDITIVE_MAX){
				return NULL;
			}
			break;
		case 'm':
			if (inc_factor < 0 || inc_factor > MULTIPLICATIVE_MAX){
				return NULL;
			}
			break;
		default:
			return NULL;
		}
		buffer = (pBuffer)calloc(1, sizeof(Buffer));
	
		if (buffer == NULL){
			return NULL;
		}
		buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));
		
		if (buffer->cb_head == NULL){
			free(buffer);
			return NULL;
		}
		/*If you have either mode 'f' or increment factor 0 you are in FIXED MODE*/
		if (o_mode == 'f' || (unsigned char)inc_factor == 0){
			buffer->inc_factor = 0;
			buffer->mode = FIXED_MODE;
		}
		/*If you have mode 'a' and an increment factor between 1 and 255 you are in ADDITIVE MODE*/
		else if (o_mode == 'a' && ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= ADDITIVE_MAX)){
			buffer->inc_factor = inc_factor;
			buffer->mode = ADDITIVE_MODE;
		}
		/*If you have mode 'm' and an increment factor between 1 and 100 you are in MULTIPLICATIVE MODE*/
		else if (o_mode == 'm' && ((unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= MULTIPLICATIVE_MAX)){
			buffer->inc_factor = inc_factor;
			buffer->mode = MULTIPLICATIVE_MODE;
		}
		else{
			/*if any errors*/
			free(buffer->cb_head);
			free(buffer);
			return NULL;
		}
		buffer->capacity = init_capacity;

		return buffer;
	}

/********************************************************************************************
Purpose:            Add a character on the buffer,depending on the buffer mode it will be resized.
Author:             Ambaiowei Charles
Called Functions:   b_isfull()
Parameters:         pBuffer const
Return Value(s):    Pointer to a Buffer structure, or NULL
Algorithm:          Check the validity of the input. When buffer is full:
*********************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol){
	
	short new_capacity = 0;
	short available_space;
	double new_increment;
	char * temporary_pointer;

	/*Make sure that the buffer and the array containing the chars are valid and check every possible bad parameter*/
	if ((pBD == NULL) || (pBD->cb_head == NULL) ||
		(pBD->addc_offset>pBD->capacity) ||
		(pBD->addc_offset<0) ||
		(pBD->mode != ADDITIVE_MODE && pBD->mode != MULTIPLICATIVE_MODE && pBD->mode != FIXED_MODE) ||
		(pBD->mode == FIXED_MODE && pBD->capacity == pBD->addc_offset) ||
		(pBD->mode == ADDITIVE_MODE && (pBD->capacity + ((unsigned char)pBD->inc_factor*sizeof(char)))<0) ||
		((pBD->mode == MULTIPLICATIVE_MODE) && (pBD->capacity == MAXIMUM_BUFFER_SIZE) && (pBD->capacity == pBD->addc_offset)))
	{
		return NULL;
	}
	/*Reset the reallocation flag*/
	pBD->r_flag = SET_DEFAULT;
	
	if (b_isfull(pBD)){
		/*the new_capacity is equal to to the current capacity plus the increment factor in bytes(Additive Mode)*/
		if (pBD->mode == ADDITIVE_MODE)
			new_capacity = pBD->capacity + ((unsigned char)pBD->inc_factor*sizeof(char));
		/*expand the buffer in multiplicative mode using the provided formula*/
		else if (pBD->mode == MULTIPLICATIVE_MODE){
			available_space = MAXIMUM_BUFFER_SIZE - pBD->capacity;
			/*new_increment is a double datatype to take care of overflow a*b*/
			new_increment = (available_space * pBD->inc_factor) / 100;
			new_capacity = pBD->capacity + (short)new_increment;
			/*If the new capacity is not bigger than current capacity then the new capacity
			is equal to the MAXIMUM_BUFFER_SIZE*/
			(new_capacity <= pBD->capacity) ? (new_capacity = MAXIMUM_BUFFER_SIZE) : (new_capacity);
		}
		/*Try to reallocate memory for the new buffer*/
		temporary_pointer = (char*)realloc(pBD->cb_head, new_capacity*sizeof(char));
		if (temporary_pointer == NULL)
			return NULL;
		/*If the temporary pointer is different from cb_head addres then set the reallocation flag*/
		if (temporary_pointer != pBD->cb_head){
			pBD->r_flag = SET_R_FLAG;
			pBD->cb_head = temporary_pointer;
		}
		pBD->capacity = new_capacity;
	}

	pBD->cb_head[pBD->addc_offset] = symbol;
	/*Increment the offset*/
	pBD->addc_offset += INC_ADDCOFFSET;
	return pBD;
	
}


/********************************************************************************************
Purpose:            Resets the buffer such that it appears empty without freeing any memory
Author:             Ambaiowei Charles
Called Functions:   conditional - Next call to b_addc()
Parameters:         Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    An Int
Algorithm:          Taking care of any run-time error and then resetting the needed
					members in the buffer structure.
*********************************************************************************************/
int b_reset(Buffer *const pBD){
	
	if (pBD == NULL ){
		return R_FAIL1;
	}
	/*Reset all the offsets to default*/
	pBD->addc_offset = SET_DEFAULT;
	pBD->mark_offset = SET_DEFAULT;
	pBD->getc_offset = SET_DEFAULT;
	pBD->r_flag = SET_DEFAULT;
	pBD->eob = SET_DEFAULT;
	return R_SUCCESS1;
}
/********************************************************************************************
Purpose:            To free the memory previously allocated for the Buffer.
Author:             Ambaiowei Charles
Called Functions:   free()
Parameters:         Data Type: Buffer * const
Return Value(s):    None
Algorithm:          Free the pointers
*********************************************************************************************/
void b_free(Buffer * const pBD){
	if (pBD != NULL ){
		free(pBD->cb_head);
		free(pBD);
	}
}
/********************************************************************************************
Purpose:            check if the buffer is full
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Buffer * const
Value:			    A pointer to a Buffer structure
Return Value(s):    An integer. 1(full buffer); 0(non-full buffer); -1(run-time error)
*********************************************************************************************/
int b_isfull(Buffer * const pBD){
	return(pBD == NULL) ? R_FAIL1 :
		((unsigned short)pBD->capacity == (pBD->addc_offset*sizeof(char))) ? BUFFER_FULL : BUFFER_NOT_FULL;
}
/********************************************************************************************
Purpose:            To return the size of the buffer
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    A Short. Size of the buffer; -1(run-time error)
*********************************************************************************************/
short b_size(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->addc_offset;
}
/********************************************************************************************
Purpose:            To return the capacity of the buffer
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:			Buffer * const
Value:			    A pointer to a Buffer structure
Return Value(s):    A Short. Capacity of the buffer; -1(run-time error)
*********************************************************************************************/
short b_capacity(Buffer*const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->capacity;
}
/********************************************************************************************
Purpose:            To return the address of the character where the mark was done.
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Value:				A pointer to a Buffer structure
Data Type:			short
Return Value(s):    The address of the character in the array in the 'mark position'
Algorithm:			Check that the parameters being passed are valid. 
*********************************************************************************************/
short b_setmark(Buffer*const pBD, short mark){
		if (pBD == NULL || pBD->cb_head == NULL ||
			/*Check that the mark is within valid values*/
			mark <= 0 || mark > pBD->addc_offset){
			return R_FAIL1;
		}
		else{
			return pBD->mark_offset = mark;
		}
	}
/********************************************************************************************
Purpose:            To return the mark offset
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value: A pointer to a Buffer structure
Return Value(s):    A Short. Mark Offset; -1(run-time error)
*********************************************************************************************/
short b_mark(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->mark_offset;
}
/********************************************************************************************
Purpose:            To return the mode of the buffer
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value: A pointer to a Buffer structure
Return Value(s):    An integer.Buffer Mode; -1(run-time error)
*********************************************************************************************/
int b_mode(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->mode;
}
/********************************************************************************************
Purpose:            To return the increment factor of the buffer
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Value: A pointer to a Buffer structure
Return Value(s):    size_t. Increment factor; 256(run-time error)
*********************************************************************************************/
size_t b_incfactor(Buffer* const pBD){
	return(pBD == NULL || pBD->cb_head == NULL) ? INC_FACTOR_FAIL : (size_t)(unsigned char)pBD->inc_factor;
}

/********************************************************************************************
Purpose:            To load the characters from a file to the buffer
Author:             Ambaiowei Charles
Called Functions:   fgetc();feof();b_addc()
Parameters:         Data Type: Buffer * const
Name:				pBD
Value:				A pointer to a Buffer structure
Data Type:			FILE * const
Name:				fi
Value:				Address to a file
Return Value(s):    An integer. 
Algorithm:			Check that the parameters being passed are valid. If not return NULL.
					If they are loop through the file and use b_addc to add each character
					to the buffer until EOF is found.
*********************************************************************************************/
int b_load(FILE*const fi, Buffer*const pBD){

	char c_character;
	/* check that fi is pointing to a valid file*/
	if (fi == NULL || pBD == NULL || pBD->cb_head == NULL){
		return R_FAIL1;
	}
	
	c_character = (char)fgetc(fi);

	while (!feof(fi)){
		
		if (b_addc(pBD, c_character) == NULL)
			return LOAD_FAIL;
		c_character = (char)fgetc(fi);
	}

	return pBD->addc_offset;

}
/********************************************************************************************
Purpose:            check if buffer is empty
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value: A pointer to a Buffer structure
Return Value(s):    An integer. 
*********************************************************************************************/
int b_isempty(Buffer*const pBD){
	if (pBD == NULL) return R_FAIL1;
	return (pBD->addc_offset == 0) ? BUFFER_EMPTY : BUFFER_NOT_EMPTY;
}
/********************************************************************************************
Purpose:            check if buffer has reached its end.
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    An integer.
*********************************************************************************************/
int b_eob(Buffer*const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->eob;
}
/********************************************************************************************
Purpose:            return current character in the array
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value: A pointer to a Buffer structure
Return Value(s):    A char. The character itself; -2(run-time error); -1(if the end of buffer
has been reached).
Algorithm:			Check that the parameters being passed are valid. If not return -2.
If they are check if the buffer is full; if it is set eob to 1;if not
return the char at getc_offset(before increment getc_offset).
*********************************************************************************************/
char b_getc(Buffer * const pBD){
	/*Make sure that the buffer and the array containing the chars are valid*/
	if (pBD == NULL){
		return R_FAIL2;
	}
	/*Check if you have reached the maximum amount of characters in the buffer*/
	if (pBD->getc_offset == pBD->addc_offset){
		pBD->eob = EOB_REACHED;
		return R_FAIL1;
	}
	else{
		pBD->eob = SET_DEFAULT;
		/*return the character and then increment the getc_offset by 1*/
		return pBD->cb_head[pBD->getc_offset++];
	}
}
/********************************************************************************************
Purpose:            print the contents of the buffer
Author:             Ambaiowei Charles
Called Functions:   b_isempty();b_getc();b_eob()
Parameters:         Data Type: Buffer * const
Name: pBD
Value: A pointer to a Buffer structure
Return Value(s):    An integer. The amount of characters in the buffer.-1(run-time error)
Algorithm:			Check that the parameters being passed are valid. If not return -1.
If the buffer is empty print a prompt notifying the user. Otherwise
reset the get_c_offset and loop through the buffer printing each
character until the end of buffer is reached.
*********************************************************************************************/
int b_print(Buffer *const pBD){
	
	char c_character;
	
	if (pBD == NULL || pBD->cb_head == NULL){
		return R_FAIL1;
	}
	
	if (b_isempty(pBD)){
		printf("The buffer is empty.");
	}
	/*Reset the getc_offset*/
	pBD->getc_offset = SET_DEFAULT;
	/*Get the first character in the buffer(which resets eob)*/
	c_character = b_getc(pBD);
	/*Loop until end of buffer is reached*/
	while (!b_eob(pBD)){
		/*Print the character*/
		printf("%c", c_character);
		c_character = b_getc(pBD);
	}
	pBD->getc_offset = SET_DEFAULT;
	printf("\n");
	return pBD->addc_offset;
}
/********************************************************************************************
Purpose:            pack the buffer and leave an extra space for the EOF
Author:             Ambaiowei Charles
Called Functions:   realloc()
Parameters:         Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    An pointer to a Buffer structure. NULL
Algorithm:			Check that the parameters being passed are valid. 
*********************************************************************************************/
Buffer *b_pack(Buffer*const pBD){
	
	char * temporary_pointer;
	/*Make sure that the buffer and the array containing the chars are valid*/
	if (pBD == NULL || pBD->cb_head == NULL ||
		(pBD->capacity == MAXIMUM_BUFFER_SIZE && b_isfull(pBD))){
		return NULL;
	}
	/*Allocate memory to pack the buffer(amount of characters in plus 1)*/
	temporary_pointer = (char*)realloc(pBD->cb_head, ((pBD->addc_offset + INC_ADDCOFFSET)*sizeof(char)));
	if (temporary_pointer == NULL){
		return NULL;
	}
	/*Set the reallocation flag to 1 if the addres of the array of characters has changed*/
	if (temporary_pointer != pBD->cb_head){
		pBD->r_flag = SET_R_FLAG;
		pBD->cb_head = temporary_pointer;
	}


	pBD->capacity = ((pBD->addc_offset + INC_ADDCOFFSET) *sizeof(char));

	return pBD;
}
/********************************************************************************************
Purpose:            To return the value of the rellocation flag.
Author:				Ambaiowei Charles
Called Functions:   None
Parameters:         Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    A char 
*********************************************************************************************/
char b_rflag(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1; 
	return pBD->r_flag;
}
/********************************************************************************************
Purpose:            To retract the getc_offset by one
Author:             Ambaiowei charles
Parameters:         Data Type: Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):    A short. 
*********************************************************************************************/
short b_retract(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->getc_offset -= 1;
}
/********************************************************************************************
Purpose:            to set getc_offset to the mark_offset
Author:             Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value:			    A pointer to a Buffer structure
Return Value(s):    A short. getc_offset; -1(run-time error)
*********************************************************************************************/
short b_retract_to_mark(Buffer* const pBD){
	if (pBD == NULL) return R_FAIL1;
	return(pBD->getc_offset = pBD->mark_offset);
}
/********************************************************************************************
Purpose:            to return the current get_c_offset
Author:				Ambaiowei Charles
Called Functions:   None
Parameters:         Data Type: Buffer * const
Name: pBD
Value:				A pointer to a Buffer structure
Return Value(s):    A short. getc_offset; -1(run-time error)
*********************************************************************************************/
short b_getcoffset(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;
	return pBD->getc_offset;
}
/********************************************************************************************
Purpose:			to return pointer stored in cb_head
Author:				Ambaiowei Charles
Called Functions:   None
Parameters:         Buffer * const
Value:				A pointer to a Buffer structure
Return Value(s):	A Char.
*********************************************************************************************/
char * b_cbhead(Buffer * const pBD){
	if (pBD->cb_head == NULL){
		return NULL;
	}
	return(pBD->cb_head);
}