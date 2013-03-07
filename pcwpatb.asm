; PCWPATB.ASM
;
; SOFTWARE: ED, MAC, SID
; HARDWARE: Amstrad PCW8256, CP/M Plus v1.4
; (The only hardware specific part is the
;  SID restart point, at label SID:)
;
; Palo Alto Tiny BASIC Interpreter Version 3.0
;
; See "Dr. DOBB's Journal" Vol.1 No.1 to 5.
; My advice is to buy the Volume 1 of DDJ:
; M&T Publishing Inc.
; 501 Galveston Drive
; REDWOOD CITY
; CA 94063
; USA
; ---
;
; CP/M port by Emmanuel ROCHE in mid-JUNE 1990!
; (Better late than never...)
;
; WARNING: run ONLY under SID.
;
;Usage: A>SID PCWPATB.HEX PCWPATB.SYM
; CP/M 3 SID - Version 3.0
; SYMBOLS
; NEXT MSZE  PC  END
; C834 C834 0100 D4C5
; #G100
;
; PALO ALTO TINY BASIC V3.0
; OK
; >
;
;Type Palo Alto Tiny BASIC commands in upper cases.
;
; >SID<CR> will return you to SID (added by ROCHE).
;
;-------------------------------
;
	CPU	8080

CR	EQU	0DH
LF	EQU	0AH
;
;-------------------------------
;
tstc MACRO OP1,OP2
	CALL	TSTCH
	DB 	OP1
 	DB 	(OP2 - $ - 1) & 0FFH
 	ENDM
;
;-------------------------------
;
item MACRO OP1,OP2
	IF OP1 == ""
  		DB	OP2 >> 8 | 080H
  		DB  	OP2 & 0FFH
 	ELSE
  		DB	OP1
  		DB	OP2 >> 8 | 080H
  		DB  	OP2 & 0FFH
 	ENDIF
	ENDM
;
;The following is the original code (slighty edited for CP/M).
;---------------------------------------------------------------
;
;        P A T B
;    PALO ALTO TINY BASIC INTERPRETER
;       VERSION 3.0
;     FOR 8080 SYSTEM
;       LI-CHEN WANG
;      26 APRIL, 1977
;
;---------------------------------------------------------------
;
;  *** MEMORY USAGE ***
;
;  0080-01FF are for variables, input line and stack
;  2000-3FFF are for Tiny BASIC text & array
;  F000-F7FF are for PATB code
;
;ROCHE> I have added an offset of 1000H, and set BOTROM at
;ROCHE> C000H, in order to be under PCWPATB.SYM under SID.
;RUNNER> BOTROM > 7FFFH
;
BOTROM 	EQU 	01000H  	;BOTtom Read Only Memory
BOTSCR 	EQU 	02000H  	;BOTtom SCRatch
TOPSCR 	EQU 	02100H  	;TOP SCRatch
BOTRAM 	EQU 	02100H  	;BOTtom Random Access Memory
DFTLMT 	EQU 	02FFFH  	;DeFaulT LiMiT

;
;---------------------------------------------------------------
;
; *** INITIALIZE
;
 	ORG 	0  		;ROCHE>
;
 	JMP 	INIT  		;ROCHE>
;
 	ORG 	BOTROM
;
INIT: 	LXI 	SP, STACK
 	CALL 	CRLF
 	LXI 	H, KEYWRD 	;at power-on, KEYWRD is
 	MVI 	A, 0C3H  	;probably not 0C3H
 	CMP 	M
 	JZ 	TELL  		;it is 0C3H, continue
 	MOV 	M, A  		;no, set it to 0C3H
 	LXI 	H, DFTLMT 	;and set default value
 	SHLD 	TXTLMT  	;in 'TXTLMT'
# 	MVI 	A, HIGH BOTROM 	;initialize RANPNT
	MVI	A, (BOTROM >> 8)
 	STA 	RANPNT+1
PURGE:	LXI H, 	TEXT+4 	;purge text area
 	SHLD 	TXTUNF
 	MVI 	H, 0FFH
 	SHLD 	TEXT
TELL: 	LXI 	D, MSG  	;tell user
 	CALL 	PRTSTG  	;*************************
 	JMP 	RSTART  	;***** jmp user init *****
    				;*************************
;-------------------------------
SID: 	RST 6  			;ROCHE> because Amstrad PCW
    				;uses Mode 1 Interrupts
;-------------------------------
MSG: 	DB 	"PALO "
 	DB 	"ALTO "
 	DB 	"TINY "
 	DB 	"BASIC"
 	DB 	" V3.0"
 	DB 	CR
;
OK: 	DB 	"OK"
 	DB 	CR
;
WHAT: 	DB 	"WHAT?"
 	DB 	CR
;
HOW: 	DB 	"HOW?"
 	DB 	CR
;
SORRY:	DB 	"SORRY"
 	DB 	CR
;
;---------------------------------------------------------------
;
; *** DIRECT COMMAND / TEXT COLLECTER ***
;
; PATB prints out "OK(CR)", and then it prompts ">" and reads
; a line. If the line starts with a non-zero number, this
; number is the line number. The line number (in 16 bit
; binary) and the rest of the line (including CR) is stored
; in the memory. If a line with the same line number is already
; there, it is replaced by the new one. If the rest of the line
; consists of a CR only, it is not stored and any existing line
; with the same line number is deleted.
;
; After a line is inserted, replaced, or deleted, the program
; loops back and ask for another line. This loop will be
; terminated when it reads a line with zero or no line number;
; and control is transfered to "DIRECT".
;
; Tiny BASIC program save area starts at the memory location
; labeled "TEXT". The end of text is marked by 2 bytes XX FF.
; Following these are 2 bytes reserved for the array element
; @(0). The content of location labeled "TXTUNF" points to one
; after @(0).
;
; The memory location "CURRNT" points to the line number that
; is currently being interpreted. While we are in this loop
; or while we are interpreting a direct command (see next
; section), "CURRNT" should point to a 0.
;
RSTART:
	LXI 	SP, STACK 	;re-initialize stack
 	LXI 	H, ST1+1 	;literal 0
 	SHLD 	CURRNT  	;CURRNT->line # = 0
ST1: 	LXI 	H, 0
 	SHLD 	LOPVAR
 	SHLD 	STKGOS
 	LXI 	D, OK  		;DE->string
 	CALL 	PRTSTG  	;print string until CR
ST2: 	MVI 	A, '>'  		;prompt '>' and
 	CALL 	GETLN  		;read a line
 	PUSH 	D  		;DE->end of line
 	LXI 	D, BUFFER 	;DE->beginning of line
 	CALL 	TSTNUM  	;test if it is a number
 	CALL 	IGNBLK
 	MOV 	A, H  		;HL=value of the # or
 	ORA 	L  		;0 if no # was found
 	POP 	B  		;BC->end of line
 	JZ 	DIRECT
 	DCX 	D  		;backup DE and save
 	MOV 	A, H  		;value of line # there
 	STAX 	D
 	DCX 	D
 	MOV 	A, L
 	STAX 	D
 	PUSH 	B  		;BC, DE -> begin, end
 	PUSH 	D
 	MOV 	A, C
 	SUB 	E
 	PUSH 	PSW  		;A=# of bytes in line
 	CALL 	FNDLN  		;find this line in save
 	PUSH 	D  		;area, DE->save area
 	JNZ 	ST3  		;NZ=not found, insert
 	PUSH 	D  		;Z=found, delete it
 	CALL 	FNDNXT  	;set DE->next line
 	POP 	B  		;BC->line to be deleted
 	LHLD 	TXTUNF  	;HL->unfilled save area
 	CALL 	MVUP  		;move up to delete
 	MOV 	H, B  		;TXTUNF->unfilled area
 	MOV 	L, C
 	SHLD 	TXTUNF  	;update
ST3: 	POP 	B  		;get ready to insert
 	LHLD 	TXTUNF  	;but first check if
 	POP 	PSW  		;the length of new line
 	PUSH 	H  		;is 3 (line # and CR)
 	CPI 	3  		;then do not insert
 	JZ 	RSTART  	;must clear the stack
 	ADD 	L  		;compute new TXTUNF
 	MOV 	E, A
 	MVI 	A, 0
 	ADC 	H
 	MOV 	D, A  		;DE->new unfilled area
 	LHLD 	TXTLMT  	;check to see if there
 	XCHG
 	CALL 	COMP  		;is enough space
 	JNC 	QSORRY 	;sorry, no room for it
 	SHLD 	TXTUNF  	;ok, update TXTUNF
 	POP 	D  		;DE->old unfilled area
 	CALL 	MVDOWN
 	POP 	D  		;DE->begin, HL->end
 	POP 	H
 	CALL 	MVUP  		;move new line to
 	JMP 	ST2  		;save area
;
;---------------------------------------------------------------
;
; *** DIRECT *** & EXEC ***
;
; This section of the code tests a string against a table.
; When a match is found, control is transfered to the section
; of code according to the table.
;
; At 'EXEC', DE should point to the string and HL should point
; to the table-1. At 'DIRECT', DE should point to the string,
; HL will be set up to point to tab1-1, which is the table of
; all direct and statement commands.
;
; A '.' in the string will terminate the test and the partial
; match will be considered as a match, e.g., 'P.', 'PR.',
; 'PRI.', 'PRIN.' or 'PRINT' will all match 'PRINT'.
;
; The table consists of any number of items. Each item is a
; string of characters with bit 7 set to 0 and a jump address
; stored hi-low with bit 7 of the high byte set to 1.
;
; End of table is an item with a jump address only. If the
; string does not match any of the other items, it will match
; this null item as default.
;
DIRECT:	LXI 	H, TAB1-1 ;*** DIRECT ***
;
EXEC: 	CALL 	IGNBLK		;*** EXEC ***
 	PUSH 	D  		;save pointer
EX1: 	LDAX 	D  		;if found '.' in string
 	INX 	D  		;before any mismatch
 	CPI 	'.'  		;we declare a match
 	JZ 	EX3
 	INX 	H  		;HL->table
 	CMP 	M  		;if match, test next
 	JZ 	EX1
 	MVI 	A, 07FH	 	;else, see if bit 7
 	DCX 	D  		;of table is set, which
 	CMP 	M  		;is the jump address (HIGH)
 	JC 	EX5  		;C=yes, matched
EX2: 	INX 	H  		;NC=no, find jump address
 	CMP 	M
 	JNC 	EX2
 	INX 	H  		;bump to next table item
 	POP 	D  		;restore string pointer
 	JMP 	EXEC  		;test again next item
EX3: 	MVI 	A, 07FH	 	;partial match, find
EX4: 	INX 	H  		;jump address, which is
 	CMP 	M  		;flagged by bit 7
 	JNC 	EX4
EX5: 	MOV 	A, M  		;load HL with the jump
 	INX 	H  		;address from the table
 	MOV 	L, M  		;****************
 	ANI 	07FH  		;*** ANI 07FH ***
 	MOV 	H, A  		;****************
 	POP 	PSW  		;clean up the garbage
 	PCHL   			;and we go do it
;
;---------------------------------------------------------------
;
; What follows is the code to execute direct and statement
; commands. Control is transfered to these points via the
; command table lookup code of 'DIRECT' and 'EXEC' in last
; section. After the command is executed, control is transfered
; to other sections as follows:
;
; For 'LIST', 'NEW', and 'STOP': go back to 'RSTART'.
; For 'RUN': go execute the first stored line if any;
;      else go back to 'RSTART'.
; For 'GOTO' and 'GOSUB': go execute the target line.
; For 'RETURN' and 'NEXT': go back to saved return line.
; For all others: if 'CURRNT' -> 0, go to 'RSTART',
;     else go execute next command. (This is done in 'FINISH'.)
;
;---------------------------------------------------------------
;
; *** NEW *** STOP *** RUN (& friends) *** & GOTO ***
;
; 'NEW(CR)' resets 'TXTUNF'.
;
; 'STOP(CR)' goes back to 'RSTART'.
;
; 'RUN(CR)' finds the first stored line, store its address
; (in 'CURRNT'), and start execute it. Note that only those
; commands in TAB2 are legal for stored program.
;
; There are 3 more entries in 'RUN':
; 'RUNNXL' finds next line, stores its address and executes it.
; 'RUNTSL' stores the address of this line and execute it.
; 'RUNSML' continues the execution on same line.
;
; 'GOTO expr(CR)' evaluates the expression, find the target
; line, and jump to 'RUNTSL' to do it.
;
NEW: 	CALL 	ENDCHK  	;*** NEW(CR) ***
 	JMP 	PURGE
;
STOP: 	CALL 	ENDCHK  	;*** STOP(CR) ***
 	JMP 	RSTART
;
RUN: 	CALL 	ENDCHK  	;*** RUN(CR) ***
 	LXI 	D, TEXT	 	;first saved line
;
RUNNXL: LXI 	H, 0  		;*** RUNNXL ***
 	CALL 	FNDLP  		;find whatever line #
 	JC 	RSTART  	;C=passed TXTUNF, quit
;
RUNTSL: XCHG   		;*** RUNTSL ***
 	SHLD 	CURRNT  	;set 'CURRNT'->line #
 	XCHG
 	INX 	D  		;bump pass line #
 	INX 	D
;
RUNSML: CALL 	CHKIO		;*** RUNSML ***
 	LXI 	H, TAB2-1 	;find command in TAB2
 	JMP 	EXEC  		;and execute it
;
GOTO: 	CALL 	EXPR  		;*** GOTO expr ***
 	PUSH 	D  		;save for error routine
 	CALL 	ENDCHK  	;must find a CR
 	CALL 	FNDLN  		;find the target line
 	JNZ 	AHOW  		;no such line #
 	POP 	PSW  		;clear the "PUSH DE"
 	JMP 	RUNTSL  	;go do it
;
;---------------------------------------------------------------
;
; *** LIST *** & PRINT ***
;
; LIST has three forms:
; 'LIST(CR)' lists all saved lines.
; 'LIST n(CR)' start list at line n.
; 'LIST n1, n2(CR)' start list at line n1 for n2 lines.
; (You can stop the listing by Control-C key.)
;
; PRINT command is 'PRINT .....;' or 'PRINT ....(CR)'
; where '...' is a list of expressions, formats, and/or strings.
; These items are separated by commas.
;
; A format is a number sign followed by a number. It controls
; the number of spaces the value of a expression is going
; to be printed. It stays effective for the rest of the print
; command unless changed by another format. If no format is
; specified, 8 positions will be used.
;
; A string is quoted in a pair of single quotes or a pair of
; double quotes.
;
; Control characters and lower case letters can be included
; inside the quotes. Another (better) way of generating control
; characters on the output is use the up-arrow character
; followed by a letter.  L means FF,  I means HT,
;  G means BELL, etc.
;
; A (CRLF) is generated after the entire list has been printed
; or if the list is a null list. Howewer if the list ended with
; a comma, no (CRLF) is generated.
;
LIST: 	CALL 	TSTNUM  	;test if there is a #
 	PUSH 	H
 	LXI 	H, 0FFFFH
 	tstc 	',', ls1
 	CALL 	TSTNUM
LS1: 	XTHL
 	CALL 	ENDCHK  	;if no #, we get a 0
 	CALL 	FNDLN		;find this or next line
LS2: 	JC 	RSTART  	;C=passed TXTUNF
 	XTHL
 	MOV 	A, H
 	ORA 	L
 	JZ 	RSTART
 	DCX 	H
 	XTHL
 	CALL 	PRTLN		;print the line
 	CALL 	PRTSTG
 	CALL 	CHKIO
 	CALL 	FNDLP  		;find next line
 	JMP 	LS2  		;and loop back
;
PRINT: 	MVI 	C, 8  		;C=# of spaces
 	tstc 	';', PR1 		;if null list & ";"
 	CALL 	CRLF  		;give CR-LF and
 	JMP 	RUNSML  	;continue same line
PR1: 	tstc 	CR, PR6  	;if null list (CR)
 	CALL 	CRLF  		;also give CR-LF and
 	JMP 	RUNNXL  	;go to next line
PR2: 	tstc 	'#', PR4 		;else, is it format?
PR3: 	CALL 	EXPR  		;yes, evaluate expr.
 	MVI 	A, 0C0H
 	ANA 	L
 	ORA 	H
 	JNZ 	QHOW
 	MOV 	C, L  		;and save it in C
 	JMP 	PR5  		;look for more to print
PR4: 	CALL 	QTSTG 		;or is it a string?
 	JMP 	PR9  		;if not, must be expr.
PR5: 	tstc 	',', PR8 		;if ",", go find next
PR6: 	tstc 	',', PR7
 	MVI 	A, ' '
 	CALL 	OUTCH
 	JMP 	PR6
PR7: 	CALL 	FIN  		;in the list
 	JMP 	PR2  		;list continues
PR8: 	CALL 	CRLF  		;list ends
 	JMP 	FINISH
PR9: 	CALL 	EXPR  		;evaluate the expr
 	PUSH 	B
 	CALL 	PRTNUM  	;print the value
 	POP 	B
 	JMP 	PR5  		;more to print?
;
;---------------------------------------------------------------
;
; *** GOSUB *** & RETURN ***
;
; 'GOSUB expr;' or 'GOSUB expr (CR)' is like the 'GOTO' command,
; except that the current text pointer, stack pointer etc. are
; save so that execution can be continued after the subroutine
; 'RETURN'. In order that 'GOSUB' can be nested (and even
; recursive), the save area must be stacked. The stack pointer
; is saved in 'STKGOS'. The old 'STKGOS' is saved in the stack.
; If we are in the main routine, 'STKGOS' is zero (this was done
; by the "main" section of the code), but we still save it as
; a flag for no further 'RETURN's.
;
; 'RETURN(CR)' undos everything that 'GOSUB' did, and thus
; return the execution to the command after the most recent
; 'GOSUB'. If 'STKGOS' is zero, it indicates that we never
; had a 'GOSUB' and is thus an error.
;
GOSUB: CALL 	PUSHA  	;save the current "FOR"
 	CALL 	EXPR  		;parameters
 	PUSH 	D  		;and text pointer
 	CALL 	FNDLN  		;find the target line
 	JNZ 	AHOW  		;not there, say "HOW?"
 	LHLD 	CURRNT  	;save old
 	PUSH 	H  		;'CURRNT' old 'STKGOS'
 	LHLD 	STKGOS
 	PUSH 	H
 	LXI 	H, 0  		;and load new ones
 	SHLD 	LOPVAR
 	DAD 	SP
 	SHLD 	STKGOS
 	JMP 	RUNTSL  	;then run that line
;
RETURN: CALL 	ENDCHK  	;there must be a CR
 	LHLD 	STKGOS  	;old stack pointer
 	MOV 	A, H  		;0 means not exist
 	ORA 	L
 	JZ 	QWHAT  	;so, we say: "WHAT?"
 	SPHL   			;else, restore it
RESTOR: POP 	H
 	SHLD 	STKGOS  	;and the old 'STKGOS'
 	POP 	H
 	SHLD 	CURRNT  	;and the old 'CURRNT'
 	POP 	D  		;old text pointer
 	CALL 	POPA  		;old "FOR" parameters
 	JMP 	FINISH
;
;---------------------------------------------------------------
;
; *** FOR *** & NEXT ***
;
; 'FOR' has two forms: 'FOR VAR=EXP1 TO EXP2 STEP EXP3' and
; 'FOR VAR=EXP1 TO EXP2' the second form means the same thing
; as the first form with EXP3=1 (i.e., with a step of +1).
; PATB will find the variable var. and set its value to the
; current value of EXP1. It also evaluates EXP2 and EXP3 and
; save all these together with the text pointer etc. in the
; 'FOR' save area, which consists of 'LOPVAR', 'LOPINC',
; 'LOPLMT', 'LOPLN', and 'LOPPT'. If there is already some-
; thing in the save area (this is indicated by a non-zero
; 'LOPVAR'), then the old save area is saved in the stack
; before the new one overwrites it. PATB will then dig in the
; stack and find out if this same variable was used in another
; currently active 'FOR' loop. If that is the case, then the
; old 'FOR' loop is deactivated (Purged from the stack).
;
; 'NEXT var' serves as the logical (not necessarilly physical)
; end of the 'FOR' loop. The control variable var. is checked
; with the 'LOPVAR'. If they are not the same, PATB digs in the
; stack to find the right one and purges all those that did not
; match. Either way, PATB then adds the 'STEP' to that variable
; and check the result with the limit. If it is within the limit,
; control loops back to the command following the 'FOR'.
; If outside the limit, the save area is purged and execution
; continues.
;
FOR: 	CALL 	PUSHA  	;save the old save area
 	CALL 	SETVAL  	;set the control var.
 	DCX 	H  		;HL is its address
 	SHLD 	LOPVAR  	;save that
 	LXI 	H, TAB4-1 	;use 'EXEC' to look
 	JMP 	EXEC  		;for the word 'TO'
FR1: 	CALL 	EXPR  		;evaluate the limit
 	SHLD 	LOPLMT  	;save that
 	LXI 	H, TAB5-1 	;use 'EXEC' to look
 	JMP 	EXEC  		;for the word 'STEP'
FR2: 	CALL 	EXPR  		;found it, get step
 	JMP 	FR4
FR3: 	LXI 	H, 1  		;not found, set to 1
FR4: 	SHLD 	LOPINC  	;save that too
 	LHLD 	CURRNT  	;save current line #
 	SHLD 	LOPLN
 	XCHG  			;and text pointer
 	SHLD 	LOPPT
 	LXI 	B, 10  		;dig into stack to
 	LHLD 	LOPVAR  	;find 'LOPVAR'
 	XCHG
 	MOV 	H, B
 	MOV 	L, B  		;HL=0 now
 	DAD 	SP  		;here is the stack
 	JMP 	FR6
FR5: 	DAD 	B  		;each level is 10 deep
FR6: 	MOV 	A, M  		;get that old 'LOPVAR'
 	INX 	H
 	ORA 	M
 	JZ 	FR7  		;0 says no more in it
 	MOV 	A, M
 	DCX 	H
 	CMP 	D  		;same as this one?
 	JNZ 	FR5
 	MOV 	A, M  		;the other half?
 	CMP 	E
 	JNZ 	FR5
 	XCHG   			;yes, found one
 	LXI 	H, 0
 	DAD 	SP  		;try to move SP
 	MOV 	B, H
 	MOV 	C, L
 	LXI 	H, 10
 	DAD 	D
 	CALL 	MVDOWN  	;and purge 10 words
 	SPHL   			;in the stack
FR7: 	LHLD 	LOPPT  	;job done, restore DE
 	XCHG
 	JMP 	FINISH  		;and continue
;
NEXT: 	CALL 	TSTV  		;get address of var.
 	JC 	QWHAT  	;no variable, "WHAT?"
 	SHLD 	VARNXT  	;yes, save it
NX1: 	PUSH 	D  		;save text pointer
 	XCHG
 	LHLD 	LOPVAR  	;get var. in 'FOR'
 	MOV 	A, H
 	ORA 	L  		;0 says never had one
 	JZ 	AWHAT  	;so we ask: "WHAT?"
 	CALL 	COMP  		;else, we check them
 	JZ 	NX2  		;ok, they agree
 	POP 	D  		;no, let's see
 	CALL 	POPA  		;purge current loop
 	LHLD 	VARNXT  	;and pop one level
 	JMP 	NX1  		;go check again
NX2: 	MOV 	E, M  		;come here when agreed
 	INX 	H
 	MOV 	D, M  		;DE=value of var.
 	LHLD 	LOPINC
 	PUSH 	H
 	MOV 	A, H
 	XRA 	D  		;S=sign differ
 	MOV 	A, D  		;A=sign of DE
 	DAD 	D  		;add one step
 	JM 	NX3  		;cannot overflow
 	XRA 	H  		;may overflow
 	JM 	NX5  		;and it did
NX3: 	XCHG
 	LHLD 	LOPVAR  	;put it back
 	MOV 	M, E
 	INX 	H
 	MOV 	M, D
 	LHLD 	LOPLMT  	;HL=limit
 	POP 	PSW  		;old HL
 	ORA 	A
 	JP 	NX4  		;step > 0
 	XCHG   			;step < 0
NX4: 	CALL 	CKHLDE  	;compare with limit
 	POP 	D  		;restore text pointer
 	JC 	NX6  		;outside limit
 	LHLD 	LOPLN  		;within limit, so
 	SHLD 	CURRNT  	;back to the saved
 	LHLD 	LOPPT  	;'CURRNT' and text
 	XCHG   			;pointer
 	JMP 	FINISH
NX5: 	POP 	H  		;overflow, purge
 	POP 	D  		;garbage in stack
NX6: 	CALL 	POPA  		;purge this loop
 	JMP 	FINISH
;
;---------------------------------------------------------------
;
; *** REM *** IF *** INPUT *** & LET (& DEFLT) ***
;
; 'REM' can be followed by anything and is ignored by PATB.
; PATB treats it like an 'IF' with a false condition.
;
; 'IF' is followed by an expression as a condition and one or
; more commands (including other 'IF's) separated by semi-colons.
; Note that the word 'THEN' is not used. PATB evaluates the expr.
; If it is non-zero, execution continues. If the expr. is zero,
; the commands that follows are ignored and execution continues
; at the next line.
;
; 'INPUT' command is like the 'PRINT' command, and is followed
; by a list of items. If the item is a string in single or
; double quotes, or is an up-arrow, it has the same effect as
; in 'PRINT'. If an item is a variable, this variable name is
; printed out followed by a colon. Then PATB waits for an expr.
; to be typed in. The variable is then set to the value of this
; expr. If the variable is proceded by a string (again in single
; or double quotes), the string will be printed followed by a
; colon. PATB then waits for input expr. and set the variable
; to the value of the expr.
;
; If the input expression is invalid, PATB will print "WHAT?",
; "HOW?" or "SORRY" and reprint the prompt and redo the input.
; The execution will not terminate unless you type Control-C.
; This is handled in 'INPERR'.
;
; 'LET' is followed by a list of items separated by commas.
; Each item consists of a variable, an equal sign, and an expr.
; PATB evaluates the expr. and set the variable to that value.
; PATB will also handle 'LET' command without the word 'LET'.
; This is done by 'DEFLT'.
;
REM: 	LXI 	H, 0  		;*** REM ***
 	JMP 	IF1  		;this is like 'IF 0'
;
IFF: 	CALL 	EXPR  		;*** IF ***
IF1: 	MOV 	A, H  		;is the expression = 0?
 	ORA 	L
 	JNZ 	RUNSML  	;no, continue
 	CALL 	FNDSKP  	;yes, skip rest of line
 	JNC 	RUNTSL  	;and run the next line
 	JMP 	RSTART  	;if no next, re-start
;
INPERR: LHLD 	STKINP  	;*** INPERR ***
 	SPHL   			;restore old SP
 	POP 	H  		;and old 'CURRNT'
 	SHLD 	CURRNT
 	POP 	D  		;and old text pointer
 	POP 	D  		;read input
;
INPUT: 	equ	$
IP1: 	PUSH 	D  		;save in case of error
 	CALL 	QTSTG  	;is next item a string?
 	JMP 	IP8  		;no
IP2: 	CALL 	TSTV  		;yes, but followed by a
 	JC 	IP5  		;variable? no.
IP3: 	CALL 	IP12
 	LXI 	D, BUFFER 	;points to buffer
 	CALL 	EXPR  		;evaluate input
 	CALL 	ENDCHK
 	POP 	D  		;ok, get old HL
 	XCHG
 	MOV 	M, E  		;save value in var.
 	INX 	H
 	MOV 	M, D
IP4: 	POP 	H  		;get old 'CURRNT'
 	SHLD 	CURRNT
 	POP 	D  		;and old text pointer
IP5: 	POP 	PSW  		;purge junk in stack
IP6: 	tstc 	',', IP7 		;is next char. ","?
 	JMP 	INPUT  		;yes, more items.
IP7: 	JMP 	FINISH
IP8: 	PUSH 	D  		;save for 'PRTSTG'
 	CALL 	TSTV  		;must be variable now
 JNC 	IP11
IP10: 	JMP 	QWHAT  	;"WHAT?" it is not?
IP11: 	MOV 	B, E
 	POP 	D
 	CALL 	PRTCHS  	;print those as prompt
 	JMP 	IP3  		;yes, input variable
IP12: 	POP 	B  		;return address
 	PUSH 	D  		;save text pointer
 	XCHG
 	LHLD 	CURRNT  	;also save 'CURRNT'
 	PUSH 	H
 	LXI 	H, IP1  		;a negative number
 	SHLD 	CURRNT  	;as a flag
 	LXI 	H, 0  		;save SP too
 	DAD 	SP
	SHLD 	STKINP
 	PUSH 	D  		;old HL
 	MVI 	A, ' '  		;print a space
 	PUSH 	B
 	JMP 	GETLN  		;and get a line
;
DEFLT: 	LDAX 	D  		;*** DEFLT ***
 	CPI 	CR  		;empty line is ok
 	JZ 	LT4  		;else, it is 'LET'
;
LET: 	equ	$  		;*** LET ***
LT2: 	CALL 	SETVAL
LT3: 	tstc 	',', LT4 		;set value to var.
 	JMP 	LET  		;item by item
LT4: 	JMP 	FINISH  		;until finish
;
;---------------------------------------------------------------
;
; *** EXPR ***
;
; 'EXPR' evaluates arithmetical or logical expressions.
; <EXPR>::=<EXPR1>
;    <EXPR1><REL.OP.><EXPR1>
; where <REL.OP.> is one of the operators in TAB6 and the result
; of these operations is 1 if true and 0 if false.
; <EXPR1>::=(+ or -)<EXPR2>(+ or -<EXPR2>)(.....)
; where () are optional and (.....) are optional repeats.
; <EXPR2>::=<EXPR3>(<* or /><EXPR3>)(.....)
; <EXPR3>::=<VARIABLE>
;      <FUNCTION>
;      (<EXPR>)
; <EXPR> is recursive so that variable '@' can have an <EXPR>
; as index. Functions can have an <EXPR> as arguments,
; and <EXPR3> can be an <EXPR> in parenthese.
;
EXPR: 	CALL 	EXPR1  		;*** EXPR ***
 	PUSH 	H  		;save <EXPR1> value
 	LXI 	H, TAB6-1 	;lookup REL.OP.
 	JMP 	EXEC  		;go do it
XPR1: 	CALL 	XPR8  		;REL.OP.">="
 	RC   			;no, return HL=0
 	MOV 	L, A  		;yes, return HL=1
 	RET
XPR2: 	CALL 	XPR8  		;REL.OP."#"
 	RZ   			;false, return HL=0
 	MOV 	L, A  		;true, return HL=1
 	RET
XPR3: 	CALL 	XPR8  		;REL.OP.">"
 	RZ   			;false
 	RC   			;also false, HL=0
 	MOV 	L, A  		;true, HL=1
 	RET
XPR4: 	CALL 	XPR8  		;REL.OP."<="
 	MOV 	L, A  		;set HL=1
 	RZ   			;REL.OP. true, return
 	RC
 	MOV 	L, H  		;else, set HL=0
 	RET
XPR5: 	CALL 	XPR8  		;REL.OP."="
 	RNZ   			;false, return HL=0
 	MOV 	L, A  		;else set HL=1
 	RET
XPR6: 	CALL 	XPR8  		;REL.OP."<"
 	RNC   			;false, return HL=0
 	MOV 	L, A  		;else set HL=1
 	RET
XPR7: 	POP 	H  		;not REL.OP.
 	RET   			;return HL=<EXPR1>
XPR8: 	MOV 	A, C  		;subroutine for all
 	POP 	H  		;REL.OP.'s
 	POP 	B
 	PUSH 	H  		;reverse top of stack
 	PUSH 	B
 	MOV 	C, A
 	CALL 	EXPR1  		;set 2nd <EXPR1>
 	XCHG   			;value in DE now
 	XTHL   			;1st <EXPR1> in HL
 	CALL 	CKHLDE  	;compare 1st with 2nd
 	POP 	D  		;restore text pointer
 	LXI 	H, 0  		;set HL=0, A=1
 	MVI 	A, 1
 	RET
;
EXPR1:	tstc 	'-', XP11 	;negative sign?
 	LXI 	H,0  		;yes, fake '0-'
 	JMP 	XP16  		;treat like subtract
XP11: 	tstc 	'+', XP12 	;positive sign? ignore
XP12: 	CALL 	EXPR2  		;1st <EXPR2>
XP13: 	tstc 	'+', XP15 	;add?
 	PUSH 	H  		;yes, save value
 	CALL 	EXPR2  		;get 2nd <EXPR2>
XP14: 	XCHG   			;2nd in DE
 	XTHL   			;1st in HL
 	MOV 	A, H  		;compare sign
 	XRA 	D
 	MOV 	A, D
 	DAD 	D
 	POP 	D  		;restore text pointer
 	JM 	XP13  		;1st 2nd sign differ
 	XRA 	H  		;1st 2nd sign equal
 	JP 	XP13  		;so is equal
 	JMP 	QHOW  		;else, we have overflow
XP15: 	tstc 	'-', XPR9 	;subtract?
XP16: 	PUSH 	H  		;yes, save 1st <EXPR2>
 	CALL 	EXPR2  		;get 2nd <EXPR2>
 	CALL 	CHGSGN  	;negate
 	JMP 	XP14  		;and add them
;
EXPR2:	CALL 	EXPR3  		;get 1st <EXPR3>
XP21: 	tstc 	'*', XP24 	;multiply?
 	PUSH 	H  		;yes, save 1st
 	CALL 	EXPR3  		;and get 2nd <EXPR3>
 	MVI 	B, 0  		;clear B for sign
 	CALL 	CHKSGN  	;check sign
 	XTHL  			;1st in HL
 	CALL 	CHKSGN  	;check sign of 1st
 	XCHG
 	XTHL
 	MOV 	A, H  		;is HL > 255 ?
 	ORA 	A
 	JZ 	XP22  		;no
 	MOV 	A, D  		;yes, how about DE
 	ORA 	D
 	XCHG   			;put smaller in HL
 	JNZ 	AHOW  		;also >, will overflow
XP22: 	MOV 	A, L  		;this is dump
 	LXI 	H, 0  		;clear result
 	ORA 	A  		;add and count
 	JZ 	XP25
XP23: 	DAD 	D
 	JC 	AHOW  		;overflow
 	DCR 	A
 	JNZ 	XP23
 	JMP 	XP25  		;finished
XP24: 	tstc 	'/', XPR9 	;divide?
 	PUSH 	H  		;yes, save 1st <EXPR3>
 	CALL 	EXPR3  		;and get 2nd one
 	MVI 	B, 0  		;clear B for sign
 	CALL 	CHKSGN  	;check sign of 2nd
 	XTHL   			;get 1st in HL
 	CALL 	CHKSGN  	;check sign of 1st
 	XCHG
 	XTHL
 	XCHG
 	MOV 	A, D  		;divide by 0?
 	ORA 	E
 	JZ 	AHOW  		;say "HOW?"
 	PUSH 	B  		;else, save sign
 	CALL 	DIVIDE  		;use subroutine
 	MOV 	H, B  		;result in HL now
 	MOV 	L, C
 	POP 	B  		;get sign back
XP25: 	POP 	D  		;and text pointer
 	MOV 	A, H  		;HL must be +
 	ORA 	A
 	JM 	QHOW  		;else, it is overflow
 	MOV 	A, B
 	ORA 	A
 	CM 	CHGSGN  	;change sign if needed
 	JMP 	XP21  		;look for more terms
;
EXPR3: 	LXI 	H, TAB3-1 	;find function in TAB3
 	JMP 	EXEC  		;and go do it
NOTF: 	CALL 	TSTV  		;no, not a function
 	JC 	XP32  		;nor a variable
 	MOV 	A, M  		;variable
 	INX 	H
 	MOV 	H, M  		;value in HL
 	MOV 	L, A
 	RET
XP32: 	CALL 	TSTNUM  	;or is it a number
 	MOV 	A, B  		;# of digit
 	ORA 	A
 	RNZ   			;ok
PARN: 	tstc 	'(', XPR0 	;no digit, must be
PARNP:	CALL 	EXPR  		;"(EXPR)"
 	tstc 	')', XPR0
XPR9: 	RET
XPR0: 	JMP 	QWHAT  	;else, say: "WHAT?"
;
RND: 	CALL 	PARN  		;*** RND(EXPR) ***
 	MOV 	A, H  		;expr must be +
 	ORA 	A
 	JM 	QHOW
 	ORA 	L  		;and non-zero
 	JZ 	QHOW
 	PUSH 	D  		;save both
 	PUSH 	H
 	LHLD 	RANPNT  	;get memory as random
 	LXI 	D, RANEND
 	CALL 	COMP
 	JC 	RA1  		;wrap around if last
 	LXI 	H, BOTROM
RA1: 	MOV 	E, M
 	INX 	H
 	MOV 	D, M
 	SHLD 	RANPNT
 	POP 	H
 	XCHG
 	PUSH 	B
 	CALL 	DIVIDE  		;RND(N)=MOD(M,N)+1
 	POP 	B
 	POP 	D
 	INX 	H
 	RET
;
ABS: 	CALL 	PARN  		;*** ABS(EXPR) ***
 	DCX 	D
 	CALL 	CHKSGN  	;check sign
 	INX 	D
 	RET
;
SIZE: 	LHLD 	TXTUNF  	;*** SIZE ***
 	PUSH 	D  		;get the number of free
 	XCHG   			;bytes between 'TXTUNF'
 	LHLD 	TXTLMT  	;and 'TXTLMT'
 	CALL 	SUBDE
 	POP 	D
 	RET
;
;---------------------------------------------------------------
;
; *** DIVIDE *** SUBDE *** CHKSGN *** CHGSGN *** & CKHLDE ***
;
; 'DIVIDE' divides HL by DE. Result in BC, remainder in HL.
;
; 'SUBDE' subtracts DE from HL.
;
; 'CHKSGN' checks sign of HL. If +, no change. If -, change sign
; and flip sign of B.
;
; 'CHGSGN' changes sign of HL and B unconditionnally.
;
; 'CKHLDE' checks sign of HL and DE. If different, HL and DE
; are interchanged. If same sign, not interchanged. Either case,
; HL DE are then compared to set the flags.
;
DIVIDE: 	PUSH 	H  		;*** DIVIDE ***
 	MOV 	L, H  		;divide H by DE
 	MVI 	H, 0
 	CALL 	DV1
 	MOV 	B, C  		;save result in B
 	MOV 	A, L  		;(remainder+L)/DE
 	POP 	H
 	MOV 	H, A
DV1: 	MVI 	C, -1  		;result in C
DV2: 	INR 	C  		;dumb routine
 	CALL 	SUBDE  		;divide by subtract
 	JNC 	DV2  		;and count
 	DAD 	D
 	RET
;
SUBDE: 	MOV 	A, L  		;*** SUBDE ***
 	SUB 	E  		;subtract DE from
 	MOV 	L, A  		;HL
 	MOV 	A, H
 	SBB 	D
 	MOV 	H, A
 	RET
;
CHKSGN: MOV 	A, H  		;*** CHKSGN ***
 	ORA 	A  		;check sign of HL
 	RP   			;if ), change sign
;
CHGSGN: MOV 	A, H  		;*** CHGSGN ***
 	ORA 	L
 	RZ
 	MOV 	A, H
 	PUSH 	PSW
 	CMA   			;change sign of HL
 	MOV 	H, A
 	MOV 	A, L
 	CMA
 	MOV 	L, A
 	INX 	H
 	POP 	PSW
 	XRA 	H
 	JP 	QHOW
 	MOV 	A, B  		;and also flip B
 	XRI 	80H
 	MOV 	B, A
 	RET
;
CKHLDE: MOV 	A, H  		;*** CKHLDE ***
 	XRA 	D  		;same sign?
 	JP 	CK1  		;yes, compare
 	XCHG   			;no, xch and comp
CK1: 	CALL 	COMP
 	RET
;
COMP: 	MOV 	A, H  		;*** COMP ***
 	CMP 	D  		;compare HL with DE
 	RNZ   			;return correct C and
 	MOV 	A, L  		;Z flags
 	CMP 	E  		;but old A is lost
 	RET
;
;---------------------------------------------------------------
;
; *** SETVAL *** FIN *** ENDCHK *** & ERROR (& friends) ***
;
; 'SETVAL' expects a variable, followed by an equal sign and
; then an expr. It evaluates the expr. and set the variable
; to that value.
;
; 'FIN' checks the end of a command. If it ended with ";",
; execution continues. If it ended with a CR, it finds the next
; line and continue from there.
;
; 'ENDCHK' checks if a command is ended with CR. This is
; required in certain commands. (GOTO, RETURN, and STOP etc.)
;
; 'ERROR' prints the string pointed by DE (and ends with CR).
; It then prints the line pointed by 'CURRNT' with a "?"
; inserted at where the old text pointer (should be on top of
; the stack) points to. Execution of TB is stopped and PATB is
; restarted. Howewer, if 'CURRNT' -> zero (indicating a direct
; command), the direct command is not printed, and if 'CURRNT'
; -> negative # (indicating 'INPUT' command), the input line is
; not printed and execution is not terminated but continued at
; 'INPERR'.
;
; Related to 'ERROR' are the following: 'QWHAT' saves text
; pointer in stack and get message "WHAT?". 'AWHAT' just get
; message "WHAT?" and jump to 'ERROR'. 'QSORRY' and 'ASORRY'
; do same kind of thing. 'QHOW' and 'AHOW' in the zero page
; section also do this.
;
SETVAL: CALL 	TSTV  		;*** SETVAL ***
 	JC 	QWHAT  	;"WHAT?" no variable
 	PUSH 	H  		;push address of var.
 	tstc 	'=', SV1 		;pass "=" sign
 	CALL 	EXPR  		;evaluate expr.
 	MOV 	B, H  		;value in BC now
 	MOV 	C, L
 	POP 	H  		;get address
 	MOV 	M, C  		;save value
 	INX 	H
 	MOV 	M, B
 	RET
;
FINISH:	CALL 	FIN  		;check end of command
SV1: 	JMP 	QWHAT  	;print "WHAT?" if wrong
;
FIN: 	tstc 	';', FI1 		;*** FIN ***
 	POP 	PSW  		;";", purge RET address
 	JMP 	RUNSML  	;continue same line
FI1: 	tstc 	CR, FI2  	;not ";", is it CR?
 	POP 	PSW  		;yes, purge RET address
 	JMP 	RUNNXL 	 ;run next line
FI2: 	RET   			;else, return to caller
;
IGNBLK: LDAX 	D  		;*** IGNBLK ***
 	CPI 	' '  		;ignore blanks
 	RNZ  			;in text (where DE->)
 	INX 	D  		;and return the first
 	JMP 	IGNBLK  	;non-blank char. in A
;
ENDCHK: CALL 	IGNBLK  	;*** ENDCHK ***
 	CPI 	CR  		;end with CR?
 	RZ   			;ok, else say: "WHAT?"
;
QWHAT:	PUSH 	D  		;*** QWHAT ***
AWHAT:	LXI 	D, WHAT  	;*** AWHAT ***
ERROR:	CALL 	CRLF
 	CALL 	PRTSTG  	;print error message
 	LHLD 	CURRNT  	;get current line #
 	PUSH 	H
 	MOV 	A, M  		;check the value
 	INX 	H
 	ORA 	M
 	POP 	D
 	JZ 	TELL  		;if zero, just restart
 	MOV 	A, M  		;if negative
 	ORA 	A
 	JM 	INPERR  	;redo input
 	CALL 	PRTLN  		;else print the line
 	POP 	B
 	MOV 	B, C
 	CALL 	PRTCHS
 	MVI 	A, '?'  		;print a "?"
 	CALL 	OUTCH
 	CALL 	PRTSTG  	;line
 	JMP 	TELL  		;then restart
QSORRY: PUSH 	D  		;*** QSORRY ***
ASORRY: LXI 	D, SORRY 	;*** ASORRY ***
 	JMP 	ERROR
;
;---------------------------------------------------------------
;
; *** FNDLN (& friends) ***
;
; 'FNDLN' finds a line with a given line # (in HL) in the text
; save area. DE is used as the text pointer. If the line is
; found, DE will point to the beginning of that line (i.e., the
; low byte of the line #), and flags are NC & Z. If that line is
; not there and a line with a higher line # is found, DE points
; to there and flags are NC & NZ. If we reached the end of text
; save area and cannot find the line, flags are C & NZ. 'FNDLN'
; will initialize DE to the beginning of the text save area to
; start the search. Some other entries of this routine will not
; initialize DE and do the search. 'FNDLP' will start with DE
; and search for the line #. 'FNDNXT' will bump DE by 2, find
; a CR and then start search. 'FNDSKP' use DE to find a CR,
; and then start search.
;
FNDLN:	MOV 	A, H  		;*** FNDLN ***
 	ORA 	A  		;check sign of HL
 	JM 	QHOW  		;it cannot be -
 	LXI 	D, TEXT  	;init. text pointer
;
FNDLP: 	INX 	D  		;is it EOT mark?
 	LDAX 	D
 	DCX 	D
 	ADD 	A
 	RC   			;C, NZ passed. end.
 	LDAX 	D  		;we did not, get byte 1
 	SUB 	L  		;is this the line?
 	MOV 	B, A  		;compare low order
 	INX 	D
 	LDAX 	D  		;get byte 2
 	SBB 	H  		;compare high order
 	JC 	FL1  		;no, not there yet
 	DCX 	D  		;else we either found
 	ORA 	B  		;it, or it is not there
 	RET   			;NC, Z=found; NC, NZ=no
;
FNDNXT: INX 	D  		;find next line
FL1: 	INX 	D  		;just passed byte 1 & 2
;
FNDSKP: LDAX 	D  		;*** FNDSKP ***
 	CPI 	CR  		;try to find CR
 	JNZ 	FL1  		;keep looking
 	INX 	D  		;found CR, skip over
 	JMP 	FNDLP  		;check if end of text
;
TSTV: 	CALL 	IGNBLK  	;*** TSTV ***
 	SUI 	'@'  		;test variables
 	RC   			;C=not a variable
 	JNZ 	TV1  		;not "@" array
 	INX 	D  		;it is the "@" array
 	CALL 	PARN  		;@ should be followed
 	DAD 	H  		;by (EXPR) as its index
 	JC 	QHOW  		;is index too big?
TSTB: 	PUSH 	D  		;will it fit?
 	XCHG
 	CALL 	SIZE  		;find size of free
 	CALL 	COMP  		;and check that
 	JC 	ASORRY  	;if not, say: "SORRY"
 	CALL 	LOCR  		;if fits, get address
 	DAD 	D  		;of @(EXPR) and put it
 	POP 	D  		;in HL
 	RET   			;C flag is cleared
TV1: 	CPI 	27  		;not @, is it A to Z?
 	CMC   			;if not return C flag
 	RC
 	INX 	D  		;if A through Z
 	LXI 	H, VARBGN-2
 	RLC   			;HL->variable
 	ADD 	L  		;return
 	MOV 	L, A  		;with C flag cleared
 	MVI 	A, 0
 	ADC 	H
 	MOV 	H, A
 	RET
;
;---------------------------------------------------------------
;
; *** TSTCH *** TSTNUM ***
;
; 'TSTCH' is used to test non-blank character in the text
; (pointed by DE) against the character that follows the call.
; If they do not match, n bytes of code will be skipped over,
; where n is between 0 and 255 and is stored in the second byte
; following the call.
;
; 'TSTNUM' is used to chack wether the text (pointed by DE) is a
; number. If a number is found, B will be non-zero and HL will
; contain the value (in binary) of the number, else B and HL
; are 0.
;
TSTCH: XTHL   			;*** TSTCH ***
 	CALL 	IGNBLK  	;ignore leading blanks
 	CMP 	M  		;and test the character
 	INX 	H  		;compare the byte that
 	JZ 	TC1  		;follows the call inst.
 	PUSH 	B  		;with the text (DE->)
 	MOV 	C, M  		;if not =, add the 2nd
 	MVI 	B, 0  		;byte that follows the
 	DAD 	B  		;call to the old PC
 	POP 	B  		;i.e., do a relative
 	DCX 	D  		;jump if not =
TC1: 	INX 	D  		;if =, skip those bytes
 	INX 	H  		;and continue
 	XTHL
 	RET
;
TSTNUM: LXI 	H, 0  		;*** TSTNUM ***
 	MOV 	B, H  		;test if the text is
 	CALL 	IGNBLK  	;a number
TN1: 	CPI 	'0'  		;if not, return 0 in
 	RC   			;B and HL
 	CPI 	03AH  		;if numbers, convert
 	RNC   			;to binary in HL and
 	MVI 	A, 0F0H  	;set B to # of digits
 	ANA 	H  		;if H>255, there is no
 	JNZ 	QHOW  		;room for next digit
 	INR 	B  		;B counts # of digits
 	PUSH 	B
 	MOV 	B, H  		;HL=10*HL+(new digit)
 	MOV 	C, L
 	DAD 	H  		;where 10* is done by
 	DAD 	H  		;shift and add
 	DAD 	B
 	DAD 	H
 	LDAX 	D  		;and (digit) is from
 	INX 	D  		;stripping the ASCII
 	ANI 	0FH  		;code
 	ADD 	L
 	MOV 	L, A
 	MVI 	A, 0
 	ADC 	H
 	MOV 	H, A
 	POP 	B
 	LDAX 	D  		;do this digit after
 	JP 	TN1  		;digit. S say overflow
QHOW: 	PUSH 	D  		;*** QHOW ***
AHOW: 	LXI 	D, HOW  	;*** AHOW ***
 	JMP 	ERROR
;
;---------------------------------------------------------------
;
; *** MVUP *** MVDOWN *** POPA *** & PUSHA ***
;
; 'MVUP' moves a block up from where DE-> to where BC-> until
; DE = HL.
;
; 'MVDOWN' moves a block down from where DE-> to where HL->
; until DE = BC.
;
; 'POPA' restores the 'FOR' loop variable save area from the
; stack
;
; 'PUSHA' stacks the 'FOR' loop variable save area into the
; stack.
;
MVUP: 	CALL 	COMP  		;*** MVUP ***
 	RZ   			;DE = HL, return
 	LDAX 	D  		;get one byte
 	STAX 	B  		;move it
 	INX 	D  		;increase both pointers
 	INX 	B
 	JMP 	MVUP  		;until done
;
MVDOWN: MOV 	A, B  		;*** MVDOWN ***
 	SUB 	D  		;test if DE = BC
 	JNZ 	MD1  		;no, go move
 	MOV 	A, C  		;maybe, other byte?
 	SUB 	E
 	RZ   			;yes, return
MD1: 	DCX 	D  		;else move a byte
 	DCX 	H  		;but first decrease
 	LDAX 	D  		;both pointers and
 	MOV 	M, A  		;then do it
 	JMP 	MVDOWN  	;loop back
;
POPA: 	POP 	B  		;BC = return address
 	POP 	H  		;restore LOPVAR, but
 	SHLD 	LOPVAR  	;=0 means no more
 	MOV 	A, H
 	ORA 	L
 	JZ 	PP1  		;yep, go return
 	POP 	H  		;nop, restore others
 	SHLD 	LOPINC
 	POP 	H
 	SHLD 	LOPLMT
 	POP 	H
 	SHLD 	LOPLN
 	POP 	H
 	SHLD 	LOPPT
PP1: 	PUSH 	B  		;BC = return address
 	RET
;
PUSHA:	LXI 	H, STKLMT 	;*** PUSHA ***
 	CALL 	CHGSGN
 	POP 	B  		;BC = return address
 	DAD 	SP  		;is stack near the top?
 	JNC 	QSORRY  	;yes, sorry for that.
 	LHLD 	LOPVAR  	;esle, save loop var.s
 	MOV 	A, H  		;but if lopvar is 0
 	ORA 	L  		;that will be all
 	JZ 	PU1
 	LHLD 	LOPPT  	;else, more to save
 	PUSH 	H
 	LHLD 	LOPLN
 	PUSH 	H
 	LHLD 	LOPLMT
 	PUSH 	H
 	LHLD 	LOPINC
 	PUSH 	H
 	LHLD 	LOPVAR
PU1: 	PUSH 	H
 	PUSH 	B  		;BC = return address
 	RET
LOCR: 	LHLD 	TXTUNF
 	DCX 	H
 	DCX 	H
 	RET
;
;---------------------------------------------------------------
;
; *** PRTSTG *** *** QTSTG *** *** PRTNUM *** & PRTLN ***
;
; 'PRTSTG' prints a string pointed by DE. It stops printing and
; returns to caller when either a CR is printed or when the next
; byte is zero. Registers A and B are changed. Register DE
; points to what follows the CR or to the zero.
;
; 'QTSTG' looks for up-arrow, single quote, or double-quote.
; If none of these, return to caller. If up-arrow, output a
; control character. If single or double quote, print the
; string in the quote and demands a matching unquote.
; After the printing, the next 3 bytes of the caller is
; skipped over (usually a jump instruction).
;
; 'PRTNUM' prints the number in HL. Leading blanks are added
; if needed to pad the number of spaces to the number in C.
; Howewer, if the number of digits is larger than the number
; in C, all digits are printed anyway. Negative sign is also
; printed and counted in. Positive sign is not.
;
; 'PRTLN' finds a saved line, prints the line number and
; a space.
;
PRTSTG: SUB 	A  		;*** PRTSTG ***
PS1: 	MOV 	B, A
PS2: 	LDAX 	D  		;get a character
 	INX 	D  		;bump pointer
 	CMP 	B  		;same as old A?
 	RZ   			;yes, return
 	CALL 	OUTCH  	;else, print it
 	CPI 	CR  		;was it a CR?
 	JNZ 	PS2  		;no, next
 	RET   			;yes, return
;
QTSTG: tstc 	'"', QT3 	;*** QTSTG ***
 	MVI 	A, '"'  		;it is a " (double quote)
QT1: 	CALL 	PS1  		;print until another
QT2: 	CPI 	CR  		;was last one a CR?
 	POP 	H  		;return address
 	JZ 	RUNNXL  	;was CR, run next line
 	INX 	H  		;skip 3 bytes on return
 	INX 	H
 	INX 	H
 	PCHL   			;return
QT3: 	tstc 	27H, QT4 	;is it a ' (single quote) ?
 	MVI 	A, 27H  		;yes, do same
 	JMP 	QT1  		;as in "
QT4: 	tstc 	5EH, QT5 	;is it an up-arrow?
 	LDAX 	D  		;yes, convert character
 	XRI 	40H  		;to control-char.
 	CALL 	OUTCH
 	LDAX 	D  		;just in case it is a CR
 	INX 	D
 	JMP 	QT2
QT5: 	RET   			;none of the above
PRTCHS: MOV 	A, E
 	CMP 	B
 	RZ
 	LDAX 	D
 	CALL 	OUTCH
 	INX 	D
 	JMP 	PRTCHS
;
PRTNUM	equ	$  		;*** PRTNUM ***
PN3: 	MVI 	B, 0  		;B=sign
 	CALL 	CHKSGN  	;check sign
 	JP 	PN4  		;no sign
 	MVI 	B, '-'  		;B=sign
 	DCR 	C  		;'-' takes space
PN4: 	PUSH 	D
 	LXI 	D, 10  		;decimal
 	PUSH 	D  		;save as a flag
 	DCR 	C  		;C=spaces
 	PUSH 	B  		;save sign & space
PN5: 	CALL 	DIVIDE  		;divide HL by 10
 	MOV 	A, B  		;result O?
 	ORA 	C
 	JZ 	PN6  		;yes, we got all
 	XTHL   			;no, save remainder
 	DCR 	L  		;and count space
 	PUSH 	H  		;HL is old BC
 	MOV 	H, B  		;move result to BC
 	MOV 	L, C
 	JMP 	PN5  		;and divide by 10
PN6: 	POP 	B  		;we got all digits in
PN7: 	DCR 	C  		;the stack
 	MOV 	A, C  		;look at space count
 	ORA 	A
 	JM 	PN8  		;no leading blanks
 	MVI 	A, ' '  		;leading blanks
 	CALL 	OUTCH
 	JMP 	PN7  		;more?
PN8: 	MOV 	A, B  		;print sign?
 	ORA 	A
 	CNZ 	OUTCH  	;maybe - or null
 	MOV 	E, L  		;last remainder in E
PN9: 	MOV 	A, E  		;check digit in E
 	CPI 	10  		;10 is flag for no more
 	POP 	D
 	RZ   			;if so, return
 	ADI 	'0'  		;else, convert to ASCII
 	CALL 	OUTCH  	;and print the digit
 	JMP 	PN9  		;go back for more
;
PRTLN: LDAX 	D  		;*** PRTLN ***
 	MOV 	L, A  		;low order line #
 	INX 	D
 	LDAX 	D  		;high order
	MOV 	H, A
 	INX 	D
 	MVI 	C, 4  		;print 4 digit line #
 	CALL 	PRTNUM
 	MVI 	A, ' '  		;followed by a blank
 	CALL 	OUTCH
 	RET
;
TAB1: 	item 	"LIST", list 	;direct commands
 	item 	"NEW", new
 	item 	"RUN", run
 	item 	"SID", sid 	;added by ROCHE
;
TAB2: 	item 	"NEXT", next 	;direct/statement
 	item 	"LET", let
 	item 	"IF", iff
 	item 	"GOTO", goto
 	item 	"GOSUB",gosub
 	item 	"RETURN",return
 	item 	"REM", rem
 	item 	"FOR", for
 	item 	"INPUT",input
 	item 	"PRINT",print
 	item 	"STOP", stop
 	item 	"", morec
;   				;************************
MOREC: JMP 	DEFLT  		;*** JMP USER-COMMAND ***
;    				;************************
TAB3: 	item 	"RND", rnd 	;functions
 	item 	"ABS", abs
 	item 	"SIZE", size
 	item 	"", moref
;    				;*************************
MOREF: JMP 	NOTF  		;*** JMP USER-FUNCTION ***
    				;*************************
TAB4: 	item 	"TO", FR1 	;"FOR" command
 	item 	"", QWHAT
;
TAB5: 	item 	"STEP", FR2 	;"FOR" command
 	item 	"", FR3
;
TAB6: 	item 	">=", XPR1 	;relation operators
 	item 	"#", XPR2
 	item 	">", XPR3
 	item 	"=", XPR5
 	item 	"<=", XPR4
 	item 	"<", XPR6
 	item 	"", XPR7
;
RANEND EQU 	$
;
;PATB original code>
;---------------------------------------------------------------
;
; *** INPUT OUTPUT ROUTINES ***
;
; User must verify and/or modify these routines
;
;---------------------------------------------------------------
;
; *** CRLF *** OUTCH ***
;
; 'CRLF' will output a CR. Only A & flags may change at return.
;
; 'OUTCH' will output the character in A. If the character is CR,
; it will also outut a LF and three nulls. Flags may change at
; return. Others registers do not.
;
; *** CHKIO *** GETLN ***
;
; 'CHKIO' checks to see if there is any input. If no input,
; it returns with Z flag. If there is input, it further checks
; wether input is Control-C. If not Control-C, it returns the
; character in A with Z flag cleared. If input is Control-C,
; 'CHKIO' jumps to 'INIT' and will not return. Only A & flags
; may change at return.
;
; 'GETLN' reads a input line into 'BUFFER'. It first prompt the
; character in A (given by the caller), then it fills the buffer
; and echos. Back-space is used to delete the last character
; (if there is one). CR signals the end of the line, and cause
; 'GETLN' to return. When buffer is full, 'GETLN' will accept
; back-space or CR only and will ignore (and will not echo)
; other characters. After the input line is stored in the buffer
; two more bytes of FF are also stored and DE points to the
; last FF. A & flags are also changed at return.
;
CRLF: 	MVI 	A, 0DH  		;CR in A
;    				;***********************
OUTCH:	JMP 	USEOUT  	;*** JMP USER-OUTPUT ***
;    				;***********************
CHKIO: 	JMP 	USEINP  	;*** JMP USER-INPUT  ***
;    				;***********************
GETLN: 	LXI 	D, BUFFER 	;*** MODIFY THIS *******
;    				;***********************
GL1: 	CALL 	OUTCH  	;prompt or echo
GL2: 	CALL 	CHKIO  		;get a character
 	JZ 	GL2  		;wait for input
 	CPI 	LF
 	JZ 	GL2
L3: 	STAX 	D  		;save char.
 	CPI 	08H  		;is it Back-Space?
 	JNZ 	GL4  		;no, more tests
 	MOV 	A, E  		;yes, delete?
# 	CPI 	LOW BUFFER
 	CPI 	(BUFFER & 0FFH)
 	JZ 	GL2  		;nothing to delete
 	LDAX 	D  		;delete
 	DCX 	D
 	JMP 	GL1
GL4: 	CPI 	CR  		;was it CR?
 	JZ 	GL5  		;yes, end of line
 	MOV 	A, E  		;else, more free room?
# 	CPI 	LOW BUFEND
 	CPI 	(BUFEND & 0FFH)
 	JZ 	GL2  		;no, wait for CR/Rub-Out
 	LDAX 	D  		;yes, bump pointer
 	INX 	D
 	JMP 	GL1
GL5: 	INX 	D  		;end of line
 	INX 	D  		;bump pointer
 	MVI 	A, 0FFH  	;put marker after it
 	STAX 	D
 	DCX 	D
 	JMP 	CRLF
;-------------------------------
;I/O Routine for Emulator
;-------------------------------
USEOUT:
	push	psw
	cpi	CR
	jnz	USEOUT1
	mvi	a, LF
	out	0FFH
	mvi	a, CR
	out	0FFH
	pop	psw
	ret
USEOUT1
	pop	psw
	out	0FFH
	ret
; 	CPI 	CR  		;was it CR?
; 	RNZ   			;no, return
; 	MVI 	A, LF  		;yes, give LF
; 	CALL 	USEOUT
; 	MVI 	A, CR
; 	RET
	ret

USEINP:
	in	0FEH
	inr	a
	rz
	dcr	a
	cpi	'q'
	jz	USEINP0
	cpi	LF
	rnz
	mvi	a, CR
	dcr	a
	inr	a
	ret
USEINP0
	jmp	INIT

;;-------------------------------
;;I/O Routines using CP/M, Cf.
;;"8080/Z80 Assembly Language"
;;by Alan R. MILLER, SYBEX, 1981
;;-------------------------------
;;MILLER> (OUT4, p.92)
;USEOUT:
; 	push 	h  		;save registers
; 	push 	d
; 	push 	b
; 	mov 	c, a  		;move byte
; 	push 	psw
; 	lxi 	h, out5  	;return address
; 	push 	h  		;put on stack
; 	lhld 	1  		;BIOS entry
; 	lxi 	d, 9  		;offset to output
; 	dad 	d  		;add together
; 	pchl   			;call BIOS
;out5: 	pop 	psw  		;restore registers
; 	pop 	b
; 	pop 	d
; 	pop 	h
;;-------------------------------
;;PATB original code>
; 	CPI 	CR  		;was it CR?
; 	RNZ   			;no, return
; 	MVI 	A, LF  		;yes, give LF
; 	CALL 	USEOUT
; 	MVI 	A, CR
; 	RET
;;-------------------------------
;;MILLER> (INSTAT, p.92)
;USEINP:
; 	push 	h  		;save registers
; 	push 	d
; 	push 	b
; 	lxi 	h, st5  		;return address
; 	push 	h  		;put on stack
; 	lhld 	1  		;BIOS entry
; 	lxi 	d, 3  		;offset to status
; 	dad 	d  		;add to addr
; 	pchl   			;call BIOS
;st5: 	pop 	b  		;restore registers
; 	pop 	d
; 	pop 	h
; 	ora 	a
;;-------------------------------
;;PATB original code>
; 	RZ   			;no input, return zero
;;-------------------------------
;;MILLER> (INPUT2, p.91)
; 	push 	h  		;save registers
; 	push 	d
; 	push 	b
; 	lxi 	h, in5  		;return address
; 	push 	h  		;put on stack
; 	lhld 	1  		;BIOS warm start
; 	lxi 	d, 6  		;offset to input
; 	dad 	d  		;add in
; 	pchl   			;call BIOS
;in5: 	pop 	b  		;restore registers
; 	pop 	d
; 	pop 	h
;;-------------------------------
;;PATB original code>
; 	ANI 	7FH
; 	CPI 	3  		;is it Control-C?
; 	RNZ   			;no, return char
; 	JMP 	INIT  		;yes, restart
;;-------------------------------
;;ROCHE>

;
; Define variables, buffer and stack in RAM
;
 	ORG 	BOTSCR
;
KEYWRD 	DS 	1  		;was INIT done?
TXTLMT 	DS 	2  		;-> limit of text area
VARBGN 	DS 	2*26  		;TB variables A-Z
CURRNT 	DS 	2  		;points to current line
STKGOS 	DS 	2  		;saves SP in 'GOSUB'
VARNXT 	equ	$		;temporary storage
STKINP 	DS 	2  		;saves SP in 'INPUT'
LOPVAR 	DS 	2  		;'FOR' loop save area
LOPINC 	DS 	2  		;increment
LOPLMT 	DS 	2  		;limit
LOPLN 	DS 	2  		;line number
LOPPT 	DS 	2  		;text pointer
RANPNT 	DS 	2  		;random number pointer
 	DS 	1  		;extra byte for buffer
BUFFER 	DS 	132  		;input buffer
BUFEND 	equ	$		;buffer end
	DS 	4  		;extra bytes for stack
STKLMT 	equ	$		;soft limit for stack
;
 	ORG 	TOPSCR
;
STACK 	equ	$		;stack starts here
;
 	ORG 	BOTRAM
;
TXTUNF 	DS 	2  		;unfilled text save area
TEXT 	DS 	2  		;text save area

	END
