/*
 *	Z80 EMULATOR
 *	$Id: z80emu.c,v 1.11 2009/01/26 08:35:01 runner Exp $
 */
#include	"Z80.h"
#include	<curses.h>
#include	<fcntl.h>
#include	<signal.h>
// #include	<sgtty.h>
#include	<unistd.h>

static	Z80	cpu1;
static	WINDOW	*regWin, *disasmWin, *memWin;
static	WINDOW	*virtualConsole;
static	int	stepFlag=TRUE;
static	int	stepView=TRUE;
static	word	breakAddress = 0x38;	/* Break Point ie.INT 8*/

static	char	MSGS[100];		/* RAM 4KB */
/*
	LD	sp,0100h
	LD	a,055h
	AND	A
	CALL	0000ah
	HALT
	RET
*/
static	byte	MEM[0x10000] = {0x31,0x00,0x01,0x3e,0x55,0x97, 0xcd,0x0a,0x00,0x76,0xc9};
 
void	ctrlC(int no)
{
	stepFlag = stepView = TRUE;
	signal(SIGINT, SIG_DFL);
}

word	subGetWord(char *msg)
{
	word	retVal;
	WINDOW	*win;
	char	buf[80];

	win = subwin(stdscr,1,60, 12,10);
	box(win,'|','-');
	wmove(win,1,1);
	wprintw(win, msg);
	wrefresh(win);
	echo();
	wgetnstr(win, buf, sizeof(buf));
	sscanf(buf,"%x",&retVal);
	werase(win);
	delwin(win);
	noecho();
	doupdate();
	refresh();

	return retVal;
}

int	main(int argc, char **argv, char **env)
{
	int	fh,c;
	char	buf[80];
	FILE	*fp;
	int	trace_flag=0;
	long	trace_count=0;

	while((c=getopt(argc,argv,"t")) != -1) {
		switch(c) {
			case 't':
				trace_flag = 1;
				break;
			default:
				break;
		}
	}
	if((fh=open(argv[optind],O_RDONLY))<0) {
		perror(argv[1]);
		return 1;
	}
	read(fh,MEM,sizeof(MEM));
	close(fh);

	/* curses */
	initscr();
	noecho();
	cbreak();
	regWin = subwin(stdscr,2,80, 0,0);
	disasmWin = subwin(stdscr, 10,20, 3,58);
	memWin = subwin(stdscr, 10,40, 3,0);
	virtualConsole = subwin(stdscr, 10,80,13,0);
	scrollok(memWin, TRUE);
	scrollok(disasmWin, TRUE);
	scrollok(virtualConsole, TRUE);

	cpu1.IPeriod = 1;
	cpu1.ICount = 0;
	cpu1.Trace = 0;
	cpu1.Trap = 0x0009;

	if( trace_flag ) fp=fopen("trace.txt","w");

	ResetZ80(&cpu1);
	for(;;) {
		if( trace_flag ) {
			fprintf(fp,"%8d ", ++trace_count);
			fprintf(fp,"PC:%04X ", cpu1.PC.W);
			fprintf(fp,"SP:%04X ", cpu1.SP.W);
			fprintf(fp,"AF:%04X ", cpu1.AF.W);
			fprintf(fp,"BC:%04X ", cpu1.BC.W);
			fprintf(fp,"DE:%04X ", cpu1.DE.W);
			fprintf(fp,"HL:%04X ", cpu1.HL.W);
			fprintf(fp,"%02X ", MEM[cpu1.PC.W]);
			DAsm(MSGS, &MEM[cpu1.PC.W]);
			fprintf(fp,"%s\n", MSGS);
		}
		if( cpu1.PC.W == breakAddress ) {
			stepView = stepFlag = TRUE;
		}
		if( stepView == TRUE ) {
			wmove(regWin, 0,0);
			wprintw(regWin, "AF:%04X ",cpu1.AF.W);
			wprintw(regWin, "BC:%04X ", cpu1.BC.W);
			wprintw(regWin, "DE:%04X ", cpu1.DE.W);
			wprintw(regWin, "HL:%04X ", cpu1.HL.W);
			wprintw(regWin, "IX:%04X ", cpu1.IX.W);
			wprintw(regWin, "IY:%04X ", cpu1.IY.W);
			wprintw(regWin, "PC:%04X ", cpu1.PC.W);
			wprintw(regWin, "SP:%04X ", cpu1.SP.W);
			wmove(regWin, 1,0);
			wprintw(regWin, "AF:%04X ",cpu1.AF1.W);
			wprintw(regWin, "BC:%04X ", cpu1.BC1.W);
			wprintw(regWin, "DE:%04X ", cpu1.DE1.W);
			wprintw(regWin, "HL:%04X ", cpu1.HL1.W);
			wprintw(regWin, "%s", (cpu1.AF.B.l&S_FLAG?"(S)":"(s)"));
			wprintw(regWin, "%s", (cpu1.AF.B.l&Z_FLAG?"(Z)":"(z)"));
			wprintw(regWin, "%s", (cpu1.AF.B.l&H_FLAG?".(H).":".(h)."));
			wprintw(regWin, "%s", (cpu1.AF.B.l&P_FLAG?"(P/V)":"(p/v)"));
			wprintw(regWin, "%s", (cpu1.AF.B.l&N_FLAG?"(N)":"(n)"));
			wprintw(regWin, "%s", (cpu1.AF.B.l&C_FLAG?"(C)":"(c)"));
			DAsm(MSGS, &MEM[cpu1.PC.W]);
			wprintw(disasmWin, "%04X:%02X %s\n\r",cpu1.PC.W, MEM[cpu1.PC.W],MSGS);
			wrefresh(regWin);
			wrefresh(disasmWin);
		}
		if( stepFlag == TRUE ) {
			c = getch();
			if( c == 'q' ) break;
			if( c == 'c' || c == 'r' ) {
				signal(SIGINT, ctrlC);
				stepFlag = FALSE;
			}
			if( c == 'r' ) {
				stepView = FALSE;
			}
			if( c == 'b' ) {
				breakAddress = subGetWord("BreakPoint in HEX Address?");
				continue;
			}
			if( c == 'P' ) {
				cpu1.PC.W = subGetWord("PC in HEX Address?");
				continue;
			}
			if( c == 'S' ) {
				cpu1.SP.W = subGetWord("SP in HEX Address?");
				continue;
			}
		}
		ExecZ80(&cpu1);
	}

	endwin();
	return 0;
}

void	WrZ80(register word Addr, register byte Value)
{
	if( stepView == TRUE ) {
		wprintw(memWin, "WR:%04X:%02X->%02X\n\r", Addr, MEM[Addr], Value);
		wrefresh(memWin);
	}
	MEM[Addr] = Value;
}

byte	RdZ80(register word Addr)
{
	if( stepView == TRUE ) {
		wprintw(memWin, "RD:%04X:%02X\n\r", Addr, MEM[Addr]);
		wrefresh(memWin);
	}
	return MEM[Addr];
}

word	LoopZ80(register Z80 *R)
{
	puts("error");
}

/*
 *	PORT FF OUTPUT CONSOLE CHARACTER
 *	IF NEED SCROLL, OUT LF,CR
 *
 *	PORT FE OUTPUT CONSOLE CHARACTER
 *	ignore upper than 0x7e and CR to scroll
 */
void	OutZ80(register word Port,register byte Value)
{
	if( (Port&0xff)==0xff ) {
		waddch(virtualConsole, Value);
		wrefresh(virtualConsole);
	} else if((Port & 0xff) == 0xfe) {
		if(Value > 0x7e) return;
		if(Value == 0x0d) {
			waddch(virtualConsole, 0x0a);
		}
		waddch(virtualConsole, Value);
		wrefresh(virtualConsole);
	}
			
}

/*
 *	PORT FF	READ KEY and WAIT
 *	PORT FE READ but non block. Return 0xff no hit key.
 */
byte	InZ80(register word Port)
{
	static int	n;

	if( (Port&0xff)==0xff ) {
		WINDOW	*win;
		win = subwin(stdscr,3,3, 10,40);
		box(win,'|','-');
		wmove(win,1,1);
		wrefresh(win);
		echo();
		n = getch();
		wclear(win);
		delwin(win);
		noecho();
		refresh();
		return n&0xff;
	} else if( (Port&0xff)==0xfe ) {

/* curses version return if non-key return 0xff;
*/
		nodelay(stdscr, TRUE);
		n = getch();
		nodelay(stdscr, FALSE);
		return n&0xff;

/* or return if non-key return zero.
		ioctl(0, FIONREAD, &n);
		return(n);
*/
	}
}


byte	DebugZ80(register Z80 *R)
{
	printf("DEBUG\n");
	return 0;
}

void	PatchZ80(register Z80 *R)
{
}

