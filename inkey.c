#include	<ncurses/curses.h>
int main(void)
{
	int	c;

	initscr();
	cbreak();
	noecho();
	timeout(0);
	while((c=getch()) == -1) {
		addstr("."); refresh();
		usleep(10000); // 10ms
	}
	endwin();  
}
