#include	<sgtty.h>

int	main()
{
	int	ret, n;

	
	ret = ioctl(0, FIONREAD, &n);
	printf("ret=%d, n=%d\n", ret, n);
}
