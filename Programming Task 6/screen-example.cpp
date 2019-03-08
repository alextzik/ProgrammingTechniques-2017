#include <iostream>
#include "ps6.h"

int main() {
	using namespace tui;

	screen scr(80,30);
	scr.clear();

	point ptred('@', 1, 3, false);
	point ptblue('#', 4, 0, true);

	scr.set_rect(0, 0, scr.ncols() - 1, scr.nrows() - 1, ptred);
	scr.set_circle(30, 15, 10, ptblue);

	scr.render();

	return 0;
}
