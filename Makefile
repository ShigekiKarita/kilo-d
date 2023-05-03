kilo: kilo.d
	$(DC) kilo.d -betterC

kiloc: kilo.c
	$(CC) kilo.c -o kiloc -Wall -Wextra -pedantic -std=c99
