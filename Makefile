kilo: kilo.d
	$(DC) kilo.d

kiloc: kilo.c
	$(CC) kilo.c -o kiloc -Wall -Wextra -pedantic -std=c99
