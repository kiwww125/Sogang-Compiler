CC=gcc
TARGET=20180085
OBJS=main.o util.o lex.yy.o

20180085 : $(OBJS)
	$(CC) -o 20180085 $(OBJS) -lfl

main.o : main.c globals.h util.h
	$(CC) -c main.c

util.o : util.c globals.h util.h
	$(CC) -c util.c 

lex.yy.o : lex.yy.c util.h globals.h 
	$(CC) -c lex.yy.c 

lex.yy.c : tiny.l 
	flex tiny.l

clean :
	rm *.o lex.yy.c 20180085