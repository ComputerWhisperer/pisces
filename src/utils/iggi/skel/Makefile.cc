#	Imakefile		Version 1.1		
#	Last Modification:	3/27/90 14:46:27	

#
# Makefile for  skel
#
DGEN	= ../misc

DEST	=  skel

GENLIB = $(DGEN)/libmisc.a

MYLIBS = $(GENLIB)

INCLUDES = -I$(DGEN)  -I. -I/usr/include/local

INCFILES = alpha.h dbase.h griph.h menu.h skelp.h udb.h

CFLAGS = -f68881 -O $(INCLUDES) -DBSD

CSOURCES = dbase.c main.c stalloc.c iltty.c menu.c alpha.c user.c meshio.c \
       skelp.c griph.c geom.c sys.c check.c u2.c udb.c

SOURCES = $(CSOURCES) 

OBJECTS = dbase.o main.o stalloc.o iltty.o menu.o alpha.o user.o meshio.o \
       skelp.o griph.o geom.o sys.o check.o u2.o udb.o


$(DEST) :	$(OBJECTS)
	cc -o $(DEST) -f68881 -g $(OBJECTS) ../misc/libmisc.a  \
	-lhigh2 -lgplot -ltermcap -lm 

