# Makefile for %[Package name: %]	-*-Makefile-*-
# Copyright (C) %y %U %a
# $Header: //depot/main/user/ryand/Bin/elisp/autoinsert/makefile.inc#1 $
# $Log: makefile.inc,v $
# Revision 1.1  1998/11/26 08:41:23  ryand
# Initial revision
#
# Revision 1.1.1.1  1998/09/14 22:04:50  ryand
# Imported sources
#

CC = gcc
CFLAGS = -g -O
MAKE = make
COMPRESS = gzip

OBJS =
SRCS = $(OBJS:.o=.c)

RUBBISH = $(OBJS) *.log *.aux *.tp *.vr *.cp *.fn *.ky *.pg

DISTFILES = $(SRCS) Makefile

all: %1
	@echo All done.

clean:
	-rm -f $(RUBBISH)

dist:
	tar cf %1.tar $DISTFILES
	$(COMPRESS) %1.tar
