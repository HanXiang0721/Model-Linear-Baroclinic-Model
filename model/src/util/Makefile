#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = util

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      =

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = ucaln.o \
		ucloc.o \
		ufftp.o \
		umtlu.o \
		usphe.o \
		uspst.o \
		usubs.o

SRCS	      = ucaln.F \
		ucloc.F \
		ufftp.F \
		umtlu.F \
		usphe.F \
		uspst.F \
		usubs.F

SRCS_ALL      = $(SRCS)

all: lib

lib:		$(OBJS)
		$(AR) $(LIBRARY) $(OBJS)
		$(RANLIB) $(LIBRARY)

clean:
		$(RM) *.o *~ core *.trace *.f *.html

index:
		@$(INDEX) $(HDRS) $(SRCS)

tags:
		@$(TAGS) $(HDRS) $(SRCS)

pack: $(SRCS:.F=.ftr)

ftr: $(SRCS_ALL:.F=.ftr)

rmftr:
	$(RM) $(FTRFILE)

sublist: 
	egrep '( SUBROU| ENTRY  )' $(SRCS) /dev/null | $(AWK) -f ../../sh/util/sublist dir=$(DIR) | sort > SUBLIST

html: $(SRCS:.F=.html)


###
