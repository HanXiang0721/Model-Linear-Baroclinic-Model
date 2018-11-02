#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = io

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      = $(INCDIR)/zcdim.F \
		$(INCDIR)/zhdim.F \
		$(INCDIR)/zidim.F \
		$(INCDIR)/zkdim.F

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = iavrg.o \
		icord.o \
		idiag.o \
		ifopn.o \
		igtio.o \
		igzio.o \
		ihist.o \
		ihsub.o \
		irdat.o \
		irwgd.o \
		is2po.o \
		istrt.o

SRCS	      = iavrg.F \
		icord.F \
		idiag.F \
		ifopn.F \
		igtio.F \
		igzio.F \
		ihist.F \
		ihsub.F \
		irdat.F \
		irwgd.F \
		is2po.F \
		istrt.F

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
iavrg.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F
icord.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F \
	$(INCDIR)/zhdim.F
idiag.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F
ifopn.o: $(INCDIR)/zhdim.F
igtio.o: $(INCDIR)/zhdim.F $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F \
	$(INCDIR)/zidim.F
igzio.o: $(INCDIR)/zhdim.F
ihdef.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F \
	$(INCDIR)/zhdim.F
ihist.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F \
	$(INCDIR)/zhdim.F
ihsub.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F \
	$(INCDIR)/zhdim.F
irdat.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F \
	$(INCDIR)/zidim.F
irwgd.o: $(INCDIR)/zhdim.F
is2po.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zidim.F \
	$(INCDIR)/zhdim.F
