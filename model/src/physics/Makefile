#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = physics

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      = $(INCDIR)/zccom.F \
		$(INCDIR)/zcdim.F \
		$(INCDIR)/zgdim.F \
		$(INCDIR)/zglev.F \
		$(INCDIR)/zhdim.F \
		$(INCDIR)/zkdim.F \
		$(INCDIR)/zpdim.F \
		$(INCDIR)/zqsat.F \
		$(INCDIR)/zradim.F \
		$(INCDIR)/zsdim.F

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = pmisc.o \
		pshl0.o \
		pstep.o \
		pvdfm.o \
		pvdfy.o

SRCS	      = pmisc.F \
		pshl0.F \
		pstep.F \
		pvdfm.F \
		pvdfy.F

SRCS_ALL      = pmisc.F \
		pshl0.F \
		pstep.F \
		pvdfm.F \
		pvdfy.F

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
pmisc.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F
pshl0.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zccom.F
pstep.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F
pvdfm.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F $(INCDIR)/zqsat.F
pvdfy.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zccom.F
