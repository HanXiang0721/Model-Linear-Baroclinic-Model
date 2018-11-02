#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = dynamics

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      = $(INCDIR)/zccom.F \
		$(INCDIR)/zcdim.F \
		$(INCDIR)/zddim.F \
		$(INCDIR)/zgdim.F \
		$(INCDIR)/zhdim.F \
		$(INCDIR)/zkdim.F \
		$(INCDIR)/zpdim.F

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = dadmn.o \
		dadvn.o \
		ddiag.o \
		ddifc.o \
		dgdyn.o \
		dhydr.o \
		dintg.o \
		dmfix.o \
		dmtrx.o \
		dsetc.o \
		dsetd.o \
		dsetv.o \
		dsphe.o \
		dstep.o \
		dterm.o \
		dtrcr.o \
		dvect.o \
		dvtmp.o

SRCS	      = dadmn.F \
		dadvn.F \
		ddiag.F \
		ddifc.F \
		dgdyn.F \
		dhydr.F \
		dintg.F \
		dmfix.F \
		dmtrx.F \
		dsetc.F \
		dsetd.F \
		dsetv.F \
		dsphe.F \
		dstep.F \
		dterm.F \
		dtrcr.F \
		dvect.F \
		dvtmp.F

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
dadmn.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zhdim.F $(INCDIR)/zccom.F
dadvn.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F
ddiag.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zccom.F
ddifc.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zpdim.F $(INCDIR)/zgdim.F $(INCDIR)/zccom.F
dg2wd.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zpdim.F $(INCDIR)/zgdim.F
dgdyn.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F
dgeng.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zpdim.F $(INCDIR)/zgdim.F
dhydr.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zccom.F
dintg.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zpdim.F $(INCDIR)/zgdim.F
dmfix.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F
dmtrx.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zccom.F
dsetc.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zccom.F
dsetv.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zccom.F
dsphe.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zccom.F
dstep.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zpdim.F $(INCDIR)/zgdim.F $(INCDIR)/zccom.F
dterm.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F
dvect.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zddim.F \
	$(INCDIR)/zccom.F
dvtmp.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zccom.F
