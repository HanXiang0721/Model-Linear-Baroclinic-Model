#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = admin

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      = $(INCDIR)/zccom.F \
		$(INCDIR)/zcdim.F \
		$(INCDIR)/zgdim.F \
		$(INCDIR)/zhdim.F \
		$(INCDIR)/zkdim.F \
		$(INCDIR)/zlsig.F \
		$(INCDIR)/zpdim.F

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = aadmn.o \
		ahist.o \
		apcon.o \
		aquit.o \
		asetc.o \
		asets.o \
		astep.o \
		astrt.o \
		astup.o \
		atime.o \
		auser.o

SRCS	      = aadmn.F \
		ahist.F \
		apcon.F \
		aquit.F \
		asetc.F \
		asets.F \
		astep.F \
		astrt.F \
		astup.F \
		atime.F \
		auser.F

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
aadmn.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F \
	$(INCDIR)/zccom.F $(INCDIR)/zpdim.F
aparm.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F \
	$(INCDIR)/zccom.F
apcon.o: $(INCDIR)/zccom.F
aquit.o: $(INCDIR)/zhdim.F
asetc.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F
asetl.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F
asets.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zlsig.F \
	$(INCDIR)/zhdim.F $(INCDIR)/zccom.F
astep.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zccom.F
astrt.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zpdim.F \
	$(INCDIR)/zgdim.F $(INCDIR)/zhdim.F
auser.o: $(INCDIR)/zcdim.F $(INCDIR)/zkdim.F $(INCDIR)/zhdim.F \
	$(INCDIR)/zccom.F
