#  Makefile for CCSR libraries
#

include	../../Mkinclude

DIR	      = mkamatpw

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS      = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) $(CPPOPT)$(OPTIONS_PROJ) 

LDFLAGS	      = $(STDLDFLAGS)

EXTHDRS	      = $(INCDIR)/zccom.F \
		$(INCDIR)/zcdim.F \
		$(INCDIR)/zgdim.F \
		$(INCDIR)/zhdim.F \
		$(INCDIR)/zkdim.F \
		$(INCDIR)/zpdim.F \
		$(INCDIR)/zqsat.F

HDRS	      =

MAKEFILE      = Makefile

OBJS	      = $(SPECIAL)

SRCS	      = $(OBJS:.o=.F)

SRCS_ALL      = 


all:		$(OBJS)
	echo $(OBJS) > SPECIALLIST
	cp $(OBJS) SPECIALLIST $(SRCDIR)

clean:
		$(RM) *.o *~ core *.trace *.f *.html SPECIALLIST

index:
		@$(INDEX) $(HDRS) $(SRCS)

tags:
		@$(TAGS) $(HDRS) $(SRCS)

pack: $(SRCS_ALL:.F=.ftr)

ftr: $(SRCS_ALL:.F=.ftr)

rmftr:
	$(RM) $(FTRFILE)

sublist: 
	egrep '( SUBROU| ENTRY  )' $(SRCS) /dev/null | $(AWK) -f $(LBMDIR)/sh/util/sublist dir=$(DIR) | sort > SUBLIST


html: $(SRCS_ALL:.F=.html)

###
