#
#       Makefile for $(LBMDIR)/src
#       

include	./Mkinclude

DIR		= 

LIBSUBDIRS	= admin dynamics physics io util sysdep
LIBSUBDIRSNP	= admin dynamics io util sysdep
SUBDIRS		= $(LIBSUBDIRS) include $(SPECIALDIR) 

FFLAGS 	       = $(STDFFLAGS) -I$(INCDIR) 
CFLAGS         = $(STDCFLAGS) -I$(INCDIR)
CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 
LDFLAGS        = $(STDLDFLAGS)

OBJS	       = $(MAIN)

SRCS	       = $(MAIN:.o=.F)
SRCS_ALL       = atmmain.F cplmain.F

NONSTDS	       = $(SPECIAL) $(NONSTD)

EXTHDRS	      = $(INCDIR)/zccom.F \
		$(INCDIR)/zcdim.F \
		$(INCDIR)/zhdim.F

HDRS	      =

MAKEFILE      = Makefile

all:	lbm

world:	dirs lib lbm

lbm:	$(LBM)

$(LBM):		includes speciallib $(MAIN) $(SYSLIBY)
	$(LD) $(LDFLAGS) $(OBJS) $(NONSTDS) $(LIBRARY) $(SYSLIBY) $(SYSLIB) -o $@
	$(RM) $(MAIN) $(NONSTDS)

lib:	includes 
	@echo "Making $(LIBRARY)"
	@for i in $(LIBSUBDIRS) ; do \
	  cd $$i ; \
	  echo "Making $$i/" ; \
	  $(MAKE) ; \
	  cd .. ; \
	done

libnp:	includes 
	@echo "Making $(LIBRARY)"
	@for i in $(LIBSUBDIRSNP) ; do \
	  cd $$i ; \
	  echo "Making $$i/" ; \
	  $(MAKE) ; \
	  cd .. ; \
	done

includes:
	@cd include; $(MAKE) "SYSTEM=$(SYSTEM)"

adminlib:
	@cd admin; $(MAKE) "SYSTEM=$(SYSTEM)"

dynamicslib:
	@cd dynamics; $(MAKE) "SYSTEM=$(SYSTEM)"

iolib:
	@cd io; $(MAKE) "SYSTEM=$(SYSTEM)"

physicslib:
	@cd physics; $(MAKE) "SYSTEM=$(SYSTEM)"

sysdeplib:
	@cd sysdep; $(MAKE) "SYSTEM=$(SYSTEM)"

utillib:
	@cd util; $(MAKE) "SYSTEM=$(SYSTEM)"

speciallib:
	@cd $(SPECIALDIR); $(MAKE) "SYSTEM=$(SYSTEM)"

dirs:
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	test -d $(BINDIR) || mkdir -p $(BINDIR)

clean: clean.local clean.dir

clean.local:
	$(RM) *.o *~ core *.trace *.f $(FTRFILE) *.html all.F \
	SPECIALLIST NONSTDLIST

clean.dir:
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  echo "Cleaning $$i/" ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" clean ; \
	  cd .. ; \
	done

clean.special:
	@for i in $(SPECIALDIR); do \
	  cd $$i ; \
	  echo "Cleaning $$i/" ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" clean ; \
	  cd .. ; \
	done

tags: tags.local tags.dir

tags.local:
	@$(TAGS) $(SRCS)
	@for i in $(LIBSUBDIRS) ; do \
	  $(TAGS) -a $$i/*.F ; \
	done	

tags.dir:
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" tags ; \
	  cd .. ; \
	done

index: index.local index.dir


index.local:
		@$(INDEX) $(HDRS) $(SRCS)

index.dir:
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" index ; \
	  cd .. ; \
	done

index.head:
	cut -c1-10,22-30,45- INDEX > HEAD	

pack:	 rmftr includes pack.local pack.dir

ftr:	includes ftr.local ftr.dir

ftr.local: $(SRCS_ALL:.F=.ftr)

pack.local: $(SRCS:.F=.ftr)

ftr.dir:
	@for i in $(LIBSUBDIRS) ; do \
	  cd $$i ; \
	  echo "FTRing $$i/" ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" ftr; \
	  cd .. ; \
	done	

pack.dir:
	@for i in $(LIBSUBDIRS) ; do \
	  cd $$i ; \
	  echo "Packing $$i/" ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" pack; \
	  cd .. ; \
	done	

rmftr:
	$(RM) $(FTRFILE)

srcall.F: includes pack
	$(MV) $(FTRFILE) srcall.F

srcall.f: srcall.F
	$(CPP) $(CPPFLAGS) srcall.F  -I$(INCDIR) > srcall.f

size:	srcall.f
	../sh/util/fmsize -d -c -l srcall.f | ../sh/util/fmsort > SIZE.$(RESOLUTION)$(MEMORYSIZE)

ftnchk: srcall.f
	ftnchek -nopretty -notruncation -array=0 srcall.f > FTNCHK

tree: TREE.all

TREE.all: srcall.f ../sh/util/fcgx
	FCG_MAXLAY=7 FCG_TRUNC='DYNMCS,PHYSCS,RDDAT,HISTIN,HISTIF,GDREAD,G2W,W2G,FFT99X' ../sh/util/fcgx < srcall.f > TREE.all

../sh/util/fcgx:
	$(FC) -o ../sh/util/fcgx ../sh/util/fcgx.F

sublist: sublist.local sublist.dir
	@for i in . $(SPECIALDIR) nonstd $(LIBSUBDIRS) ; do \
	   cat $$i/SUBLIST ; \
	done | \
        sort +0 -2 | sort +0 -1 -u  > SUBLIST.all
	../sh/util/e2j SUBLIST.all > SUBLIST.jpn

sublist.local:
	egrep '( PROGRAM| SUBROUTINE | ENTRY  )' $(SRCS) /dev/null | $(AWK) -f ../sh/util/sublist | sort > SUBLIST

sublist.dir: 
	@for i in $(LIBSUBDIRS) nonstd $(SPECIALDIR); do \
	  cd $$i ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" sublist; \
	  cd .. ; \
	done	

html:	clean.html sublist html.local html.dir html.list html.tree

clean.html:
	$(RM) *.html */*.html

html.local: $(SRCS_ALL:.F=.html)

html.dir:
	@for i in $(SUBDIRS) ; do \
	  cd $$i ; \
	  $(MAKE) "SYSTEM=$(SYSTEM)" html; \
	  cd .. ; \
	done	

html.list:
	$(LBMDIR)/sh/util/makehtmllist SUBLIST.jpn > SUBLIST.html

html.tree: TREE.all
	$(LBMDIR)/sh/util/makehtmltree TREE.all > TREE.html

install.html:
	tar cf - *.html */*.html | ( cd $(HTML_LBM)/src; tar xvf - )

###
atmmain.o: $(INCDIR)/zcdim.F $(INCDIR)/zpdim.F $(INCDIR)/zwdim.F \
	   $(INCDIR)/zhdim.F $(INCDIR)/zccom.F $(INCDIR)/zcord.F
cplmain.o: $(INCDIR)/zcdim.F $(INCDIR)/zpdim.F $(INCDIR)/zwdim.F \
	   $(INCDIR)/zhdim.F $(INCDIR)/zccom.F $(INCDIR)/zcord.F

