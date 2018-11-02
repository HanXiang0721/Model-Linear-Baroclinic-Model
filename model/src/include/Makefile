#  Makefile for CCSR libraries
#

include	../Mkinclude

DIR	      = include

FFLAGS 	      = $(STDFFLAGS) -I$(INCDIR) 

CFLAGS        = $(STDCFLAGS) -I$(INCDIR)

CPPFLAGS       = $(STDCPPFLAGS) $(CPPOPT)-I$(INCDIR) 

MAKEFILE      = Makefile

EXTHDRS	      =

HDRS	      =

OBJS	      = 

SRCS          = zccom.F \
		zcord.F \
		zcorg.F \
		zcdim.F \
		zadim.F \
		zddim.F \
		zgdim.F \
		zglev.F \
		zhdim.F \
		zidim.F \
		zkdim.F \
		zlsig.F \
		zpdim.F \
		zqdim.F \
		zqsat.F \
		zradim.F \
		zsdim.F \
		zwdim.F

SRCS_ALL      = zccom.F \
		zcord.F \
		zcorg.F \
		zcdim.F \
		zadim.F \
		zat10.F \
		zat106s5.F \
		zat21.F \
		zat213s5.F \
		zat21m.F \
		zat21x.F \
		zat21xm.F \
		zat42.F \
		zat42m.F \
		zat5.F \
		zat5m.F \
		zat63.F \
		zat63m.F \
		zav00.F \
		zav00m.F \
		zav02.F \
		zav02m.F \
		zddim.F \
		zgdim.F \
		zglev.F \
		zhdim.F \
		zidim.F \
		zkdim.F \
		zkg311c.F \
		zkg333c.F \
		zkl12c.F \
		zkl16c.F \
		zkl1c.F \
		zkl20c.F \
		zkl24c.F \
		zkl27c.F \
		zkl45c.F \
		zkl5c.F \
		zlg311c.F \
		zlg333c.F \
		zll12c.F \
		zll16c.F \
		zll1c.F \
		zll20c.F \
		zll24c.F \
		zll27c.F \
		zll45c.F \
		zll5c.F \
		zllsig.F \
		zlsig.F \
		zpcom.F \
		zpdim.F \
		zqdim.F \
		zqsat.F \
		zradim.F \
		zsdim.F \
		zwdim.F


SELECT	      = za$(RESOLUTION).sel

SELINC	      = zcdim.F zkdim.F zlsig.F 

DEPINC        = za$(HRESOLUTION).$(MEMORYSIZE)F \
		zk$(VRESOLUTION).F \
		zl$(VRESOLUTION).F

GSELECT	      = zg$(GRESOLUTION).sel

GSELINC	      = zgdim.F zglev.F

GDEPINC       = zk$(GRESOLUTION).F \
		zl$(GRESOLUTION).F

all: $(SELECT) $(GSELECT)

$(SELECT): $(SELINC) $(DEPINC)
		$(CP) za$(HRESOLUTION).$(MEMORYSIZE)F zadim.F
		$(CP) zk$(VRESOLUTION).F zkdim.F
		$(CP) zl$(VRESOLUTION).F zlsig.F
		$(ECHO) $(PROJECT) > za$(RESOLUTION).sel

$(GSELECT): $(GSELINC) $(GDEPINC)
		$(CP) zk$(GRESOLUTION).F zgdim.F
		$(CP) zl$(GRESOLUTION).F zglev.F
		$(ECHO) $(PROJECT) > zg$(GRESOLUTION).sel

clean:
		$(RM) *.o *~ core *.trace *.f *.html

index:
		@$(INDEX) $(HDRS) $(SRCS)

tags:
		@$(TAGS) $(HDRS) $(SRCS)

pack: $(SRCS:.F=.ftr)

ftr: $(SRCS:.F=.ftr)

rmftr:
	$(RM) $(FTRFILE)

html: $(SRCS:.F=.html)

.SUFFIXES : .mF .F

.F.mF:
	sed -e "s/IJSDIM *= *IDIM/IJSDIM = IJDIM/" $< > $@

###
