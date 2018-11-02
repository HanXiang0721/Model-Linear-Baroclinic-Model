include ../../Lmake.inc

include ../include/make.inc.$(ARC)

SDEC = dim.f

SLIN = lin.o

SLINPWF = linpwf.o

SLINBR = linbr.o

STRN = trn.o

STRNBR = trnbr.o

all: dec lin trn

pwf: dec linpwf trn

br: dec linbr trnbr

dec: $(SDEC) ; \
     $(CP) ../include/dim_$(HRES)$(VRES)$(ZWTRN).f $(SDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(SDEC) > dimtmp.f ; \
     $(MV) dimtmp.f $(SDEC)

lin: $(SLIN) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SLIN) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

linpwf: $(SLINPWF) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SLINPWF) $(LAPACKLIB) $(BLASLIB)

linbr: $(SLINBR) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SLINBR) $(LAPACKLIB) $(BLASLIB)

trn: $(STRN) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(STRN) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

trnbr: $(STRNBR) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(STRNBR) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o *.f~ 

eclean:
	rm -f *.o *~ lin linpw linbr trn trnpw trnbr

.f.o : ; $(FORTRAN) $(OPTS) -c $<
