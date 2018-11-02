include ../../Lmake.inc

include ../include/make.inc.$(ARC)

SDEC = dim.f

SINV = inverse.o

SCOV = mkcov.o

OFRC = ofrc.o

RFRC = rfrc.o

all: dec inv cov ofrc rfrc

dec: $(SDEC) ; \
     $(CP) ../include/dim_$(HRES)$(VRES)$(ZWTRN).f $(SDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(SDEC) > dimtmp.f ; \
     $(MV) dimtmp.f $(SDEC) 

inv: $(SINV) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SINV) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

cov: $(SCOV) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SCOV) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

ofrc: $(OFRC) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(OFRC) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

rfrc: $(RFRC) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(RFRC) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o *.f~ 

eclean:
	rm -f *.o *~ lin lin1v trn trn1v

.f.o : ; $(FORTRAN) $(OPTS) -c $<
