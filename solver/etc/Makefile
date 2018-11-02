include ../../Lmake.inc

include ../include/make.inc.$(ARC)

SDEC = dim.f

RANDOM = random.o

OUTSTR = outstr.o

CHKVL = chkvl.o

OUTS2P = outs2p.o

DF1 = dfeed_lib1.o

DF2 = dfeed_lib2.o

DF3 = dfeed_lib3.o

DF4 = dfeed_lib4.o

all: dec random outstr chkvl outs2p

stm: dec random outstr chkvl outs2p

dfeed: dec dfeed_lib1 dfeed_lib2 dfeed_lib3 dfeed_lib4

dec: $(SDEC) ; \
     $(CP) ../include/dim_$(HRES)$(VRES)$(ZWTRN).f $(SDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(SDEC) > dimtmp.f ; \
     $(MV) dimtmp.f $(SDEC) 

random: $(RANDOM) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(RANDOM) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

outstr: $(OUTSTR) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(OUTSTR) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

chkvl: $(CHKVL) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(CHKVL) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

outs2p: $(OUTS2P) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(OUTS2P) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

dfeed_lib1: $(DF1) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(DF1) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

dfeed_lib2: $(DF2) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(DF2) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

dfeed_lib3: $(DF3) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(DF3) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

dfeed_lib4: $(DF4) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(DF4) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o *~

.f.o : ; $(FORTRAN) $(OPTS) -c $<
