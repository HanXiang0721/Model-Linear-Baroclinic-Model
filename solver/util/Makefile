include ../../Lmake.inc

include ../include/make.inc.$(ARC)

SDEC = dim.f

CDEC = hdim.f

BSGRD = bsgrd.o

REDST = redist.o

MKFRC = mkfrcng.o

MYMKFRC = mymkfrcng.o

MYMKFRC222 = mymkfrcng222.o

MKFRCS = mkfrcsst.o

MKFRCB = mkfrcbr.o

FVEC  = fvec.o

GTGR  = gt2gr.o

NCPBS = ncepsbs.o

NCPBBS = ncep1vbs.o

ECMBS = ecmsbs.o

all: dec hdec bsgrd redist mkfrcng mkfrcsst fvec gt2gr ncepsbs ncep1vbs ecmsbs mymkfrcng mymkfrcng222

bs: dec hdec bsgrd ncepsbs ncep1vbs ecmsbs

br: dec hdec mkfrcbr gt2gr ncep1vbs

dec: $(SDEC) ; \
     $(CP) ../include/dim_$(HRES)$(VRES)$(ZWTRN).f $(SDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(SDEC) > dimtmp.f ; \
     $(MV) dimtmp.f $(SDEC) 

hdec: $(CDEC) ; \
     $(CP) ../include/hdim_$(HRES).f $(CDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(CDEC) > hdimtmp.f ; \
     $(MV) hdimtmp.f $(CDEC) 

bsgrd: $(BSGRD) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(BSGRD) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

redist: $(REDST) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(REDST) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

mkfrcng: $(MKFRC) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(MKFRC) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

mymkfrcng: $(MYMKFRC) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(MYMKFRC) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

mymkfrcng222: $(MYMKFRC222) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(MYMKFRC222) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

mkfrcsst: $(MKFRCS) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(MKFRCS) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

mkfrcbr: $(MKFRCB) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(MKFRCB) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

fvec:	 $(FVEC) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(FVEC) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

gt2gr:	 $(GTGR) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(GTGR) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

ncepsbs: $(NCPBS) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(NCPBS) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

ncep1vbs: $(NCPBBS) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(NCPBBS) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

ecmsbs: $(ECMBS) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(ECMBS) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o *~ dec hdec bsgrd redist mkfrcng mkfrcsst fvec gt2gr ncepsbs ncep1vbs ecmsbs mymkfrcng mymkfrcng222

.f.o : ; $(FORTRAN) $(OPTS) -c $<
