include ../../Lmake.inc

include ../include/make.inc.$(ARC)

SDEC = dim.f

SEOF = eof.o

SEOFB = eofbr.o

SSVD = svd.o

SSVDB = svdbr.o

SETRN = etrn.o

SETRNB = etrnbr.o

SSTRN = strn.o

SSTRNB = strnbr.o

all: dec eof svd etrn strn 

br: dec svdbr strnbr eofbr etrnbr

dec: $(SDEC) ; \
     $(CP) ../include/dim_$(HRES)$(VRES)$(ZWTRN).f $(SDEC) ; \
     sed -e 's/BYTE/'$(MBYT)'/g' $(SDEC) > dimtmp.f ; \
     $(MV) dimtmp.f $(SDEC) 

eof: $(SEOF) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SEOF) $(LAPACKLIB) $(BLASLIB)

eofbr: $(SEOFB) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SEOFB) $(LAPACKLIB) $(BLASLIB)

svd: $(SSVD) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SSVD) $(LAPACKLIB) $(BLASLIB)

svdbr: $(SSVDB) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SSVDB) $(LAPACKLIB) $(BLASLIB)

etrn: $(SETRN) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SETRN) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

etrnbr: $(SETRNB) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SETRNB) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

strn: $(SSTRN) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SSTRN) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

strnb: $(SSTRNB) ; \
     $(LOADER) $(LOADOPTS) -o $@ \
     $(SSTRNB) $(LAPACKLIB) $(BLASLIB) $(LDLIBS)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o *.f~ 

eclean:
	rm -f *.o *~ eof svd etrn strn svd1v strn1v svdzm strnzm svd15 strn15

.f.o : ; $(FORTRAN) $(OPTS) -c $<



