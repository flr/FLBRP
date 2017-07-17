HEADERS = -I../../inst/include -I../adolc
CXXFLAGS = -std=c++11

ADOLC_CXXFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0 -DADOLC_USE_CALLOC=1
ADOLC_CFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0 -DADOLC_USE_CALLOC=1

# List and order of files from ADOLC Makefile
ADCOBJS= adalloc.o rpl_malloc.o adouble.o \
	convolut.o fortutils.o interfaces.o interfacesf.o \
	taping.o tape_handling.o zos_forward.o fos_forward.o \
	fov_forward.o hos_forward.o hov_forward.o hov_wk_forward.o \
	fos_reverse.o fov_reverse.o hos_reverse.o hos_ov_reverse.o \
	hov_reverse.o forward_partx.o zos_pl_forward.o \
	fos_pl_reverse.o fos_pl_sig_reverse.o fos_pl_forward.o \
	fov_pl_forward.o fos_pl_sig_forward.o fov_pl_sig_forward.o \
	externfcts.o checkpointing.o fixpoint.o \
	fov_offset_forward.o revolve.o advector.o adouble_tl.o \
	param.o externfcts2.o

library: lib

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -c $<

%.o: %.cpp
	@echo CXX $<
	@${CXX} ${CXXFLAGS} ${HEADERS} ${ADOLC_CXXFLAGS} -c $<

LIB = ../adolc.a

lib: $(LIB)

$(LIB): $(ADCOBJS)
	@${AR} -rucs $(LIB) ${ADCOBJS}

clean:
		@-rm -f $(LIB)
		@-rm -f $(ADCOBJS)
