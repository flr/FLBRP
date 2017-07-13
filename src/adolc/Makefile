HEADERS = -I../../inst/include -I./

ADOLC_CXXFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0 -DADOLC_USE_CALLOC=1
ADOLC_CFLAGS= -DHAVE_MALLOC=1 -DHAVE_REALLOC=1 -DADOLC_VERSION=1 -DADOLC_SUBVERSION=9 -DADOLC_PATCHLEVEL=0 -DADOLC_USE_CALLOC=1



# From ADOLC Makefile
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
#ADCOBJS=adalloc.o malloc.o \
#	adouble.o avector.o convolut.o fortutils.o forward_partx.o \
#	fos_forward.o fos_reverse.o fov_forward.o fov_reverse.o \
#	hos_forward.o hos_ov_reverse.o hos_reverse.o \
#	hov_forward.o hov_reverse.o hov_wk_forward.o \
#	interfaces.o interfacesf.o \
#	taputil.o taputilc.o tayutil.o tayutilc.o zos_forward.o
#DRVOBJS=drivers/drivers.o drivers/driversf.o drivers/odedrivers.o \
#	drivers/odedriversf.o drivers/taylor.o
#SPAOBJS=sparse/int_forward_s.o sparse/int_forward_t.o \
#	sparse/int_reverse_s.o sparse/int_reverse_t.o \
#	sparse/jacutils.o sparse/sparse.o sparse/sparsedrivers.o \
#	sparse/graph_hess.o sparse/graph_jac.o sparse/hessutils.o
#TAPOBJS=tapedoc/tapedoc.o

library: lib

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -fPIC -c $<

%.o: %.cpp
	@echo CXX $<
	@clang++ -std=gnu++11 ${HEADERS} ${ADOLC_CXXFLAGS} -fPIC -c $<
#@${CXX} ${HEADERS} ${ADOLC_CXXFLAGS} -fPIC -c $<

LIB = ../adolc.a

lib: $(LIB)

$(LIB): $(ADCOBJS)
	@${AR} -rucs $(LIB) ${ADCOBJS}
	#@make -C drivers -f Makefile library
	#@make -C sparse -f Makefile library
	#@make -C tapedoc -f Makefile library
	#@${AR} -rucs $(LIB) ${ADCOBJS} ${DRVOBJS} ${SPAOBJS} ${TAPOBJS}

#clean:
#		@-rm -f $(LIB)
#		@-rm -f $(ADCOBJS)
#		@make -C drivers -f Makefile clean
#		@make -C sparse -f Makefile clean
#		@make -C tapedoc -f Makefile clean
