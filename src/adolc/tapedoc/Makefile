HEADERS=-I../../../inst/include

TAPOBJS=tapedoc.o

%.o: %.c
	@echo CC  $<
	@${CC} ${HEADERS} ${ADOLC_CFLAGS} -c $<
	
library: $(TAPOBJS)

clean:
	@-rm -f *.o
