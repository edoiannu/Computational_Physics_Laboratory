NVCC=nvcc -arch=sm_20

LIBS=

OBJS=

INCLUDE=





### basic version with limited number of elements;
### all products are summed on the host
#default: matr0.x
#default: matr0timing.x
default: matr1.x





#########################################
%.x: %.o
	$(NVCC) $(OBJS) $(LIBS) $< -o $@

matr0.o: matr0.cu
	$(NVCC) -c $(INCLUDE) matr0.cu

matr0timing.o: matr0timing.cu
	$(NVCC) -c $(INCLUDE) matr0timing.cu

matr1.o: matr1.cu
	$(NVCC) -c $(INCLUDE) matr1.cu


clean:
	rm -f *.x *.o

