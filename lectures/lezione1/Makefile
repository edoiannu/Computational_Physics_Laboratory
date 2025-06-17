NVCC=nvcc -arch=sm_20

LIBS=

OBJS=

INCLUDE=





### basic version with limited number of elements;
### all products are summed on the host
#default: sp0.x

### basic version with arbitrary number of elements
### all products are summed on the host
#default: sp1.x


### intermediate version with arbitrary number of elements
### partial sums are performed in each block using the shared memory
#default: sp2.x

### intermediate version with arbitrary number of elements
### partial sums are performed in each block using the shared memory
### the final sum is computed on the GPU by one single thread with a for loop
#default: sp3naive.x

### final version with arbitrary number of elements
### partial sums are performed in each block using the shared memory
### the final sum is computed on the GPU by one all the threads 0 of the
### different blocks with a lock
default: sp3.x





#########################################
%.x: %.o
	$(NVCC) $(OBJS) $(LIBS) $< -o $@

sp0.o: sp0.cu
	$(NVCC) -c $(INCLUDE) sp0.cu

sp1.o: sp1.cu
	$(NVCC) -c $(INCLUDE) sp1.cu

sp2.o: sp2.cu
	$(NVCC) -c $(INCLUDE) sp2.cu

sp3naive.o: sp3naive.cu
	$(NVCC) -c $(INCLUDE) sp3naive.cu

sp3.o: sp3.cu
	$(NVCC) -c $(INCLUDE) sp3.cu



clean:
	rm -f *.x *.o

