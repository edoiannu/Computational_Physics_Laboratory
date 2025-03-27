#include <iostream>
using namespace std;
#include <cstdlib>
#include "lock.h"

#define NTHREADS 1024
#define NBLOCKS  32


#define N 10000

///////////////////////////////////////////////////////////
__global__ void scalar_prod(Lock lock, float *a, float *b, float *c){

int offset=blockDim.x*gridDim.x;

int tid = threadIdx.x + blockDim.x*blockIdx.x;

int cacheIndex = threadIdx.x;

float temp=0;

__shared__ float cachearray[NTHREADS];


////////////////////////

while (tid < N){

temp += a[tid]*b[tid];

tid+=offset;

};

cachearray[cacheIndex] = temp;

////////////////////////

__syncthreads();

////////////////////////

int i=blockDim.x/2;

while(i!=0){

if (cacheIndex < i)
   cachearray[cacheIndex]+=cachearray[cacheIndex+i];

__syncthreads();

i/=2;

};

////////////////////////


  if (cacheIndex ==0){
    lock.lock();
    *c += cachearray[0];
    lock.unlock();
  }


}



/////////////////////////////////////////////////////////////
int main( void ){

float *a, *b;
float *dev_a, *dev_b, *dev_c;
float sp;
Lock lock;

a = (float *)malloc(N*sizeof(float));
b = (float *)malloc(N*sizeof(float));


for( int i=0; i<N; i++){
a[i]=pow(0.33333333,i+1);
b[i]=0.5;
};


cudaMalloc( (void **)&dev_a, N*sizeof(float));
cudaMalloc( (void **)&dev_b, N*sizeof(float));
cudaMalloc( (void **)&dev_c, sizeof(float));


cudaMemcpy( dev_a, a, N*sizeof(float), cudaMemcpyHostToDevice);
cudaMemcpy( dev_b, b, N*sizeof(float), cudaMemcpyHostToDevice);

scalar_prod<<<32,32>>>(lock, dev_a, dev_b, dev_c);

cudaMemcpy( &sp, dev_c, sizeof(float), cudaMemcpyDeviceToHost);



cout << "scalar product a.b = " << sp << "\n";

cudaFree(dev_a);
cudaFree(dev_b);
cudaFree(dev_c);


return 0;

}