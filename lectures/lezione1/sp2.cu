#include <iostream>
using namespace std;
#include <cstdlib>


#define NTHREADS 1024
#define NBLOCKS  32


#define N 1000

///////////////////////////////////////////////////////////
__global__ void scalar_prod(float *a, float *b, float *c){

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

if (cacheIndex==0)
   c[blockIdx.x]=cachearray[0];


}



int main( void ){

float *a, *b, *partial_c;
float *dev_a, *dev_b, *dev_partial_c;
float sp;


a = (float *)malloc(N*sizeof(float));
b = (float *)malloc(N*sizeof(float));
partial_c = (float *)malloc(NBLOCKS*sizeof(float));


for( int i=0; i<N; i++){
a[i]=pow(0.3333,i+1);
b[i]=0.5;
};


cudaMalloc( (void **)&dev_a, N*sizeof(float));
cudaMalloc( (void **)&dev_b, N*sizeof(float));
cudaMalloc( (void **)&dev_partial_c, NBLOCKS*sizeof(float));


cudaMemcpy( dev_a, a, N*sizeof(float), cudaMemcpyHostToDevice);
cudaMemcpy( dev_b, b, N*sizeof(float), cudaMemcpyHostToDevice);

scalar_prod<<<32,32>>>(dev_a, dev_b, dev_partial_c);

cudaMemcpy( partial_c, dev_partial_c, NBLOCKS*sizeof(float), cudaMemcpyDeviceToHost);

sp=0;
for( int i=0; i<NBLOCKS; i++){
cout << i << "  " << partial_c[i] << "\n";
sp+=partial_c[i];
};

cout << "scalar product a.b = " << sp << "\n";

cudaFree(dev_a);
cudaFree(dev_b);
cudaFree(dev_partial_c);


return 0;

}