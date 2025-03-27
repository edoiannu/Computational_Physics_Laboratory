#include <iostream>
using namespace std;
#include <cstdlib>

#define N 1025


__global__ void scalar_prod(int *a, int *b, int *c){

int offset=blockDim.x*gridDim.x;

int tid = threadIdx.x + blockDim.x*blockIdx.x;

while (tid < N){

c[tid] = a[tid]*b[tid];

tid+=offset;
};

}



int main( void ){

int a[N], b[N], c[N];
int *dev_a, *dev_b, *dev_c;
int sp;

for( int i=0; i<N; i++){
a[i]=2;
b[i]=1;
};


cudaMalloc( (void **)&dev_a, N*sizeof(int));
cudaMalloc( (void **)&dev_b, N*sizeof(int));
cudaMalloc( (void **)&dev_c, N*sizeof(int));


cudaMemcpy( dev_a, a, N*sizeof(int), cudaMemcpyHostToDevice);
cudaMemcpy( dev_b, b, N*sizeof(int), cudaMemcpyHostToDevice);

scalar_prod<<<32,32>>>(dev_a, dev_b, dev_c);

cudaMemcpy( &c, dev_c, N*sizeof(int), cudaMemcpyDeviceToHost);

sp=0;
for( int i=0; i<N; i++){
sp+=c[i];
};

cout << "scalar product a.b = " << sp << "\n";

cudaFree(dev_a);
cudaFree(dev_b);
cudaFree(dev_c);


return 0;

}