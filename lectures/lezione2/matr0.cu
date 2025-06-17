#include <iostream>
using namespace std;
#include <cstdlib>


#define NTHREADS 1024
#define NBLOCKS  32


#define N 512




///////////////////////////////////////////////////////////
__global__ void matr_prod(float *a, float *b, float *c){

int offset=blockDim.x*gridDim.x;

int tid = threadIdx.x;  // colonne
int bid = blockIdx.x;   // righe

int l = tid+N*bid; // elemento

float temp=0;
for(int k=0; k<N; k++){
temp+=a[N*bid+k]*b[tid+N*k];
};

c[l]=temp;

}



/////////////////////////////////////////////////////////////
int main( void ){

float *a, *b, *c, *cgpu;
float *dev_a, *dev_b, *dev_c;
float sp;
int l;

a = (float *)malloc(N*N*sizeof(float));
b = (float *)malloc(N*N*sizeof(float));
c = (float *)malloc(N*N*sizeof(float));
cgpu = (float *)malloc(N*N*sizeof(float));


for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

     l=j+i*N;
     a[l]=drand48();
     b[l]=drand48();
     };
};

for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

          l=j+i*N;
          c[l]=0;

	  for( int k=0; k<N; k++){
               c[l]+= a[i*N+k]*b[j+k*N];
	       };

// cout << i << "," << j << "  " << a[l] << "   " << b[l] << "  " << c[l] << "\n";

};
};




cudaMalloc( (void **)&dev_a, N*N*sizeof(float));
cudaMalloc( (void **)&dev_b, N*N*sizeof(float));
cudaMalloc( (void **)&dev_c, N*N*sizeof(float));

cudaMemcpy( dev_a, a, N*N*sizeof(float), cudaMemcpyHostToDevice);
cudaMemcpy( dev_b, b, N*N*sizeof(float), cudaMemcpyHostToDevice);

matr_prod<<<N,N>>>(dev_a, dev_b, dev_c);

cudaMemcpy( cgpu, dev_c, N*N*sizeof(float), cudaMemcpyDeviceToHost);

for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

          l=j+i*N;
cout << l << "  " << c[l]/cgpu[l] << "\n";

};
};

cudaFree(dev_a);
cudaFree(dev_b);
cudaFree(dev_c);


return 0;

}