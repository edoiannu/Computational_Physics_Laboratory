#include <iostream>
using namespace std;
#include <cstdlib>


#define NTHREADS 1024
#define NBLOCKS  32


#define N 16




///////////////////////////////////////////////////////////
__global__ void matr_prod(float *a, float *b, float *c){


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

//////////////////////////////////////////////////////////////
void matrix_product_gpu(float *a, float *b, float *cgpu){
float *dev_a, *dev_b, *dev_c;


cudaMalloc( (void **)&dev_a, N*N*sizeof(float));
cudaMalloc( (void **)&dev_b, N*N*sizeof(float));
cudaMalloc( (void **)&dev_c, N*N*sizeof(float));

cudaMemcpy( dev_a, a, N*N*sizeof(float), cudaMemcpyHostToDevice);
cudaMemcpy( dev_b, b, N*N*sizeof(float), cudaMemcpyHostToDevice);

matr_prod<<<N,N>>>(dev_a, dev_b, dev_c);

cudaMemcpy( cgpu, dev_c, N*N*sizeof(float), cudaMemcpyDeviceToHost);

cudaFree(dev_a);
cudaFree(dev_b);
cudaFree(dev_c);


}



//////////////////////////////////////////////////////////////
void matrix_product_cpu(float *a, float *b, float *c){
int l;

for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

          l=j+i*N;
          c[l]=0;

	  for( int k=0; k<N; k++){
               c[l]+= a[i*N+k]*b[j+k*N];
	       };

};
};

return;

}

/////////////////////////////////////////////////////////////
int main( void ){

float *a, *b, *c, *cgpu;
int l;

a = (float *)malloc(N*N*sizeof(float));
b = (float *)malloc(N*N*sizeof(float));
c = (float *)malloc(N*N*sizeof(float));
cgpu = (float *)malloc(N*N*sizeof(float));

cudaEvent_t start1,stop1,start2,stop2;
cudaEventCreate(&start1);
cudaEventCreate(&stop1);
cudaEventCreate(&start2);
cudaEventCreate(&stop2);
float elapsed;




for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

     l=j+i*N;
     a[l]=drand48();
     b[l]=drand48();
     };
};



cudaEventRecord(start1,0);
   matrix_product_cpu(a,b,c);
cudaEventRecord(stop1,0);
cudaEventSynchronize(stop1);
cudaEventElapsedTime(&elapsed, start1, stop1);
cout << N << " cpu  " << elapsed/1000. << "\n";


cudaEventRecord(start2,0);
   matrix_product_gpu(a,b,cgpu);
cudaEventRecord(stop2,0);
cudaEventSynchronize(stop2);
cudaEventElapsedTime(&elapsed, start2, stop2);
cout << N << " gpu  " << elapsed/1000. << "\n";




for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

          l=j+i*N;
//if (abs(c[l]/cgpu[l]-1.) > 0.0001)
//    cout << l << "  " << c[l]/cgpu[l] << "\n";
    cout << l << "  " << c[l]<< "  " << cgpu[l] << "\n";

};
};

cudaEventDestroy(start1);
cudaEventDestroy(start2);
cudaEventDestroy(stop1);
cudaEventDestroy(stop2);

return 0;

}