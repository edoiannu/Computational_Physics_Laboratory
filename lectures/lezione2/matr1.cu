#include <iostream>
using namespace std;
#include <cstdlib>
#include <cmath>

#define NTHREADS 32



#define N 2


///////////////////////////////////////////////////////////
// kernel che effettuano il calcolo
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

__global__ void matr_prod_shared(float *A, float *B, float *C, int width){

    int ib = blockIdx.y;
    int jb = blockIdx.x;                
    int it = threadIdx.y;
    int jt = threadIdx.x;

    int aBegin = width*NTHREADS*ib;
    int aEnd   = aBegin + width - 1;
    int aStep  = NTHREADS;
    
    int bBegin = NTHREADS*jb;
    int bStep  = NTHREADS * width;

float Csub=0;

for( int a = aBegin, b = bBegin; a<= aEnd; a+=aStep, b+=bStep){

__shared__ float As[NTHREADS][NTHREADS];
__shared__ float Bs[NTHREADS][NTHREADS];

As[it][jt] = A[a + width*it + jt];
Bs[it][jt] = A[b + width*it + jt];
__syncthreads();


for(int k=0; k<NTHREADS; ++k)
    Csub+=As[it][k]*Bs[k][jt];


__syncthreads();

}

int c = width*NTHREADS*ib + NTHREADS*jb;
C[ c + width*it + jt] = Csub;

};



__global__ void completa_matrice(float *a, int num){

        int i = blockIdx.y*blockDim.y + threadIdx.y;
        int j = blockIdx.x*blockDim.x + threadIdx.x;
        int offset = gridDim.x*blockDim.x*i + j;
        
        if(i >= num || j >= num) 
                a[offset] = 0.;


}

/////////////////////////////////////////////////////////////
// kernel che gestiscono l'esecuzione sulla GPU
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
void matrix_product_gpu_shared(float *a, float *b, float *cgpush){
float *dev_a, *dev_b, *dev_c;
float *check;


int nblocks = (N+NTHREADS-1)/NTHREADS;
int width = nblocks*NTHREADS;


cout << "width=" << width << "\n";

check = (float *)malloc(width*width*sizeof(float));


cudaMalloc( (void **)&dev_a, width*width*sizeof(float));
cudaMalloc( (void **)&dev_b, width*width*sizeof(float));
cudaMalloc( (void **)&dev_c, width*width*sizeof(float));

cudaMemcpy2D( dev_a, width*sizeof(float), a, N*sizeof(float), N*sizeof(float),N,cudaMemcpyHostToDevice);
cudaMemcpy2D( dev_b, width*sizeof(float), b, N*sizeof(float), N*sizeof(float),N,cudaMemcpyHostToDevice);

dim3 c_threads(NTHREADS,NTHREADS);
dim3 c_blocks(nblocks,nblocks);
dim3 threads(NTHREADS,NTHREADS);
dim3 blocks(nblocks,nblocks);

 if(N % NTHREADS != 0) {
                completa_matrice<<<c_blocks,c_threads>>>(dev_a, width);
                completa_matrice<<<c_blocks,c_threads>>>(dev_b, width);
        };

matr_prod_shared<<<blocks,threads>>>(dev_a, dev_b, dev_c, width);

cudaMemcpy2D( cgpush, N*sizeof(float), dev_c, width*sizeof(float), N*sizeof(float),N,cudaMemcpyDeviceToHost);

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

float *a, *b, *c, *cgpu, *cgpush;
int l;

a = (float *)malloc(N*N*sizeof(float));
b = (float *)malloc(N*N*sizeof(float));
c = (float *)malloc(N*N*sizeof(float));
cgpu = (float *)malloc(N*N*sizeof(float));
cgpush = (float *)malloc(N*N*sizeof(float));

cudaEvent_t start1,stop1,start2,stop2,start3,stop3;
cudaEventCreate(&start1);
cudaEventCreate(&stop1);
cudaEventCreate(&start2);
cudaEventCreate(&stop2);
cudaEventCreate(&start3);
cudaEventCreate(&stop3);
float elapsed;




for( int i=0; i<N; i++){
     for( int j=0; j<N; j++){

     l=j+i*N;
//     a[l]=drand48();
//     b[l]=drand48();

a[l]=1.;
b[l]=1.;

};
};



cudaEventRecord(start1,0);
   matrix_product_cpu(a,b,c);
cudaEventRecord(stop1,0);
cudaEventSynchronize(stop1);
cudaEventElapsedTime(&elapsed, start1, stop1);
cout << N << " cpu  " << elapsed/1000. << "\n";


cudaEventRecord(start3,0);
   matrix_product_gpu_shared(a,b,cgpush);
cudaEventRecord(stop3,0);
cudaEventSynchronize(stop3);
cudaEventElapsedTime(&elapsed, start3, stop3);
cout << N << " gpush" << elapsed/1000. << "\n";

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
    cout << l << "  " << c[l]<< "  " << cgpu[l] << "  " << cgpush[l] << "\n";

};
};



cudaEventDestroy(start1);
cudaEventDestroy(start2);
cudaEventDestroy(stop1);
cudaEventDestroy(stop2);


return 0;

}
