#include <stdio.h>
#include <OpenCL/opencl.h>
#include <math.h>

void initTridiagonal(int, float[], float[]);
void solveTridiagonal(int, float[], float[]);
void addFourierMode(int, float [], int);
void prolongate(int, float [], float []);
void printVector(int, float []);
void printArray(int, float []);
void textual_Output(int, float [], char [], FILE *);
void restrict_func(int, float [], float []);
void relax(int, float [], float [], float);

int main()
{
  const float w = 2.0 / 3.0;
  const int NDIMS = 1;
  const int N = 64;
  const int fd = 12;
  const int  nsteps = 10;

  int i;
  float V1h[N+1] = {0}, Vp1h[N+1] = {0};
  float V2h[N/2+1] = {0}, V4h[N/4+1] = {0}, V8h[N/8+1] = {0};
  FILE *file;
  char *str;
  file = fopen("c_error_time.dat", "w");

  cl_platform_id* platforms;
  cl_uint platformCount;
  clGetPlatformIDs(0, NULL, &platformCount);
  printf("Num of platforms = %u\n", platformCount);

  printf("Fin\n");
  return 0;

  addFourierMode(N, V1h, 1);
  addFourierMode(N, V1h, 6);
  addFourierMode(N, V1h, 16);
  for (i = 0; i <= N; ++i)
    V1h[i] = (1./3.)*V1h[i];


  // Relax solution on 1h mesh
  /*   textual_Output(N/2+1, V1h, "1h_0", file); */
  for (i = 2; i <= nsteps; i += 2) {
    relax(N, V1h, Vp1h, w);
    relax(N, Vp1h, V1h, w);
  }
  /*   textual_Output(N/2+1, V1h, "1h_mid", file); */
  printVector(N, V1h);
  // Relax solution on 2h mesh
  for (i = 0; i < N + 1; ++i)
    Vp1h[i] = 0;
  restrict_func(N, V1h, V2h);
  /*   textual_Output(N/2+1, V2h, "2h_0", file); */
  for (i = 2; i <= nsteps; i += 2) {
    relax(N/2, V2h, Vp1h, w);
    relax(N/2, Vp1h, V2h, w);
  }
  /*   textual_Output(N/2+1, V2h, "2h_mid", file); */
  printVector(N/2, V2h);
  // Relax solution on 4h mesh
  for (i = 0; i < N + 1; ++i)
    Vp1h[i] = 0;
  restrict_func(N/2, V2h, V4h);
  /*   textual_Output(N/4+1, V4h, "4h_0", file); */
  for (i = 2; i <= nsteps; i += 2) {
    relax(N/4, V4h, Vp1h, w);
    relax(N/4, Vp1h, V4h, w);
  }
  /*   textual_Output(N/4+1, V4h, "4h_mid", file); */
  printVector(N/4, V4h);

  // Relax solution on 8h mesh
  for (i = 0; i < N + 1; ++i)
    Vp1h[i] = 0;
  restrict_func(N/4, V4h, V8h);
  /*   textual_Output(N/8+1, V8h, "8h_0", file); */
  for (i = 2; i <= nsteps; i += 2) {
    relax(N/8, V8h, Vp1h, w);
    relax(N/8, Vp1h, V8h, w);
  }
  /*   textual_Output(N/8+1, V8h, "8h_mid", file); */
  printVector(N/8, V8h);

  int n = N / 8;
  float A[n*n];
  initTridiagonal(n, A, V8h);
  solveTridiagonal(n, A, V8h);
/*   printArray(n, A); */
  printf("===solve\n");
  printVector(n-1, V8h);

/*   for (i = 2; i <= nsteps; i += 2) { */
/*     relax(N/8, V8h, Vp1h, w); */
/*     relax(N/8, Vp1h, V8h, w); */
/*   } */
/*   /\*   textual_Output(N/8+1, V8h, "8h_end", file); *\/ */
/*   printVector(N/8, V8h); */

  printf("===end relax, start prolongate\n");
  prolongate(N/4, V4h, V8h);
  printVector(N/4, V4h);
  /*   textual_Output(N/4+1, V4h, "4h_end", file); */
  prolongate(N/2, V2h, V4h);
  printVector(N/2, V2h);
  /*   textual_Output(N/2+1, V2h, "2h_end", file); */
  prolongate(N, V1h, V2h);
  printVector(N, V1h);
  /*   textual_Output(N+1, V1h, "1h_end", file); */

  fclose(file);  

  printf("Fin\n");
}

void addFourierMode(int N, float V[], int k)
{
  int i;
  for (i = 0; i <= N; ++i)
    V[i] = V[i] + sin(i*k*M_PI/N);
}

void printVector(int N, float V[])
{
  int i;
  for (i = 0;  i <= N; ++i)
    printf("%.3f ", V[i]);
  printf("\n\n");
}

void restrict_func(int N, float V1h[], float V2h[])
{
  int i, ii, m;
 
  m = N / 2 - 1;
  for (i = 2; i <= m + 1; ++i) {
    ii = 2 * i - 1;
    V2h[i] = 0.25 * (V1h[ii - 1] + 2.0 * V1h[ii] + V1h[ii+1]);
  }
}

void relax(int N, float A[], float Adv[], float w)
{
  int i;
  for (i = 0; i < N; ++i)
    Adv[i] = (1.0 - w) * A[i] + 0.5 * w * (A[i-1] + A[i+1]);
}

void initTridiagonal(int n, float A[], float b[])
{
  int i, j;
  for (i = 0; i < n; ++i) {
    for (j = 0; j < n; ++j) {
      if (i == j)
	A[i*n+j] = 2;
      else if (abs(i-j) == 1)
	A[i*n+j] = 1;
      else
	A[i*n+j] = 0;
    }
  }
}

void solveTridiagonal(int n, float A[], float b[])
{
  int i, j;
  float reduction;

  // Gaussian elimination
  // row reduction
  for (i = 0; i < n-1; ++i) {
    reduction = A[(i+1)*n+i] / A[i*n+i];
    A[(i+1)*n+i] = 0;
    A[(i+1)*n+i+1] -= A[i*n+i+1] * reduction;
    b[i+1] -= b[i] * reduction;
  }
  // back substition
  for (i = 2; i > 0; --i) {
    reduction = A[(i-1)*n+i] / A[i*n+i];
    A[(i-1)*n+i] = 0;
    b[i-1] -= b[i] * reduction;
  }
  // solve
  for (i = 0; i < n; ++i) {
    b[i] /=  A[i*n+i];
    A[i*n+i] = 1;
  }
}

void printArray(int n, float A[])
{
  int i, j;
  for (i = 0;  i < n; ++i) {
    for (j = 0;  j < n; ++j)
      printf("%.1f ", A[i*n+j]);
    printf("\n");
  }
  printf("\n");
}

void prolongate(int N, float V1h[], float V2h[]) {
  int i, ii , m;
  m = N/2 - 1; // interior coarse cells
  V1h[2] = 0.5*(V2h[1] + V2h[2]); // first interior cell
  for (i = 2; i <= m+1; ++i) {
    ii = 2*i - 1;
    V1h[ii] = V2h[i];
    V1h[ii+1] = 0.5*(V2h[i] + V2h[i+1]);
  }
}

void textual_Output(int N, float b[], char str[], FILE *file)
{

  int i;
  for (i = 0; i <= N; ++i)
    fprintf(file, "%.1f ", b[i]);
  fprintf(file, "\n");

}
