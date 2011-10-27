// Make extended syntax go away
//
#define ELEMENTAL
#define PURE

// Array size
//
#define N 100

//
// The function adds two input array elements and returns the output
// as an output parameter.
//

void PURE ELEMENTAL elemental_add(const float * a, const float * b, float * c)
{
   *c = *a + *b;
}

int main(int argc, char* argv[])
{
   float A[N], B[N], C[N];

   // initialize arrays...

   // call elemental function

   elemental_add(A, B, C);
}


int main_compiler_creates(int argc, char* argv[])
{
   int i;
   float A[N], B[N], C[N];

   // initialize arrays...

   // inline elemental function
   for (i = 0; i < N; i++) {
      C[i] = A[i] + B[N];
   }
}


