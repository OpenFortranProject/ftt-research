void identity(float *A,float *B,unsigned int inputSize,float *tiles,unsigned int tileSize)
{
  int k = get_global_id(0);
  if ( k < inputSize )
    B[k] = A[k];
}
