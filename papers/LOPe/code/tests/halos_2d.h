#define K1_ (get_global_id(0))
#define K2_ (get_global_id(1))

#define N1_ (64*1024)
#define N2_ (1024)

#define A_H1L_ 1
#define A_H1R_ 1
#define A_H2L_ 1
#define A_H2R_ 1
#define A_S1_  1
#define A_S2_  (A_H1L_ + A_H1R_ + N1_)


#define B_H1L_ 1
#define B_H1R_ 1
#define B_H2L_ 1
#define B_H2R_ 1
#define B_S1_  1
#define B_S2_  (B_H1L_ + B_H1R_ + N1_)

#define C_H1L_ 1
#define C_H1R_ 1
#define C_H2L_ 1
#define C_H2R_ 1
#define C_S1_  1
#define C_S2_  (C_H1L_ + C_H1R_ + N1_)

// First dimension (Left/Right) halo size for the given SIDE of the array NAME
#define HALO1(NAME,SIDE)   ( (NAME ## _H1 ## SIDE ## _) )

// Second dimension (Bottom/Top) halo size for the given SIDE of the array NAME
#define HALO2(NAME,SIDE)   ( (NAME ## _H2 ## SIDE ## _) )

// 1D index for the array NAME and offset OFF
#define IDX1(NAME,OFF)     ( (OFF) + (K1_) + (NAME ## _H1L_) )

// 2D index for the array NAME and offset OFF
#define IDX2(NAME,OFF1,OFF2) (  ((OFF1) + (K1_) + (NAME ## _H1L_)) * (NAME ## _S1_) \
                              + ((OFF2) + (K2_) + (NAME ## _H2L_)) * (NAME ## _S2_)  )
