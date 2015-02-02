#define K1_ (get_global_id(0))

//#define N1_ (16*1024*1024)
#define N1_ (16)

#define A_H1L_ 2
#define A_H1R_ 2
#define A_S1_  1

#define B_H1L_ 2
#define B_H1R_ 2
#define B_S1_  1

#define C_H1L_ 2
#define C_H1R_ 2
#define C_S1_  1

// 1D halo size for the given SIDE of the array NAME
#define HALO1(NAME,SIDE)   ( (NAME ## _H1 ## SIDE ## _) )

// 1D index for the array NAME and offset OFF
#define IDX1(NAME,OFF)     ( (OFF) + (K1_) + (NAME ## _H1L_) )
