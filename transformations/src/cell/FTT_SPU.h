/**
 * Copyright (c) 2005, 2006 Los Alamos National Security, LLC.  This
 * material was produced under U.S. Government contract DE-
 * AC52-06NA25396 for Los Alamos National Laboratory (LANL), which is
 * operated by the Los Alamos National Security, LLC (LANS) for the
 * U.S. Department of Energy. The U.S. Government has rights to use,
 * reproduce, and distribute this software. NEITHER THE GOVERNMENT NOR
 * LANS MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. If software is modified to
 * produce derivative works, such modified software should be clearly
 * marked, so as not to confuse it with the version available from
 * LANL.
 */

/*
 *  FTT.h
 *
 *  Created by Tanveer on 10/8/08.
 *
 */

#ifndef FTT_H_
#define FTT_H_

#include <simdmath.h>
#include <spu_intrinsics.h>

#define VSHIFT_RIGHT ((vector unsigned char){0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b})
#define VSHIFT_LEFT ((vector unsigned char){0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13})

// MODULO FUNCTIONS
vec_int4 mod_i4(vec_int4 i, vec_int4 j)
{
	return (divi4(i,j)).rem;
}

vec_uint4 mod_u4(vec_uint4 i, vec_uint4 j)
{
	return (divu4(i,j)).rem;
}

vec_llong2 llmod_i2(vec_llong2 i, vec_llong2 j)
{
	return (lldivi2(i,j)).rem;
}

vec_ullong2 llmod_u2(vec_ullong2 i, vec_ullong2 j)
{
	return (lldivu2(i,j)).rem;
}

// DIVISION FUNCTIONS
vec_int4 div_i4(vec_int4 i, vec_int4 j)
{
	return (divi4(i,j)).quot;
}

vec_uint4 div_u4(vec_uint4 i, vec_uint4 j)
{
	return (divu4(i,j)).quot;
}

vec_llong2 lldiv_i2(vec_llong2 i, vec_llong2 j)
{
	return (lldivi2(i,j)).quot;
}

vec_ullong2 lldiv_u2(vec_ullong2 i, vec_ullong2 j)
{
	return (lldivu2(i,j)).quot;
}

// MULTIPLY FUNCTION -- version 1.0
vec_int4 mul_i4(vec_int4 i, vec_int4 j)
{
	vec_double2 vd_i0;
	vec_double2 vd_i1;
	vec_double2 vd_j0;
	vec_double2 vd_j1;
	vec_double2 vd_result0;
	vec_double2 vd_result1;
	
	int result0;
	int result1;
	int result2;
	int result3;
	vec_int4 result;
	
	double d_i0 = (double) spu_extract(i, 0);
	double d_i1 = (double) spu_extract(i, 1);
	double d_i2 = (double) spu_extract(i, 2);
	double d_i3 = (double) spu_extract(i, 3);
	
	double d_j0 = (double) spu_extract(j, 0);
	double d_j1 = (double) spu_extract(j, 1);
	double d_j2 = (double) spu_extract(j, 2);
	double d_j3 = (double) spu_extract(j, 3);
	
	vd_i0 = spu_insert(d_i0, vd_i0, 0);
	vd_i0 = spu_insert(d_i1, vd_i0, 1);
	
	vd_i1 = spu_insert(d_i2, vd_i1, 0);
	vd_i1 = spu_insert(d_i3, vd_i1, 1);
	
	vd_j0 = spu_insert(d_j0, vd_j0, 0);
	vd_j0 = spu_insert(d_j1, vd_j0, 1);
	
	vd_j1 = spu_insert(d_j2, vd_j1, 0);
	vd_j1 = spu_insert(d_j3, vd_j1, 1);
	
	vd_result0 = spu_mul(vd_i0, vd_j0);
	vd_result1 = spu_mul(vd_i1, vd_j1);
	
	result0 = (int) spu_extract(vd_result0, 0);
	result1 = (int) spu_extract(vd_result0, 1);
	result2 = (int) spu_extract(vd_result1, 0);
	result3 = (int) spu_extract(vd_result1, 1);
	
	result = spu_insert(result0, result, 0);
	result = spu_insert(result1, result, 1);
	result = spu_insert(result2, result, 2);
	result = spu_insert(result3, result, 3);
	
	return result;	
}

vec_uint4 mul_u4(vec_uint4 i, vec_uint4 j)
{
	vec_double2 vd_i0;
	vec_double2 vd_i1;
	vec_double2 vd_j0;
	vec_double2 vd_j1;
	vec_double2 vd_result0;
	vec_double2 vd_result1;
	
	unsigned int result0;
	unsigned int result1;
	unsigned int result2;
	unsigned int result3;
	vec_uint4 result;
	
	double d_i0 = (double) spu_extract(i, 0);
	double d_i1 = (double) spu_extract(i, 1);
	double d_i2 = (double) spu_extract(i, 2);
	double d_i3 = (double) spu_extract(i, 3);
	
	double d_j0 = (double) spu_extract(j, 0);
	double d_j1 = (double) spu_extract(j, 1);
	double d_j2 = (double) spu_extract(j, 2);
	double d_j3 = (double) spu_extract(j, 3);
	
	vd_i0 = spu_insert(d_i0, vd_i0, 0);
	vd_i0 = spu_insert(d_i1, vd_i0, 1);
	
	vd_i1 = spu_insert(d_i2, vd_i1, 0);
	vd_i1 = spu_insert(d_i3, vd_i1, 1);
	
	vd_j0 = spu_insert(d_j0, vd_j0, 0);
	vd_j0 = spu_insert(d_j1, vd_j0, 1);
	
	vd_j1 = spu_insert(d_j2, vd_j1, 0);
	vd_j1 = spu_insert(d_j3, vd_j1, 1);
	
	vd_result0 = spu_mul(vd_i0, vd_j0);
	vd_result1 = spu_mul(vd_i1, vd_j1);
	
	result0 = (unsigned int) spu_extract(vd_result0, 0);
	result1 = (unsigned int) spu_extract(vd_result0, 1);
	result2 = (unsigned int) spu_extract(vd_result1, 0);
	result3 = (unsigned int) spu_extract(vd_result1, 1);
	
	result = spu_insert(result0, result, 0);
	result = spu_insert(result1, result, 1);
	result = spu_insert(result2, result, 2);
	result = spu_insert(result3, result, 3);
	
	return result;	
}


//INTEGER MADD -- uses 'mul_i4'
vec_int4 madd_i4 (vec_int4 a, vec_int4 b, vec_int4 c)
{
	return (spu_add(mul_i4(a, b), c));
}

//INTEGER MSUB -- uses 'mul_i4'
vec_int4 msub_i4 (vec_int4 a, vec_int4 b, vec_int4 c)
{
	return (spu_sub(mul_i4(a, b), c));
}





#endif /* FTT_H_ */