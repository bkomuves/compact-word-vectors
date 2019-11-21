
#include <stdint.h>
#include <string.h>

#include <stdio.h>      // DEBUGGING ONLY

#include "blob.h"

// -----------------------------------------------------------------------------

void vec_identity(int n, const uint64_t *src, int* pm, uint64_t *tgt) {
  memcpy(tgt, src, n<<3);
  *pm = n;
}

// -----------------------------------------------------------------------------

inline uint64_t nbit_mask (int n) { 
  uint64_t mask = 1;       // fucking c implicit conversions
  mask = (mask << n) - 1;
  return mask;
}

inline uint64_t nbit_compl_mask(int n) { 
  uint64_t mask = 1;                   
  mask = (mask << n) - 1;
  return ~mask;
}

inline int required_bits(uint64_t x)
{
  int bits = 0;
  while(x > 0) { x = (x>>1); bits++; }
  if (bits == 0) { bits = 1; }
  bits = (bits+3) & (~3);
  return bits;
}

inline int required_reso(uint64_t x)
{
  return ( (required_bits(x) >> 2) - 1 );
}

#define MAX_SMALL_LENGTH 31
#define MAX_SMALL_BITS   16

// header of the empty vector (small, 4 bits, 0 length)
#define EMPTY_HEADER 0

#define SMALL_HEADER(len,reso) (     ((reso) << 1) | (((uint64_t)(len)) << 3) )
#define   BIG_HEADER(len,reso) ( 1 | ((reso) << 1) | (((uint64_t)(len)) << 5) )

#define VEC_HEADER_CODE             \
  uint64_t head = src[0];           \
  int is_small  = (head & 1) ^ 1;   \
                                    \
  int header_bits, reso_bits, len_bits, len_ofs; \
  uint64_t reso_mask, len_mask, header_mask;     \
                 \
  if (is_small)  \
  {              \
    header_bits = 8;   header_mask = 0xff;  \
    reso_bits   = 2;   reso_mask   = 0x03;  \
    len_bits    = 5;   len_mask    = 0x1f;  \
  }                                         \
  else                                      \
  {                                         \
    header_bits = 32;  header_mask = 0xffffffff;  \
    reso_bits   =  4;  reso_mask   = 0x0f;        \
    len_bits    = 27;  len_mask    = 0x07ffffff;  \
  }                                               \
                                                  \
  int reso,bits,len;                              \
  reso = (head >> 1) & reso_mask;                 \
  bits = ((reso + 1) << 2);                       \
  len  = (head >> (1 + reso_bits)) & len_mask;    

                  
//printf("len  = %d\n",len ); \
//printf("bits = %d\n",bits); \
//printf("reso = %d\n",reso); \

// -----------------------------------------------------------------------------

void copy_elements_into
  ( int  src_len , int src_bits , const uint64_t *src , int src_bit_ofs
  , int *tgt_len , int tgt_bits ,       uint64_t *tgt , int tgt_bit_ofs
  )
{
  const uint64_t *p = src;  
        uint64_t *q = tgt;

  int p_ofs = src_bit_ofs;
  int q_ofs = tgt_bit_ofs;

  p += (p_ofs >> 6); p_ofs &= 63;
  q += (q_ofs >> 6); q_ofs &= 63;

  for(int i=0;i<src_len;i++)
  {
    uint64_t elem, tmp;

    // read next element
    int p_new = p_ofs + src_bits;
    if (p_new <= 64) {
      elem = (p[0] >> p_ofs);
    }
    else {
      elem = (  p[0]                        >>     p_ofs ) 
           | ( (p[1] & nbit_mask(p_new-64)) << (64-p_ofs));         
    }
    elem &= nbit_mask(src_bits);
    if (p_new >= 64) {
      p_ofs = p_new-64;
      p++;
    } 
    else { 
      p_ofs = p_new; 
    }

// printf("src elem #%i = %x\n",i,elem);

    // write next element
    int q_new = q_ofs + tgt_bits;
    if (q_new <= 64) {
      tmp  = q[0] & nbit_mask(q_ofs);
      q[0] = tmp  | (elem << q_ofs);
    }
    else {
      tmp  = q[0] & nbit_mask(q_ofs);
      q[0] = tmp  | (elem << q_ofs);
      q[1] = elem >> (64-q_ofs);
    }
    if (q_new >= 64) {
      q_ofs = q_new-64;
      q++;
    } 
    else { 
      q_ofs = q_new; 
    }
  } 

  if (q_ofs == 0) {
    *tgt_len = (q - tgt);
  }
  else {
    *tgt_len = (q - tgt + 1);
  }

}

// -----------------------------------------------------------------------------

void vec_tail(int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  VEC_HEADER_CODE

  if (len==0) { tgt[0] = EMPTY_HEADER; *pm = 1; return; }

  if (is_small) { 
    shift_right(bits, n, src, pm, tgt);
    tgt[0] = (tgt[0] & (~header_mask)) | SMALL_HEADER(len-1,reso);
  }
  else {
    shift_right(bits, n, src, pm, tgt);
    tgt[0] = (tgt[0] & (~header_mask)) | BIG_HEADER(len-1,reso);
  }
}

// -----------------------------------------------------------------------------

void vec_cons(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  VEC_HEADER_CODE

  int x_bits = required_bits(x);
  int x_reso = required_reso(x);

  if (len==0) {
    // cons to an empty vector
    if (x_reso <= 2) {
      tgt[0] = SMALL_HEADER(1,x_reso) | (x << 8); 
      *pm = 1; 
      return; 
    }
    else {
      tgt[0] = BIG_HEADER(1,x_reso) | (x << 32); 
      uint64_t y = (x >> 32);
      if (y==0) {
        *pm = 1; 
      } 
      else {
        tgt[1] = y;
        *pm = 2; 
      }
      return;
    }
  } 

  if (x_bits <= bits) {
    // the new element fits without changing the resolution
    if (is_small) { 
      // the old vector is small
      if (len+1 <= MAX_SMALL_LENGTH) {
        // the length fits, too
        uint64_t mask = nbit_compl_mask(8+bits);
        shift_left(bits, n, src, pm, tgt);
        tgt[0] = (tgt[0] & mask) | SMALL_HEADER(len+1,reso) | (x << 8);
      }
      else {
        // the length does not fit
        if (bits <= 32) {
          // the new element fits into the first word
          uint64_t mask = nbit_compl_mask(32+bits);
          shift_left(bits+24, n, src, pm, tgt);
          tgt[0] = (tgt[0] & mask) | BIG_HEADER(len+1,reso) | (x << 32);
        }
        else {
          // the new element does not fit into the first word
          uint64_t mask = nbit_compl_mask(bits-32);
          shift_left(bits+24, n, src, pm, tgt);
          tgt[0] = BIG_HEADER(len+1,reso) | (x << 32);
          tgt[1] = (tgt[1] & mask) | (x >> 32);
        }
      }
    }
    else {
      // the old vector is big
      if (bits <= 32) {
        // the new element fits into the first word
        shift_left(bits, n, src, pm, tgt);
        if (bits < 32) {
          uint64_t mask = nbit_compl_mask(32+bits);
          tgt[0] = (tgt[0] & mask) | BIG_HEADER(len+1,reso) | (x << 32);
        }
        else {
          tgt[0] =                   BIG_HEADER(len+1,reso) | (x << 32);
        }
      }
      else {
        // the new element does not fit into the first word
        uint64_t mask = nbit_compl_mask(bits-32);
        shift_left(bits, n, src, pm, tgt);
        tgt[0] = BIG_HEADER(len+1,reso) | (x << 32);
        tgt[1] = (tgt[1] & mask) | (x >> 32);
      }
    }
  }
  else {
    // the new element needs more bits
    if ( (x_bits <= MAX_SMALL_BITS) && (len+1 <= MAX_SMALL_LENGTH) ) {
      // but we still fit into a small vector
      tgt[0] = SMALL_HEADER(len+1,x_reso) | (x << 8);
      copy_elements_into
        ( len ,   bits , src , header_bits
        , pm  , x_bits , tgt , 8 + x_bits 
        );
    }
    else {
      // we need a big vector
      tgt[0] = BIG_HEADER(len+1,x_reso) | (x << 32);
      uint64_t y = (x >> 32);
      if (y > 0) { tgt[1] = y; }
      copy_elements_into
        ( len ,   bits , src , header_bits
        , pm  , x_bits , tgt , 32 + x_bits 
        );
    }
  }
}
