
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

// ones for the first n bit, zeros for the rest
inline uint64_t nbit_mask (int n) { 
  uint64_t mask = 1;       // fucking c implicit conversions
  if (n<64) { 
    mask = (mask << n) - 1; 
  }
  else { 
    mask = 0;
    mask = ~mask;      // 0xfffff...f
  }
  return mask;
}

// zeros for the first n bit, ones for the rest
inline uint64_t nbit_compl_mask(int n) { 
  uint64_t mask = 1;
  if (n<64) { 
    mask = (mask << n) - 1; 
    return ~mask;
  }
  return 0;
}

// the minimum required bits to a store a given number, rounded up to multiples of 4
inline int required_bits_not_rounded(uint64_t x)
{
  int bits = 0;
  while(x > 0) { x = (x>>1); bits++; }
  if (bits == 0) { bits = 1; }
  return bits;
}

// the minimum required bits to a store a given number, rounded up to multiples of 4
inline int required_bits(uint64_t x)
{
  int bits = 0;
  while(x > 0) { x = (x>>1); bits++; }
  if (bits == 0) { bits = 1; }
  bits = (bits+3) & (~3);
  return bits;
}

int export_required_bits_not_rounded(uint64_t x) { return required_bits_not_rounded(x); }
int export_required_bits            (uint64_t x) { return required_bits            (x); }

inline int bits2reso(int bits)
{
  return ( (bits >> 2) - 1 );
}

inline int required_reso(uint64_t x)
{
  return ( (required_bits(x) >> 2) - 1 );
}

// -----------------------------------------------------------------------------

#define MAX_SMALL_LENGTH 31
#define MAX_SMALL_BITS   16

// header of the empty vector (small, 4 bits, 0 length)
#define EMPTY_HEADER 0

#define SMALL_HEADER(len,reso) (     ((reso) << 1) | (((uint64_t)(len)) << 3) )
#define   BIG_HEADER(len,reso) ( 1 | ((reso) << 1) | (((uint64_t)(len)) << 5) )

#define VEC_HEADER_CODE(src)        \
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

// -------------------------------------
                  
#define VEC_READ_LOOP           \
  const uint64_t *p = src;      \
  int p_ofs = header_bits;      \
  uint64_t elem_mask = nbit_mask(bits); \
  for(int i=0;i<len;i++) {      \
    uint64_t elem;              \
    /* read next element */         \
    int p_new = p_ofs + bits;       \
    if (p_new <= 64) {              \
      elem = (p[0] >> p_ofs);       \
    }                               \
    else {                          \
      elem = (  p[0]                        >>     p_ofs )       \
           | ( (p[1] & nbit_mask(p_new-64)) << (64-p_ofs));      \
    }                               \
    elem &= elem_mask;              \
    if (p_new >= 64) {              \
      p_ofs = p_new-64;             \
      p++;                \
    }                     \
    else {                \
      p_ofs = p_new;      \
    }

// zipping, extended with zeros, length given by the use (zip_len)
#define VEC_ZIP_LOOP              \
  const uint64_t *p1 = src1;      \
  const uint64_t *p2 = src2;      \
  int p_ofs1 = header_bits1;      \
  int p_ofs2 = header_bits2;      \
  uint64_t elem_mask1 = nbit_mask(bits1); \
  uint64_t elem_mask2 = nbit_mask(bits2); \
  for(int i=0;i<zip_len;i++) {        \
    uint64_t elem1=0, elem2=0;        \
    if (i<len1) {                     \
      /* read next element #1 */      \
      int p_new1 = p_ofs1 + bits1;    \
      if (p_new1 <= 64) {             \
        elem1 = (p1[0] >> p_ofs1);    \
      }                               \
      else {                          \
        elem1 = (  p1[0]                         >>     p_ofs1 )       \
              | ( (p1[1] & nbit_mask(p_new1-64)) << (64-p_ofs1));      \
      }                                 \
      elem1 &= elem_mask1;              \
      if (p_new1 >= 64) {               \
        p_ofs1 = p_new1-64;             \
        p1++;                \
      }                      \
      else {                 \
        p_ofs1 = p_new1;     \
      }                      \
    }                        \
    if (i<len2) {                     \
      /* read next element #2 */      \
      int p_new2 = p_ofs2 + bits2;    \
      if (p_new2 <= 64) {             \
        elem2 = (p2[0] >> p_ofs2);    \
      }                               \
      else {                          \
        elem2 = (  p2[0]                         >>     p_ofs2 )       \
              | ( (p2[1] & nbit_mask(p_new2-64)) << (64-p_ofs2));      \
      }                                 \
      elem2 &= elem_mask2;              \
      if (p_new2 >= 64) {               \
        p_ofs2 = p_new2-64;             \
        p2++;                \
      }                      \
      else {                 \
        p_ofs2 = p_new2;     \
      }                      \
    }
    
//  write next element            
#define WRITE_ELEMENT(elem)           \
    int q_new = q_ofs + tgt_bits;     \
    if (q_new <= 64) {                \
      uint64_t tmp;                   \
      tmp  = q[0] & nbit_mask(q_ofs); \
      q[0] = tmp  | (elem << q_ofs);  \
    }                                 \
    else {                            \
      uint64_t tmp;                   \
      tmp  = q[0] & nbit_mask(q_ofs); \
      q[0] = tmp  | (elem << q_ofs);  \
      q[1] = elem >> (64-q_ofs);      \
    }                                 \
    if (q_new >= 64) {                \
      q_ofs = q_new-64;               \
      q++;                            \
    }                                 \
    else {                            \
      q_ofs = q_new;                  \
    }     
  
   
#define STORE_OUTPUT_LENGTH(tgt_len)  \
  if (q_ofs == 0) {                   \
    *tgt_len = (q - tgt);             \
  }                                   \
  else {                              \
    *tgt_len = (q - tgt + 1);         \
  }   
  
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

  uint64_t elem_mask = nbit_mask(src_bits); 

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
    elem &= elem_mask;
    if (p_new >= 64) {
      p_ofs = p_new-64;
      p++;
    } 
    else { 
      p_ofs = p_new; 
    }

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
  VEC_HEADER_CODE(src)

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

uint64_t vec_head_tail(int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  VEC_HEADER_CODE(src)

  if (len==0) { tgt[0] = EMPTY_HEADER; *pm = 1; return 0; }

  if (is_small) { 
    uint64_t head;
    head = (src[0] >> 8) & nbit_mask(bits);
    shift_right(bits, n, src, pm, tgt);
    tgt[0] = (tgt[0] & (~header_mask)) | SMALL_HEADER(len-1,reso);
    return head;
  }
  else {
    uint64_t head;
    if (bits <= 32) {
      head = (src[0] >> 32) & nbit_mask(bits);
    } 
    else {
      head = ((src[0] >> 32) | (src[1] << 32)) & nbit_mask(bits);
    }
    shift_right(bits, n, src, pm, tgt);
    tgt[0] = (tgt[0] & (~header_mask)) | BIG_HEADER(len-1,reso);
    return head;
  }
}

// -----------------------------------------------------------------------------
// CONS

void vec_cons(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  VEC_HEADER_CODE(src)

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
        shift_left_strict(bits, n, src, pm, tgt);
        tgt[0] = (tgt[0] & mask) | SMALL_HEADER(len+1,reso) | (x << 8);
      }
      else {
        // the length does not fit
        if (bits <= 32) {
          // the new element fits into the first word
          uint64_t mask = nbit_compl_mask(32+bits);
          shift_left_strict(bits+24, n, src, pm, tgt);
          tgt[0] = (tgt[0] & mask) | BIG_HEADER(len+1,reso) | (x << 32);
        }
        else {
          // the new element does not fit into the first word
          uint64_t mask = nbit_compl_mask(bits-32);
          shift_left_strict(bits+24, n, src, pm, tgt);
          tgt[0] = BIG_HEADER(len+1,reso) | (x << 32);
          tgt[1] = (tgt[1] & mask) | (x >> 32);
        }
      }
    }
    else {
      // the old vector is big
      if (bits <= 32) {
        // the new element fits into the first word
        shift_left_strict(bits, n, src, pm, tgt);
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
        shift_left_strict(bits, n, src, pm, tgt);
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

// -----------------------------------------------------------------------------
// SNOC

#define SNOC_WRITE(tgt_ofs,y_bits)   \
  int bit_ofs  = (tgt_ofs);          \
  int word_ofs = (bit_ofs) >> 6;     \
  bit_ofs &= 63;                     \
  int new_ofs  = bit_ofs + y_bits;   \
  if  (new_ofs <= 64) {                  \
    uint64_t mask = nbit_mask(bit_ofs);  \
    tgt[word_ofs] = (tgt[word_ofs] & mask) | (x << bit_ofs);   \
  }                                      \
  else {                                 \
    uint64_t mask = nbit_mask(bit_ofs);  \
    tgt[word_ofs  ] = (tgt[word_ofs] & mask) | (x << bit_ofs);  \
    tgt[word_ofs+1] = (x >> (64 - bit_ofs));                    \
  }                          \
  if (new_ofs <= 64) {       \
    *pm = word_ofs + 1;      \
  }                          \
  else {                     \
    *pm = word_ofs + 2;      \
  }
  
  
void vec_snoc(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  VEC_HEADER_CODE(src)

  int x_bits = required_bits(x);
  int x_reso = required_reso(x);

  if (len==0) {
    // snoc to an empty vector
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
        memcpy(tgt, src, n<<3);
        uint64_t mask = nbit_compl_mask(8);
        tgt[0] = (tgt[0] & mask) | SMALL_HEADER(len+1,reso);
        SNOC_WRITE( 8+bits*len , bits )        
      }
      else {
        // the length does not fit
        shift_left_strict(24, n, src, pm, tgt);
        uint64_t mask = nbit_compl_mask(32);
        tgt[0] = (tgt[0] & mask) | BIG_HEADER(len+1,reso);
        SNOC_WRITE( 32+bits*len , bits )        
      }
    }
    else {
      // the old vector is big
      memcpy(tgt, src, n<<3);
      uint64_t mask = nbit_compl_mask(32);
      tgt[0] = (tgt[0] & mask) | BIG_HEADER(len+1,reso);
      SNOC_WRITE( 32+bits*len , bits )        
    }
  }
  else {
    // the new element needs more bits
    if ( (x_bits <= MAX_SMALL_BITS) && (len+1 <= MAX_SMALL_LENGTH) ) {
      // but we still fit into a small vector
      tgt[0] = SMALL_HEADER(len+1,x_reso);
      copy_elements_into
        ( len ,   bits , src , header_bits
        , pm  , x_bits , tgt , 8  
        );
      SNOC_WRITE(8 + x_bits*len, x_bits)        
    }
    else {
      // we need a big vector
      tgt[0] = BIG_HEADER(len+1,x_reso);
      copy_elements_into
        ( len ,   bits , src , header_bits
        , pm  , x_bits , tgt , 32  
        );
      SNOC_WRITE(32 + x_bits*len, x_bits)        
    }
  }
}

// -----------------------------------------------------------------------------
// folds

uint64_t vec_max(int n, const uint64_t *src) 
{
  VEC_HEADER_CODE(src)
  uint64_t max = 0;
  VEC_READ_LOOP
    max = (elem > max) ? elem : max;
  }
  return max;
}

uint64_t vec_sum(int n, const uint64_t *src) 
{
  VEC_HEADER_CODE(src)
  uint64_t sum = 0;
  VEC_READ_LOOP
    sum += elem;
  }
  return sum;
}

// -----------------------------------------------------------------------------
// zipping folds

// strictly equal (as vectors)
uint64_t vec_equal_strict(int n1, const uint64_t *src1, int n2, const uint64_t *src2) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;

  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  if (len1 != len2) {
    return 0;
  }
      
  int bool = 1;
  int zip_len = len1;
  VEC_ZIP_LOOP
    if (elem1 != elem2) {
      bool = 0;
      break;
    }
  }
  return bool;
}

// equal when extended by zeros (as monomials, partitions, etc)
uint64_t vec_equal_extzero(int n1, const uint64_t *src1, int n2, const uint64_t *src2) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;

  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  int bool = 1;
  int zip_len = (len1>=len2) ? len1 : len2;
  VEC_ZIP_LOOP
    if (elem1 != elem2) {
      bool = 0;
      break;
    }
  }
  return bool;
}

uint64_t vec_less_or_equal(int n1, const uint64_t *src1, int n2, const uint64_t *src2) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;
  
  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  int bool = 1;
  int zip_len = (len1>=len2) ? len1 : len2;
  VEC_ZIP_LOOP
    if (elem1 > elem2) {
      bool = 0;
      break;
    }
    if (i >= len1-1) { break; }   // if we are over the first list, then 0 <= anything 
  }
  return bool;
}

// dominance order of partitions
uint64_t vec_partial_sums_less_or_equal(int n1, const uint64_t *src1, int n2, const uint64_t *src2) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;

  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  int bool = 1;
  int zip_len = (len1>=len2) ? len1 : len2;
  uint64_t sum1 = 0;
  uint64_t sum2 = 0;
  VEC_ZIP_LOOP
    sum1 += elem1;
    sum2 += elem2;
    if (sum1 > sum2) {
      bool = 0;
      break;
    }
    if (i >= len1-1) { break; }   // if we are over the first list, then sum(0) <= sum(anything) 
  }
  return bool;
}

// -----------------------------------------------------------------------------

void vec_add(int n1, const uint64_t *src1, int n2, const uint64_t *src2, int *pm, uint64_t *tgt) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;
  
  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  int zip_len = (len1>=len2) ? len1 : len2;

  // compute upper bound for the result
  uint64_t bound = 0;
  { VEC_ZIP_LOOP
      uint64_t x = elem1 + elem2;
      if (x > bound) { bound = x; }
    }
  }
  
  int tgt_bits = required_bits(bound);
  int tgt_reso = bits2reso(tgt_bits);

  uint64_t *q = tgt;
  int q_ofs;
  
  // write header
  if ( (tgt_bits <= MAX_SMALL_BITS) && (zip_len <= MAX_SMALL_LENGTH) ) {
    q[0]  = SMALL_HEADER( zip_len , tgt_reso );  
    q_ofs = 8;
  }
  else {
    q[0]  = BIG_HEADER( zip_len , tgt_reso );  
    q_ofs = 32;
  }
  
  // write result
  VEC_ZIP_LOOP
    uint64_t y = elem1 + elem2;
    WRITE_ELEMENT(y)
  }

  STORE_OUTPUT_LENGTH(pm)
}

// -----------------------------------------------------------------------------

// subtraction with overflow indicator
int vec_sub_overflow(int n1, const uint64_t *src1, int n2, const uint64_t *src2, int *pm, uint64_t *tgt) 
{
  int len1,bits1,header_bits1;
  int len2,bits2,header_bits2;
  
  { VEC_HEADER_CODE(src1) ; len1  = len ; bits1 = bits ; header_bits1 = header_bits; }
  { VEC_HEADER_CODE(src2) ; len2  = len ; bits2 = bits ; header_bits2 = header_bits; }

  int zip_len = (len1>=len2) ? len1 : len2;

  // compute upper bound for the result
  uint64_t bound = 0;
  { VEC_ZIP_LOOP
      uint64_t x = elem1 + elem2;
      if (x > bound) { bound = x; }
    }
  }
  
  int tgt_bits = required_bits(bound);
  int tgt_reso = bits2reso(tgt_bits);

  uint64_t *q = tgt;
  int q_ofs;
  
  // write header
  if ( (tgt_bits <= MAX_SMALL_BITS) && (zip_len <= MAX_SMALL_LENGTH) ) {
    q[0]  = SMALL_HEADER( zip_len , tgt_reso );  
    q_ofs = 8;
  }
  else {
    q[0]  = BIG_HEADER( zip_len , tgt_reso );  
    q_ofs = 32;
  }
 
  int overflow = 0; 
  // write result
  VEC_ZIP_LOOP
    if (elem2 > elem1) { overflow = 1; }
    uint64_t y = elem1 - elem2;
    WRITE_ELEMENT(y)
  }

  STORE_OUTPUT_LENGTH(pm)
  return overflow;
}

// -----------------------------------------------------------------------------
