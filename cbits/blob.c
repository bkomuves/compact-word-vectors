
#include <stdint.h>
#include <string.h>

// -----------------------------------------------------------------------------

void identity(int n, const uint64_t *src, int* pm, uint64_t *tgt) {
  memcpy(tgt, src, n<<3);
  *pm = n;
}

void cons(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt) {
  memcpy(tgt+1, src, n<<3);
  tgt[0] = x;
  *pm = n+1;
}

void snoc(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt) {
  memcpy(tgt, src, n<<3);
  tgt[n] = x;
  *pm = n+1;
}

void tail(int n, const uint64_t *src, int* pm, uint64_t *tgt) {
  if (n>1) {
    memcpy(tgt, src+1, (n-1)<<3);
    *pm = n-1;
  }
  else {
    tgt[0] = 0;
    *pm = 1;
  }
}

// -----------------------------------------------------------------------------
                                        
#define IDX_RIGHT(i,shift)  ( ( (i) +     (shift) ) % n )
#define IDX_LEFT( i,shift)  ( ( (i) + n - (shift) ) % n )

void rotate_left_words(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  int k = k0 % n;  
  for(int i=0;i<n;i++) {
    tgt[i] = src[ IDX_LEFT(i,k) ];
  } 
  *pm = n;
} 

void rotate_right_words(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  int k = k0 % n;  
  for(int i=0;i<n;i++) {
    tgt[i] = src[ IDX_RIGHT(i,k) ];
  } 
  *pm = n;
} 

// we assume that 0 <= k < 64
void rotate_left_bits(int k, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  if (k==0) { identity(n,src,pm,tgt); return; }

  int r      = 64 - k;
  uint64_t x = src[n-1];
  for(int i=0;i<n;i++) {
    tgt[i] = (src[i] << k) | (x >> r);
    x      =  src[i];
  } 
  *pm = n;
}

// we assume that 0 <= k < 64
void rotate_right_bits(int k, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  if (k==0) { identity(n,src,pm,tgt); return; }

  int r      = 64 - k;
  uint64_t x = src[n-1];
  for(int i=0;i<n-1;i++) {
    tgt[i] = (src[i] >> k) | (src[i+1] << r);
  } 
  tgt[n-1] = (src[n-1] >> k) | (src[0] << r);
  *pm = n;
}

// we assume that 0 <= k < 64
void rotate_left(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  if (k0==0) { identity(n,src,pm,tgt); return; }

  int k =  k0 & 63;
  int s = (k0 >> 6) % n;
  int r = 64 - k;
  uint64_t x = src[ IDX_LEFT(n-1,s) ];

  if (k==0) { rotate_left_words(s,n,src,pm,tgt); return; }
  if (s==0) { rotate_left_bits (k,n,src,pm,tgt); return; }

  for(int i=0;i<n;i++) {
    int o  = IDX_LEFT(i,s);
    tgt[i] = (src[o] << k) | (x >> r);
    x      =  src[o];
  } 
  *pm = n;
}

// we assume that 0 <= k < 64
void rotate_right(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt) 
{
  if (k0==0) { identity(n,src,pm,tgt); return; }

  int k =  k0 & 63;
  int s = (k0 >> 6) % n;
  int r =  64 - k;
  uint64_t x = src[ IDX_RIGHT(n-1,s) ];

  if (k==0) { rotate_right_words(s,n,src,pm,tgt); return; }
  if (s==0) { rotate_right_bits (k,n,src,pm,tgt); return; }

  for(int i=0;i<n;i++) {
    tgt[i] = ( src[ IDX_RIGHT(i  ,s) ] >> k) 
           | ( src[ IDX_RIGHT(i+1,s)]  << r);
  } 
  *pm = n;
}

// -----------------------------------------------------------------------------

