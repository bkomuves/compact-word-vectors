
#include <stdint.h>

// -----------------------------------------------------------------------------

void identity(int n, const uint64_t *src, int* pm, uint64_t *tgt);
void tail    (int n, const uint64_t *src, int* pm, uint64_t *tgt);
void cons(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt);
void snoc(uint64_t x, int n, const uint64_t *src, int* pm, uint64_t *tgt);

void rotate_left_words (int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt);
void rotate_right_words(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void rotate_left_bits (int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void rotate_right_bits(int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void rotate_left (int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void rotate_right(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt); 

void shift_left_words (int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void shift_right_words(int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void shift_left_bits (int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void shift_right_bits(int k, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void shift_left (int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt); 
void shift_right(int k0, int n, const uint64_t *src, int* pm, uint64_t *tgt); 

// -----------------------------------------------------------------------------
