#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "memory.h"
#include "fail.h"
#include "engine.h"

#if GC_VERSION == GC_MARK_N_SWEEP

static void* memory_start = NULL;
static void* memory_end = NULL;

static uvalue_t* bitmap_start = NULL;

static value_t* heap_start = NULL;
static value_t* heap_end = NULL;
static value_t heap_start_v = 0;
static value_t heap_end_v = 0;
static value_t* heap_first_block = NULL;

#define FREE_LISTS_COUNT 32
static value_t* free_list_heads[FREE_LISTS_COUNT];

#define MIN_BLOCK_SIZE 1
#define HEADER_SIZE 1

// Header management

static value_t header_pack(tag_t tag, value_t size) {
  return (size << 8) | (value_t)tag;
}

static tag_t header_unpack_tag(value_t header) {
  return (tag_t)(header & 0xFF);
}

static value_t header_unpack_size(value_t header) {
  return header >> 8;
}

// Bitmap management

static int bitmap_is_bit_set(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  return (bitmap_start[word_index] & ((uvalue_t)1 << bit_index)) != 0;
}

static void bitmap_set_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] |= (uvalue_t)1 << bit_index;
}

static void bitmap_clear_bit(value_t* ptr) {
  assert(heap_start <= ptr && ptr < heap_end);
  long index = ptr - heap_start;
  long word_index = index / (long)VALUE_BITS;
  long bit_index = index % (long)VALUE_BITS;
  bitmap_start[word_index] &= ~((uvalue_t)1 << bit_index);
}

// Virtual <-> physical address translation

static void* addr_v_to_p(value_t v_addr) {
  return (char*)memory_start + v_addr;
}

static value_t addr_p_to_v(void* p_addr) {
  return (value_t)((char*)p_addr - (char*)memory_start);
}

// Free lists management

static value_t real_size(value_t size) {
  assert(0 <= size);
  return size < MIN_BLOCK_SIZE ? MIN_BLOCK_SIZE : size;
}

static unsigned int free_list_index(value_t size) {
  assert(0 <= size);
  return size >= FREE_LISTS_COUNT ? FREE_LISTS_COUNT - 1 : (unsigned int)size;
}

char* memory_get_identity() {
  return "mark & sweep garbage collector";
}

void memory_setup(size_t total_byte_size) {
  memory_start = malloc(total_byte_size);
  if (memory_start == NULL)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  memory_end = (char*)memory_start + total_byte_size;
}

void memory_cleanup() {
  assert(memory_start != NULL);
  free(memory_start);

  memory_start = memory_end = NULL;
  bitmap_start = NULL;
  heap_start = heap_end = NULL;
  heap_start_v = heap_end_v = 0;
  for (int l = 0; l < FREE_LISTS_COUNT; ++l)
    free_list_heads[l] = NULL;
}

void* memory_get_start() {
  return memory_start;
}

void* memory_get_end() {
  return memory_end;
}

void memory_set_heap_start(void* ptr) {
  assert(memory_start <= ptr && ptr < memory_end);

  const size_t bh_size =
    (size_t)((char*)memory_end - (char*)ptr) / sizeof(value_t);

  const size_t bitmap_size = (bh_size - 1) / (VALUE_BITS + 1) + 1;
  const size_t heap_size = bh_size - bitmap_size;

  bitmap_start = ptr;
  memset(bitmap_start, 0, bitmap_size * sizeof(value_t));

  heap_start = (value_t*)bitmap_start + bitmap_size;
  heap_end = heap_start + heap_size;
  assert(heap_end == memory_end);

  heap_start_v = addr_p_to_v(heap_start);
  heap_end_v = addr_p_to_v(heap_end);

  heap_first_block = heap_start + HEADER_SIZE;
  const value_t initial_block_size = (value_t)(heap_end - heap_first_block);
  heap_first_block[-1] = header_pack(tag_None, initial_block_size);
  heap_first_block[0] = 0;

  for (int l = 0; l < FREE_LISTS_COUNT - 1; ++l)
    free_list_heads[l] = memory_start;
  free_list_heads[FREE_LISTS_COUNT - 1] = heap_first_block;
}

static value_t get_header_size(value_t* block_pointer) {
  return header_unpack_size(block_pointer[-1]);
}

static tag_t get_tag(value_t* block_pointer) {
  return header_unpack_tag(block_pointer[-1]);
}

static value_t get_if_equal_size(value_t sz1, value_t sz2) {
  if (sz1 == sz2 || sz1 + 1 == sz2) return (value_t) 1;
  return (value_t)0;
}

static void clean_block(value_t* ptr, value_t sz) {
  value_t size_of_data_str = sizeof(value_t);
  memset(ptr, 0, sz*size_of_data_str);
}

static void init_header(value_t* ptr, value_t sz, tag_t tag) {
  ptr[-1] = header_pack(tag, sz);
}


static void init_current_block(
  value_t* current,
  value_t size,
  tag_t tag
) {
  init_header(current, size, tag);
  clean_block(current, size);
  bitmap_set_bit(current);
}


static value_t* allocate_for_equal(
  value_t* prev,
  value_t* current,
  value_t size,
  tag_t tag
) {
  
  if (prev != NULL) {
     prev[0] = current[0];
  } else { // nothing to adjust for the pointers here!!!
    value_t lastIndex = FREE_LISTS_COUNT - 1;
    free_list_heads[lastIndex] = addr_v_to_p(current[0]);
  }

  init_current_block(current, size, tag);
  return current;
}


static value_t* allocate_for_greator(
  value_t* prev,
  value_t* current,
  value_t size,
  value_t block_size,
  tag_t tag
) {
    value_t* next_block = current + size + 1; 
    value_t next_block_size = block_size - (size + 1);
    next_block[0] = current[0];
    
    if(prev != NULL){
      prev[0] = next_block[0];
    }
    else {
      int lastIndex = FREE_LISTS_COUNT-1;
      free_list_heads[lastIndex] = next_block;   
    }
    init_header(next_block, next_block_size, tag_None);
    init_current_block(current, size, tag);
    return current;
}

static value_t check_for_overflow(value_t* ptr) {
  if (ptr >= heap_start_v && ptr < heap_end_v) return (value_t) 1;
  else return (value_t) 0;
}

static value_t check_for_overflow_a(value_t* ptr) {
  if (ptr >= heap_start && ptr < heap_end) return (value_t) 1;
  else return (value_t) 0;
}

static value_t* allocate(tag_t tag, value_t size) {
  assert(0 <= size);
  
  // translate size to avoid errors
  value_t translated_size = size;
  if (size < 1)
    translated_size = real_size(size);
  translated_size = real_size(size);

  // find the first free list that can hold the block
  // and allocate the block from it
  value_t lastIndex = 32;
  lastIndex = FREE_LISTS_COUNT - 1;
  value_t* current_block = free_list_heads[lastIndex];
  value_t* prev_block_pointer = NULL;

  if (free_list_heads[lastIndex] == memory_start) return NULL;

  while(check_for_overflow_a(current_block) == 1){// check if this condition is correct ?
    value_t current_block_size = get_header_size(current_block);
    // printf("Stuck in allocate %d", current_block);

    // if size is less then put prev_block_pointer to current
    // make current to move forward.
    value_t delta = current_block_size - translated_size;

    if(delta == 0 || delta == 1) 
      return allocate_for_equal(
        prev_block_pointer, 
        current_block, 
        current_block_size,
        tag
      );  

    else if (delta > 1) 
      return allocate_for_greator(
        prev_block_pointer, 
        current_block, 
        translated_size,
        current_block_size,
        tag
      );
           
    else {
      prev_block_pointer = current_block;
      current_block = addr_v_to_p(current_block[0]);
    } 
      
  }
  
  return NULL;
}



static value_t check_if_lsb_zero(value_t val) {
  value_t last_two_bits_one = 3;
  return (value_t)(val & last_two_bits_one);
}

static void mark(value_t* block) {
  // printf("marking !!");
  if (block >= heap_end || block < heap_start) return;

  // avoid visting the block again
  if (bitmap_is_bit_set(block) == 0) return;

  // mark the block as visited
  // bitmap_set_bit(block);
  bitmap_clear_bit(block);

  // get size and do DFS on sizes
  value_t sz = get_header_size(block);
  int i = 0;
  while (i < sz) {
    value_t val = block[i];
    if (check_if_lsb_zero(val) == 0) {
      value_t* ptr = (value_t*) val;
      if (check_for_overflow(ptr)) mark(addr_v_to_p(ptr));
    }
    i++;
  }
}

static void cln_block(value_t* ptr, value_t size) {
  init_header(ptr, size, tag_None);
  bitmap_clear_bit(ptr);
}

static void clean_blocks() {
  value_t* ptr = heap_first_block;

  while (check_for_overflow_a(ptr)) {
    // printf("cleaning \n");
    value_t size = get_header_size(ptr);
    tag_t tag = get_tag(ptr);

    value_t* nextPtr = ptr +size + 1;
    if (tag != tag_None) {
      if (bitmap_is_bit_set(ptr) == 1) cln_block(ptr, size);
      else bitmap_set_bit(ptr);
    }
    ptr = nextPtr;
  }
}

static void coalese_blocks_and_gen_freelist() {
  value_t* ptr = heap_first_block;
  value_t* head = NULL;
  value_t* prev = NULL;

  while (check_for_overflow_a(ptr)) {
    // printf("coalese \n");
    value_t size = get_header_size(ptr);
    tag_t tag = get_tag(ptr);
    value_t* nextPtr = ptr + size + 1;
    
    if (tag == tag_None) {
      value_t coalese_block_size = 0;
      while (check_for_overflow_a(nextPtr)) {
        tag_t tag_inside = get_tag(nextPtr);
        if (tag_inside != tag_None) break;

        value_t add_factor = get_header_size(nextPtr) + 1; 
        coalese_block_size = coalese_block_size + add_factor;
        nextPtr = nextPtr + add_factor;
      }
      coalese_block_size = coalese_block_size + size;
      init_header(ptr, coalese_block_size, tag_None);

      if (head == NULL) {
        head = ptr;
      } else {
        prev[0] = addr_p_to_v(ptr);
      }
      prev = ptr;
      ptr[0] = 0;
    }
    ptr = nextPtr;
  }
  free_list_heads[FREE_LISTS_COUNT - 1] = head;
}


static void gather_new_free_list() {
  value_t* head = NULL;
  value_t* prev = NULL;

  value_t* ptr = heap_first_block;
  while (ptr < heap_end) {
    // printf("gather new list \n");
    value_t size = get_header_size(ptr);
    tag_t tag = get_tag(ptr);

    value_t* nextPtr = ptr + size + 1;
    if (tag == tag_None) {
      if (head == NULL) {
        head = ptr;
      } else {
        prev[0] = addr_p_to_v(ptr);
      }
      prev = ptr;
      ptr[0] = 0;
    }
    ptr = nextPtr;
  }
  free_list_heads[FREE_LISTS_COUNT - 1] = head;
}

static void sweep() {
  clean_blocks();
  coalese_blocks_and_gen_freelist();
  // gather_new_free_list();
}

value_t* memory_allocate(tag_t tag, value_t size) {
  value_t* first_try = allocate(tag, size);
  if (first_try != NULL)
    return first_try;

  value_t* lb = engine_get_Lb();
  if (lb != memory_start) mark(lb);
  value_t* ib = engine_get_Ib();
  if (ib != memory_start) mark(ib);
  value_t* ob = engine_get_Ob();
  if (ob != memory_start) mark(ob);

  sweep();

  value_t* second_try = allocate(tag, size);
  if (second_try != NULL)
    return second_try;

  fail("\ncannot allocate %d words of memory, even after GC\n", size);
}

value_t memory_get_block_size(value_t* block) {
  return header_unpack_size(block[-1]);
}

tag_t memory_get_block_tag(value_t* block) {
  return header_unpack_tag(block[-1]);
}

#endif
