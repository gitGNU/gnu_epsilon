/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

   This file is part of GNU epsilon.

   GNU epsilon is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU epsilon is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>. */


#include "large.h"
#include "malloc.h"
#include <epsilongc_threads.h>

static epsilongc_semaphore_t destroy_condemned_large_objects_semaphore = NULL;

epsilongc_large_object_header_t epsilongc_large_object_payload_to_large_object_header(const epsilongc_word_t payload_pointer){
  /* Large object buffers are page-aligned, so we can simply zero the
     least-significant bits of the payload address; this assumes that the
     payload is in the first page. */
  return (epsilongc_large_object_header_t)
    (((epsilongc_unsigned_integer_t)payload_pointer) & EPSILONGC_PAGE_BITMASK);
}

epsilongc_word_t epsilongc_large_object_header_to_large_object_payload(const epsilongc_large_object_header_t header_pointer){
  return header_pointer->payload_beginning;
}

epsilongc_large_object_header_t
epsilongc_make_large_object_and_return_its_header(const epsilongc_integer_t payload_size_in_words,
                                                  const epsilongc_integer_t alignment_in_words,
                                                  const epsilongc_kind_tag_t tag,
                                                  const epsilongc_kind_datum_t datum,
                                                  const epsilongc_tracer_t tracer,
                                                  const epsilongc_integer_t pointers_no_in_the_worst_case){
  /* Just on debug, check whether the alignment is reasonable. Is this too inefficient
     for production? */
#ifdef ENABLE_ASSERTIONS
  if(alignment_in_words <= 0)
    epsilongc_fatal("epsilongc_make_large_object(): the alignment must be at least one word");
#endif // #ifdef ENABLE_ASSERTIONS
  
  /* Compute the required offset for the first word of the payload to respect alignment;
     consider that the header+payload buffer is page-aligned, and we may need to have a
     padding between the header and the payload: */
  const epsilongc_integer_t payload_size_in_bytes = payload_size_in_words * sizeof(epsilongc_word_t);
  const epsilongc_integer_t alignment_in_bytes = alignment_in_words * sizeof(epsilongc_word_t);
  const epsilongc_integer_t disalignment_of_the_first_payload_byte =
    sizeof(struct epsilongc_large_object_header) % alignment_in_bytes;
  const epsilongc_integer_t padding_size_in_bytes =
    (alignment_in_bytes - disalignment_of_the_first_payload_byte) % alignment_in_bytes;
  epsilongc_assert_on_debug(padding_size_in_bytes >= 0);
  epsilongc_assert_on_debug(padding_size_in_bytes < alignment_in_bytes);
  epsilongc_assert_on_debug(((sizeof(struct epsilongc_large_object_header) + padding_size_in_bytes) % alignment_in_bytes) == 0);
  
  /* Ok, now that we know the padding size we can compute the total buffer size for
     header + padding + payload, and how many pages it would take, if it used pages: */
  const epsilongc_integer_t buffer_size_in_bytes =
    sizeof(struct epsilongc_large_object_header) + padding_size_in_bytes + payload_size_in_bytes;
  const epsilongc_integer_t pages_no =
    (((buffer_size_in_bytes + 1) % EPSILONGC_PAGE_SIZE_IN_BYTES) == 0) ?
    (buffer_size_in_bytes + 1) / EPSILONGC_PAGE_SIZE_IN_BYTES :
    (buffer_size_in_bytes + 1) / EPSILONGC_PAGE_SIZE_IN_BYTES + 1;
  
  /* printf("--------------------------\n"); */
  /* printf("Payload size: %i bytes\n", (int)payload_size_in_bytes); */
  /* printf("Alignment:    %i bytes\n\n", (int)alignment_in_bytes); */
  /* printf("Header size:  %i bytes\n", (int)sizeof(struct epsilongc_large_object_header)); */
  /* printf("Padding:      %i bytes\n", (int)padding_size_in_bytes); */
  /* printf("Buffer size:  %i bytes (%i %i-bytes pages)\n", (int)buffer_size_in_bytes, (int)pages_no, EPSILONGC_PAGE_SIZE_IN_BYTES); */
  /* printf("--------------------------\n"); */
  
  /* Allocate the buffer and fill the header. The payload content is *not* initialized: */
  void *buffer_as_void_star;
  if(EPSILONGC_UNLIKELY(posix_memalign(&buffer_as_void_star,
                                       EPSILONGC_PAGE_SIZE_IN_BYTES,
                                       buffer_size_in_bytes) != 0))
    epsilongc_fatal("posix_memalign() failed when making a large object");
  epsilongc_large_object_header_t buffer = buffer_as_void_star;
  
  buffer->tag = tag;
  buffer->datum = datum;
  buffer->tracer = tracer;
  buffer->is_marked = false;
  buffer->object_size_in_words = payload_size_in_words;
  buffer->size_in_pages = pages_no;
  buffer->pointers_no_in_the_worst_case = pointers_no_in_the_worst_case;
  buffer->payload_beginning =
    ((char*)buffer) + sizeof(struct epsilongc_large_object_header) + padding_size_in_bytes;
  
  /* Check that the payload is in the first page: some insane alignment requirements
     could invalidate this assumption, and we depend on it: */
  if(EPSILONGC_UNLIKELY(epsilongc_large_object_payload_to_large_object_header(buffer->payload_beginning) != buffer))
    epsilongc_fatal("the required alignment for a large object is too big");
  
  /* Ok, we've made the data structure without requiring any critical section, but now we have
     to update some global structures. Add the object to the list of alive large objects and
     add an entry per "page" in the page table; finally add stack blocks: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  EPSILONGC_PREPEND_OBJECT_TO_LIST(large_object_headers, large_object_headers,
                                   epsilongc_large_object_header_t,
                                   buffer, &epsilongc_alive_large_objects);
  epsilongc_add_large_object_header_to_the_set_of_pages(buffer);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_make_or_destroy_stack_blocks_as_needed(pointers_no_in_the_worst_case, 1);

  /* Shall we collect? */
  if(epsilongc_should_we_garbage_collect()){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
    printf("%s: collecting at large object allocation time.\n", epsilongc_calling_thread_name()); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
    epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
    epsilongc_request_garbage_collection();
  } // if
  else
    epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
  
  /* Return a pointer to the header, i.e. to the beginning of the whole buffer: */
  return buffer;
}

/* Deallocate the large object whose header is given; global structures are
   not touched, and no synchronization is performed:  */
inline static void 
deallocate_large_object_buffer(const epsilongc_large_object_header_t large_object_header){
  //printf("%s: Freeing %p\n", epsilongc_calling_thread_name(), large_object_header);
  free(large_object_header); // this free()s the whole buffer, of course
  // To do: this is possible only on GNU...
}

/* Make the given large object (a pointer to its *header* must be supplied) dead
   by moving it to the appropriate list, without destroying it. The object is
   assumed to be alive. This is safe to call from the garbage collector, as it
   doesn't use singal-unsafe primitives such as malloc() and free(); the page
   table is not touched here. */
static void epsilongc_condemn_large_object_unlocked(const epsilongc_large_object_header_t large_object_header){
  /* Detach the object from the list of alive large objects, and attach it
     to the list of dead large objects: */
  epsilongc_assert_on_debug(EPSILONGC_DOES_ELEMENT_BELONG_TO_LIST(large_object_headers, large_object_headers,
                                                                  epsilongc_large_object_header_t, large_object_header,
                                                                  &epsilongc_alive_large_objects));
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(large_object_headers, large_object_headers, epsilongc_large_object_header_t,
                                     large_object_header, &epsilongc_alive_large_objects);
  EPSILONGC_APPEND_OBJECT_TO_LIST(large_object_headers, large_object_headers, epsilongc_large_object_header_t,
                                  large_object_header, &epsilongc_condemned_large_objects);
}

void epsilongc_sweep_large_objects_unlocked(epsilongc_sweep_statistics_t sweep_statistics){
  /* For each object in the list of alive large objects... */
  epsilongc_large_object_header_t large_object_header;
  for(large_object_header = EPSILONGC_FIRST_ELEMENT_OF_LIST(large_object_headers, &epsilongc_alive_large_objects);
      large_object_header != NULL;
      /* the "increment" is in the body */){
    /* Get a pointer to the next object now, as we may soon modify the
       pointer in large_object_header: */
    epsilongc_large_object_header_t next_large_object_header =
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(large_object_headers, large_object_header);
    
    /* Condemn the current object, if it's unmarked; if it's marked then
       unmark it: */
    sweep_statistics->swept_pages_no += large_object_header->size_in_pages;
    if(! large_object_header->is_marked){
      sweep_statistics->dead_bytes += large_object_header->size_in_pages * EPSILONGC_PAGE_SIZE_IN_BYTES;
      epsilongc_condemn_large_object_unlocked(large_object_header);
    }
    else{
      sweep_statistics->alive_bytes += large_object_header->size_in_pages * EPSILONGC_PAGE_SIZE_IN_BYTES;
      large_object_header->is_marked = false;
    } // else
    
    /* Ok, go on with the next large object in the alive list (or NULL): */
    large_object_header = next_large_object_header;
  } // for
}

/* Destroy all the objects in the list of dead large objects, with the
   appropriate synchronization: */
static void epsilongc_destroy_all_dead_large_objects_in_the_list(void){
  /* This is a single big critical section; large object collectign is
     currently not parallel at all, and quite inefficient. I don't believe
     that to be a problem in practice, as large objects should be very
     few in practice.
     We want to destroy all condemned large objects before the beginning
     of the next collection, hence the need for the single critical
     section which competes with page change. */
  epsilongc_integer_t delta_pointers = 0;
  epsilongc_integer_t delta_objects = 0;
  epsilongc_integer_t destroyed_pages_no = 0;
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);

  /* Make a copy of the dead object list (just a few pointers;
     obviously elements are not copied here), and reset the
     old list, so that we can freely mess with the old list. */
  struct epsilongc_large_object_header_list copy_of_the_dead_list =
    epsilongc_condemned_large_objects;
  EPSILONGC_INITIALIZE_LIST(large_object_headers, &epsilongc_condemned_large_objects);

  /* Remove references to the soon-to-be-dead objects
     from the page table and destroy stack blocks: */
  epsilongc_large_object_header_t large_object_header;
  for(large_object_header = EPSILONGC_FIRST_ELEMENT_OF_LIST(large_object_headers, &copy_of_the_dead_list);
      large_object_header != NULL;
      large_object_header = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(large_object_headers, large_object_header)){
    epsilongc_remove_large_object_header_from_the_set_of_pages(large_object_header);
    delta_objects --;
    delta_pointers -= large_object_header->pointers_no_in_the_worst_case;
    destroyed_pages_no += large_object_header->size_in_pages;
  } // for
  
  /* Deallocate each object: */
  large_object_header =
    EPSILONGC_FIRST_ELEMENT_OF_LIST(large_object_headers, &copy_of_the_dead_list);
  while(large_object_header != NULL){
    /* Read the "next" pointer now, before we destroy the buffer containing it: */
    epsilongc_large_object_header_t next_large_object_header =
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(large_object_headers, large_object_header);
    
    /* Destroy the buffer. Note that we're not detaching the element from the
       list, as we don't care if it becomes inconsistent: we're destroying
       the whole list: */
    deallocate_large_object_buffer(large_object_header);
    
    /* Advance the pointer: */
    large_object_header = next_large_object_header;
  } // while
  
  /* Destroy stack blocks, and close the big critical section: */
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_make_or_destroy_stack_blocks_as_needed(delta_pointers, delta_objects);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
  /*
  printf("Destroyed %3.2fMb of large objects (%i pages); we now have %i pages.\n",
         destroyed_pages_no / 1024. / 1024. * EPSILONGC_PAGE_SIZE_IN_BYTES,
         (int)destroyed_pages_no,
         (int)epsilongc_pages_no());
  */
}

void epsilongc_request_condemned_large_object_destruction(void){
  /* Wake up the large object destroyer thread: */
  epsilongc_v_semaphore(destroy_condemned_large_objects_semaphore);
}

void* epsilongc_destroy_large_objects_thread(void *unused){
  /* In an infinite loop... */
  while(true){
    /* Wait until we have something to destroy... */
    epsilongc_p_semaphore(destroy_condemned_large_objects_semaphore);
    
    /* And destroy everything on the list. This internally performs all
       the needed synchronization: */
    epsilongc_destroy_all_dead_large_objects_in_the_list();
    
    /* The list is now empty. */
  } // while
  return NULL; // never reached
}

/* Destroy all the large objects in the given list: */
static void epsilongc_destroy_large_objects_unlocked(struct epsilongc_large_object_header_list *list){
  /* For each dead large object...: */
  epsilongc_large_object_header_t large_object_header =
    EPSILONGC_FIRST_ELEMENT_OF_LIST(large_object_headers, list);
  while(large_object_header != NULL){
    /* Read the "next" pointer now, before we destroy the buffer containing it: */
    epsilongc_large_object_header_t next_large_object_header =
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(large_object_headers, large_object_header);
    /* Detach the dead large object from the list and destroy it: */
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(large_object_headers, large_object_headers, epsilongc_large_object_header_t,
                                       large_object_header, list);
    epsilongc_remove_large_object_header_from_the_set_of_pages(large_object_header);
    deallocate_large_object_buffer(large_object_header);
    /* Advance the pointer: */
    large_object_header = next_large_object_header;
  } // while
}

struct epsilongc_large_object_header_list epsilongc_alive_large_objects;
struct epsilongc_large_object_header_list epsilongc_condemned_large_objects;

void epsilongc_initialize_large_object_support(void){
  EPSILONGC_INITIALIZE_LIST(large_object_headers, &epsilongc_alive_large_objects);
  EPSILONGC_INITIALIZE_LIST(large_object_headers, &epsilongc_condemned_large_objects);
  /* Make the semaphore, if not already present: */
  if(destroy_condemned_large_objects_semaphore == NULL)
    destroy_condemned_large_objects_semaphore =
      epsilongc_make_semaphore("large-object-destroyer", 0, 0);
}

void epsilongc_finalize_large_object_support(void){
  /* Destroy all large objects, alive or dead, at finalization time: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  epsilongc_destroy_large_objects_unlocked(&epsilongc_alive_large_objects);
  epsilongc_destroy_large_objects_unlocked(&epsilongc_condemned_large_objects);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}
