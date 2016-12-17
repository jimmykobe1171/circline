#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>

#ifndef __HASHMAP_H__
#define __HASHMAP_H__

#define MAP_MISSING -3  /* No such element */
#define MAP_FULL -2 	/* Hashmap is full */
#define MAP_OMEM -1 	/* Out of Memory */
#define MAP_OK 0 	/* OK */

struct hashmap_element{
	char* key;
	int in_use;
	void* data[2];
};

struct hashmap_map{
	int table_size;
	int size;
	int32_t keytype;
	int32_t valuetype;
	struct hashmap_element *data;
};


typedef int (*Func)(void*, void*, void*);


/*
 * Return an empty hashmap. Returns NULL if empty.
*/
extern struct hashmap_map* hashmap_new(int32_t keytyp,int32_t valuetyp);

/*
 * Iteratively call f with argument (item, data) for
 * each element data in the hashmap. The function must
 * return a map status code. If it returns anything other
 * than MAP_OK the traversal is terminated. f must
 * not reenter any hashmap functions, or deadlock may arise.
 */
extern int hashmap_iterate(struct hashmap_map* m, Func f);

extern int hashmap_print(struct hashmap_map* m);

extern struct List* hashmap_keys(struct hashmap_map* m);
/*
 * Add an element to the hashmap. Return MAP_OK or MAP_OMEM.
 */
extern struct hashmap_map* hashmap_put(struct hashmap_map* m,...);

/*
 * Get an element from the hashmap. Return MAP_OK or MAP_MISSING.
 */
extern void* hashmap_get(struct hashmap_map* m,...);

/*
 * Remove an element from the hashmap. Return MAP_OK or MAP_MISSING.
 */
extern struct hashmap_map* hashmap_remove(struct hashmap_map* m,...);

/*
 * Free the hashmap
 */
extern void hashmap_free(struct hashmap_map* m);

/*
 * Get the current size of a hashmap
 */
extern int hashmap_length(struct hashmap_map* m);
extern int32_t hashmap_keytype(struct hashmap_map* m);
extern int32_t hashmap_valuetype(struct hashmap_map* m);

#endif //__HASHMAP_H__
