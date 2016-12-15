#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

// one element of a list.
struct List {
	int32_t type;
	int32_t size;
	void* *arr;
	int32_t curPos;
};

struct List* createList(
	int32_t type
);

struct List* addListHelper(
	struct List * list,
	void* addData
);
void* getList(struct List* list, int index);
int32_t pirntList(struct List * list);


char* get_str_from_void_ptr(void * ptr);

