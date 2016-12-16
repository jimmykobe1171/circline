#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#ifndef _LIST_H_
#define _LIST_H_

// one element of a list.
struct List {
	int32_t type;
	int32_t size;
	void* *arr;
	int32_t curPos;
};

int32_t rangeHelper(int size, int index);
struct List* createList( int32_t type);
struct List* addListHelper( struct List * list, void* addData);
struct List* addList(struct List* list, ...);
void* getList(struct List* list, int index);
int32_t popList(struct List* list);
int32_t setList(struct List* list, int index, ...);
int getListSize(struct List* list);
int32_t removeList(struct List* list, int index);
int32_t pirntList(struct List * list);

#endif
