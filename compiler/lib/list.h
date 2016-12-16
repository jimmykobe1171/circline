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

int32_t rangeHelper(int size, int index);
struct List* createList( int32_t type);
struct List* addListHelper( struct List * list, void* addData);
struct List* concatList(struct List* list1, struct List* list2);
struct List* pushList(struct List* list, ...);
struct List* addList(struct List* list, ...);
void* getList(struct List* list, int index);
void* popList(struct List* list);
int32_t setList(struct List* list, int index, ...);
int getListSize(struct List* list);
int32_t removeList(struct List* list, int index);
int32_t pirntList(struct List * list);



