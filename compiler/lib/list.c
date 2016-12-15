#include "list.h"

struct List* createList(
	int32_t type
) {
	struct List* new = (struct List*) malloc(sizeof(struct List));
	// default initialize size is 1
	new->size = 1;
	new->type = type;
	// means that the next element would be added at curPos
	new->curPos = 0;
	new->arr = (int *)malloc(new->size * sizeof(int));
	return new;
}

struct List* addList(
	struct List * list,
	int32_t addData
){
	if (list->curPos >= list->size){
		list->size = list->size * 2;
		// double size
		list->arr = (int *) realloc(list->arr, list->size * sizeof (int));
	}
	*(list->arr + list->curPos) = addData;
	list->curPos++;
	return list;
}

int32_t printList(struct List * list){
	int curPos = list->curPos - 1;
	int p = 0;
	printf("list:[");
	while(p < curPos){
		printf("%d, ", *(list->arr + p));
		p++;
	}
	printf("%d]\n", *(list->arr + curPos));
	return 1;
}

char* get_str_from_void_ptr(void * ptr){
	return (char *) ptr;
}