#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "utils.h"
#include "cast.h"
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
	new->arr = (void**)malloc(new->size * sizeof(void*));
	return new;
}

int rangeHelper(int size, int index){
	if(size <= -index || size <= index || size == 0){
		printf("Error! Index out of Range!\n");
		exit(1);
	}
	if (index < 0){
		index += size;
	}
	return index;
}

struct List* addListHelper(
	struct List * list,
	void* addData
){
	if (list->curPos >= list->size){
		list->size = list->size * 2;
		// double size
		list->arr = (void**) realloc(list->arr, list->size * sizeof (void*));
	}
	*(list->arr + list->curPos) = addData;
	list->curPos++;
	return list;
}

struct List* concatList(struct List* list1, struct List* list2){
	for(int i =0; i < list2->curPos; i++){
		list1 = addListHelper(list1, *(list2->arr+i));
	}
	return list1;
}

struct List* addList(struct List* list, ...) {
	va_list ap;
	va_start(ap, 1);
	void * data;
	int* tmp0;
	double* tmp1;
	bool* tmp2;
	switch (list->type) {
		case INT:
			data = InttoVoid(va_arg(ap, int));
			break;

		case FLOAT:
			data = FloattoVoid(va_arg(ap, double));
			break;

		case BOOL:
			data = BooltoVoid(va_arg(ap, bool));
			break;

		case STRING:
			data = StringtoVoid(va_arg(ap, char*));
			break;

		case NODE:
			data = NodetoVoid(va_arg(ap, struct Node*));
			break;

		case GRAPH:
			data = GraphtoVoid(va_arg(ap, struct Graph*));
			break;

		default:
			break;
	}
  va_end(ap);
  return addListHelper(list, data);
}

void* getList(struct List* list, int index){
	index = rangeHelper(list->curPos, index);
	return *(list->arr + index);
}

void* popList(struct List* list){
	if(list->curPos-1 < 0){
		printf("Error! Nothing Can be poped T.T\n");
		exit(1);
	}
	void* add = *(list->arr + list->curPos-1);
	list->curPos--;
	return add;
}

int32_t setList(struct List* list, int index, ...){
	index = rangeHelper(list->curPos, index);
	va_list ap;
	va_start(ap, 1);
	void * data;
	int* tmp0;
	double* tmp1;
	bool* tmp2;
	switch (list->type) {
		case INT:
			data = InttoVoid(va_arg(ap, int));
			break;

		case FLOAT:
			data = FloattoVoid(va_arg(ap, double));
			break;

		case BOOL:
			data = BooltoVoid(va_arg(ap, bool));
			break;

		case STRING:
			data = StringtoVoid(va_arg(ap, char*));
			break;

		case NODE:
			data = NodetoVoid(va_arg(ap, struct Node*));
			break;

		case GRAPH:
			data = GraphtoVoid(va_arg(ap, struct Graph*));
			break;

		default:
			break;
	}
	*(list->arr + index) = data;
	return 0;
}

int getListSize(struct List* list){
	return list->curPos;
}

int32_t removeList(struct List* list, int index){
	index =rangeHelper(list->curPos, index);
	for(int i=index; i < list->curPos; i++){
		*(list->arr + i) = *(list->arr + i+1);
	}
	list->curPos--;
	return 0;
}

int32_t printList(struct List * list){
	int curPos = list->curPos - 1;
	int p = 0;
	printf("list:[");
	switch (list->type) {
		case INT:
			while(p < curPos){
				printf("%d, ", *((int*)(*(list->arr + p))));
				p++;
			}
			printf("%d", *((int*)(*(list->arr + p))));
			break;

		case FLOAT:
			while(p < curPos){
				printf("%f, ", *((double*)(*(list->arr + p))));
				p++;
			}
			printf("%f", *((double*)(*(list->arr + p))));
			break;

		case BOOL:
			while(p < curPos){
				printf("%s, ", *((bool*)(*(list->arr + p))) ? "true" : "false");
				p++;
			}
			printf("%s", *((bool*)(*(list->arr + p))) ? "true" : "false");
			break;

		case STRING:
			while(p < curPos){
				printf("%s, ", ((char*)(*(list->arr + p))));
				p++;
			}
			printf("%s", ((char*)(*(list->arr + p))));
			break;

		case NODE:
			while(p < curPos){
				printNode((struct Node*)(*(list->arr + p)));
				p++;
			}
			printNode((struct Node*)(*(list->arr + p)));
			break;

		case GRAPH:
			while(p < curPos){
				printGraph((struct Graph*)(*(list->arr + p)));
				p++;
			}
			printGraph((struct Graph*)(*(list->arr + p)));
			break;

		default:
			printf("Unsupported List Type!\n");
			return 1;
	}
	printf("]\n");
	return 0;
	// int p = 0;
	// printf("list:[");
	// while(p < curPos){
	// 	printf(fmt, *(list->arr + p));
	// 	printf(", ");
	// 	p++;
	// }
	// printf(fmt, *(list->arr + curPos));
	// printf("]\n");
	// return 1;
}



// int main() {
// 	struct List* a = createList(INT);
// 	addList(a, 10);
// 	addList(a, 5);
// 	addList(a, 7);
// 	addList(a, 9);
// 	setList(a, 0, 3);
// 	removeList(a, 0);
	
// 	printList(a);
// 	//printNode(VoidtoNode(getList(a,2)));
// }
