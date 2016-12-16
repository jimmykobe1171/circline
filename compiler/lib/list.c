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
	new->size = 8;
	new->type = type;
	// means that the next element would be added at curPos
	new->curPos = 0;
	new->arr = (void**)malloc(new->size * sizeof(void*));
	return new;
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
			data = BooltoVoid(va_arg(ap, double));
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
	int curPos = list->curPos -1;
	return *(list->arr + index);
}

int32_t setList(struct List* list, int index, ...){
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
			data = BooltoVoid(va_arg(ap, double));
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
	int curPos = list->curPos - 1;
	*(list->arr + index) = data;
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
				printf("%d, ", *((bool*)(*(list->arr + p))));
				p++;
			}
			printf("%d", *((bool*)(*(list->arr + p))));
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

char* get_str_from_void_ptr(void * ptr){
	return (char *) ptr;
}


// int main() {
// 	struct List* a = createList(INT);
// 	addList(a, 10);
// 	addList(a, 5);
// 	setList(a, 0, 3);
// 	printList(a);
// 	//printNode(VoidtoNode(getList(a,2)));
// }
