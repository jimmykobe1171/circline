#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "utils.h"
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

struct List* addList(int n, ...) {
	va_list ap;

	va_start(ap, n);
	int32_t type = va_arg(ap, int);
	struct List * list = va_arg(ap, struct List *);
	void * data;
	int* tmp0;
	double* tmp1;
	bool* tmp2;
	switch (type) {
		case INT:
			tmp0 = (int*)malloc(sizeof(int));
			*tmp0 = va_arg(ap, int);
			data = (void*) tmp0;
			break;

		case FLOAT:
			tmp1 = (double*)malloc(sizeof(double));
			*tmp1 = va_arg(ap, double);
			data = (void*) tmp1;
			break;

		case BOOL:
			tmp2 = (bool*)malloc(sizeof(bool));
			*tmp2 = va_arg(ap, bool);
			data = (void*) tmp2;
			break;

		case STRING:
			data = (void*) va_arg(ap, char*);
			break;

		default:
			break;
	}
  va_end(ap);
  return addListHelper(list, data);
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

int main() {
	struct List* a = createList(STRING);
	addList(3, STRING, a, "123");
	addList(3, STRING, a, "456");
	addList(3, STRING, a, "789");
	printList(a);
}
