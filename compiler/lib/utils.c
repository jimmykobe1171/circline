#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include "utils.h"

struct Node* createNode(
	int32_t id,
	int32_t type,
	int32_t a,
	double b,
	bool c,
	char* d
) {
	struct Node* new = (struct Node*) malloc(sizeof(struct Node));
	new->id = id;
	new->type = type;
	new->a = a;
	new->b = b;
	new->c = c;
	new->d = d;
	return new;
}

struct List* createList(
	int32_t type
) {
	struct List* new = (struct List*) malloc(sizeof(struct List));
	// default initialize size is 1
	new->size = 3;
	new->type = type;
	// means that the next element would be added at curPos
	new->curPos = 3;
	new->arr = (int *)malloc(new->size * sizeof(int));
	*(new->arr + 0) = 1;
	*(new->arr + 1) = 2;
	*(new->arr + 2) = 3;
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

int printNode(struct Node * node) {
	if (node == NULL) return 0;
	switch (node->type) {
		case 0:
			printf("node:%d\n", node->a);
			break;
		case 1:
			printf("node:%f\n", node->b);
			break;
		case 2:
			printf("node:%s\n", node->c ? "true" : "false");
			break;
		case 3:
			printf("node:%s\n", node->d);
			break;
		default:
			break;
	}
	return 1;
}

int printList(struct List * list){
	int curPos = list->curPos - 1;
	int p = 0;
	while(p <= curPos){
		printf("element of list:%d\n", *(list->arr + p));
		p++;
	}
	return 1;
}



// int main() {
// 	//test node
// 	// struct Node* a = createNode(1, 0, 12, 0, 0, NULL);
// 	// struct Node* b = createNode(1, 1, 0, 1.2, 0, NULL);
// 	// struct Node* c = createNode(1, 2, 0, 0, 0, NULL);
// 	// struct Node* d = createNode(1, 3, 0, 0, 1, "Hello World!");
// 	// print_node(a);
// 	// print_node(b);
// 	// print_node(c);
// 	// print_node(d);

// 	//test list
// 	struct List* list = createList(1);
// 	printf("list type:%d\n", list->type);
// 	// struct List* newList = addList(addList(addList(addList(list, 52), 53), 54), 55);
// 	printList(list);

// }