#include <stdint.h>

#ifndef _UTILS_H_
#define _UTILS_H_

struct Node {
	int32_t id;
	int32_t type;
	int32_t a;
	double b;
	bool c;
	char* d;
};

struct Node* createNode(
	int32_t id,
	int32_t type,
	int32_t a,
	double b,
	bool c,
	char* d
);

// one element of a list.
struct List {
	int32_t type;
	int32_t size;
	int32_t *arr;
	int32_t curPos;
};

struct List* createList(
	int32_t type
);

struct List* addList(
	struct List * list,
	int32_t addData
);

int pirntList(struct List * list);

int printNode(struct Node * node);

#endif /* #ifndef _UTILS_H_ */