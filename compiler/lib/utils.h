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

int print_node(struct Node * node);

#endif /* #ifndef _UTILS_H_ */