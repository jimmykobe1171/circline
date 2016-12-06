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

int print_node(struct Node * node) {
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

// int main() {
// 	struct Node* a = createNode(1, 0, 12, 0, 0, NULL);
// 	struct Node* b = createNode(1, 1, 0, 1.2, 0, NULL);
// 	struct Node* c = createNode(1, 2, 0, 0, 0, NULL);
// 	struct Node* d = createNode(1, 3, 0, 0, 1, "Hello World!");
// 	print_node(a);
// 	print_node(b);
// 	print_node(c);
// 	print_node(d);
// }