#include <stdint.h>

#ifndef _UTILS_H_
#define _UTILS_H_

/************************************
	Type Declaration
************************************/

struct Node {
	int32_t id;
	int32_t type;
	int32_t a;
	double b;
	bool c;
	char* d;
};

struct Edge {
	struct Node* sour;
	struct Node* dest;
	int32_t type;
	int32_t a;
	double b;
	bool c;
	char* d;
};

struct Graph {
	int32_t vn;
	int32_t en;
	int32_t vn_len;
 	int32_t en_len;
	struct Node** nodes;
	struct Edge* edges;
};

/************************************
	Node Methods
************************************/

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

int32_t printNode(struct Node * node);

/************************************
	Edge Methods
************************************/

struct Edge createEdge(
	struct Node* sour,
	struct Node* dest,
	int32_t type,
	int32_t a,
	double b,
	bool c,
	char* d
);

/************************************
	Graph Methods
************************************/

struct Graph* createGraph();
struct Graph* copyGraph(struct Graph* a);
int32_t graphAddNode(struct Graph* g, struct Node * node);
int32_t graphAddEdge(
	struct Graph* g,
	struct Node* sour,
	struct Node* dest,
	int32_t type,
	int32_t a,
	double b,
	bool c,
	char* d
);
int32_t printGraph(struct Graph* g);

#endif /* #ifndef _UTILS_H_ */