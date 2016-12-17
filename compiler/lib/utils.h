#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "list.h"

#ifndef _UTILS_H_
#define _UTILS_H_

int32_t printBool(bool a);

/************************************
	Type Declaration
************************************/

#define INT 0
#define FLOAT 1
#define BOOL 2
#define STRING 3
#define NODE 4
#define GRAPH 5
#define LIST 6
#define DICT 7
#define EDGE 8

#define RIGHT_LINK 0
#define LEFT_LINK 1
#define DOUBLE_LINK 2


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
	struct Node* root;
	struct Node** nodes;
	struct Edge* edges;
};



/************************************
	Node Methods
************************************/

struct Node* createNode(int32_t id, int32_t type, ...);

void* nodeGetValue(struct Node* node, int32_t type);
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

int32_t printEdge(struct Edge * edge);
void* edgeGetValue(struct Edge* edge, int32_t type);

/************************************
	Graph Methods
************************************/

struct Graph* createGraph();
struct Graph* copyGraph(struct Graph* a);
struct Graph* mergeGraph(struct Graph* a, struct Graph* b);
struct List* subGraph(struct Graph* a, struct Graph* b);
struct Node* graphGetRoot(struct Graph* g);
int32_t graphSetRoot(struct Graph* g, struct Node * root);
int32_t graphAddList(struct Graph* g, int direction, struct List * l, struct List * edges);
int32_t graphAddNode(struct Graph* g, struct Node * node);
struct List* graphGetAllNodes(struct Graph* g);
struct List* graphRemoveNode(struct Graph* g, struct Node * node);
int32_t graphAddEdgeP( struct Graph* g, struct Node* sour, struct Node* dest, int32_t type, ...);
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
bool graphEdgeExist(struct Graph* g, struct Node* sour, struct Node* dest);
struct Edge* graphGetEdge(struct Graph* g, struct Node* sour, struct Node* dest);
int32_t graphNumOfNodes(struct Graph* g);
int32_t graphNumOfEdges(struct Graph* g);
struct List* graphGetChildNodes(struct Graph* g, struct Node* rt);
int32_t printGraph(struct Graph* g);

#endif /* #ifndef _UTILS_H_ */
