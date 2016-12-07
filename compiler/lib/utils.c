#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "utils.h"

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
	new->size = 1;
	new->type = type;
	// means that the next element would be added at curPos
	new->curPos = 0;
	new->arr = (int *)malloc(new->size * sizeof(int));
	// *(new->arr + 0) = 1;
	// *(new->arr + 1) = 2;
	// *(new->arr + 2) = 3;
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

int32_t printNode(struct Node * node) {
	if (node == NULL) return 1;
	switch (node->type) {
		case 0:
			printf("node%3d: %d\n", node->id, node->a);
			break;
		case 1:
			printf("node%3d: %f\n", node->id, node->b);
			break;
		case 2:
			printf("node%3d: %s\n", node->id, node->c ? "true" : "false");
			break;
		case 3:
			printf("node%3d: %s\n", node->id, node->d);
			break;
		default:
			printf("node%3d\n", node->id);
			break;
	}
	return 0;
}

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
) {
	struct Edge e = {sour, dest, type, a, b, c, d};
	return e;
}

int32_t printEdge(struct Edge * edge) {
	if (edge == NULL) return 1;
	switch (edge->type) {
		case 0:
			printf("edge%3d->%3d: %d\n", edge->sour->id, edge->dest->id, edge->a);
			break;
		case 1:
			printf("edge%3d->%3d: %f\n", edge->sour->id, edge->dest->id, edge->b);
			break;
		case 2:
			printf("node%3d->%3d: %s\n", edge->sour->id, edge->dest->id, edge->c ? "true" : "false");
			break;
		case 3:
			printf("edge%3d->%3d: %s\n", edge->sour->id, edge->dest->id, edge->d);
			break;
		default:
			printf("edge%3d->%3d\n", edge->sour->id, edge->dest->id);
			break;
	}
	return 0;
}

/************************************
	Graph Methods
************************************/

// struct Graph {
// 	int32_t vn;
// 	int32_t en;
//  int32_t vn_len;
//  int32_t en_len;
// 	Node** nodes;
// 	Edge* edges;
// };

struct Graph* createGraph() {
	struct Graph* g = (struct Graph*) malloc( sizeof(struct Graph) );
	g->vn = 0;
	g->en = 0;
	g->vn_len = 16;
	g->en_len = 64;
	g->nodes = (struct Node**) malloc( sizeof(struct Node*) * 16 );
	g->edges = (struct Edge*) malloc( sizeof(struct Edge) * 64 );
	return g;
}
struct Graph* copyGraph(struct Graph* a) {
	struct Graph* g = (struct Graph*) malloc( sizeof(struct Graph) );
	memcpy(g, a, sizeof(struct Graph));
	g->nodes = (struct Node**) malloc( sizeof(struct Node*) * a->vn_len );
	g->edges = (struct Edge*) malloc( sizeof(struct Edge) * a->en_len );
	int i;
	for (i=0; i<a->vn; i++) {
		g->nodes[i] = a->nodes[i];
	}
	struct Edge* tmp;
	for (i=0; i<a->en; i++) {
		tmp = (struct Edge*) malloc( sizeof(struct Edge) );
		memcpy(tmp, &a->edges[i], sizeof(struct Edge));
		g->edges[i] = *tmp;
	}
	return g;
}

int32_t graphAddNode(struct Graph* g, struct Node * node) {
	if (g == NULL) {
		printf("Graph doesn't exist!\n");
		return 1;
	}
	if (g->vn + 1 >= g->vn_len) {
		printf("# Graph Nodes reach the limit!\n");
		return 1;
	}
	int i;
	// Nodes already exist in the graph
	for (i=0; i<g->vn; i++) {
		if (g->nodes[i] == node) return 0;
	}
	g->nodes[i] = node;
	g->vn++;
	return 0;
}

int32_t graphAddEdge(
	struct Graph* g,
	struct Node* sour,
	struct Node* dest,
	int32_t type,
	int32_t a,
	double b,
	bool c,
	char* d
) {
	if (g == NULL) {
		printf("Graph doesn't exist!\n");
		return 1;
	}
	if (g->vn + 1 >= g->vn_len) {
		printf("# Graph Nodes reach the limit!\n");
		return 1;
	}
	int i;
	// Nodes already exist in the graph
	for (i=0; i<g->en; i++) {
		if (g->edges[i].sour == sour && g->edges[i].dest == dest) {
			g->edges[i].type = type;
			g->edges[i].a = a;
			g->edges[i].b = b;
			g->edges[i].c = c;
			g->edges[i].d = d;
			return 0;
		}
	}
	struct Edge e = createEdge(sour, dest, type, a, b, c, d);
	g->edges[i] = e;
	g->en++;
	return 0;
}

int32_t printGraph(struct Graph* g) {
	printf("# Nodes: %d\n", g->vn);
	int i;
	for (i=0; i<g->vn; i++) {
		printNode(g->nodes[i]);
	}
	printf("--------------------------------------\n");
	printf("# Edges: %d\n", g->en);
	for (i=0; i<g->en; i++) {
		printEdge(&g->edges[i]);
	}
	return 0;
}

int printList(struct List * list){
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



// int main() {
// 	//test list
	// struct List* list = createList(1);
	// printf("list type:%d\n", list->type);
	// struct List* newList = addList(addList(addList(addList(list, 52), 53), 54), 55);
	// printList(list);

// 	struct Node* a = createNode(1, 0, 12, 0, 0, NULL);
// 	struct Node* b = createNode(2, 1, 0, 1.2, 0, NULL);
// 	struct Node* c = createNode(3, 2, 0, 0, 0, NULL);
// 	struct Node* d = createNode(4, 3, 0, 0, 1, "Hello World!");
// 	struct Graph* g = createGraph();
// 	graphAddNode(g, a);
// 	graphAddNode(g, b);
// 	graphAddNode(g, c);
// 	graphAddNode(g, d);
// 	graphAddEdge(g, a, b, 3,0,0,0,"Edge1");
// 	graphAddEdge(g, b, c, 2,0,0,1,NULL);

// 	struct Graph* g2 = copyGraph(g);
// 	g->edges[0].d = "ffff";
// 	d->d = "????";
// 	graphAddEdge(g2, c, d, 1,0,3.3,0,NULL);

// 	printGraph(g);
// 	printf("****************************\n");
// 	printGraph(g2);

// }