#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "utils.h"
#include "hashmap.c"
#include "list.c"
#include "cast.c"

/************************************
	Node Methods
************************************/

struct Node* createNode(int32_t id, int32_t type, ...) {
	struct Node* new = (struct Node*) malloc(sizeof(struct Node));
	new->id = id;
	new->type = type;

	va_list ap;
	va_start(ap, 1);
	switch (type) {
		case INT:
			new->a = va_arg(ap, int);	break;
		case FLOAT:
			new->b = va_arg(ap, double);	break;
		case BOOL:
			new->c = va_arg(ap, bool);	break;
		case STRING:
			new->d = va_arg(ap, char*);	break;
		default:
			break;
	}
  va_end(ap);
  return new;
}

int32_t printNode(struct Node * node) {
	if (node == NULL) exit(1);
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
	if (edge == NULL) exit(1);
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

int32_t graphAddEdgeHelper(struct Graph* g, struct Edge e) {
	if (g == NULL) exit(1);
	int i;
	for (i=0; i < g->en; i++) {
		if (g->edges[i].sour == e.sour && g->edges[i].dest == e.dest) {
			g->edges[i] = e;
			return 0;
		}
	}
	g->edges[i] = e;
	g->en ++;
	return 0;
}

int32_t graphAddEdgeP( struct Graph* g, struct Node* sour, struct Node* dest, int32_t type, ...) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	if (g->vn + 1 >= g->vn_len) {
		printf("[Error] # Graph Nodes reach the limit!\n");
		exit(1);
	}
	if (graphAddNode(g, sour) > 0) exit(1);
	if (graphAddNode(g, dest) > 0) exit(1);

	// Assign the Edge Value
	struct Edge e = createEdge(sour, dest, type, 0, 0, 0, NULL);
	va_list ap;
	va_start(ap, 1);
	void* tmp = va_arg(ap, void*);
	switch (type) {
		case INT:
			e.a = *((int*)tmp);	break;
		case FLOAT:
			e.b = *((double*)tmp);	break;
		case BOOL:
			e.c = *((bool*)tmp);	break;
		case STRING:
			e.d = ((char*)tmp);	break;
		default:
			break;
	}
  va_end(ap);

	int i;
	// Edges already exist in the graph
	for (i=0; i<g->en; i++) {
		if (g->edges[i].sour == sour && g->edges[i].dest == dest) {
			g->edges[i] = e;
			return 0;
		}
	}
	g->edges[i] = e;
	g->en++;
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
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	if (g->vn + 1 >= g->vn_len) {
		printf("[Error] # Graph Nodes reach the limit!\n");
		exit(1);
	}
	if (graphAddNode(g, sour) > 0) exit(1);
	if (graphAddNode(g, dest) > 0) exit(1);
	int i;
	// Edges already exist in the graph
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

struct Graph* createGraph() {
	struct Graph* g = (struct Graph*) malloc( sizeof(struct Graph) );
	g->vn = 0;
	g->en = 0;
	g->vn_len = 16;
	g->en_len = 64;
	g->root = NULL;
	g->nodes = (struct Node**) malloc( sizeof(struct Node*) * 16 );
	g->edges = (struct Edge*) malloc( sizeof(struct Edge) * 64 );
	return g;
}
struct Graph* copyGraph(struct Graph* a) {
	if (a == NULL) return NULL;
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

struct Graph* mergeGraph(struct Graph* a, struct Graph* b) {
	if (b == NULL) return copyGraph(a);
	if (a == NULL) return copyGraph(b);
	struct Graph* gh = copyGraph(a);
	int i;
	for (i=0; i< b->vn; i++) {
		graphAddNode(gh, b->nodes[i]);
	}
	for (i=0; i< b->en; i++) {
		graphAddEdgeHelper(gh, b->edges[i]);
	}
	return gh;
}
struct Node* graphGetRoot(struct Graph* g) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	return g->root;
}
int32_t graphSetRoot(struct Graph* g, struct Node * root) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	if (root == NULL) {
		printf("[Error] Root node doesn't exist!\n");
		exit(1);
	}
	int i;
	for (i=0; i<g->vn; i++) {
		if (g->nodes[i] == root) {
			g->root = root;
			return 0;
		}
	}
	printf("[Error] Root doesn't exist in the graph!\n");
	exit(1);
}

int32_t graphAddList(struct Graph* g, int direction, struct List * l, struct List * edges) {
	if (g == NULL || g->root == NULL || l == NULL) {
		printf("[Error] Graph or List doesn't exist!\n");
		exit(1);
	}
	int i, j, lsize = l->curPos, rsize = edges == NULL ? 0 : edges->curPos;
	if (lsize != rsize && rsize > 1) {
		printf("[Error] Edge List Not Compatible with Node/Graph List!\n");
		exit(1);
	}
	for (i=0; i<lsize; i++) {
		struct Node * node = NULL;
		if (l->type == GRAPH) {
			// Merge the graph
			struct Graph * gh_tmp = (struct Graph*)l->arr[i];
			node = gh_tmp->root;
			for (j=0; j< gh_tmp->vn; j++) {
				graphAddNode(g, gh_tmp->nodes[j]);
			}
			for (j=0; j< gh_tmp->en; j++) {
				graphAddEdgeHelper(g, gh_tmp->edges[j]);
			}
		} else if (l->type == NODE) {
			node = (struct Node*)l->arr[i];
		} else {
			printf("[Error] GraphAddList List Type doesn't supported!!\n");
			exit(1);
		}

		if (node == NULL) continue;
		if (edges != NULL && edges->curPos > 0) {
			int edgePos = edges->curPos == 1 ? 0 : i;
			switch (direction) {
				case RIGHT_LINK:
					graphAddEdgeP(g, g->root, node, edges->type, edges->arr[edgePos]); break;
				case LEFT_LINK:
					graphAddEdgeP(g, node, g->root, edges->type, edges->arr[edgePos]); break;
				case DOUBLE_LINK:
					graphAddEdgeP(g, g->root, node, edges->type, edges->arr[edgePos]);
					graphAddEdgeP(g, node, g->root, edges->type, edges->arr[edgePos]);
					break;
				default:
					break;
			}
		} else {
			switch (direction) {
				case RIGHT_LINK:
					graphAddEdgeP(g, g->root, node, -1, NULL); break;
				case LEFT_LINK:
					graphAddEdgeP(g, node, g->root, -1, NULL); break;
				case DOUBLE_LINK:
					graphAddEdgeP(g, g->root, node, -1, NULL);
					graphAddEdgeP(g, node, g->root, -1, NULL);
					break;
				default:
					break;
			}
		}
	}
	return 0;
}

int32_t graphAddNode(struct Graph* g, struct Node * node) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	if (g->vn + 1 >= g->vn_len) {
		printf("[Warning] # Graph Nodes reach the limit!\n");
		exit(1);
	}
	int i;
	// Nodes already exist in the graph
	for (i=0; i<g->vn; i++) {
		if (g->nodes[i] == node) return 0;
	}
	// Update the root if the graph is empty
	if (g->root == NULL) {
		g->root = node;
	}
	g->nodes[i] = node;
	g->vn++;
	return 0;
}

int32_t graphNumOfNodes(struct Graph* g) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	return g->vn;
}

int32_t graphNumOfEdges(struct Graph* g) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	return g->en;
}

struct List* graphGetChildNodes(struct Graph* g, struct Node* rt) {
	if (g == NULL) {
		printf("[Error] Graph doesn't exist!\n");
		exit(1);
	}
	struct List* children = createList(NODE);
	if (rt == NULL) return children;
	int i;
	for (i=0; i< g->en; i++) {
		if (g->edges[i].sour == rt) {
			addList(children, g->edges[i].dest);
		}
	}
	return children;
}

int32_t printGraph(struct Graph* g) {
	if (g == NULL) exit(1);
	printf("#Nodes: %d  ", g->vn);
	if (g->root != NULL) {
		printf("Root Node: %d\n", g->root->id);
	} else {
		printf("\n");
	}
	int i;
	for (i=0; i<g->vn; i++) {
		printNode(g->nodes[i]);
	}
	printf("--------------------------------------\n");
	printf("#Edges: %d\n", g->en);
	for (i=0; i<g->en; i++) {
		printEdge(&g->edges[i]);
	}
	return 0;
}



//test list
// int main() {
	// test list
	// struct List* list = createList(1);
	// printf("list type:%d\n", list->type);
	// struct List* newList = addList(addList(addList(addList(list, 52), 53), 54), 55);
	// printList(list);

	// struct Node* a = createNode(1, 0, 12, 0, 0, NULL);
	// struct Node* b = createNode(2, 1, 0, 1.2, 0, NULL);
	// struct Node* c = createNode(3, 2, 0, 0, 0, NULL);
	// struct Node* d = createNode(4, 3, 0, 0, 1, "Hello World!");
	// struct Graph* g = createGraph();
	// graphAddNode(g, a);
	// graphAddNode(g, b);
	// graphAddNode(g, c);
	// graphAddNode(g, d);
	// graphAddEdge(g, a, b, 3,0,0,0,"Edge1");
	// graphAddEdge(g, b, c, 2,0,0,1,NULL);

	// struct Graph* g2 = copyGraph(g);
	// g->edges[0].d = "ffff";
	// d->d = "????";
	// graphAddEdge(g2, c, d, 1,0,3.3,0,NULL);

	// printGraph(g);
	// printf("****************************\n");
	// printGraph(g2);

	// void * ptr = "xxx";
	// printf("%s\n", get_str_from_void_ptr(ptr));
	// exit(1);
// }



// below is the test for dict

// #include <stdlib.h>
// #include <stdio.h>
// #include <assert.h>

// #include "hashmap.h"

// #define KEY_MAX_LENGTH (256)
// #define KEY_PREFIX ("somekey")
// #define KEY_COUNT (1024*1024)

// typedef struct data_struct_s
// {
//     char key_string[KEY_MAX_LENGTH];
//     int number;
// } data_struct_t;


// int main()
// {
//     int index;
//     int error;
//     map_t mymap;
//     char key_string[KEY_MAX_LENGTH];
//     data_struct_t* value;

//     mymap = hashmap_new();

//     /* First, populate the hash map with ascending values */
//     /* Store the key string along side the numerical value so we can free it later */
//     value = malloc(sizeof(data_struct_t));
//     value->number = 1;
//     strcpy(value->key_string, "Warrior");
//     printf("%s\n", value->key_string);
//     hashmap_put(mymap, value->key_string, value);
//     data_struct_t* tmp = malloc(sizeof(data_struct_t));
//     int a = hashmap_get(mymap, value->key_string, (void**)(&tmp));
//     printf("%s:%d", tmp->key_string, tmp->number);
//     // error = hashmap_remove(mymap, key_string);
//     /* Now, destroy the map */
//     hashmap_free(mymap);

//     exit(1);
// }


// 	struct List* list = createList(1);
// 	printf("list type:%d\n", list->type);
// 	struct List* newList = addList(addList(addList(addList(list, 52), 53), 54), 55);
// 	printList(list);
// }

// test graph
// int main() {
// 	// struct Node* a = createNode(1, 0, 12, 0, 0, NULL);
// 	// struct Node* b = createNode(2, 1, 0, 1.2, 0, NULL);
// 	// struct Node* c = createNode(3, 2, 0, 0, 0, NULL);
// 	// struct Node* d = createNode(4, 3, 0, 0, 1, "Hello World!");
// 	// struct Node* a = createNode(1, 3, "a");
// 	// struct Node* b = createNode(2, 3, "b");
// 	// struct Node* c = createNode(3, 3, "c");
// 	// struct Node* d = createNode(4, 3, "d");
//
// 	// struct List* l = createList(NODE);
// 	// addList(l, b);
// 	// addList(l, c);
// 	// addList(l, d);
//
// 	// struct List* e = createList(STRING);
// 	// addList(e, "e1");
// 	// addList(e, "e2");
// 	// addList(e, "e3");
// 	//
// 	// struct Graph* g = createGraph();
// 	// graphAddNode(g, a);
// 	// graphAddList(g, RIGHT_LINK, l, e);
// 	// printGraph(g);
//
// 	// graphAddNode(g, b);
// 	// graphAddNode(g, c);
// 	// // graphAddNode(g, d);
// 	// graphAddEdge(g, a, b, 3,0,0,0,"Edge1");
// 	// graphAddEdge(g, b, c, 2,0,0,1,NULL);
// 	//
// 	// struct Graph* g2 = createGraph();
// 	// // graphAddEdge(g2, a, b, 0,11,0,0,NULL);
// 	// graphAddEdge(g2, c, d, 1,0,3.3,0,NULL);
// 	//
// 	// struct Graph* g3 = mergeGraph(g, g2);
// 	//
// 	// printGraph(g);
// 	// printf("****************************\n");
// 	// printGraph(g2);
// 	// printf("****************************\n");
// 	// printGraph(g3);
// }
