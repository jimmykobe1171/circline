#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "cast.h"
#include "utils.h"

int32_t VoidtoInt(void* add){
	return *((int32_t*) add);
}

double VoidtoFloat(void* add){
	return *((double*) add);
}

bool VoidtoBool(void* add){
	return *((bool*) add);
}

char* VoidtoString(void* add){
	return (char*) add;
}

struct Node* VoidtoNode(void* add){
	return (struct Node*) add;
}

struct Graph* VoidtoGraph(void* add){
	return (struct Graph*) add;
}

void* InttoVoid(int32_t val){
	int* tmp = (int*)malloc(sizeof(int));
	*tmp = val;
	return (void*) tmp;
}

void* FloattoVoid(double val){
	double* tmp = (double*)malloc(sizeof(double));
	*tmp = val;
	return (void*) tmp;
}

void* BooltoVoid(bool val){
	bool* tmp = (bool*)malloc(sizeof(bool));
	*tmp = val;
	return (void*) tmp;
}

void* StringtoVoid(char* val){
	return (void*) val;
}

void* NodetoVoid(struct Node* val){
	return (void*) val;
}

void* GraphtoVoid(struct Graph* val){
	return (void*) val;
}

bool isInt(int32_t d){
	return (d==INT);
};
// bool isFloat(int32_t d){return (d==FLOAT);};
bool isFloat(int32_t d){return null;};
bool isBool(int32_t d){return (d==BOOL);};
bool isString(int32_t d){return (d==STRING);};
bool isNode(int32_t d){return (d==NODE);};
bool isGraph(int32_t d){return (d==GRAPH);};
