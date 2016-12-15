#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

int VoidtoInt(void* add);
double VoidtoFloat(void* add);
bool VoidtoBool(void* add);
char* VoidtoString(void* add);
struct Node* VoidtoNode(void* add);
struct Graph* VoidtoGraph(void* add);

void* InttoVoid(int val);
void* FloattoVoid(double val);
void* BooltoVoid(bool val);
void* StringtoVoid(char* val);
void* NodetoVoid(struct Node* val);
void* GraphtoVoid(struct Graph* val);