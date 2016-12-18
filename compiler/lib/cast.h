#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

int32_t VoidtoInt(void* add);
double VoidtoFloat(void* add);
bool VoidtoBool(void* add);
char* VoidtoString(void* add);
struct Node* VoidtoNode(void* add);
struct Graph* VoidtoGraph(void* add);

void* InttoVoid(int32_t val);
void* FloattoVoid(double val);
void* BooltoVoid(bool val);
void* StringtoVoid(char* val);
void* NodetoVoid(struct Node* val);
void* GraphtoVoid(struct Graph* val);

bool isInt(int32_t d);
bool isFloat(int32_t d);
bool isBool(int32_t d);
bool isString(int32_t d);
bool isNode(int32_t d);
bool isGraph(int32_t d);
