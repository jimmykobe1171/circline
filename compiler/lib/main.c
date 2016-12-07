/*
 * A unit test and example of how to use the simple C hashmap
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "hashmap.h"
#include "hashmap.c"

#define KEY_MAX_LENGTH (256)
#define KEY_PREFIX ("somekey")
#define KEY_COUNT (1024*1024)

typedef struct data_struct_s
{
    char key_string[KEY_MAX_LENGTH];
    int number;
} data_struct_t;

int main()
{
    int index;
    int error;
    map_t mymap;
    char key_string[KEY_MAX_LENGTH];
    data_struct_t* value;
    
    mymap = hashmap_new();

    /* First, populate the hash map with ascending values */
    /* Store the key string along side the numerical value so we can free it later */
    // value = malloc(sizeof(data_struct_t));

    // value->number = 1;
    // strcpy(value->key_string, "Warrior");
    // printf("%s\n", value->key_string);
    void * str = "xxx";
    hashmap_put(mymap, "a", str);
    // data_struct_t* tmp = malloc(sizeof(data_struct_t));
    char* a = hashmap_get(mymap, "a");
    printf("%s", a);
    // error = hashmap_remove(mymap, key_string);
    /* Now, destroy the map */
    hashmap_free(mymap);

    return 1;
}