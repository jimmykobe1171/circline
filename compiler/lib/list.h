

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

int32_t pirntList(struct List * list);

char* get_str_from_void_ptr(void * ptr);