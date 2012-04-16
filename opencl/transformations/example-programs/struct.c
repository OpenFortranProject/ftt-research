typedef struct foo {
  int x;
  int y;
} foo_t;

int bar(foo_t f) {
  return f.x + f.y;
}
