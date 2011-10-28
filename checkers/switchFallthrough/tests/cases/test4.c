#include <stdio.h>

int main() {
  int x = 2;

  switch(x) {
  case 1: break;
  case 3: x = 3;
  case 2: default: break;
  }

}
