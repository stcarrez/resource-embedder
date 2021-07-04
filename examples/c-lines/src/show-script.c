#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "scripts.h"
#include "scripts.c"

int main() 
{
  printf("Create SQLite %ld lines\n", scripts_contents[create_database].length);
  for (int i = 0; i < scripts_contents[create_database].length; i++) {
    puts(scripts_contents[create_database].content[i]);
  }
  printf("Drop SQLite %ld lines\n", scripts_contents[drop_database].length);
  for (int i = 0; i < scripts_contents[drop_database].length; i++) {
    puts(scripts_contents[drop_database].content[i]);
  }

  return 0;
}
