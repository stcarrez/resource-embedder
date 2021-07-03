#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "scripts.h"
#include "scripts.c"

int main() 
{
  printf("Create SQLite\n");
  for (int i = 0; i < scripts_contents[Id_create_database].length; i++) {
    puts(scripts_contents[Id_create_database].content[i]);
  }
  printf("Drop SQLite\n");
  for (int i = 0; i < scripts_contents[Id_drop_database].length; i++) {
    puts(scripts_contents[Id_drop_database].content[i]);
  }

  return 0;
}
