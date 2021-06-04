#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "config.h"
#include "config.c"

int main() 
{
   const struct config_content* c = config_get_content ("example.conf");
   if (c == NULL) {
      printf ("FAIL: No configuration file 'example.conf'\n");
      return 1;
   }

   return (write (STDOUT_FILENO, c->content, c->size) == c->size) ? 0 : 1;
}
