#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "help.h"
#include "man.h"

void print(const char* name) 
{
   const struct man_content* c = man_get_content (name);
   if (c == NULL) 
     {
       c = man_get_help_content (name);
       if (c == NULL)
         {
           printf ("FAIL: No help for '%s'\n", name);
           exit (1);
         }
     }

   if (write (STDOUT_FILENO, c->content, c->size) != c->size)
     {
       exit (1);
     }
}


int main(int argc, char** argv)
{
   if (argc <= 1) 
     {
       printf("Help names:\n");
       for (int i = 0; i < help_names_length; i++)
         {
           printf("  %s\n", help_names[i]);
         }
       printf("Man pages:\n");
       for (int i = 0; i < man_names_length; i++)
         {
           printf("  %s\n", man_names[i]);
         }
        return 0;
     }
   for (int i = 1; i < argc; i++)
     {
        print (argv[i]);
     }
   return 0;
}
