#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "web.h"

void print(const char* name) 
{
   const struct web_content* c = web_get_content (name);
   if (c == NULL) 
     {
       printf ("FAIL: No help for '%s'\n", name);
       exit (1);
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
       printf("Web names:\n");
       for (int i = 0; i < web_names_length; i++)
         {
           printf("  %s\n", web_names[i]);
         }
        return 0;
     }
   for (int i = 1; i < argc; i++)
     {
        print (argv[i]);
     }
   return 0;
}
