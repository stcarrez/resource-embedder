#include "resources1.h"
#include <stdlib.h>
#include <stdio.h>

#include "resources2.c"

int main() 
{
   const struct resources2_content* c = resources2_get_content ("main.html");
   if (c == NULL) {
      printf ("FAIL: No content 'main.html'\n");
      return 1;
   }
   
   if (c->size != 356) {
      printf ("FAIL: Invalid length for 'main.html'\n");
      return 1;
   }

   c = resources2_get_content ("js/main.js");
   if (c == NULL) {
      printf ("FAIL: No content 'js/main.js'\n");
      return 1;
   }
   if (c->size != 87) {
      printf ("FAIL: Invalid length for 'js/main.js'\n");
      return 1;
   }

   c = resources2_get_content ("css/main.css");
   if (c == NULL) {
      printf ("FAIL: No content 'css/main.css'\n");
      return 1;
   }
   
   if (c->size != 60) {
      printf ("FAIL: Invalid length for 'css/main.css'\n");
      return 1;
   }

   if (c != &resources2_contents[Id_cssmain_css]) {
      printf ("FAIL: Invalid variable for Id_cssmain_css and 'css/main.css'\n");
      return 1;
   }

   printf ("PASS: ");
   for (int i = 0; i < c->size; i++) {
      if (resources2_contents[Id_cssmain_css].content[i] != '\n') {
         putchar(c->content[i]);
      }
   }
   printf("\n");
   return 0;
}
