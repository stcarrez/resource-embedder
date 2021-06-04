package main

import (
   "os"
   "fmt"
   "go-help/help"
   "go-help/man"
)

func print(name string) {
   c1 := man.Get_content(name)
   if (c1 != nil) {
      fmt.Println(string(c1.Content))
      return;
   }

   c2 := help.Get_content(name);
   if (c2 == nil) {
       fmt.Println("FAIL: No configuration file ", name)
       return
   }
   fmt.Println(string(c2.Content))
}

func main() {
   if (len(os.Args) <= 1) {
      fmt.Println("Help names:")
      for _, name := range help.Names {
          fmt.Println("  ", name);
      }

      fmt.Println("Man pages:")
      for _, name := range man.Names {
          fmt.Println("  ", name);
      }
      return
   }

   for _, arg := range os.Args[1:] {
       print(arg)
   }
}
