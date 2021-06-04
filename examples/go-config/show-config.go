package main

import (
   "fmt"
   "go-config/config"
)


func main() {
   c := config.Get_content("example.conf")
   if (c == nil) {
      fmt.Println("FAIL: No configuration file 'example.conf'")
      return
   }

   fmt.Println(string(c.Content))
}
