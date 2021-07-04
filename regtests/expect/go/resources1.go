// Advanced Resource Embedder 1.1.0
package Resources1

import (
  "strings"
  "sort"
)

type Content struct {
    Content  []byte
    Size  int64
    Modtime  int64
    Format   int
}


var names= []string {
  "css/main.css",
  "js/main.js",
  "main.html",

}


var contents = []Content {
 { []byte("body {\n    background: #eee;  \n}\np {\n    color: #2a" +
"2a2a;  \n}"),
    60,  1622894890, 0,
 }, 
 { []byte("var elec = {\n    e12: [ 1.0, 1.2, 1.5, 1.8, 2.2, 2." +
"7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2 ]\n}\n"),
    87,  1622894887, 0,
 }, 
 { []byte("<!DOCTYPE html>\n<html xmlns=\x22http://www.w3.org/1999" +
"/xhtml\x22 lang=\x22en\x22>\n  <head>\n    <meta http-equiv=\x22C" +
"ontent-Type\x22 content=\x22text/html; charset=UTF-8\x22 />\n" +
"    <title>Test</title>\n    <link media=\x22screen\x22 ty" +
"pe=\x22text/css\x22 rel=\x22stylesheet\x22 href=\x22css/main.css\x22 " +
"/>\n  </head>\n  <body>\n    <p>Hello Test</p>\n    <sc" +
"ript src=\x22js/main.js\x22></script>\n  </body>\n</html>\n"),
    356,  1622894877, 0,
 }, 
}

// Returns the data stream with the given name or null.
func Get_content(name string) (*Content) {
    i := sort.Search(3, func(i int) bool {
        return strings.Compare(names[i], name) >= 0
    })
    if i < 3 && strings.Compare(names[i], name) == 0 {
        return &contents[i]
    }
    return nil
}

