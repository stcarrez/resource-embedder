// Advanced Resource Embedder 1.4.0
package Resources2

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
 { []byte("\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x03K\xcaO\xa9T\xa8\xe6R\x00\x82\xa4\xc4\xe4\xec\xf4\xa2\xfc\xd2\xbc\x14+\x05\xe5\xd4\xd4Tk\x05\x05\xaeZ\xae\x02\xa8lr~N~\x11P" +
"\xc2(\x11\x04\xc1r\x00\xaf\x93\xc12<\x00\x00\x00"),
    66,  1622183580, 0,
 }, 
 { []byte("\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x03+K,RH\xcdIMV\xb0U\xa8\xe6R\x00\x82TC#+\x85h\x05C=\x03\x1d a\x04\x22LA\x84\x85\x8e\x82\x11\x88k\xa4" +
"g\xae\xa3\x60\xacg\x0c\x22,u\x14L@\x5cS=3\x1d\x053\x90\x12\x0b=#\x85X\xaeZ.\x004\xb3\xfc\x1aW\x00\x00\x00"),
    90,  1622183646, 0,
 }, 
 { []byte("<!DOCTYPE html>\n<html xmlns=\x22http://www.w3.org/1999" +
"/xhtml\x22 lang=\x22en\x22>\n  <head>\n    <meta http-equiv=\x22C" +
"ontent-Type\x22 content=\x22text/html; charset=UTF-8\x22 />\n" +
"    <title>Test</title>\n    <link media=\x22screen\x22 ty" +
"pe=\x22text/css\x22 rel=\x22stylesheet\x22 href=\x22css/main.css\x22 " +
"/>\n  </head>\n  <body>\n    <p>Hello Test</p>\n    <sc" +
"ript src=\x22js/main.js\x22></script>\n  </body>\n</html>\n"),
    356,  1622183738, 0,
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

