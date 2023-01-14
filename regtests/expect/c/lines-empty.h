// Advanced Resource Embedder 1.3.0
#ifndef _LINES_H_
#define _LINES_H_

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _STRUCT_LINES_CONTENT_TYPE_
#define _STRUCT_LINES_CONTENT_TYPE_

struct lines_content {
  const char* const* content;
  size_t length;
};

#endif

enum {
   Id_empty_txt
};

extern const struct lines_content lines_contents[];

#ifdef __cplusplus
}
#endif

#endif /* _LINES_H_ */
