// Advanced Resource Embedder
#ifndef _MAN_H_
#define _MAN_H_

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _MAN_CONTENT_TYPE_
#define _MAN_CONTENT_TYPE_

struct man_content {
  const unsigned char *content;
  size_t size;
  time_t modtime;
  int format;
};

#endif

// Sorted array of names composing the resource.
extern const char* const man_names[];
static const int man_names_length = 3;

// Returns the data stream with the given name or null.
extern const struct man_content* man_get_content(const char* name);

#ifdef __cplusplus
}
#endif

#endif /* _MAN_H_ */
