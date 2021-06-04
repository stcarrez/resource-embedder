// Advanced Resource Embedder
#ifndef _CONFIG_H_
#define _CONFIG_H_

#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _CONFIG_CONTENT_TYPE_
#define _CONFIG_CONTENT_TYPE_

struct config_content {
  const unsigned char *content;
  size_t size;
  time_t modtime;
  int format;
};

#endif

// Returns the data stream with the given name or null.
extern const struct config_content* config_get_content(const char* name);

#ifdef __cplusplus
}
#endif

#endif /* _CONFIG_H_ */
