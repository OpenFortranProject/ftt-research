#ifndef UTIL_HPP
#define UTIL_HPP
#include <rose.h>
#include <fstream>

#define DEBUG

// debug only prints something if DEBUG is defined
int debug(const char * fmt, ...);

// this only iterate over the pointers if DEBUG is defined
// printPointers exsits solely for debug purposes
#ifdef DEBUG
template <typename T>
void printPointers(const std::vector<const T *> pointers)
{
  debug("[%s]: ", __func__);
  for(typename std::vector<const T *>::const_iterator i = pointers.begin(); i != pointers.end(); i++)
  {
    debug("%p ", (void*)*i);
  }
  debug("\n");
}

template <typename T>
void printPointers(const std::map<const T *, const T *> pointers)
{
  debug("[%s]: ", __func__);
  for(typename std::map<const T *, const T *>::const_iterator i = pointers.begin(); i != pointers.end(); i++)
  {
    debug("%p ", (void*)(i->first));
  }
  debug("\n");
}
#else /* !DEBUG */
template <typename T>
void printPointers(const std::vector<const T *>) {}
template <typename T>
void printPointers(const std::map<const T *, const T *>) {}
#endif /* DEBUG */

#ifdef DEBUG
#define dout std::cout
#else
extern std::ofstream dout;
#endif

#endif /* UTIL_HPP */
