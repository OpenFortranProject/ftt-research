#include "Util.hpp"
#include <cstdarg>
#include <cstdio>

#ifdef DEBUG
int debug(const char * fmt, ...)
{
   int retval = 0;
   va_list args;

   va_start(args, fmt);
   retval = vprintf(fmt, args);
   va_end(args);

   return retval;
}
#else
int debug(const char *, ...) { return 0; }
#endif

#ifndef DEBUG
std::ofstream dout("/dev/null");
#endif
