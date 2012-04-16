#include <rose.h>

using namespace SageBuilder;
using namespace SageInterface;

template<class From, class To>
class NodeMapper {
   public:
   typedef const From * const From_t;
   typedef To   * To_t;

   virtual ~NodeMapper() { }

   virtual To_t map(From_t from, SgScopeStatement * const) const = 0;

};
