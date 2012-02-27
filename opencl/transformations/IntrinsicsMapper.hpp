#include <rose.h>
#include "NodeMapper.hpp"

using namespace SageBuilder;
using namespace SageInterface;

class IntrinsicsMapper : NodeMapper<SgFunctionCallExp, SgExpression>
{
   public:
   IntrinsicsMapper();

   To_t map(From_t, SgScopeStatement * const scope) const;

   ~IntrinsicsMapper() {}

   private:
   To_t mapMergeIntrinsic(From_t from) const;


   // Some intrinsics map directly to function calls in
   // opencl, for those we have a map of substitutions
   std::map<std::string, std::string> substitutions;
};
