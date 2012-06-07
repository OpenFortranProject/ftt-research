#include "cfgTraversal.h"
#include <boost/foreach.hpp>

#define foreach BOOST_FOREACH

cfgTraversal::cfgTraversal()
: paths()
{}

void cfgTraversal::analyzePath(PathT& path) {
  paths.push_back(path);
}

int cfgTraversal::getNumberOfPaths() const
{
  return paths.size();
}
std::vector<cfgTraversal::PathT> cfgTraversal::getAllPaths() const
{
  return paths;
}
