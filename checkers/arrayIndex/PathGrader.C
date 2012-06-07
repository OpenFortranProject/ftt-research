#include "PathGrader.h"
#include <boost/foreach.hpp>

#define foreach BOOST_FOREACH

PathGrader::PathGrader()
: paths()
{}

void PathGrader::analyzePath(PathT& path) {
  paths.push_back(path);
}

int PathGrader::getNumberOfPaths() const
{
  return paths.size();
}
std::vector<PathGrader::PathT> PathGrader::getAllPaths() const
{
  return paths;
}
