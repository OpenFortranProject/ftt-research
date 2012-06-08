#ifndef PATHGRADER_H
#define PATHGRADER_H
#include <vector>
#include <staticSingleAssignment.h>
#include <backstroke/backstrokeCFG.h>
#include "graphProcessing.h"
#include "compass.h"

//renaming to Vertex and Edge to save space
typedef ssa_private::DataflowCfgFilter CfgNodeT;
typedef Backstroke::CFG<CfgNodeT> ControlFlowGraph;

typedef ControlFlowGraph::CFGNodeType Vertex;
typedef ControlFlowGraph::CFGEdgeType Edge;

class PathGrader : public SgGraphTraversal<ControlFlowGraph>
{
  public:
    typedef std::vector<Vertex> PathT;
  private:
    std::vector<PathT> paths;
  public:
    PathGrader();

    virtual void analyzePath(PathT& path);

    int getNumberOfPaths() const;
    int getScore() const;

    std::vector<PathT> getAllPaths() const;
    int checkNode(const SgVariableSymbol* const var,
                  SgNode* const node,
                  const SgNode * const dominator,
                  Compass::OutputObject & output);
    void checkIndex(const SgVariableSymbol* const var,
                    SgNode* const node, const std::vector<std::vector<SgNode *> >& slice,
                    const std::string index_name, int check_flag, int level,
                    int array_dimension, const std::string array_name, Compass::OutputObject& output);
    
};

template <class T>
inline std::string to_string (const T& t) {
  std::stringstream ss;
  ss << t;
  return ss.str();
}

#endif /* PATHGRADER_H */
