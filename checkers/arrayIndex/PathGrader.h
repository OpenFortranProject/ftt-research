#include <vector>
#include <staticSingleAssignment.h>
#include <backstroke/backstrokeCFG.h>
#include "graphProcessing.h"

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
    std::vector<PathT> getAllPaths() const;

};

