#include <vector>
#include <staticSingleAssignment.h>
#include <backstroke/backstrokeCFG.h>
#include "graphProcessing.h"

//renaming to Vertex and Edge to save space
typedef ssa_private::DataflowCfgFilter CfgNodeT;
typedef Backstroke::CFG<CfgNodeT> ControlFlowGraph;
//typedef boost::graph_traits<ControlFlowGraph::Graph>::vertex_descriptor Vertex;   /**< Graph vertex type. */
//typedef boost::graph_traits<ControlFlowGraph::Graph>::edge_descriptor   Edge;     /**< Graph edge type. */
typedef ControlFlowGraph::CFGNodeType Vertex;
typedef ControlFlowGraph::CFGEdgeType Edge;

//visitor class, analyze path runs on the paths as they are completed, it is declared in the graphProcessing.h as a virtual function
//that will be implemented by the user
class cfgTraversal : public SgGraphTraversal<ControlFlowGraph>
{
  public:
    typedef std::vector<Vertex> PathT;
  private:
    std::vector<PathT> paths;
  public:
    cfgTraversal();

    virtual void analyzePath(PathT& path);

    int getNumberOfPaths() const;
    std::vector<PathT> getAllPaths() const;

};

