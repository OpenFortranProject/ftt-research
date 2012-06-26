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
    typedef std::vector< Vertex > PathT;
  private:
    unsigned int score;
    unsigned int numPaths;
    const ControlFlowGraph& cfg;
    const SgVariableSymbol& var;
    SgNode& node;
    const std::string& index_name;
    int check_flag;
    int level;
    int array_dimension;
    const std::string& array_name;
    Compass::OutputObject& output;

    std::vector<SgName> intrinsics;

    unsigned int
    checkNode(const SgNode& var,
              SgNode& node,
              const SgFortranDo& dominator,
              int & checkedOnLine);
    unsigned int
    checkNode(const SgNode& var,
              SgNode& node,
              const SgWhileStmt& dominator,
              int & checkedOnLine);
    unsigned int
    checkNode(const SgNode& var,
              SgNode& node,
              const SgIfStmt& dominator,
              int & checkedOnLine);
    unsigned int
    checkNode(const SgNode& var,
              SgNode& node,
              const SgSwitchStatement& dominator,
              int & checkedOnLine);
    unsigned int
    checkNode(const SgNode& var,
              SgNode& node,
              const SgNode & dominator,
              int & checkedOnLine);

    int
    checkConditional(const SgExpression * const conditional);

    unsigned int
    checkIndex(const PathT& path, int & checkedOnLine);

    bool
    hasIndexVar(SgExpression * const expr, const SgNode * const var);
   
    bool
    isLiteralConstant(const SgExpression * const expr);

    bool
    isIntrinsic(const SgExpression * const expr);

  public:
    PathGrader(const ControlFlowGraph& cfg,
               const SgVariableSymbol& var,
               SgNode& node,
               const std::string& index_name,
               int check_flag, int level, int array_dimension,
               const std::string& array_name,
               Compass::OutputObject& output);

    virtual void analyzePath(PathT& path);

    int getNumberOfPaths() const;
    int getScore() const;
};

template <class T>
inline std::string to_string (const T& t) {
  std::stringstream ss;
  ss << t;
  return ss.str();
}

#endif /* PATHGRADER_H */
