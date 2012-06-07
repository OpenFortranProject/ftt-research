#include "cfgTraversal.h"

cfgTraversal::cfgTraversal()
: pths(0), tltnodes(0)
{}

//The path analysis function, relatively self explanatory                                                                                                                
void cfgTraversal::analyzePath(std::vector<Vertex>& pth) {
    tltnodes += pth.size();
    pths++;
}



