/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/graph_jac.h
 Revision: $Id: graph_jac.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: This file containts utilities for graph coloring and seed matrix 
           algorithms.
 
 
 Copyright (c) 2005
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History: 20050418 andrea: newly generated
 
----------------------------------------------------------------------------*/
#if !defined (ADOLC_SPARSE_GRAPH_JAC_H)
#define ADOLC_SPARSE_GRAPH_JAC_H 1

#include "../common.h"

BEGIN_C_DECLS
/****************************************************************************/
/* structures needed for graph construction                                 */

typedef struct EdgeElement {
    int dest;			         /*index of vertix*/
    struct EdgeElement *next;	/* next edge element*/
}
EdgeElement;

typedef struct VertixElement {
    int color;
    struct EdgeElement* edge;
}
VertixElement;

/* Graph definition as a matrix of vertices*/
typedef struct VertixElement** Graph;

/*Used Colors element*/
typedef struct UsedColorElement {
    int color;
    struct UsedColorElement* next;
}
UsedColorElement;

/****************************************************************************/
/* graph functions needed by jacutils.c, sparsedrivers.c                    */

ADOLC_DLL_EXPORT void Delete_Graph(Graph G, int size);

ADOLC_DLL_EXPORT int GreedyPartialD2Coloring (Graph G, int size, int rows);

ADOLC_DLL_EXPORT Graph Bipartite_Graph
(int m,int n, unsigned int** JacobianVector);

ADOLC_DLL_EXPORT void Generate_Seed_from_Graph
(int indep, double** Seed, Graph G, int size, int colors);

/****************************************************************************/
END_C_DECLS

#endif /* ADOLC_SPARSE_GRAPH_JAC_H */

