/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/graph_hess.h
 Revision: $Id: graph_hess.h 134 2009-03-03 14:25:24Z imosqueira $
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
 
 History: 20050629 andrea: newly integrated
                           Code Added by Arijit Tarafdar (tarafdar@cs.odu.edu) 
                           and Assefaw Gebremedhin (assefaw@cs.odu.edu), 
                           Old Dominion University, Norfolk, VA
 
----------------------------------------------------------------------------*/
#if !defined (ADOLC_SPARSE_GRAPH_HESS_H)
#define ADOLC_SPARSE_GRAPH_HESS_H 1

#include "../common.h"
#include <vector>
using namespace std;

#define STEP_DOWN(INPUT) ((INPUT) - 1)
#define STEP_UP(INPUT) ((INPUT) + 1)

#define _INVALID -2
#define _UNKNOWN -1
#define _FALSE 0
#define _TRUE 1

#define _OFF 0
#define _ON 1

#define VERBOSE _FALSE
#define DEBUG _FALSE

/***Global Function 1***/
ADOLC_DLL_EXPORT int ReadSparsityPattern
(vector<int> &, vector<int> &, unsigned int **, int);

/***Global Function 2***/
ADOLC_DLL_EXPORT int FindStarColoring
(vector<int> &, vector<int> &, vector<int> & , vector<int> &);

/***Global Function 3***/
ADOLC_DLL_EXPORT int CheckStarColoring
(vector<int> &, vector<int> &, vector<int> &);

/***Global Function 4***/
ADOLC_DLL_EXPORT int PrintVertexColors
(vector<int> &);

/***Global Function 5***/
ADOLC_DLL_EXPORT int GenerateSeed
(int, double **, vector<int> &, vector<int> &, vector<int> &, int);

#endif /* ADOLC_SPARSE_GRAPH_HESS_H */
