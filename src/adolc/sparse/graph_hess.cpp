/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/graph_hess.cpp
 Revision: $Id: graph_hess.cpp 134 2009-03-03 14:25:24Z imosqueira $
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

#include <iostream>

#include <map>
#include <string>
#include <vector>

#include "sparse/graph_hess.h"
using namespace std;

/***Global Function 1***/

int ReadSparsityPattern(vector<int> & vi_Vertices, vector<int> & vi_Edges, unsigned int ** uip2_SparsityPattern, int i_RowCount) {
    int i, j;

    int i_ElementCount, i_PositionCount;

#if DEBUG == 1

    cout<<endl;
    cout<<"DEBUG | Graph Coloring | Sparsity Pattern"<<endl;
    cout<<endl;

    for(i=0; i<i_RowCount; i++) {
        cout<<i<<"\t"<<" : ";

        i_PositionCount = uip2_SparsityPattern[i][0];

        for(j=0; j<i_PositionCount; j++) {
            if(j == STEP_DOWN(i_PositionCount)) {
                cout<<uip2_SparsityPattern[i][STEP_UP(j)]<<" ("<<i_PositionCount<<")";
            } else {
                cout<<uip2_SparsityPattern[i][STEP_UP(j)]<<", ";
            }

        }

        cout<<endl;
    }

    cout<<endl;

#endif

    vi_Vertices.clear();
    vi_Vertices.push_back(_FALSE);

    vi_Edges.clear();

    for(i=0; i<i_RowCount; i++) {
        i_ElementCount = _FALSE;

        i_PositionCount = uip2_SparsityPattern[i][0];

        for(j=0; j<i_PositionCount; j++) {
            if((signed) uip2_SparsityPattern[i][STEP_UP(j)] != i) {
                vi_Edges.push_back((signed) uip2_SparsityPattern[i][STEP_UP(j)]);

                i_ElementCount++;
            }

        }

        vi_Vertices.push_back(vi_Vertices.back() + i_ElementCount);
    }

#if DEBUG == 1

    int i_VertexCount, i_EdgeCount;

    cout<<endl;
    cout<<"DEBUG | Graph Coloring | Graph Format"<<endl;
    cout<<endl;

    cout<<"Vertices"<<"\t"<<" : ";

    i_VertexCount = (signed) vi_Vertices.size();

    for(i=0; i<i_VertexCount; i++) {
        if(i == STEP_DOWN(i_VertexCount)) {
            cout<<vi_Vertices[i]<<" ("<<i_VertexCount<<")";
        } else {
            cout<<vi_Vertices[i]<<", ";
        }
    }

    cout<<endl;

    cout<<"Edges"<<"\t"<<" : ";

    i_EdgeCount = (signed) vi_Edges.size();

    for(i=0; i<i_EdgeCount; i++) {
        if(i == STEP_DOWN(i_EdgeCount)) {
            cout<<vi_Edges[i]<<" ("<<i_EdgeCount<<")";
        } else {
            cout<<vi_Edges[i]<<", ";
        }
    }

    cout<<endl;

#endif


    return(_FALSE);
}

/***Global Function 2***/

int FindStarColoring(vector<int> & vi_Vertices, vector<int> & vi_Edges, vector<int> & vi_VertexOrder, vector<int> & vi_VertexColors) {
    int i, j, k;

    int _FOUND;

    int i_ColorID, i_StarID;

    int i_HighestColor;

    int i_PresentVertex;

    int i_VertexCount, i_EdgeCount;

    int i_VertexOne, i_VertexTwo;

    vector<int> vi_MemberEdges;

    vector<int> vi_CandidateColors;

    vector<int> vi_EdgeStarMap;
    vector<int> vi_StarHubMap;

    vector<int> vi_FirstTreated;

    vector<int> vi_FirstSeenOne, vi_FirstSeenTwo;

    map< int, map<int, int> > mimi2_VertexEdgeMap;

    i_VertexCount = STEP_DOWN((signed) vi_Vertices.size());

    i_EdgeCount = (signed) vi_Edges.size();

    vi_EdgeStarMap.clear();
    vi_EdgeStarMap.resize((unsigned) i_EdgeCount/2, _UNKNOWN);

    vi_StarHubMap.clear();
    vi_StarHubMap.resize((unsigned) i_EdgeCount/2, _UNKNOWN);

    vi_CandidateColors.clear();
    vi_CandidateColors.resize((unsigned) i_VertexCount, _UNKNOWN);

    vi_FirstSeenOne.clear();
    vi_FirstSeenOne.resize((unsigned) i_VertexCount, _UNKNOWN);

    vi_FirstSeenTwo.clear();
    vi_FirstSeenTwo.resize((unsigned) i_VertexCount, _UNKNOWN);

    vi_FirstTreated.clear();
    vi_FirstTreated.resize((unsigned) i_EdgeCount, _UNKNOWN);

    k = _FALSE;

    for(i=0; i<i_VertexCount; i++) {
        for(j=vi_Vertices[i]; j<vi_Vertices[STEP_UP(i)]; j++) {
            if(i < vi_Edges[j]) {
                mimi2_VertexEdgeMap[i][vi_Edges[j]] = k;

                vi_EdgeStarMap[k] = k;

                k++;
            }

        }
    }

#if VERBOSE == _TRUE

    cout<<endl;

#endif

    i_HighestColor = _UNKNOWN;

    for(i=0; i<i_VertexCount; i++) {
        i_PresentVertex = vi_VertexOrder[i];

#if VERBOSE == _TRUE

        cout<<"DEBUG | Star Coloring | Coloring Vertex "<<STEP_UP(i_PresentVertex)<<"/"<<i_VertexCount<<endl;

#endif
        for(j=vi_Vertices[i_PresentVertex]; j<vi_Vertices[STEP_UP(i_PresentVertex)]; j++) {
            i_ColorID = vi_VertexColors[vi_Edges[j]];

            if(i_ColorID == _UNKNOWN) {
                continue;
            }

            vi_CandidateColors[i_ColorID] = i_PresentVertex;

            i_VertexOne = vi_FirstSeenOne[i_ColorID];
            i_VertexTwo = vi_FirstSeenTwo[i_ColorID];

            if(i_VertexOne == i_PresentVertex) {
                if(vi_FirstTreated[i_VertexTwo] != i_PresentVertex) {

                    for(k=vi_Vertices[i_VertexTwo]; k<vi_Vertices[STEP_UP(i_VertexTwo)]; k++) {
                        if(vi_Edges[k] == i_PresentVertex) {
                            continue;
                        }

                        if(vi_VertexColors[vi_Edges[k]] == _UNKNOWN) {
                            continue;
                        }

                        vi_CandidateColors[vi_VertexColors[vi_Edges[k]]] = i_PresentVertex;

                    }

                    vi_FirstTreated[i_VertexTwo] = i_PresentVertex;

                }

                for(k=vi_Vertices[vi_Edges[j]]; k<vi_Vertices[STEP_UP(vi_Edges[j])]; k++) {
                    if(vi_Edges[k] == i_PresentVertex) {
                        continue;
                    }

                    if(vi_VertexColors[vi_Edges[k]] == _UNKNOWN) {
                        continue;
                    }

                    vi_CandidateColors[vi_VertexColors[vi_Edges[k]]] = i_PresentVertex;

                }

                vi_FirstTreated[vi_Edges[j]] = i_PresentVertex;
            } else {
                vi_FirstSeenOne[i_ColorID] = i_PresentVertex;
                vi_FirstSeenTwo[i_ColorID] = vi_Edges[j];

                for(k=vi_Vertices[vi_Edges[j]]; k<vi_Vertices[STEP_UP(vi_Edges[j])]; k++) {
                    if(vi_Edges[k] == i_PresentVertex) {
                        continue;
                    }

                    if(vi_VertexColors[vi_Edges[k]] == _UNKNOWN) {
                        continue;
                    }

                    if(vi_Edges[j] < vi_Edges[k]) {
                        if(vi_StarHubMap[vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][vi_Edges[k]]]] == vi_Edges[k]) {
                            vi_CandidateColors[vi_VertexColors[vi_Edges[k]]] = i_PresentVertex;
                        }
                    } else {
                        if(vi_StarHubMap[vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[k]][vi_Edges[j]]]] == vi_Edges[k]) {
                            vi_CandidateColors[vi_VertexColors[vi_Edges[k]]] = i_PresentVertex;
                        }
                    }
                }
            }
        }

        for(j=0; j<i_VertexCount; j++) {
            if(vi_CandidateColors[j] != i_PresentVertex) {
                vi_VertexColors[i_PresentVertex] = j;

                if(i_HighestColor < j) {
                    i_HighestColor = j;
                }

                break;
            }
        }

        for(j=vi_Vertices[i_PresentVertex]; j<vi_Vertices[STEP_UP(i_PresentVertex)]; j++) {

            _FOUND = _FALSE;

            if(vi_VertexColors[vi_Edges[j]] == _UNKNOWN) {
                continue;
            }

            for(k=vi_Vertices[vi_Edges[j]]; k<vi_Vertices[STEP_UP(vi_Edges[j])]; k++) {
                if(vi_Edges[k] == i_PresentVertex) {
                    continue;
                }


                if(vi_VertexColors[vi_Edges[k]] == _UNKNOWN) {
                    continue;
                }

                if(vi_VertexColors[vi_Edges[k]] == vi_VertexColors[i_PresentVertex]) {
                    _FOUND = _TRUE;

                    if(vi_Edges[j] < vi_Edges[k]) {
                        i_StarID = vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][vi_Edges[k]]];

                        vi_StarHubMap[i_StarID] = vi_Edges[j];

                        if(i_PresentVertex < vi_Edges[j]) {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[i_PresentVertex][vi_Edges[j]]] = i_StarID;
                        } else {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][i_PresentVertex]] = i_StarID;
                        }
                    } else {
                        i_StarID = vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[k]][vi_Edges[j]]];

                        vi_StarHubMap[i_StarID] = vi_Edges[j];

                        if(i_PresentVertex < vi_Edges[j]) {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[i_PresentVertex][vi_Edges[j]]] = i_StarID;
                        } else {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][i_PresentVertex]] = i_StarID;
                        }
                    }

                    break;

                }
            }

            if (!_FOUND) {
                i_VertexOne = vi_FirstSeenOne[vi_VertexColors[vi_Edges[j]]];
                i_VertexTwo = vi_FirstSeenTwo[vi_VertexColors[vi_Edges[j]]];

                if((i_VertexOne == i_PresentVertex) && (i_VertexTwo != vi_Edges[j])) {
                    if(i_PresentVertex < i_VertexTwo) {
                        i_StarID = vi_EdgeStarMap[mimi2_VertexEdgeMap[i_PresentVertex][i_VertexTwo]];

                        vi_StarHubMap[i_StarID] = i_PresentVertex;

                        if(i_PresentVertex < vi_Edges[j]) {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[i_PresentVertex][vi_Edges[j]]] = i_StarID;
                        } else {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][i_PresentVertex]] = i_StarID;
                        }
                    } else {
                        i_StarID = vi_EdgeStarMap[mimi2_VertexEdgeMap[i_VertexTwo][i_PresentVertex]];

                        vi_StarHubMap[i_StarID] = i_PresentVertex;

                        if(i_PresentVertex < vi_Edges[j]) {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[i_PresentVertex][vi_Edges[j]]] = i_StarID;
                        } else {
                            vi_EdgeStarMap[mimi2_VertexEdgeMap[vi_Edges[j]][i_PresentVertex]] = i_StarID;
                        }
                    }
                }
            }
        }
    }

#if VERBOSE == _TRUE

    cout<<endl;

#endif

    return(STEP_UP(i_HighestColor));
}

/***Global Function 3***/

int CheckStarColoring(vector<int> & vi_VertexColors, vector<int> & vi_Vertices, vector<int> & vi_Edges) {
    int i, j, k, l;

    int i_VertexCount, i_EdgeCount;

    int i_FirstColor, i_SecondColor, i_ThirdColor, i_FourthColor;

    int i_ViolationCount;

    i_VertexCount = STEP_DOWN((signed) vi_Vertices.size());

    i_EdgeCount = (signed) vi_Edges.size();

    i_ViolationCount = _FALSE;

#if VERBOSE == _TRUE

    cout<<endl;
    cout<<"Star Coloring | Violation Check"<<endl;
    cout<<endl;

#endif

    for(i=0; i<i_VertexCount; i++) {
        i_FirstColor = vi_VertexColors[i];

        for(j=vi_Vertices[i]; j<vi_Vertices[STEP_UP(i)]; j++) {
            i_SecondColor = vi_VertexColors[vi_Edges[j]];

            if(i_SecondColor == i_FirstColor) {
                i_ViolationCount++;

                cout<<"Violation "<<i_ViolationCount<<"\t : "<<STEP_UP(i)<<" ["<<STEP_UP(i_FirstColor)<<"] - "<<STEP_UP(vi_Edges[j])<<" ["<<STEP_UP(i_SecondColor)<<"]"<<endl;

                continue;
            }

            for(k=vi_Vertices[vi_Edges[j]]; k<vi_Vertices[STEP_UP(vi_Edges[j])]; k++) {
                if(vi_Edges[k] == i) {
                    continue;
                }

                i_ThirdColor = vi_VertexColors[vi_Edges[k]];

                if(i_ThirdColor == i_SecondColor) {
                    i_ViolationCount++;

                    cout<<"Violation "<<i_ViolationCount<<"\t : "<<STEP_UP(vi_Edges[j])<<" ["<<STEP_UP(i_SecondColor)<<"] - "<<STEP_UP(vi_Edges[k])<<" ["<<STEP_UP(i_ThirdColor)<<"]"<<endl;

                    continue;
                }

                if(i_ThirdColor != i_FirstColor) {
                    continue;

                }

                if(i_ThirdColor == i_FirstColor) {
                    for(l=vi_Vertices[vi_Edges[k]]; l<vi_Vertices[STEP_UP(vi_Edges[k])]; l++) {
                        if((vi_Edges[l] == vi_Edges[j])) {
                            continue;
                        }

                        i_FourthColor = vi_VertexColors[vi_Edges[l]];

                        if(i_FourthColor == i_ThirdColor) {
                            i_ViolationCount++;

                            cout<<"Violation "<<i_ViolationCount<<"\t : "<<STEP_UP(vi_Edges[k])<<" ["<<STEP_UP(i_ThirdColor)<<"] - "<<STEP_UP(vi_Edges[l])<<" ["<<STEP_UP(i_FourthColor)<<"]"<<endl;

                        }

                        if(i_FourthColor == i_SecondColor) {
                            i_ViolationCount++;

                            cout<<"Violation "<<i_ViolationCount<<"\t : "<<STEP_UP(i)<<" ["<<STEP_UP(i_FirstColor)<<"] - "<<STEP_UP(vi_Edges[j])<<" ["<<STEP_UP(i_SecondColor)<<"] - "<<STEP_UP(vi_Edges[k])<<" ["<<STEP_UP(i_ThirdColor)<<"] - "<<STEP_UP(vi_Edges[l])<<" ["<<STEP_UP(i_FourthColor)<<"]"<<endl;

                            continue;
                        }
                    }
                }
            }
        }
    }

#if VERBOSE == _TRUE

    if(!i_ViolationCount) {
        cout<<"No Violation Found"<<endl;
    }

    cout<<endl;
    cout<<"[Total Violations = "<<i_ViolationCount<<"]"<<endl;
    cout<<endl;
#endif

    return(i_ViolationCount);
}

int PrintVertexColors(vector<int> & vi_VertexColors) {
    int i;

    int i_VertexCount;

    int i_HighestColor;

    i_VertexCount = (signed) vi_VertexColors.size();

    i_HighestColor = _UNKNOWN;

    cout<<endl;
    cout<<"Graph Coloring | Vertex Colors"<<endl;
    cout<<endl;

    for(i=0; i<i_VertexCount; i++) {
        cout<<"Vertex "<<STEP_UP(i)<<"\t"<<" : "<<STEP_UP(vi_VertexColors[i])<<endl;

        if(i_HighestColor < vi_VertexColors[i]) {
            i_HighestColor = vi_VertexColors[i];
        }
    }

    cout<<endl;
    cout<<"[Total Colors = "<<STEP_UP(i_HighestColor)<<"]"<<endl;
    cout<<endl;

    return(STEP_UP(i_HighestColor));
}


/***Global Function 4***/
int GenerateSeed(int indep, double ** Seed, vector<int> & vi_Vertices, vector<int> & vi_Edges, vector<int> & vi_VertexColors, int colors) {
    int i;
    int start;

    int size;

    size = STEP_DOWN((signed) vi_Vertices.size());

    start = size - indep;

    /* allocate and initialize Seed matrix */
    for (i=0; i<indep; i++) {
        Seed[i]= (double*) calloc(colors,sizeof(double));
    }

    /* scan the graph and mark the color of each column row in the matrix*/
    for (i=start; i<size ;i++) {
        Seed[i - start][vi_VertexColors[i]] = 1;
    }

    return(_TRUE);
}
