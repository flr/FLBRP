/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/graph_jac.c
 Revision: $Id: graph_jac.c 134 2009-03-03 14:25:24Z imosqueira $
 Contents: graph utilities for SPARSE package
 
 Copyright (c) 2005
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History: 20050413 andrea: newly created 
 
----------------------------------------------------------------------------*/

#include "sparse/sparse.h"
#include "sparse/graph_jac.h"
#include "oplate.h"
#include "adalloc.h"

#include <math.h>
#include <stdio.h>

BEGIN_C_DECLS
/****************************************************************************/
/*GRAPH COLORING -- graphs data structure
                    and greddy coloring algorithms coding*/
/*--------------------------------------------------------------------------*/
/* actions on graphs                                                        */

/*initialize graph*/

Graph initializeGraph (int size) {
    int i;
    Graph G;
    G = (struct VertixElement**) malloc(sizeof(struct VertixElement*) * size);
    for (i=0; i<size; i++) G[i]= 0;
    return G;
}

/* add new vertix*/

void AddVertix(Graph G, int index) {
    G[index]= (struct VertixElement*) malloc(sizeof(struct VertixElement));
    G[index]->color =0;   /*no color*/
    G[index]->edge  =0;
}

/* add edge*/

void addEdge(Graph G, int index, int destination) {
    struct EdgeElement* e;
    if (G[index]!= 0) {
        if (G[index]->edge == 0)  /*first edge for this index*/
        {
            G[index]->edge = (EdgeElement*) malloc (sizeof(EdgeElement));
            G[index]->edge->dest = destination;
            G[index]->edge->next = 0 ;
        } else {
            e = G[index]->edge;
            while((e->dest != destination)&&(e->next  !=0))
                e = e->next;
            if(e->dest != destination) {
                e->next = (EdgeElement*) malloc (sizeof(EdgeElement));
                e->next->dest = destination;
                e->next->next = 0;
            }
        }
    }
}

void Delete_Graph(Graph G, int size) {
    int i;
    struct EdgeElement *e, *e1;

    i=0;
    while ((i<size)&&(G[i]!= 0)) {
        if (G[i]->edge != 0) {
            e = G[i]->edge;
            while(e !=0) {
                e1 = e->next;
                free ((EdgeElement*) e);
                e = e1;
            }
        }
        i++;
    }
}

void printGraph(Graph G, int size) {
    int i , j;
    struct EdgeElement* e;
    i=0;
    while ((i<size)&&(G[i]!= 0)) {
        printf("Vertix no.%d\n", i);
        printf("Vertix color %d\n", G[i]->color);
        printf("Edges :\n");
        if (G[i]->edge == 0)
            printf("No Edges\n");
        else {
            e = G[i]->edge;
            j = 1;
            while(e !=0) {
                printf("Edge no.%d \n", j);
                printf("destination :%d \n", e->dest);
                e = e->next;
                j++;
            }
        }
        printf("****************************:\n");
        i++;
    }
}

/*--------------------------------------------------------------------------*/
/* generate  graphs                                                         */

/* For Jacobian computation */

/* bipartite Graph */

Graph Bipartite_Graph (int m,int n, unsigned int** JacobianVector) {
    int i,j;
    Graph G = NULL;

    if (m > 0) {
        G = initializeGraph(m+n);
        /*create m rows vertices*/
        for (i=0; i<m; i++) AddVertix(G, i);
        /* create n cols vertices -- it will be numbered from n+1 to n+m+1 */
        for (j=0; j<n; j++) AddVertix(G, j+m);
        /* create edges by scaning the vertix and
         * create edges from every row to the columns in its entry */

        for (i=0; i<m; i++) {
            for (j=1; j<=JacobianVector[i][0]; j++) {
                addEdge(G, i, JacobianVector[i][j]+m);
                addEdge(G, JacobianVector[i][j]+m, i);
            }
        }
    }

    return G;
}

/*--------------------------------------------------------------------------*/
/* graph coloring                                                           */

/* For Jacobian computation */

/* greedy 2 distance coloring algorithm */

int GreedyPartialD2Coloring (Graph G, int size, int rows) {
    struct EdgeElement* e;
    struct EdgeElement* e2;
    int *forbiddenColors;
    int i, j, found, min;
    int colorsNo = 0;

    forbiddenColors = (int *)malloc(sizeof(int)*size);

    if (rows > 0) {
        for(i=0; i<size; i++) forbiddenColors[i] = i+1;

        i=rows;
        /* color(v_1) = 1 */
        G[i]->color = 1;
        colorsNo=1;
        for(i=rows+1;i<size;i++) {
            e = G[i]->edge;
            while (e != NULL) {
                e2 =G[e->dest]->edge;
                /* for each colored vertex w of neighbour 2 of v_i do */
                while (e2 != NULL) {
                    if (G[e2->dest]->color != 0) {
                        /* forbiddenColors(color(w)) = i; */
                        forbiddenColors[G[e2->dest]->color-1] = i;
                    }
                    e2 = e2->next;
                }
                e = e->next;
            }

            /* color(v_i) = min {c: forbiddenColors(c) != i} */
            min = colorsNo+1;

            j = 0;
            found = 0;
            while ((j < colorsNo) && (found == 0)) {
                if(forbiddenColors[j] != i) {
                    min = j+1;
                    found = 1;
                }
                j++;
            }
            G[i]->color = min;

            if (min > colorsNo)
                colorsNo++;
        }
    }

    return colorsNo;
}

/*--------------------------------------------------------------------------*/
/* Generate Seed matrix from Graph                                          */

void Generate_Seed_from_Graph
(int indep, double** Seed, Graph G, int size, int colors) {
    int i,j;
    int start;

    start = size - indep;

    /* allocate and initialize Seed matrix */
    for (i=0; i<indep; i++) {
        Seed[i]= (double*) calloc(colors,sizeof(double));
    }

    /* scan the graph and mark the color of each column row in the matrix*/
    for (i=start; i<size ;i++) {
        j= G[i]->color-1;
        Seed[i-start][j] = 1;
    }
}

END_C_DECLS
