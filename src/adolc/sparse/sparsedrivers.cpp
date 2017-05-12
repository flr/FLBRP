/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/sparsedrivers.c
 Revision: $Id: sparsedrivers.cpp 134 2009-03-03 14:25:24Z imosqueira $
 Contents: "Easy To Use" C++ interfaces of SPARSE package
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History: 20040414 kowarz:  adaption to configure - make - make install
          19990308 christo: myalloc1_ushort -> myalloc1_uint
          19990302 christo: new interface of jac_pat(...)
          19981130 olvo:    newly created from driversc.c
 
----------------------------------------------------------------------------*/

// Code incorporated by Arijit Tarafdar (tarafdar@cs.odu.edu)
// and Assefaw Gebremedhin (assefaw@cs.odu.edu), Old Dominion University, Norfolk, VA

//***START***//

#include <vector>
#include "sparse/graph_hess.h"

//***END***//

#include "../../../inst/include/sparse/sparse.h"
#include "../../../inst/include/sparse/jacutils.h"
#include "../../../inst/include/sparse/graph_jac.h"
#include "../../../inst/include/oplate.h"
#include "../../../inst/include/adalloc.h"
#include "../../../inst/include/interfaces.h"
#include "../../../inst/include/taputil.h"
#include "../../../inst/include/tayutil.h"
#include "../../../inst/include/taputil_p.h"

#include <math.h>
#include <stdio.h>
using namespace std;

BEGIN_C_DECLS

/*---------------------------------------------------------------------------*/
/*                                                         jacobian pattern  */
/*                                                                           */

int jac_pat(
    short        tag,        /* tape identification                        */
    int          depen,      /* number of dependent variables              */
    int          indep,      /* number of independent variables            */
    double       *basepoint, /* independant variable values                */
    unsigned int *rb,
    /* rb[0] = number of blocks of dependent variables
       dependent variable j=0..depen-1 belongs to row block rb[1+j],
       if rb == NULL  each dependent variable will be considered
                      as a block of dependent variables (rb[0]=depen)      */
    unsigned int *cb,
    /* cb[0] = number of blocks of independent variables
       independent variable i=0..indep-1 belongs to column block cb[1+i]
       if cb == NULL  each independent variable will be considered
                      as a block of independent variables (cb[0]=indep)    */
    unsigned int **crs,
    /* returned compressed row block-index storage
         crs[ rb[0] ][ non-zero blocks of independent variables per row ]  
       crs[depen. block][0] = non-zero indep. bl. w.r.t current depen. bl.  
       crs[depen. block][1 .. crs[depen. bl.][0]] :                 
         indeces of non-zero blocks of independent variables  with
           respect to the current block of dependent variables             */
    int          *options
    /* control options
                    options[0] : way of bit pattern propagation
                               0 - automatic detection (default)
                               1 - forward mode 
                               2 - reverse mode
                    options[1] : test the computational graph control flow
                               0 - safe mode (default)
                               1 - tight mode                              */

) {
    int             rc= -1, i, j;
    unsigned int    depen_blocks, indep_blocks, *rowbl, *colbl;
    int             ctrl_options[3];

    if (rb == NULL) {
        rb = myalloc1_uint(1+depen);
        rowbl = rb;
        *rowbl++ = depen;
        for (j=0; j<depen; j++)
            *rowbl++ = j;
    }
    if (cb == NULL) {
        cb = myalloc1_uint(1+indep);
        colbl = cb;
        *colbl++ = indep;
        for (i=0; i<indep; i++)
            *colbl++ = i;
    }

    rowbl = rb;
    depen_blocks = *rowbl++;
    for (j=0; j<depen; j++)
        if ( *rowbl++ >= depen_blocks ) {
            fprintf(DIAG_OUT,"ADOL-C user error in jac_pat(...) : "
                    "bad dependent block index rb[%i]=%i >= rb[0]=%i !\n",
                    j+1, *(rowbl-1), depen_blocks);
            exit(-1);
        }
    colbl = cb;
    indep_blocks = *colbl++;
    for (i=0; i<indep; i++)
        if ( *colbl++ >= indep_blocks ) {
            fprintf(DIAG_OUT,"ADOL-C user error in jac_pat(...) : "
                    "bad independent block index cb[%i]=%i >= cb[0]=%i !\n",
                    i+1, *(colbl-1), indep_blocks);
            exit(-1);
        }

    if (crs == NULL) {
        fprintf(DIAG_OUT,"ADOL-C user error in jac_pat(...) : "
                "parameter crs may not be NULL !\n");
        exit(-1);
    } else
        for (i=0; i<depen_blocks; i++)
            crs[i] = NULL;

    if (( options[0] < 0 ) || (options[0] > 2 ))
        options[0] = 0; /* default */
    if (( options[1] < 0 ) || (options[1] > 1 ))
        options[1] = 0; /* default */
    ctrl_options[0] = options[0];
    ctrl_options[1] = options[1];
    ctrl_options[2] = 0; /* no output intern */

    rc = block_pattern( tag, depen, indep,  basepoint,
                        rb, cb, crs, ctrl_options);

    return(rc);
}

/****************************************************************************/

int sparse_jac(
    short          tag,        /* tape identification                     */
    int            depen,      /* number of dependent variables           */
    int            indep,      /* number of independent variables         */
    int            repeat,     /* indicated repeated call with same seed  */
    double        *basepoint,  /* independant variable values             */
    int           *nnz,        /* number of nonzeros                      */
    unsigned int  **rind,      /* row index                               */
    unsigned int  **cind,      /* column index                            */
    double        **values     /* non-zero values                         */
) {
    int i, l;
    unsigned int j;
    static double **Seed;
    static double *y;
    static double **B=NULL;
    static unsigned int *rind_in;
    static unsigned int *cind_in;
    static unsigned int **JP=NULL;
    static double *values_in;
    static unsigned int *colors;
    static int nnz_in, p;
    unsigned int  *rowbl=NULL;
    unsigned int  *colbl=NULL;
    int ctrl[2];
    int ret_val;
    int found;


    /* Generate sparsity pattern, determine nnz, allocate memory */
    if (repeat == 0) {
        rowbl = NULL;
        colbl = NULL;
        if (JP != NULL) {
            free((unsigned int*) *JP);
            free((unsigned int*) JP);
        }
        JP    = (unsigned int **) malloc(depen*sizeof(unsigned int *));
        ctrl[0] = 0;
        ctrl[1] = 0;

        /* generate sparsity pattern */
        ret_val = jac_pat(tag, depen, indep, basepoint, rowbl, colbl, JP, ctrl);

        if (ret_val < 0) {
            printf(" ADOL-C error in sparse_jac() \n");
            return ret_val;
        }
        nnz_in = 0;
        for (i=0;i<depen;i++) {
            for (j=1;j<=JP[i][0];j++)
                nnz_in++;
        }

        *nnz = nnz_in;

        free((unsigned int*) rowbl);
        free((unsigned int*) colbl);

        generate_seed_jac(depen, indep, JP, &Seed, &p);

        if (*rind != NULL)
            free((unsigned int*) *rind);
        *rind = (unsigned int *) malloc(nnz_in*sizeof(unsigned int));
        rind_in = *rind;

        if (*cind != NULL)
            free((unsigned int*) *cind);
        *cind = (unsigned int *) malloc(nnz_in*sizeof(unsigned int));
        cind_in = *cind;

        if (*values != NULL)
            free((double*) *values);
        *values = (double *) malloc(nnz_in*sizeof(double));
        values_in = *values;

        if (B != NULL) {
            myfree2(B);
        }

        B = myalloc2(depen,p);

        if (y != NULL)
            myfree1(y);
        y=myalloc1(depen);

        if (colors != NULL)
            free((unsigned int*) colors);
        colors = (unsigned int *) malloc(indep*sizeof(unsigned int));

        for(i=0; i<indep ;i++) {
            found = 0;
            l = 0;
            while((l<p) && (found == 0)) {
                if (Seed[i][l] == 1) {
                    colors[i] = l;
                    found = 1;
                }
                l++;
            }
        }
    }


    if (nnz_in != *nnz) {
        printf(" ADOL-C error in sparse_jac():"
               " Number of nonzeros not consistens,"
               " repeat call with repeat = 0 \n");
        return -3;
    }

    /* compute jacobian times matrix product */

    ret_val = fov_forward(tag, depen, indep, p, basepoint, Seed, y, B);

    /* store nonzero entries in sparse formate */
    l = 0;
    for (i=0;i<depen;i++) {
        for (j=1;j<=JP[i][0];j++) {
            rind_in[l] = i;
            cind_in[l] = JP[i][j];
            values_in[l] = B[i][colors[cind_in[l]]];
            l++;
        }
    }

    return ret_val;

}


/****************************************************************************/

int sparse_hess(
    short          tag,        /* tape identification                     */
    int            indep,      /* number of independent variables         */
    int            repeat,     /* indicated repeated call with same seed  */
    double        *basepoint,  /* independant variable values             */
    int           *nnz,        /* number of nonzeros                      */
    unsigned int  **rind,      /* row index                               */
    unsigned int  **cind,      /* column index                            */
    double        **values     /* non-zero values                         */
) {
    int i, l;
    unsigned int j, k;
    static double **seed;
    static double*** Xppp;
    static double*** Yppp;
    static double*** Zppp;
    static double**  Upp;
    static unsigned int *rind_in;
    static unsigned int *cind_in;
    static unsigned int **HP;
    static double *values_in;
    static unsigned int *colors;
    static int nnz_in, p;
    double y;
    int ret_val;
    int found;

    /* Generate sparsity pattern, determine nnz, allocate memory */
    if (repeat == 0) {
        if (HP != NULL) {
            free((unsigned int*) *HP);
            free((unsigned int*) HP);
        }
        HP    = (unsigned int **) malloc(indep*sizeof(unsigned int *));

        /* generate sparsity pattern */
        ret_val = hess_pat(tag, indep, basepoint, HP, 0);

        if (ret_val < 0) {
            printf(" ADOL-C error in sparse_hess() \n");
            return ret_val;
        }
        nnz_in = 0;
        for (i=0;i<indep;i++) {
            for (j=1;j<=HP[i][0];j++)
                nnz_in++;
        }

        *nnz = nnz_in;

        generate_seed_hess(indep, HP, &seed, &p);

        if (*rind != NULL)
            free((unsigned int*) *rind);
        *rind = (unsigned int *) malloc(nnz_in*sizeof(unsigned int));
        rind_in = *rind;

        if (*cind != NULL)
            free((unsigned int*) *cind);
        *cind = (unsigned int *) malloc(nnz_in*sizeof(unsigned int));
        cind_in = *cind;

        if (*values != NULL)
            free((double*) *values);
        *values = (double *) malloc(nnz_in*sizeof(double));
        values_in = *values;


        if (colors != NULL)
            free((unsigned int*) colors);
        colors = (unsigned int *) malloc(indep*sizeof(unsigned int));

        for(i=0; i<indep ;i++) {
            found = 0;
            l = 0;
            while((l<p) && (found == 0)) {
                if (seed[i][l] == 1) {
                    colors[i] = l;
                    found = 1;
                }
                l++;
            }
        }

        if (Xppp != NULL)
            myfree3(Xppp);
        Xppp = myalloc3(indep,p,1);

        if (Yppp != NULL)
            myfree3(Yppp);
        Yppp = myalloc3(1,p,1);

        if (Zppp != NULL)
            myfree3(Zppp);
        Zppp = myalloc3(p,indep,2);

        if (Upp == NULL) {
            Upp = myalloc2(1,2);
            Upp[0][0] = 1;
            Upp[0][1] = 0;
        }
    }

    if (Upp == NULL) {
        printf(" ADOL-C error in sparse_hess():"
               " First call with repeat = 0 \n");
        return -3;
    }

    if (nnz_in != *nnz) {
        printf(" ADOL-C error in sparse_hess():"
               " Number of nonzeros not consistent,"
               " new call with repeat = 0 \n");
        return -3;
    }

    for (i=0; i<indep; i++)
        for (l=0;l<p;l++)
            Xppp[i][l][0] = seed[i][l];

    ret_val = hov_wk_forward(tag,1,indep,1,2,p,basepoint,Xppp,&y,Yppp);
    MINDEC(ret_val,hos_ov_reverse(tag,1,indep,1,p,Upp,Zppp));

    l = 0;
    for (i=0;i<indep;i++) {
        for (j=1;j<=HP[i][0];j++) {
            rind_in[l] = i;
            cind_in[l] = HP[i][j];
            found = 0;
            k = 1;
            while ((k <= HP[i][0]) && (found == 0)) {
                if ((colors[HP[i][k]] == colors[cind_in[l]])
                        && (HP[i][k] != cind_in[l]))
                    found = 1;
                k++;
            }
            if (found == 0) {
                values_in[l] = Zppp[colors[cind_in[l]]][i][1];
            } else {
                values_in[l] = Zppp[colors[i]][ cind_in[l]][1];
            }

            l++;
        }
    }

    return ret_val;

}

/****************************************************************************/

void generate_seed_jac
(int m, int n, unsigned int **jacpat, double*** seed, int *p) {
    int size;
    int num_colors;
    Graph G;


    size = n+m;

    if (m > 0) {
        G = Bipartite_Graph(m, n, jacpat);

        num_colors = GreedyPartialD2Coloring (G, size , m);

        *seed = (double**) malloc(sizeof(double*) * n);
        Generate_Seed_from_Graph(n, *seed, G, size, num_colors);

        *p = num_colors;

        Delete_Graph(G, n+m);
    } else
        *p = 0;


}

/****************************************************************************/

void generate_seed_hess(int n, unsigned int **hesspat, double*** seed, int *p) {
// Code incorporated by Arijit Tarafdar (tarafdar@cs.odu.edu)
// and Assefaw Gebremedhin (assefaw@cs.odu.edu), Old Dominion University, Norfolk, VA

//***START***//

    int i;
    int i_VertexCount;
    int i_HighestColor;

    vector<int> vi_Vertices, vi_Edges;
    vector<int> vi_VertexOrder;
    vector<int> vi_VertexColors;

    vi_Vertices.clear();
    vi_Edges.clear();

    vi_VertexColors.clear();

    ReadSparsityPattern(vi_Vertices, vi_Edges, hesspat, n);

    vi_VertexOrder.clear();

    i_VertexCount = STEP_DOWN((signed) vi_Vertices.size());

    for(i=0; i<i_VertexCount; i++) {
        vi_VertexOrder.push_back(i);
    }

    vi_VertexColors.clear();
    vi_VertexColors.resize((unsigned) i_VertexCount, _UNKNOWN);

    i_HighestColor = FindStarColoring(vi_Vertices, vi_Edges, vi_VertexOrder, vi_VertexColors);

    CheckStarColoring(vi_VertexColors, vi_Vertices, vi_Edges);

    //    PrintVertexColors(vi_VertexColors);

    *seed = (double**) malloc(sizeof(double*) * n);

    GenerateSeed(n, *seed, vi_Vertices, vi_Edges, vi_VertexColors, i_HighestColor);

    *p = i_HighestColor;

//***END***//
}

END_C_DECLS
