/** \file sparse/hessutils.c */

/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     sparse/hessutils.c
 Revision: $Id: hessutils.c 134 2009-03-03 14:25:24Z imosqueira $
 Contents: drivers and utilities for sparse Hessians
 
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
#include "oplate.h"
#include "adalloc.h"
#include "taputil.h"
#include "tayutil.h"
#include "taputil_p.h"

#include <math.h>
#include <stdio.h>

/*--------------------------------------------------------------------------*/
/*                                                             other macros */
#define FMIN_ADOLC(x,y)  ((y<x)?y:x)
#define MINDEC(a,b) if ((a) > (b)) (a) = (b)

BEGIN_C_DECLS

static int for_location_cnt;
static int dep_cnt;
static int ind_cnt;

/****************************************************************************/
/* set structure                                                            */

/**
 * This is the type used for the list elements. The entry is either a counter
 * (first element of the NID list) or the index of an independent variable.
 */
typedef struct IndexElement {
    locint  entry;
    struct IndexElement* next;
}
IndexElement;

/****************************************************************************/
/* set operations                                                           */

/*--------------------------------------------------------------------------*/
/* delete set                                                               */

/**
 * This function is used to delete the index domain list for the variable
 * given by index "ind".
 */
void delete_index_domain(int ind, IndexElement **ind_dom) {
    IndexElement*  temp_ind;
    IndexElement*  temp;

    temp_ind = ind_dom[ind];
    while (temp_ind != NULL) {
        temp = temp_ind->next;
        free((IndexElement*) temp_ind);
        temp_ind = temp;
    }

    ind_dom[ind] = NULL;
}

/*--------------------------------------------------------------------------*/
/* operations on index domains                                              */

/**
 * Creates an index domain list for the variable with index "res" as copy from
 * the index domain list of variable with index "arg".
 */
void copy_index_domain(int res, int arg, IndexElement **ind_dom) {
    int first;
    IndexElement*  temp_arg;
    IndexElement*  temp_res;
    IndexElement*  temp;

    if (res != arg) {
        delete_index_domain(res, ind_dom);
    } else return; /* if "arg" equals "res" => no action needed! */

    temp_arg = ind_dom[arg];
    temp_res = ind_dom[res]; /* is NULL due to delete_index_domain */
    first = 0;
    while (temp_arg != NULL) {
        if(first == 0) {
            temp_res = (struct IndexElement*)
                malloc(sizeof(struct IndexElement));
            ind_dom[res] = temp_res;
            first = 1;
        } else {
            temp = (struct IndexElement*) malloc(sizeof(struct IndexElement));
            temp_res->next = temp;
            temp_res = temp;
        }
        temp_res->entry = temp_arg->entry;
        temp_res->next  = NULL;
        temp_arg = temp_arg->next;
    }
}

/**
 * This function extends the index domain list of the result by the subset of
 * indexes of the argument list not yet contained in the result list. The
 * result list is kept in a sorted fashion (entry-based).
 */
void merge_2_index_domains(int res, int arg, IndexElement **ind_dom) {
    IndexElement*  temp_arg;
    IndexElement*  temp_res;
    IndexElement*  temp;

    temp_arg = ind_dom[arg];
    temp_res = ind_dom[res];

    if (temp_res == NULL) { /* no index domain for res, so far => copy*/
        while (temp_arg != NULL) {
            temp = (struct IndexElement*)
                malloc(sizeof(struct IndexElement));
            /* Remark: (ind_dom[res] == NULL) was intended, right? */
            if (ind_dom[res] == temp_res) { /* first list element */
                ind_dom[res] = temp;
                temp->next = NULL;
                temp_res = temp;
                temp_res->entry = temp_arg->entry;
                temp_res = temp_res->next;
                temp_arg = temp_arg->next;
            } else { /* append */
                temp->next = NULL;
                temp->entry = temp_arg->entry;
                temp_res = temp;
                temp_arg = temp_arg->next;
            }
        }
    } else { /* merge */
        while (temp_arg != NULL) {
            if (temp_arg->entry < temp_res->entry) { /* < */
                temp = (struct IndexElement*)
                    malloc(sizeof(struct IndexElement));
                if (ind_dom[res] == temp_res) { /* new first list element */
                    ind_dom[res] = temp;
                    temp->next = temp_res;
                    temp->entry = temp_arg->entry;
                } else {
                    temp->next = temp_res->next;
                    temp->entry = temp_res->entry;
                    temp_res->entry = temp_arg->entry;
                    temp_res->next = temp;
                    temp_res = temp_res->next;
                }
                temp_arg = temp_arg->next;
            } else {
                if (temp_arg->entry == temp_res->entry) { /* == , so skip */
                    temp_arg = temp_arg->next;
                } else { /* > */
                    if (temp_res->next == NULL) { /* append */
                        temp = (struct IndexElement*)
                            malloc(sizeof(struct IndexElement));
                        temp_res->next = temp;
                        temp_res = temp_res->next;
                        temp_res->entry = temp_arg->entry;
                        temp_res->next = NULL;
                        temp_arg = temp_arg->next;
                    } else {
                        temp_res = temp_res->next; /* continue */
                    }
                }
            }
        }
    }
}

/**
 * Combine the two index domain list for "arg1" and "arg2" into a new list
 * associated to "res". The result list is created in a sorted fashion
 * (entry-based).
 */
void combine_2_index_domains
(int res, int arg1, int arg2, IndexElement **ind_dom) {
    int first;
    IndexElement* temp_arg1 = NULL;
    IndexElement* temp_arg2 = NULL;
    IndexElement* temp_res = NULL;
    IndexElement* temp = NULL;
    IndexElement* temp1 = NULL;
    IndexElement* temp2 = NULL;

    if ((res != arg1) && (res != arg2)) {
        /* delete list for "res" if not argument */
        delete_index_domain(res, ind_dom);
    } else { /* keep pointers for later list deletion */
        temp1 = ind_dom[arg1];
        temp2 = ind_dom[arg2];
    }

    temp_arg1 = ind_dom[arg1];
    temp_arg2 = ind_dom[arg2];
    ind_dom[res] = NULL; /* The "res" index domain list hast to be empty if
                            both argument lists are empty! */
    first = 0;

    while ((temp_arg1 != NULL) || (temp_arg2 != NULL)) {
        if (first == 0) {
            temp_res = (struct IndexElement*) malloc(sizeof(struct IndexElement));
            ind_dom[res] = temp_res;
            first = 1;
        } else {
            temp = (struct IndexElement*) malloc(sizeof(struct IndexElement));
            temp_res->next = temp;
            temp_res = temp;
        }
        if ((temp_arg1 != NULL) && (temp_arg2 != NULL)) {
            /* both list have more elements */
            if (temp_arg1->entry < temp_arg2->entry) { /* < */
                temp_res->entry = temp_arg1->entry;
                temp_res->next = NULL;
                temp_arg1 = temp_arg1->next;
            } else { /* >= */
                if (temp_arg1->entry > temp_arg2->entry) { /* > */
                    temp_res->entry = temp_arg2->entry;
                    temp_res->next = NULL;
                    temp_arg2 = temp_arg2->next;
                } else { /* == */
                    temp_res->entry = temp_arg2->entry;
                    temp_res->next = NULL;
                    temp_arg1 = temp_arg1->next;
                    temp_arg2 = temp_arg2->next;
                }
            }
        } else { /* one list is out of elements */
            if (temp_arg1 != NULL) {
                temp_res->entry = temp_arg1->entry;
                temp_res->next = NULL;
                temp_arg1 = temp_arg1->next;
            } else {
                temp_res->entry = temp_arg2->entry;
                temp_res->next = NULL;
                temp_arg2 = temp_arg2->next;
            }
        }
    }

    /* if argument was equal to "res" delete input list */
    if (res == arg1) {
        temp_arg1 = temp1;
        while (temp_arg1 != NULL) {
            temp = temp_arg1->next;
            free((IndexElement*) temp);
            temp_arg1 = temp;
        }
    }
    if (res == arg2) {
        temp_arg2 = temp2;
        while (temp_arg2 != NULL) {
            temp = temp_arg2->next;
            free((IndexElement*) temp);
            temp_arg2 = temp;
        }
    }

}

/**
 * Extend index domain list for "res" by the lists for "arg1" and "arg2".
 * Indexes in the result list are unique and sorted.
 */
void merge_3_index_domains
(int res, int arg1, int arg2, IndexElement **ind_dom) {
    merge_2_index_domains(res, arg1, ind_dom);
    merge_2_index_domains(res, arg2, ind_dom);
}

/*--------------------------------------------------------------------------*/
/* operations on nonlinearity domains                                       */

/**
 * Extend the list of nonlinear dependencies (Nonlinear interaction domain
 * -- NID) for each element of the index domain list of "arg1" by the index
 * domain list of "arg2" and vice versa. NID list elements are unique and
 * sorted.
 * This is the worker for "extend_nonlinearity_domain_unary" and
 * "extend_nonlinearity_domain_binary"!
 */
void extend_nonlinearity_domain_binary_step
(int arg1, int arg2, IndexElement **ind_dom, IndexElement **nonl_dom) {
    IndexElement* temp_arg1;
    IndexElement* temp_arg2;
    IndexElement* temp_nonl;
    IndexElement* temp1;
    IndexElement* nonl_num;

    temp_arg1 = ind_dom[arg1];
    /* for each element of the index domain list of "arg1" */
    while  (temp_arg1 != NULL) {
        temp_nonl = nonl_dom[temp_arg1->entry];
        temp_arg2 = ind_dom[arg2]; /* index domain list of "arg2" */
        /* The first element of the NID list stores the number of elements
         * of the list - 1. Create this special element if not available. */
        if (temp_nonl == NULL) {
            temp_nonl = (struct IndexElement*)
                malloc(sizeof(struct IndexElement));
            nonl_dom[temp_arg1->entry] = temp_nonl;
            temp_nonl->entry = 0;
            temp_nonl->next = NULL;
        }
        nonl_num = temp_nonl; /* kept for updating the element count */
        if (nonl_num->entry == 0) { /* empty list */
            while(temp_arg2 != NULL) { /* append index domain list of "arg" */
                temp_nonl->next = (struct IndexElement*)
                    malloc(sizeof(struct IndexElement));
                temp_nonl = temp_nonl->next;
                temp_nonl->entry = temp_arg2->entry;
                temp_nonl->next = NULL;
                temp_arg2 = temp_arg2->next;
                nonl_num->entry = nonl_num->entry+1;
            }
        } else { /* merge lists */
            temp_nonl = temp_nonl->next; /* skip counter */
            while (temp_arg2 != NULL) {
                if (temp_arg2->entry < temp_nonl->entry) { /* < */
                    temp1 = (struct IndexElement*)
                        malloc(sizeof(struct IndexElement));
                    /* set temp1->entry to temp_arg2->entry, insert temp1 after
                     * temp_nonl and swap the entries of temp1 and
                     * temp_nonl to correct the sequence */
                    temp1->next = temp_nonl->next;
                    temp1->entry = temp_nonl->entry;
                    temp_nonl->entry = temp_arg2->entry;
                    temp_nonl->next = temp1;
                    temp_nonl = temp_nonl->next;
                    temp_arg2 = temp_arg2->next;
                    nonl_num->entry = nonl_num->entry+1;
                } else {
                    if (temp_arg2->entry == temp_nonl->entry) { /* == */
                        temp_arg2 = temp_arg2->next; /* skip */
                    } else { /* > */
                        if (temp_nonl->next == NULL) { /* end of the NID list
                                                          => append */
                            temp1 = (struct IndexElement*)
                                malloc(sizeof(struct IndexElement));
                            temp_nonl->next = temp1;
                            temp_nonl = temp_nonl->next;
                            temp_nonl->entry = temp_arg2->entry;
                            temp_nonl->next = NULL;
                            temp_arg2 = temp_arg2->next;
                            nonl_num->entry = nonl_num->entry+1;
                        } else { /* continue */
                            temp_nonl = temp_nonl->next;
                        }
                    }
                }
            }
        }
        temp_arg1 = temp_arg1->next;
    }
}

/**
 * Extend the list of nonlinear dependencies (Nonlinear interaction domain
 * -- NID) for each element of the index domain list of "arg" by the index
 * domain list of "arg". NID list elements are unique and sorted.
 * This is the function for unary operations!
 */
void extend_nonlinearity_domain_unary
(int arg, IndexElement **ind_dom, IndexElement **nonl_dom) {
    extend_nonlinearity_domain_binary_step(arg, arg, ind_dom, nonl_dom);
}

/**
 * Extend the list of nonlinear dependencies (Nonlinear interaction domain
 * -- NID) for each element of the index domain list of "arg1" by the index
 * domain list of "arg2" and vice versa. NID list elements are unique and
 * sorted.
 * This is the function for binary operations!
 */
void extend_nonlinearity_domain_binary
(int arg1, int arg2, IndexElement **ind_dom, IndexElement **nonl_dom) {
    extend_nonlinearity_domain_binary_step(arg1, arg2, ind_dom, nonl_dom);
    extend_nonlinearity_domain_binary_step(arg2, arg1, ind_dom, nonl_dom);
}


/****************************************************************************/
/*                                                                 hess pat */

int hess_pat
(
    short          tag,       /* tape identification                      */
    int            indcheck,  /* number of independent variables          */
    double        *basepoint, /* independant variable values              */
    unsigned int **crs,       /* returned row index storage
                                     * crs[i][0] = number of non-zeroentries 
                                     *             in row i
                                     * crs[i][1 .. crs[i][0]]:
                                     *             indices of non-zero entries  */
    int            option
    /* control option
     * option : test the computational graph
     *          control flow
     *          0 - safe mode (default)
     *          1 - tight mode                  */
) {
    /*--------------------------------------------------------------------------*/
    /*                                                            ALL VARIABLES */
    unsigned char operation; /* operation code */
    int tape_stats[11];      /* tape stats */
    int ret_c =3;            /* return value */
    int max_live;

    /* index domains */
    IndexElement** ind_dom;

    /* nonlinear interaction domains */
    IndexElement** nonl_dom;

    /* temppraries */
    IndexElement*  temp_res;
    IndexElement*  temp;


    locint size = 0;
    locint res  = 0;
    locint arg  = 0;
    locint arg1 = 0;
    locint arg2 = 0;
    locint checkSize;

    double coval = 0, *d = 0, y;

    int indexi = 0;

    int l=0, ls, i;

    int buffer;
    static int fax;

    /* Taylor stuff */
    static double  *T0;
    double         T0temp;
#define T0res  T0temp
#define T0arg  T0temp

#if defined(ADOLC_DEBUG)
    /****************************************************************************/
    /*                                                           DEBUG MESSAGES */
    fprintf(DIAG_OUT,"Call of hess_pat with tag: %d, n: %d,\n",
            tag, indcheck);

#endif

    /****************************************************************************/
    /*                                                                    INITs */

    tapestats(tag,tape_stats);
    ind_cnt          = tape_stats[0];
    dep_cnt          = tape_stats[1];
    for_location_cnt = tape_stats[2];
    buffer           = tape_stats[4];

    max_live = for_location_cnt;

    set_buf_size(buffer);

    if ((1 != dep_cnt)||(indcheck != ind_cnt)) {
        fprintf(DIAG_OUT,"ADOL-C error: forward sweep on tape %d  aborted!\n",tag);
        fprintf(DIAG_OUT,"Number of dependent and/or independent variables passed"
                " to forward is\ninconsistant with number"
                " recorded on tape %d \n",tag);
        exit (-1);
    }

    if (for_location_cnt compsize fax) {
        if (fax)
            free((char *) T0);
        T0 = myalloc1(for_location_cnt);
        fax = for_location_cnt;
    }

    if (for_location_cnt compsize fax) {
        if (fax)
            free((char *) T0);
        T0 = myalloc1(for_location_cnt);
        fax = for_location_cnt;
    }

    /* index domains */
    ind_dom = (struct IndexElement**) malloc(sizeof(struct IndexElement*) * max_live);
    /* nonlinearity domains */
    nonl_dom = (struct IndexElement**) malloc(sizeof(struct IndexElement*) * ind_cnt);

    for(i=0;i<max_live;i++)
        ind_dom[i] = NULL;
    for(i=0;i<ind_cnt;i++)
        nonl_dom[i] = NULL;

    if (crs == NULL) {
        fprintf(DIAG_OUT,"ADOL-C user error in hess_pat(...) : "
                "parameter crs may not be NULL !\n");
        exit(-1);
    } else
        for (i=0; i<ind_cnt; i++)
            crs[i] = NULL;

    /****************************************************************************/
    /*                                                            FORWARD SWEEP */

    /* Initialize the Forward Sweep */
    init_for_sweep(tag);

    operation=get_op_f();
    while (operation !=end_of_tape) {

        switch (operation) {

            /****************************************************************************/
            /*                                                                  MARKERS */

            /*--------------------------------------------------------------------------*/
        case end_of_op:                                          /* end_of_op */
            get_op_block_f();
            operation=get_op_f();
            /* Skip next operation, it's another end_of_op */
            break;

            /*--------------------------------------------------------------------------*/
        case end_of_int:                                        /* end_of_int */
            get_loc_block_f();
            break;

            /*--------------------------------------------------------------------------*/
        case end_of_val:                                        /* end_of_val */
            get_val_block_f();
            break;
            /*--------------------------------------------------------------------------*/
        case start_of_tape:                                  /* start_of_tape */
        case end_of_tape:                                      /* end_of_tape */
            break;


            /****************************************************************************/
            /*                                                               COMPARISON */

            /*--------------------------------------------------------------------------*/
        case eq_zero:                                              /* eq_zero */
            arg = get_locint_f();

            if (T0[arg] != 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator eq_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            ret_c = 0;
            break;

            /*--------------------------------------------------------------------------*/
        case neq_zero:                                            /* neq_zero */
            arg = get_locint_f();

            if (T0[arg] == 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator neq_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            break;

            /*--------------------------------------------------------------------------*/
        case le_zero:                                              /* le_zero */
            arg = get_locint_f();

            if (T0[arg] > 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator le_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            if (T0[arg] == 0)
                ret_c = 0;
            break;

            /*--------------------------------------------------------------------------*/
        case gt_zero:                                              /* gt_zero */
            arg = get_locint_f();

            if (T0[arg] <= 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator gt_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            break;

            /*--------------------------------------------------------------------------*/
        case ge_zero:                                              /* ge_zero */
            arg = get_locint_f();

            if (T0[arg] < 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator ge_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            if (T0[arg] == 0)
                ret_c = 0;
            break;

            /*--------------------------------------------------------------------------*/
        case lt_zero:                                              /* lt_zero */
            arg = get_locint_f();

            if (T0[arg] >= 0) {
                fprintf(DIAG_OUT,
                        "ADOL-C Warning: Branch switch detected in comparison "
                        "(operator lt_zero).\n"
                        "Forward sweep aborted! Retaping recommended!\n");
                end_sweep();
                return (-1);
            }
            break;


            /****************************************************************************/
            /*                                                              ASSIGNMENTS */

            /*--------------------------------------------------------------------------*/
        case assign_a:           /* assign an adouble variable an    assign_a */
            /* adouble value. (=) */
            arg = get_locint_f();
            res = get_locint_f();

            T0[res] = T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case assign_d:            /* assign an adouble variable a    assign_d */
            /* double value. (=) */
            res   = get_locint_f();
            coval = get_val_f();


            T0[res] = coval;

            delete_index_domain(res, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case assign_d_zero:  /* assign an adouble variable a    assign_d_zero */
            /* double value. (0) (=) */
            res   = get_locint_f();


            T0[res] = 0.0;

            delete_index_domain(res, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case assign_d_one:    /* assign an adouble variable a    assign_d_one */
            /* double value. (1) (=) */
            res   = get_locint_f();


            T0[res] = 1.0;

            delete_index_domain(res, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case assign_ind:       /* assign an adouble variable an    assign_ind */
            /* independent double value (<<=) */
            res = get_locint_f();

            T0[res] = basepoint[indexi];

            temp_res = ind_dom[res];
            while (temp_res != NULL) {
                temp = temp_res->next;
                free((IndexElement*) temp_res);
                temp_res = temp;
            }

            ind_dom[res] = (struct IndexElement*) malloc(sizeof(struct IndexElement));
            ind_dom[res]->entry = indexi;
            ind_dom[res]->next = NULL;

            ++indexi;
            break;

            /*--------------------------------------------------------------------------*/
        case assign_dep:           /* assign a float variable a    assign_dep */
            /* dependent adouble value. (>>=) */
            res = get_locint_f();

            y = T0[res];
            break;


            /****************************************************************************/
            /*                                                   OPERATION + ASSIGNMENT */

            /*--------------------------------------------------------------------------*/
        case eq_plus_d:            /* Add a floating point to an    eq_plus_d */
            /* adouble. (+=) */
            res   = get_locint_f();
            coval = get_val_f();

            T0[res] += coval;

            break;

            /*--------------------------------------------------------------------------*/
        case eq_plus_a:             /* Add an adouble to another    eq_plus_a */
            /* adouble. (+=) */
            arg = get_locint_f();
            res = get_locint_f();

            T0[res] += T0[arg];

            merge_2_index_domains(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case eq_min_d:       /* Subtract a floating point from an    eq_min_d */
            /* adouble. (-=) */
            res = get_locint_f();
            coval = get_val_f();


            T0[res] -= coval;

            break;

            /*--------------------------------------------------------------------------*/
        case eq_min_a:        /* Subtract an adouble from another    eq_min_a */
            /* adouble. (-=) */
            arg = get_locint_f();
            res = get_locint_f();


            T0[res] -= T0[arg];

            merge_2_index_domains(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case eq_mult_d:              /* Multiply an adouble by a    eq_mult_d */
            /* flaoting point. (*=) */
            res   = get_locint_f();
            coval = get_val_f();

            T0[res] *= coval;

            break;

            /*--------------------------------------------------------------------------*/
        case eq_mult_a:       /* Multiply one adouble by another    eq_mult_a */
            /* (*=) */
            arg = get_locint_f();
            res = get_locint_f();

            T0[res] *= T0[arg];

            merge_2_index_domains(res, arg, ind_dom);


            extend_nonlinearity_domain_binary(res, arg, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case incr_a:                        /* Increment an adouble    incr_a */
            res   = get_locint_f();

            T0[res]++;

            break;

            /*--------------------------------------------------------------------------*/
        case decr_a:                        /* Increment an adouble    decr_a */
            res   = get_locint_f();

            T0[res]--;

            break;


            /****************************************************************************/
            /*                                                        BINARY OPERATIONS */

            /*--------------------------------------------------------------------------*/
        case plus_a_a:                 /* : Add two adoubles. (+)    plus a_a */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = T0[arg1] + T0[arg2];

            combine_2_index_domains(res, arg1, arg2, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case plus_d_a:             /* Add an adouble and a double    plus_d_a */
            /* (+) */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            T0[res] = T0[arg] + coval;

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case min_a_a:              /* Subtraction of two adoubles     min_a_a */
            /* (-) */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();


            T0[res] = T0[arg1] - T0[arg2];

            combine_2_index_domains(res, arg1, arg2, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case min_d_a:                /* Subtract an adouble from a    min_d_a */
            /* double (-) */
            arg =get_locint_f();
            res = get_locint_f();
            coval = get_val_f();

            T0[res] = coval - T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case mult_a_a:               /* Multiply two adoubles (*)    mult_a_a */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = T0[arg1] * T0[arg2];

            combine_2_index_domains(res, arg1, arg2, ind_dom);

            extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
            /* olvo 991122: new op_code with recomputation */
        case eq_plus_prod:   /* increment a product of           eq_plus_prod */
            /* two adoubles (*) */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] += T0[arg1] * T0[arg2];

            merge_3_index_domains(res, arg1, arg2, ind_dom);

            extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
            /* olvo 991122: new op_code with recomputation */
        case eq_min_prod:    /* decrement a product of            eq_min_prod */
            /* two adoubles (*) */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] -= T0[arg1] * T0[arg2];

            merge_3_index_domains(res, arg1, arg2, ind_dom);

            extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case mult_d_a:         /* Multiply an adouble by a double    mult_d_a */
            /* (*) */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            T0[res] = T0[arg] * coval;

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case div_a_a:           /* Divide an adouble by an adouble    div_a_a */
            /* (/) */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = T0[arg1] / T0[arg2];

            combine_2_index_domains(res, arg1, arg2, ind_dom);

            extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

            extend_nonlinearity_domain_unary(arg2, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case div_d_a:             /* Division double - adouble (/)    div_d_a */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            T0[res] = coval / T0[arg];

            copy_index_domain(res, arg, ind_dom);

            extend_nonlinearity_domain_unary(arg, ind_dom, nonl_dom);

            break;


            /****************************************************************************/
            /*                                                         SIGN  OPERATIONS */

            /*--------------------------------------------------------------------------*/
        case pos_sign_a:                                        /* pos_sign_a */
            arg   = get_locint_f();
            res   = get_locint_f();

            T0[res] = T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case neg_sign_a:                                        /* neg_sign_a */
            arg   = get_locint_f();
            res   = get_locint_f();

            T0[res] = -T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;


            /****************************************************************************/
            /*                                                         UNARY OPERATIONS */

            /*--------------------------------------------------------------------------*/
        case exp_op:                          /* exponent operation    exp_op */
            arg = get_locint_f();
            res = get_locint_f();

            T0[res] = exp(T0[arg]);

            copy_index_domain(res, arg, ind_dom);

            extend_nonlinearity_domain_unary(arg, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case sin_op:                              /* sine operation    sin_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res]  = sin(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case cos_op:                            /* cosine operation    cos_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res]  = cos(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case atan_op:                                              /* atan_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res]=atan(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case asin_op:                                              /* asin_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = asin(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case acos_op:                                              /* acos_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = acos(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

#ifdef ATRIG_ERF

            /*--------------------------------------------------------------------------*/
        case asinh_op:                                            /* asinh_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = asinh(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case acosh_op:                                           /* acosh_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = acosh(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case atanh_op:                                            /* atanh_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = atanh(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            extend_nonlinearity_domain_unary(arg1, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case erf_op:                                                /* erf_op */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            T0[res] = erf(T0[arg1]);

            copy_index_domain(res, arg1, ind_dom);

            break;

#endif

            /*--------------------------------------------------------------------------*/
        case log_op:                                                /* log_op */
            arg = get_locint_f();
            res = get_locint_f();

            T0[res] = log(T0[arg]);

            copy_index_domain(res, arg, ind_dom);

            extend_nonlinearity_domain_unary(arg, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case pow_op:                                                /* pow_op */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            T0[res] = pow(T0[arg], coval);

            copy_index_domain(res, arg, ind_dom);

            extend_nonlinearity_domain_unary(arg, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case sqrt_op:                                              /* sqrt_op */
            arg = get_locint_f();
            res = get_locint_f();


            T0[res] = sqrt(T0[arg]);

            copy_index_domain(res, arg, ind_dom);

            extend_nonlinearity_domain_unary(arg, ind_dom, nonl_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case gen_quad:                                            /* gen_quad */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            res  = get_locint_f();

            fprintf(DIAG_OUT,
                    "ADOL-C Warning: forward sweep aborted; sparse mode not available for gen_quad!\n");
            end_sweep();
            return -2;

            break;

            /*--------------------------------------------------------------------------*/
        case min_op:                                                /* min_op */
            arg1  = get_locint_f();
            arg2  = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            /* olvo 980923 changed order to allow x = min(x,y) etc. */

            /* olvo/mitev 980721 return value (taken from below) */
            if (T0[arg1] > T0[arg2]) {
                if (coval)
                    MINDEC(ret_c,2);
            } else
                if (T0[arg1] < T0[arg2]) {
                    if (!coval)
                        MINDEC(ret_c,2);
                } else
                    if (arg1 != arg2)
                        MINDEC(ret_c,1);

            T0[res] = FMIN_ADOLC(T0[arg1], T0[arg2]);

            if (option == 0)
                combine_2_index_domains(res, arg1, arg2, ind_dom);
            else {
                if (T0[arg1] < T0[arg2])
                    copy_index_domain(res, arg1, ind_dom);
                else {
                    if (T0[arg1] > T0[arg2])
                        copy_index_domain(res, arg2, ind_dom);
                    else
                        combine_2_index_domains(res, arg1, arg2, ind_dom);
                }

            }

            break;

            /*--------------------------------------------------------------------------*/
        case abs_val:                                              /* abs_val */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            /* olvo 980923 changed order to allow x = min(x,y) etc. */

            /* olvo/mitev 980721 ec n3l (taken from below) */
            if (T0[arg] < 0.0) {
                if (coval)
                    MINDEC(ret_c,2);
            } else
                if (T0[arg] > 0.0) {
                    if (!coval)
                        MINDEC(ret_c,2);
                }

            T0[res] = fabs(T0[arg]);

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case ceil_op:                                              /* ceil_op */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            T0[res]=ceil(T0[arg]);
            /* olvo/mitev 980721 ec n2l (taken from below) */
            if (coval != T0[res])
                MINDEC(ret_c,2);

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case floor_op:                 /* Compute ceil of adouble    floor_op */
            arg   = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            T0[res] = floor(T0[arg]);
            /* olvo/mitev 980721 ec n2l (taken from below) */
            if (coval != T0[res])
                MINDEC(ret_c,2);

            copy_index_domain(res, arg, ind_dom);

            break;


            /****************************************************************************/
            /*                                                             CONDITIONALS */

            /*--------------------------------------------------------------------------*/
        case cond_assign:                                      /* cond_assign */
            arg   = get_locint_f();
            arg1  = get_locint_f();
            arg2  = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            /* olvo 980924 changed order to allow reflexive ops */

            if (T0[arg] > 0) {
                if (coval <= 0.0)
                    MINDEC(ret_c,2);
                T0[res] = T0[arg1];

                if (option == 0)
                    combine_2_index_domains(res, arg1, arg2, ind_dom);
                else
                    copy_index_domain(res, arg1, ind_dom);

            } else {
                if (coval > 0.0)
                    MINDEC(ret_c,2);
                if (T0[arg] == 0)
                    MINDEC(ret_c,0);
                T0[res] = T0[arg2];

                if (option == 0)
                    combine_2_index_domains(res, arg1, arg2, ind_dom);
                else
                    copy_index_domain(res, arg1, ind_dom);

            }
            break;

            /*--------------------------------------------------------------------------*/
        case cond_assign_s:                                  /* cond_assign_s */
            arg   = get_locint_f();
            arg1  = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();


            if (T0[arg] > 0) {
                if (coval <= 0.0)
                    MINDEC(ret_c,2);
                T0[res] = T0[arg1];

                copy_index_domain(res, arg1, ind_dom);

            } else {
                if (T0[arg] == 0)
                    MINDEC(ret_c,0);
            }
            break;


            /****************************************************************************/
            /*                                                       VECTOR ASSIGNMENTS */

            /*--------------------------------------------------------------------------*/
        case assign_av:                                          /* assign_av */
            arg  = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for assign_a */

                T0[res] = T0[arg];

                copy_index_domain(res, arg, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case assign_dv:                                          /* assign_dv */
            size  = get_locint_f();
            res   = get_locint_f();
            d     = get_val_v_f(size);

            for (ls=0; ls<size; ls++) { /* code for assign_d */

                T0[res] = *d++;

                delete_index_domain(res, ind_dom);

                res++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case assign_indvec:                                  /* assign_indvec */
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for assign_ind */

                T0[res] = basepoint[indexi];

                temp_res = ind_dom[res];
                while (temp_res != NULL) {
                    temp = temp_res->next;
                    free((IndexElement*) temp_res);
                    temp_res = temp;
                }

                ind_dom[res] = (struct IndexElement*) malloc(sizeof(struct IndexElement));
                ind_dom[res]->entry = indexi;
                ind_dom[res]->next = NULL;

                ++indexi;
                res++;
            }
            break;


            /****************************************************************************/
            /*                                            VECTOR OPERATION + ASSIGNMENT */

            /*--------------------------------------------------------------------------*/
        case eq_plus_av:                                        /* eq_plus_av */
            arg  = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for eq_plus_a */

                T0[res] += T0[arg];

                merge_2_index_domains(res, arg, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case eq_min_av:                                          /* eq_min_av */
            arg  = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for eq_min_a */

                T0[res] -= T0[arg];

                merge_2_index_domains(res, arg, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case eq_mult_av_d:                                    /* eq_mult_av_d */
            size  = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            for (ls=0; ls<size; ls++) { /* code for eq_mult_d*/

                T0[res] *= coval;

                merge_2_index_domains(res, arg, ind_dom);

                res++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case eq_mult_av_a:                                    /* eq_mult_av_a */
            arg  = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            /* olvo 980929 new strategy to check for overwrites
               (changes computation order) */
            checkSize = res+size;

            for (ls=0; ls<size; ls++) {
                if (res == arg) /* skip res==arg first */
                    res++;
                if (res == checkSize) /* checks if arg==res was skipped */
                    res = arg;

                T0[res] *= T0[arg];

                merge_2_index_domains(res, arg, ind_dom);

                extend_nonlinearity_domain_binary(res, arg, ind_dom, nonl_dom);

                res++;
            }
            break;


            /****************************************************************************/
            /*                                                 BINARY VECTOR OPERATIONS */

            /*--------------------------------------------------------------------------*/
        case plus_av_av:                                        /* plus_av_av */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for plus_a_a */

                T0[res] = T0[arg1] + T0[arg2];

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                res++;
                arg1++;
                arg2++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case sub_av_av:                                          /* sub_av_av */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            for (ls=0; ls<size; ls++) { /* code for min_a_a */

                T0[res] = T0[arg1] - T0[arg2];

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                res++;
                arg1++;
                arg2++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case dot_av_av:                                          /* dot_av_av */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();


            T0res = 0.0;
            /* olvo 980924 check for overwrites -- if necessary use
               tempories for res-stuff to allow reflexive ops */

            for (ls=0; ls<size; ls++) { /* code for mult_a_a  */
                T0res += T0[arg1] * T0[arg2];

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

                arg1++;
                arg2++;
            }

            /* copy results if necessary */
            T0[res] = T0res;
            break;

            /*--------------------------------------------------------------------------*/
        case mult_a_av:                                          /* mult_a_av */
            arg1 = get_locint_f();
            arg2 = get_locint_f();
            size = get_locint_f();
            res  = get_locint_f();

            /* olvo 980929 new strategy to check for overwrites
               (changes computation order) */
            checkSize = res+size;

            for (ls=0; ls<size; ls++) {
                if (res == arg2) /* skip res==arg2 first */
                { res++;
                    arg1++;
                }
                if (res == checkSize) /* checks if arg2==res was skipped */
                { arg1 -= res-arg2;
                    res = arg2;
                }


                T0[res] = T0[arg1] * T0[arg2];

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

                res++;
                arg1++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case mult_d_av:                                          /* mult_d_av */
            arg   = get_locint_f();
            size  = get_locint_f();
            res   = get_locint_f();
            coval = get_val_f();

            for (ls=0; ls<size; ls++) { /* code for mult_d_a */

                T0[res] = T0[arg] * coval;

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case div_av_a:                                            /* div_av_a */
            arg1   = get_locint_f();
            arg2   = get_locint_f();
            size   = get_locint_f();
            res    = get_locint_f();


            /* olvo 980929 new strategy to check for overwrites
               (changes computation order) */
            checkSize = res+size;

            for (ls=0; ls<size; ls++) {
                if (res == arg2) /* skip res==arg2 first */
                { res++;
                    arg1++;
                }
                if (res == checkSize) /* checks if arg2==res was skipped */
                { arg1 -= res-arg2;
                    res = arg2;
                }


                T0[res] = T0[arg1] / T0[arg2];

                combine_2_index_domains(res, arg1, arg2, ind_dom);

                extend_nonlinearity_domain_binary(arg1, arg2, ind_dom, nonl_dom);

                extend_nonlinearity_domain_unary(arg2, ind_dom, nonl_dom);

                res++;
                arg1++;
            }
            break;


            /****************************************************************************/
            /*                                                               SUBSCRIPTS */

            /*--------------------------------------------------------------------------*/
        case subscript:                                          /* subscript */
            arg2 = get_locint_f(); /* Base */
            arg1 = get_locint_f(); /* pointer to variable containing offset */
            res  = get_locint_f();

            arg  = arg2 + (int)(T0[arg1]);

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);

            T0[res] = T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case subscript_l:                                      /* subscript_l */
            arg2 = get_locint_f();  /* Base */
            arg1 = get_locint_f();
            arg  = get_locint_f();

            res  = arg2 + (int)(T0[arg1]);

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);


            T0[res] = T0[arg];

            copy_index_domain(res, arg, ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case subscript_ld:                                    /* subscript_ld */
            arg2 = get_locint_f(); /* Base */
            arg1 = get_locint_f(); /* pointer to variable containing offset */

            arg  = arg2+(int)(T0[arg1]);


            T0[arg] = get_val_f();

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);

            delete_index_domain(arg,ind_dom);

            break;

            /*--------------------------------------------------------------------------*/
        case m_subscript:                                      /* m_subscript */
            arg2 = get_locint_f(); /* Base */
            arg1 = get_locint_f(); /* pointer to variable containing offset */
            size = get_locint_f();
            res  = get_locint_f();

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);

            arg = arg2 + (int)(T0[arg1])*size;
            for (ls=0; ls<size; ls++) { /* olvo 980721 new nl */

                T0[res] = T0[arg];

                copy_index_domain(res, arg, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case m_subscript_l:                                  /* m_subscript_l */
            arg2 = get_locint_f();  /* Base LHS */
            arg1 = get_locint_f();  /* Offset LHS */
            size = get_locint_f();
            arg  = get_locint_f();  /* RHS */

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);

            res = arg2 + (int)(T0[arg1])*size;
            for (ls=0; ls<size; ls++) {
                T0[res] = T0[arg];

                copy_index_domain(res, arg, ind_dom);

                res++;
                arg++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case m_subscript_ld:                                /* m_subscript_ld */
            arg2 = get_locint_f(); /* Base */
            arg1 = get_locint_f(); /* pointer to variable containing offset */
            arg  = get_locint_f(); /* offset in the vector itself */
            size = get_locint_f();

            if ( (int)(T0[arg1]) != (int)(get_val_f()) )
                MINDEC(ret_c,2);

            d      = get_val_v_f(size);

            res = arg2 + (int)(T0[arg1])*size + arg;
            for (ls=0; ls<size; ls++) {

                T0[res] = d[l];

                delete_index_domain(res,ind_dom);

                res++;
            }
            break;


            /****************************************************************************/
            /*                                                          REMAINING STUFF */

            /*--------------------------------------------------------------------------*/
        case take_stock_op:                                  /* take_stock_op */
            size = get_locint_f();
            res  = get_locint_f();
            d    = get_val_v_f(size);

            for (ls=0;ls<size;ls++) {
                T0[res]=*d;
                res++;
                d++;
            }
            break;

            /*--------------------------------------------------------------------------*/
        case death_not:                                          /* death_not */
            arg1=get_locint_f();
            arg2=get_locint_f();

            break;

            /*--------------------------------------------------------------------------*/
        default:                                                   /* default */
            /* Die here, we screwed up */

            fprintf(DIAG_OUT,"ADOL-C fatal error in hess_pat: no such operation %d\n", operation);
            exit(-1);
            break;

        } /* endswitch */


        /* Read the next operation */
        operation=get_op_f();

    }  /* endwhile */


    end_sweep();


    for(i=0;i<ind_cnt;i++) {
        if (nonl_dom[i] != NULL) {
            crs[i] = (unsigned int*) malloc(sizeof(unsigned int) * (nonl_dom[i]->entry+1));
            temp = nonl_dom[i]->next;
            crs[i][0] = nonl_dom[i]->entry;
            for(l=1;l<=crs[i][0];l++) {
                crs[i][l] = temp->entry;
                temp = temp->next;
            }
        } else {
            crs[i] = (unsigned int *) malloc(sizeof(unsigned int));
            crs[i][0] = 0;
        }
    }

    return ret_c;
}

/*******************************************************************************/
END_C_DECLS
