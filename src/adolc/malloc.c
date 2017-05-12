/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     malloc.c
 Revision: $Id: malloc.c 134 2009-03-03 14:25:24Z imosqueira $
 Contents: malloc replacements for not gnu compatible malloc system functions
 
 opyright (c) 2005
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
History:
         20050617 kowarz: initial version
 
----------------------------------------------------------------------------*/

#include "malloc.h"

#undef malloc
#undef realloc
#undef calloc

void *malloc();
void *calloc();
void *realloc();

/* Allocate an n-byte block from heap, n>=1 */

void *rpl_malloc(size_t n) {
    if (n==0) n=1;
    return malloc(n);
}

void *rpl_calloc(size_t n, size_t size) {
    if (n==0) n=1;
    if (size==0) size=1;
    return calloc(n, size);
}

void *rpl_realloc(void *ptr, size_t size) {
    if (size==0) size=1;
    if (ptr==NULL) ptr=rpl_malloc(1);
    return realloc(ptr, size);
}

