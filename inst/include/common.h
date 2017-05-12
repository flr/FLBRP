/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     common.h
 Revision: $Id: common.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: Common (global) ADOL-C header  
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
----------------------------------------------------------------------------*/

#if !defined(ADOLC_COMMON_H)
#define ADOLC_COMMON_H 1

/*--------------------------------------------------------------------------*/
/* system dependend configuration */
#if HAVE_CONFIG_H
#  include "config.h"
#endif

/*--------------------------------------------------------------------------*/
/* developer and user parameters */
#include "dvlparms.h"
#include "usrparms.h"

/*--------------------------------------------------------------------------*/
/* standard includes */
#include <stdlib.h>
#include <stdio.h>

/*--------------------------------------------------------------------------*/
/* malloc/calloc/ralloc replacments */
#if !defined(HAVE_MALLOC)
#  include "malloc.h"
#  define malloc rpl_malloc
#  define calloc rpl_calloc
#endif
#if !defined(HAVE_REALLOC)
#  include "malloc.h"
#  define realloc rpl_realloc
#endif

/*--------------------------------------------------------------------------*/
/* windows dll exports/imports */
#if defined(ADOLC_DLL)
#	define ADOLC_DLL_EXPORT __declspec(dllexport)
#else
#	define ADOLC_DLL_EXPORT
#endif

/*--------------------------------------------------------------------------*/
/* further helpful macros */
#if defined(__cplusplus)
#  define BEGIN_C_DECLS extern "C" {
#  define END_C_DECLS   }
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif

#define MAXDEC(a,b) if ((a) < (b)) (a) = (b)
#define MINDEC(a,b) if ((a) > (b)) (a) = (b)

#define MAX_ADOLC(a,b) ( (a)<(b)? (b):(a) )
#define MIN_ADOLC(a,b) ( (a)>(b)? (b):(a) )

/*--------------------------------------------------------------------------*/
#endif

