/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     malloc.h
 Revision: $Id: malloc.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: malloc replacements for not gnu compatible malloc system functions
 
 Copyright (c) 2005
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

#if !defined(ADOLC_MALLOC_H)
#  define ADOLC_MALLOC_H 1

#if defined(HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stddef.h>

#if defined(ADOLC_DLL)
#	define ADOLC_DLL_EXPORT __declspec(dllexport)
#else
#	define ADOLC_DLL_EXPORT
#endif

#if defined(__cplusplus)
extern "C" {
#endif

    ADOLC_DLL_EXPORT void *rpl_malloc(size_t);
    ADOLC_DLL_EXPORT void *rpl_calloc(size_t, size_t);
    ADOLC_DLL_EXPORT void *rpl_realloc(void *, size_t);

#if defined(__cplusplus)
}
#endif

#endif /* ADOLC_MALLOC_H */
