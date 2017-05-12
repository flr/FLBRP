#if !defined(ADOLC_ADOUBLE_P_H)
#define ADOLC_ADOUBLE_P_H 1
/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     adouble_p.h
 Revision: $Id: adouble_p.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: adouble_p.h contains the private funtions from the active conext
           necessary to compile ADOL-C but not intended to be called by ADOL-C
           users.
 
 Copyright (c) 2005
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20050218 kowarz: newly created
 
----------------------------------------------------------------------------*/

#include "common.h"

/****************************************************************************/
/*                                                         THIS FILE IS C++ */
#ifdef __cplusplus

ADOLC_DLL_EXPORT locint next_loc();
ADOLC_DLL_EXPORT locint next_loc( int size );

ADOLC_DLL_EXPORT void free_loc( locint old_loc, int size );

/****************************************************************************/
/*                                                         STOCK OPERATIONS */
ADOLC_DLL_EXPORT void take_stock();
ADOLC_DLL_EXPORT locint keep_stock();

#endif  // __cplusplus
/*****************************************************************************/

#endif  // ADOLC_ADOUBLE_P_H
