/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     fortutils.h
 Revision: $Id: fortutils.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: Internal tools to handle Fortran arrays
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20040423 kowarz: adapted to configure - make - make install
          19981130 olvo:   newly created from driversc.c
 
----------------------------------------------------------------------------*/

#if !defined(ADOLC_FORTUTILS_H)
#define ADOLC_FORTUTILS_H 1

#include "common.h"

/****************************************************************************/
/*                                                         Now the C THINGS */
BEGIN_C_DECLS

ADOLC_DLL_EXPORT void spread1(int m, fdouble* x, double* X);
ADOLC_DLL_EXPORT void pack1(int m, double* X, fdouble* x);

ADOLC_DLL_EXPORT void spread2(int m, int n, fdouble* x, double** X);
ADOLC_DLL_EXPORT void pack2(int m, int n, double** X, fdouble* x);

ADOLC_DLL_EXPORT void spread3(int m, int n, int p, fdouble* x, double*** X);
ADOLC_DLL_EXPORT void pack3(int m, int n, int p, double*** X, fdouble* x);

END_C_DECLS

/****************************************************************************/
#endif
