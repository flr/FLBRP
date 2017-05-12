/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     adouble.h
 Revision: $Id: adouble.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: adouble.h contains the basis for the class of adouble
           included here are all the possible functions defined on
           the adouble class.  Notice that, as opposed to ealier versions,
           both the class adub and the class adouble are derived from a base
           class (badouble).  See below for further explanation.
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20041110 kowarz: tapeless (scalar/vector) forward version added
          20040423 kowarz: adapted to configure - make - make install
          20000107 olvo:   iostream.h instaed of stream.h  
          19991210 olvo:   checking the changes
          19991122 olvo:   new op_codes eq_plus_prod eq_min_prod
                           for  y += x1 * x2
                           and  y -= x1 * x2  
          19981201 olvo:   last check: 
                           - taputil things changed, includes 
          19980820 olvo:   new comparison strategy & some inlines
          19980709 olvo:   modified sign operators
 
----------------------------------------------------------------------------*/

/****************************************************************************/
/*
  NOTICE that the purpose of the class adub is merely to avoid the 
  generation and recording of an extra return adouble for each elementary 
  operation and function call. The same result can be achieved much
  more elegantly with GNUs named return variables, which would also 
  achieve the desired last in first out pattern for adouble construction 
  and destruction.
*/
/****************************************************************************/

#if !defined(ADOLC_ADOUBLE_H)
#define ADOLC_ADOUBLE_H 1

/****************************************************************************/
/*                                                         THIS FILE IS C++ */
#ifdef __cplusplus

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <cmath>
using std::cout;
using std::cin;
using std::cerr;
using std::ostream;
using std::istream;

#include "common.h"
#include "taputil.h"

/* NOTICE: There are automatic includes at the end of this file! */

#undef TAPELESS
#undef SAFE
#if defined(ADOLC_TAPELESS)
#  define TAPELESS
#  undef SAFE
#endif

#if defined(SAFE_ADOLC_TAPELESS)
#  define TAPELESS
#  define SAFE
#endif

#if !defined(TAPELESS)

/****************************************************************************/
/*                                             FORWARD DECLARATIONS (TAPES) */

/*--------------------------------------------------------------------------*/
class adouble;
class adub;
class badouble;
class badoublev;
class adoublev;
class adubv;
class along;
/* class doublev;  that's history */

/*--------------------------------------------------------------------------*/
void ADOLC_DLL_EXPORT condassign( double &res, const double &cond,
                                  const double &arg1, const double &arg2 );
void ADOLC_DLL_EXPORT condassign( double &res, const double &cond,
                                  const double &arg );

double ADOLC_DLL_EXPORT fmin( const double &x, const double &y );
double ADOLC_DLL_EXPORT fmax( const double &x, const double &y );


/****************************************************************************/
/*                                                           CLASS BADOUBLE */

/*
   The class badouble contains the basic definitions for 
   the arithmetic operations, comparisons, etc. 
   This is a basic class from which the adub and adouble are 
   derived.  Notice that the constructors/destructors for 
   the class badouble are of the trivial variety.  This is the
   main difference among badoubles, adubs, and adoubles.
*/
class ADOLC_DLL_EXPORT badouble {
    friend ADOLC_DLL_EXPORT class badoublev;
protected:
    locint location;
    badouble( void ) {};
    // must be public when using gcc >= 3.4 ( problems with value() )
    // (see GCC 3.4 Release Series - Changes, New Features, and Fixes)
    //
    // badouble( const badouble& a ) {location = a.location;};
    badouble( locint lo ) {
        location = lo;
    };

public:
    /*--------------------------------------------------------------------------*/
    badouble( const badouble& a ) {
        location = a.location;
    }
    ;           /* ctor */

    inline locint loc( void ) const;                         /* Helpful stuff */

    /*------------------------------------------------------------------------*/
    badouble& operator >>= ( double& );                        /* Assignments */
    badouble& operator <<= ( double );
    badouble& operator = ( double );
    badouble& operator = ( const badouble& );
    badouble& operator = ( const adub& );
    double value() const;
    /* badouble& operator = ( const adouble& );
       !!! olvo 991210: was the same as badouble-assignment */

    /*--------------------------------------------------------------------------*/
    friend ADOLC_DLL_EXPORT std::ostream& operator << ( std::ostream&, const badouble& );  /* IO friends */
    friend ADOLC_DLL_EXPORT std::istream& operator >> ( std::istream&, const badouble& );

    /*------------------------------------------------------------------------*/
    badouble& operator += ( double );               /* Operation + Assignment */
    badouble& operator += ( const badouble& );
    badouble& operator -= ( double y );
    badouble& operator -= ( const badouble& );
    badouble& operator *= ( double );
    badouble& operator *= ( const badouble& );
    badouble& operator /= ( double );
    badouble& operator /= ( const badouble& );
    /* olvo 991122 n2l: new special op_codes */
    badouble& operator += ( const adub& );
    badouble& operator -= ( const adub& );

    /*--------------------------------------------------------------------------*/
    /* Comparison (friends) */
    inline friend int operator != ( const badouble&, const badouble& );
    inline friend int operator != ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator != ( const badouble&, double );
    inline friend int operator == ( const badouble&, const badouble& );
    inline friend int operator == ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator == ( const badouble&, double );
    inline friend int operator <= ( const badouble&, const badouble& );
    inline friend int operator <= ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator <= ( const badouble&, double );
    inline friend int operator >= ( const badouble&, const badouble& );
    inline friend int operator >= ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator >= ( const badouble&, double );
    inline friend int operator >  ( const badouble&, const badouble& );
    inline friend int operator >  ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator >  ( const badouble&, double );
    inline friend int operator <  ( const badouble&, const badouble& );
    inline friend int operator <  ( double, const badouble& );
    friend ADOLC_DLL_EXPORT int operator <  ( const badouble&, double );


    /*--------------------------------------------------------------------------*/
    /* sign operators (friends) */
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble& x );
    friend ADOLC_DLL_EXPORT adub operator - ( const badouble& x );

    /*--------------------------------------------------------------------------*/
    /* binary operators (friends) */
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator + ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator - ( const badouble&, const badouble& );
    inline friend adub operator - ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator - ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator * ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator * ( double, const badouble& );
    inline friend adub operator * ( const badouble&, double );
    inline friend adub operator / ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator / ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator / ( double, const badouble& );

    /*--------------------------------------------------------------------------*/
    /* unary operators (friends) */
    friend ADOLC_DLL_EXPORT adub exp  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub log  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub sqrt ( const badouble& );
    friend ADOLC_DLL_EXPORT adub sin  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub cos  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub tan  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub asin ( const badouble& );
    friend ADOLC_DLL_EXPORT adub acos ( const badouble& );
    friend ADOLC_DLL_EXPORT adub atan ( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* special operators (friends) */
    friend ADOLC_DLL_EXPORT adouble atan2 ( const badouble&, const badouble& );
    /* no internal use of condassign: */
    friend ADOLC_DLL_EXPORT adub    pow   ( const badouble&, double );
    /* uses condassign internally */
    friend ADOLC_DLL_EXPORT adouble pow   ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adouble pow   ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub    log10 ( const badouble& );
    /* User defined version of logarithm to test extend_quad macro */
    friend ADOLC_DLL_EXPORT adouble myquad( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* Additional ANSI C standard Math functions Added by DWJ on 8/6/90 */
    friend ADOLC_DLL_EXPORT adub sinh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub cosh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub tanh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub asinh ( const badouble& );
    friend ADOLC_DLL_EXPORT adub acosh ( const badouble& );
    friend ADOLC_DLL_EXPORT adub atanh ( const badouble& );

    friend ADOLC_DLL_EXPORT adub fabs  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub ceil  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub floor ( const badouble& );

    friend ADOLC_DLL_EXPORT adub fmax ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmax ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmax ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub fmin ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmin ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmin ( const badouble&, double );

    friend ADOLC_DLL_EXPORT adub ldexp ( const badouble&, int );
    friend ADOLC_DLL_EXPORT adub frexp ( const badouble&, int* );
    friend ADOLC_DLL_EXPORT adub erf   ( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* Conditionals */
    friend ADOLC_DLL_EXPORT void condassign( adouble &res, const adouble &cond,
            const adouble &arg1, const adouble &arg2 );
    friend ADOLC_DLL_EXPORT void condassign( adouble &res, const adouble &cond,
            const adouble &arg );
    friend ADOLC_DLL_EXPORT void condassign( along &res, const adouble &cond,
            const adouble &arg1, const adouble &arg2 );
    friend ADOLC_DLL_EXPORT void condassign( along &res, const adouble &cond,
            const adouble &arg );
};



/****************************************************************************/
/*                                                               CLASS ADUB */

/*
   The class Adub
   ---- Basically used as a temporary result.  The address for an
        adub is usually generated within an operation.  That address
        is "freed" when the adub goes out of scope (at destruction time).
   ---- operates just like a badouble, but it has a destructor defined for it.
*/

class ADOLC_DLL_EXPORT adub:public badouble {
    friend ADOLC_DLL_EXPORT class adouble;
    /* added Sep/01/96 */
    friend ADOLC_DLL_EXPORT class asub;
    friend ADOLC_DLL_EXPORT class along;
protected:
    adub( locint lo ):badouble(lo) {};
    adub( void ):badouble(0) {
        fprintf(DIAG_OUT,"ADOL-C error: illegal default construction of adub"
                " variable\n");
        exit(-2);
    };
    adub( double ):badouble(0) {
        fprintf(DIAG_OUT,"ADOL-C error: illegal  construction of adub variable"
                " from double\n");
        exit(-2);
    };

public:

    /*--------------------------------------------------------------------------*/
    /* sign operators (friends) */
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble& x );
    friend ADOLC_DLL_EXPORT adub operator - ( const badouble& x );

    /*--------------------------------------------------------------------------*/
    /* binary operators (friends) */
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator + ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator + ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator - ( const badouble&, const badouble& );
    inline friend adub operator - ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator - ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator * ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator * ( double, const badouble& );
    inline friend adub operator * ( const badouble&, double );
    inline friend adub operator / ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub operator / ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub operator / ( double, const badouble& );

    /*--------------------------------------------------------------------------*/
    /* unary operators (friends) */
    friend ADOLC_DLL_EXPORT adub exp  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub log  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub sqrt ( const badouble& );
    friend ADOLC_DLL_EXPORT adub sin  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub cos  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub tan  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub asin ( const badouble& );
    friend ADOLC_DLL_EXPORT adub acos ( const badouble& );
    friend ADOLC_DLL_EXPORT adub atan ( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* special operators (friends) */
    /* no internal use of condassign: */
    friend ADOLC_DLL_EXPORT adub    pow   ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub    log10 ( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* Additional ANSI C standard Math functions Added by DWJ on 8/6/90 */
    friend ADOLC_DLL_EXPORT adub sinh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub cosh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub tanh  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub asinh ( const badouble& );
    friend ADOLC_DLL_EXPORT adub acosh ( const badouble& );
    friend ADOLC_DLL_EXPORT adub atanh ( const badouble& );

    friend ADOLC_DLL_EXPORT adub fabs  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub ceil  ( const badouble& );
    friend ADOLC_DLL_EXPORT adub floor ( const badouble& );

    friend ADOLC_DLL_EXPORT adub fmax ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmax ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmax ( const badouble&, double );
    friend ADOLC_DLL_EXPORT adub fmin ( const badouble&, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmin ( double, const badouble& );
    friend ADOLC_DLL_EXPORT adub fmin ( const badouble&, double );

    friend ADOLC_DLL_EXPORT adub ldexp ( const badouble&, int );
    friend ADOLC_DLL_EXPORT adub frexp ( const badouble&, int* );
    friend ADOLC_DLL_EXPORT adub erf   ( const badouble& );

    /*--------------------------------------------------------------------------*/
    /* vector operations (friends) */
    friend ADOLC_DLL_EXPORT adub operator*( const badoublev&, double* );
    friend ADOLC_DLL_EXPORT adub operator*( double*, const badoublev& );
    friend ADOLC_DLL_EXPORT adub operator*( const badoublev&, const badoublev& );

#ifdef overwrite
    ~adub();
#endif
};


/****************************************************************************/
/*                                                            CLASS ADOUBLE */
/*
  The class adouble.
  ---Derived from badouble.  Contains the standard constructors/destructors.
  ---At construction, it is given a new address, and at destruction, that
     address is freed.
*/
class ADOLC_DLL_EXPORT adouble:public badouble {
    friend ADOLC_DLL_EXPORT class along;
public:
    adouble( const adub& );
    adouble( const along& );
    adouble( const adouble& );
    adouble( void );
    adouble( double );
    /* adub prevents postfix operators to occur on the left
       side of an assignment which would not work  */
    adub operator++( int );
    adub operator--( int );
    badouble& operator++( void );
    badouble& operator--( void );
    /*   inline double value(); */
#ifdef overwrite
    ~adouble();
#endif

    adouble& operator = ( double );
    adouble& operator = ( const badouble& );
    /* adouble& operator = ( const adouble& );
       !!! olvo 991210 was the same as badouble-assignment */
    adouble& operator = ( const adub& );
};

/****************************************************************************/
/*                                                               CLASS ASUB */
class ADOLC_DLL_EXPORT asub:public badouble {
    locint base,
    offset;
public:
    asub( locint start, locint index );
#ifdef overwrite
    ~asub();
#endif
    asub& operator <<= ( double );
    asub& operator =   ( double );
    /* asub& operator =   ( const adub& );
       !!! olvo 991210 is the same as normal assignment */
    asub& operator =   ( const badouble& );
    /* added Sep/01/96 */
    /* olvo 991210 seems to be a provisional version */
    asub& operator =   ( const asub& );
    asub& operator +=  ( double );
    asub& operator +=  ( const badouble& );
    asub& operator -=  ( double x );
    asub& operator -=  ( const badouble& );
    asub& operator *=  ( double x );
    asub& operator *=  ( const badouble& );
    asub& operator /=  ( double x );
    asub& operator /=  ( const badouble& );
    /* adub prevents postfix operators to occur on the left
       side of an assignment which would not work  */
    adub operator++( int );
    adub operator--( int );
    asub& operator++( void );
    asub& operator--( void );
};


/****************************************************************************/
/*                                                              CLASS ALONG */
/* The class along was originally designed for the sole purpose of
   allowing subscripting operations with computable ON THE TAPE.
   The current definition refers to badouble, i.e. it is a 
   floating point class. The current constuction allows to 
   use alongs in the same way as adoubles. Especially adubs can 
   can be used as subscripts, i.e. the results of "active computations".
   This useful because people want to compute the indices on the tape.
   Notice, that along does NOT perform integer arithmetic. 
   This is the major disadvantage of the current along. */
class ADOLC_DLL_EXPORT along:public badouble {
    friend ADOLC_DLL_EXPORT class adouble;
public:
    along( const adub& );
    along( const along& );
    along( void );
    along( int );
#ifdef overwrite
    ~along();
#endif
    along& operator = ( int );
    /* along& operator = ( const adouble& );
       !!! olvo 991210 is the same as badouble-assignment */
    along& operator = ( const badouble& );
    along& operator = ( const along& );
    along& operator = ( const adub& );
    /* adub prevents postfix operators to occur on the left
       side of an assignment which would not work  */
    adub operator++( int );
    adub operator--( int );
    along&  operator++( void );
    along&  operator--( void );
};


/****************************************************************************/
/*                                                       INLINE DEFINITIONS */

/*--------------------------------------------------------------------------*/
inline locint badouble::loc( void ) const {
    return location;
}

/*--------------------------------------------------------------------------*/
/* Comparison */
inline int operator != ( const badouble& u, const badouble& v ) {
    return (u-v != 0);
}

inline int operator != ( double coval, const badouble& v) {
    if (coval)
        return (-coval+v != 0);
    else
        return (v != 0);
}

inline int operator == ( const badouble& u, const badouble& v ) {
    return (u-v == 0);
}

inline int operator == ( double coval, const badouble& v) {
    if (coval)
        return (-coval+v == 0);
    else
        return (v == 0);
}

inline int operator <= ( const badouble& u, const badouble& v ) {
    return (u-v <= 0);
}

inline int operator <= ( double coval, const badouble& v ) {
    if (coval)
        return (-coval+v >= 0);
    else
        return (v >= 0);
}

inline int operator >= ( const badouble& u, const badouble& v ) {
    return (u-v >= 0);
}

inline int operator >= ( double coval, const badouble& v ) {
    if (coval)
        return (-coval+v <= 0);
    else
        return (v <= 0);
}

inline int operator > ( const badouble& u, const badouble& v ) {
    return (u-v > 0);
}

inline int operator > ( double coval, const badouble& v ) {
    if (coval)
        return (-coval+v < 0);
    else
        return (v < 0);
}

inline int operator < ( const badouble& u, const badouble& v ) {
    return (u-v < 0);
}

inline int operator < ( double coval, const badouble& v ) {
    if (coval)
        return (-coval+v > 0);
    else
        return (v > 0);
}

/*--------------------------------------------------------------------------*/
/* Subtract a floating point from an adouble  */
inline adub operator - ( const badouble& x , double coval ) {
    return (-coval) + x;
}

/*--------------------------------------------------------------------------*/
/* Multiply an adouble by a floating point */
inline adub operator * (const badouble& x, double coval) {
    return coval * x;
}

/*--------------------------------------------------------------------------*/
/* Divide an adouble by a floating point */
inline adub operator / (const badouble& x, double coval) {
    return (1.0/coval) * x;
}

/****************************************************************************/
/*                                                        AUTOMTIC INCLUDES */
#include "avector.h" /* active vector classes */

/****************************************************************************/
/* tapeless implementation                                                  */
/****************************************************************************/
#else

#if defined(NUMBER_DIRECTIONS)
static unsigned int numdir=NUMBER_DIRECTIONS;
#  define ADVAL                adval[NUMBER_DIRECTIONS]
#  define ADVAL_TYPE           const double *
#  define FOR_I_EQ_0_LT_NUMDIR for (unsigned int _i=0; _i<numdir; ++_i)
#  define ADVAL_I              adval[_i]
#  define ADV_I                adv[_i]
#  define V_I                  v[_i]
#else
#  define ADVAL                adval
#  define ADVAL_TYPE           double
#  define FOR_I_EQ_0_LT_NUMDIR
#  define ADVAL_I              adval
#  define ADV_I                adv
#  define V_I                  v
#endif

namespace adtl {

#if defined(NUMBER_DIRECTIONS)
void setNumDir(const unsigned int p) {
    if (p>NUMBER_DIRECTIONS) numdir=NUMBER_DIRECTIONS;
    else numdir=p;
}
#endif

inline double fmin( const double &x, const double &y ) {
    if (x<y) return x;
    else return y;
}

inline double fmax( const double &x, const double &y ) {
    if (x>y) return x;
    else return y;
}

inline const double makeNaN() {
#if defined(non_num)
    double a,b;
    a=non_num;
    b=non_den;
    return a/b;
#else
#  error Error: non_num undefined!
#endif
}

class adouble {
public:
    // ctors
    inline adouble() {}
    inline adouble(const double v);
    inline adouble(const double v, ADVAL_TYPE adv);
    inline adouble(const adouble& a);

    /*******************  temporary results  ******************************/
    // sign
    inline const adouble operator - () const;
    inline const adouble operator + () const;

    // addition
    inline const adouble operator + (const double v) const;
    inline const adouble operator + (const adouble& a) const;
    inline friend
    const adouble operator + (const double v, const adouble& a);

    // substraction
    inline const adouble operator - (const double v) const;
    inline const adouble operator - (const adouble& a) const;
    inline friend
    const adouble operator - (const double v, const adouble& a);

    // multiplication
    inline const adouble operator * (const double v) const;
    inline const adouble operator * (const adouble& a) const;
    inline friend
    const adouble operator * (const double v, const adouble& a);

    // division
    inline const adouble operator / (const double v) const;
    inline const adouble operator / (const adouble& a) const;
    inline friend
    const adouble operator / (const double v, const adouble& a);

    // inc/dec
    inline const adouble operator ++ ();
    inline const adouble operator ++ (int);
    inline const adouble operator -- ();
    inline const adouble operator -- (int);

    // functions
    inline friend const adouble tan(const adouble &a);
    inline friend const adouble exp(const adouble &a);
    inline friend const adouble log(const adouble &a);
    inline friend const adouble sqrt(const adouble &a);
    inline friend const adouble sin(const adouble &a);
    inline friend const adouble cos(const adouble &a);
    inline friend const adouble asin(const adouble &a);
    inline friend const adouble acos(const adouble &a);
    inline friend const adouble atan(const adouble &a);

    inline friend const adouble atan2(const adouble &a, const adouble &b);
    inline friend const adouble pow(const adouble &a, double v);
    inline friend const adouble pow(const adouble &a, const adouble &b);
    inline friend const adouble pow(double v, const adouble &a);
    inline friend const adouble log10(const adouble &a);

    inline friend const adouble sinh (const adouble &a);
    inline friend const adouble cosh (const adouble &a);
    inline friend const adouble tanh (const adouble &a);
#if defined(ATRIG_ERF)
    inline friend const adouble asinh (const adouble &a);
    inline friend const adouble acosh (const adouble &a);
    inline friend const adouble atanh (const adouble &a);
#endif
    inline friend const adouble fabs (const adouble &a);
    inline friend const adouble ceil (const adouble &a);
    inline friend const adouble floor (const adouble &a);
    inline friend const adouble fmax (const adouble &a, const adouble &b);
    inline friend const adouble fmax (double v, const adouble &a);
    inline friend const adouble fmax (const adouble &a, double v);
    inline friend const adouble fmin (const adouble &a, const adouble &b);
    inline friend const adouble fmin (double v, const adouble &a);
    inline friend const adouble fmin (const adouble &a, double v);
    inline friend const adouble ldexp (const adouble &a, const adouble &b);
    inline friend const adouble ldexp (const adouble &a, const double v);
    inline friend const adouble ldexp (const double v, const adouble &a);
    inline friend const double frexp (const adouble &a, int* v);
#if defined(ATRIG_ERF)
    inline friend const adouble erf (const adouble &a);
#endif


    /*******************  nontemporary results  ***************************/
    // assignment
    inline void operator = (const double v);
    inline void operator = (const adouble& a);

    // addition
    inline void operator += (const double v);
    inline void operator += (const adouble& a);

    // substraction
    inline void operator -= (const double v);
    inline void operator -= (const adouble& a);

    // multiplication
    inline void operator *= (const double v);
    inline void operator *= (const adouble& a);

    // division
    inline void operator /= (const double v);
    inline void operator /= (const adouble& a);

    // not
    inline int operator ! () const;

    // comparision
    inline int operator != (const adouble&) const;
    inline int operator != (const double) const;
    inline friend int operator != (const double, const adouble&);

    inline int operator == (const adouble&) const;
    inline int operator == (const double) const;
    inline friend int operator == (const double, const adouble&);

    inline int operator <= (const adouble&) const;
    inline int operator <= (const double) const;
    inline friend int operator <= (const double, const adouble&);

    inline int operator >= (const adouble&) const;
    inline int operator >= (const double) const;
    inline friend int operator >= (const double, const adouble&);

    inline int operator >  (const adouble&) const;
    inline int operator >  (const double) const;
    inline friend int operator >  (const double, const adouble&);

    inline int operator <  (const adouble&) const;
    inline int operator <  (const double) const;
    inline friend int operator <  (const double, const adouble&);

    /*******************  getter / setter  ********************************/
    inline double getValue() const;
    inline void setValue(const double v);
    inline ADVAL_TYPE getADValue() const;
    inline void setADValue(ADVAL_TYPE v);
#if defined(NUMBER_DIRECTIONS)
    inline double getADValue(const unsigned int p) const;
    inline void setADValue(const unsigned int p, const double v);
#endif

    /*******************  i/o operations  *********************************/
    inline friend ostream& operator << ( ostream&, const adouble& );
    inline friend istream& operator >> ( istream&, adouble& );


private:
    // internal variables
    double val;
    double ADVAL;
};

/*******************************  ctors  ************************************/
adouble::adouble(const double v) : val(v) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=0.0;
}

adouble::adouble(const double v, ADVAL_TYPE adv) : val(v) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=ADV_I;
}

adouble::adouble(const adouble& a) : val(a.val) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=a.ADVAL_I;
}

/*************************  temporary results  ******************************/
// sign
const adouble adouble::operator - () const {
    adouble tmp;
    tmp.val=-val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=-ADVAL_I;
    return tmp;
}

const adouble adouble::operator + () const {
    return *this;
}

// addition
const adouble adouble::operator + (const double v) const {
    return adouble(val+v, adval);
}

const adouble adouble::operator + (const adouble& a) const {
    adouble tmp;
    tmp.val=val+a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I+a.ADVAL_I;
    return tmp;
}

const adouble operator + (const double v, const adouble& a) {
    return adouble(v+a.val, a.adval);
}

// subtraction
const adouble adouble::operator - (const double v) const {
    return adouble(val-v, adval);
}

const adouble adouble::operator - (const adouble& a) const {
    adouble tmp;
    tmp.val=val-a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I-a.ADVAL_I;
    return tmp;
}

const adouble operator - (const double v, const adouble& a) {
    adouble tmp;
    tmp.val=v-a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=-a.ADVAL_I;
    return tmp;
}

// multiplication
const adouble adouble::operator * (const double v) const {
    adouble tmp;
    tmp.val=val*v;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I*v;
    return tmp;
}

const adouble adouble::operator * (const adouble& a) const {
    adouble tmp;
    tmp.val=val*a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I*a.val+val*a.ADVAL_I;
    return tmp;
}

const adouble operator * (const double v, const adouble& a) {
    adouble tmp;
    tmp.val=v*a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=v*a.ADVAL_I;
    return tmp;
}

// division
const adouble adouble::operator / (const double v) const {
    adouble tmp;
    tmp.val=val/v;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I/v;
    return tmp;
}

const adouble adouble::operator / (const adouble& a) const {
    adouble tmp;
    tmp.val=val/a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=(ADVAL_I*a.val-val*a.ADVAL_I)/(a.val*a.val);
    return tmp;
}

const adouble operator / (const double v, const adouble& a) {
    adouble tmp;
    tmp.val=v/a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=(-v*a.ADVAL_I)/(a.val*a.val);
    return tmp;
}

// inc/dec
const adouble adouble::operator ++ () {
    ++val;
    return *this;
}

const adouble adouble::operator ++ (int) {
    adouble tmp;
    tmp.val=val++;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I;
    return tmp;
}

const adouble adouble::operator -- () {
    --val;
    return *this;
}

const adouble adouble::operator -- (int) {
    adouble tmp;
    tmp.val=val--;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=ADVAL_I;
    return tmp;
}

// functions
const adouble tan(const adouble& a) {
    adouble tmp;
    double tmp2;
    tmp.val=::tan(a.val);
    tmp2=::cos(a.val);
    tmp2*=tmp2;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble exp(const adouble &a) {
    adouble tmp;
    tmp.val=::exp(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp.val*a.ADVAL_I;
    return tmp;
}

const adouble log(const adouble &a) {
    adouble tmp;
    tmp.val=::log(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    if (a.val>0 || a.val==0 && a.ADVAL_I>=0) tmp.ADVAL_I=a.ADVAL_I/a.val;
    else tmp.ADVAL_I=makeNaN();
    return tmp;
}

const adouble sqrt(const adouble &a) {
    adouble tmp;
    tmp.val=::sqrt(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    if (a.val>0 || a.val==0 && a.ADVAL_I>=0) tmp.ADVAL_I=a.ADVAL_I/tmp.val/2;
    else tmp.ADVAL_I=makeNaN();
    return tmp;
}

const adouble sin(const adouble &a) {
    adouble tmp;
    double tmp2;
    tmp.val=::sin(a.val);
    tmp2=::cos(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I;
    return tmp;
}

const adouble cos(const adouble &a) {
    adouble tmp;
    double tmp2;
    tmp.val=::cos(a.val);
    tmp2=-::sin(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I;
    return tmp;
}

const adouble asin(const adouble &a) {
    adouble tmp;
    tmp.val=::asin(a.val);
    double tmp2=::sqrt(1-a.val*a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble acos(const adouble &a) {
    adouble tmp;
    tmp.val=::acos(a.val);
    double tmp2=-::sqrt(1-a.val*a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble atan(const adouble &a) {
    adouble tmp;
    tmp.val=::atan(a.val);
    double tmp2=1+a.val*a.val;
    tmp2=1/tmp2;
    if (tmp2!=0)
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=a.ADVAL_I*tmp2;
    else
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=0.0;
    return tmp;
}

const adouble atan2(const adouble &a, const adouble &b) {
    adouble tmp;
    tmp.val=::atan2(a.val, b.val);
    double tmp2=a.val*a.val;
    double tmp3=b.val*b.val;
    double tmp4=tmp3/(tmp2+tmp3);
    if (tmp4!=0)
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=(a.ADVAL_I*b.val-a.val*b.ADVAL_I)/tmp3*tmp4;
    else
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=0.0;
    return tmp;
}

const adouble pow(const adouble &a, double v) {
    adouble tmp;
    tmp.val=::pow(a.val, v);
    double tmp2=v*::pow(a.val, v-1);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I;
    return tmp;
}

const adouble pow(const adouble &a, const adouble &b) {
    adouble tmp;
    tmp.val=::pow(a.val, b.val);
    double tmp2=b.val*::pow(a.val, b.val-1);
    double tmp3=::log(a.val)*tmp.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I+tmp3*b.ADVAL_I;
    return tmp;
}

const adouble pow(double v, const adouble &a) {
    adouble tmp;
    tmp.val=::pow(v, a.val);
    double tmp2=tmp.val*::log(v);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I;
    return tmp;
}

const adouble log10(const adouble &a) {
    adouble tmp;
    tmp.val=::log10(a.val);
    double tmp2=::log((double)10)*a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble sinh (const adouble &a) {
    adouble tmp;
    tmp.val=::sinh(a.val);
    double tmp2=::cosh(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I*tmp2;
    return tmp;
}

const adouble cosh (const adouble &a) {
    adouble tmp;
    tmp.val=::cosh(a.val);
    double tmp2=::sinh(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I*tmp2;
    return tmp;
}

const adouble tanh (const adouble &a) {
    adouble tmp;
    tmp.val=::tanh(a.val);
    double tmp2=::cosh(a.val);
    tmp2*=tmp2;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

#if defined(ATRIG_ERF)
const adouble asinh (const adouble &a) {
    adouble tmp;
    tmp.val=::asinh(a.val);
    double tmp2=::sqrt(a.val*a.val+1);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble acosh (const adouble &a) {
    adouble tmp;
    tmp.val=::acosh(a.val);
    double tmp2=::sqrt(a.val*a.val-1);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}

const adouble atanh (const adouble &a) {
    adouble tmp;
    tmp.val=::atanh(a.val);
    double tmp2=1-a.val*a.val;
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=a.ADVAL_I/tmp2;
    return tmp;
}
#endif

const adouble fabs (const adouble &a) {
    adouble tmp;
    tmp.val=::fabs(a.val);
    int as=0;
    if (a.val>0) as=1;
    if (a.val<0) as=-1;
    if (as!=0)
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=a.ADVAL_I*as;
    else
        FOR_I_EQ_0_LT_NUMDIR {
            as=0;
            if (a.ADVAL_I>0) as=1;
            if (a.ADVAL_I<0) as=-1;
                tmp.ADVAL_I=a.ADVAL_I*as;
            }
            return tmp;
}

const adouble ceil (const adouble &a) {
    adouble tmp;
    tmp.val=::ceil(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=0.0;
    return tmp;
}

const adouble floor (const adouble &a) {
    adouble tmp;
    tmp.val=::floor(a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=0.0;
    return tmp;
}

const adouble fmax (const adouble &a, const adouble &b) {
    adouble tmp;
    double tmp2=a.val-b.val;
    if (tmp2<0) {
        tmp.val=b.val;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=b.ADVAL_I;
    } else {
        tmp.val=a.val;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=a.ADVAL_I;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I<b.ADVAL_I) tmp.ADVAL_I=b.ADVAL_I;
                else tmp.ADVAL_I=a.ADVAL_I;
                }
            }
}
return tmp;
}

const adouble fmax (double v, const adouble &a) {
    adouble tmp;
    double tmp2=v-a.val;
    if (tmp2<0) {
        tmp.val=a.val;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=a.ADVAL_I;
    } else {
        tmp.val=v;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=0.0;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I>0) tmp.ADVAL_I=a.ADVAL_I;
                else tmp.ADVAL_I=0.0;
                }
            }
}
return tmp;
}

const adouble fmax (const adouble &a, double v) {
    adouble tmp;
    double tmp2=a.val-v;
    if (tmp2<0) {
        tmp.val=v;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=0.0;
    } else {
        tmp.val=a.val;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=a.ADVAL_I;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I>0) tmp.ADVAL_I=a.ADVAL_I;
                else tmp.ADVAL_I=0.0;
                }
            }
}
return tmp;
}

const adouble fmin (const adouble &a, const adouble &b) {
    adouble tmp;
    double tmp2=a.val-b.val;
    if (tmp2<0) {
        tmp.val=a.val;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=a.ADVAL_I;
    } else {
        tmp.val=b.val;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=b.ADVAL_I;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I<b.ADVAL_I) tmp.ADVAL_I=a.ADVAL_I;
                else tmp.ADVAL_I=b.ADVAL_I;
                }
            }
}
return tmp;
}

const adouble fmin (double v, const adouble &a) {
    adouble tmp;
    double tmp2=v-a.val;
    if (tmp2<0) {
        tmp.val=v;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=0.0;
    } else {
        tmp.val=a.val;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=a.ADVAL_I;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I<0) tmp.ADVAL_I=a.ADVAL_I;
                else tmp.ADVAL_I=0.0;
                }
            }
}
return tmp;
}

const adouble fmin (const adouble &a, double v) {
    adouble tmp;
    double tmp2=a.val-v;
    if (tmp2<0) {
        tmp.val=a.val;
        FOR_I_EQ_0_LT_NUMDIR
        tmp.ADVAL_I=a.ADVAL_I;
    } else {
        tmp.val=v;
        if (tmp2>0) {
            FOR_I_EQ_0_LT_NUMDIR
            tmp.ADVAL_I=0.0;
        } else {
            FOR_I_EQ_0_LT_NUMDIR
            {
                if (a.ADVAL_I<0) tmp.ADVAL_I=a.ADVAL_I;
                else tmp.ADVAL_I=0.0;
                }
            }
}
return tmp;
}

const adouble ldexp (const adouble &a, const adouble &b) {
    return a*pow(2.,b);
}

const adouble ldexp (const adouble &a, const double v) {
    return a*::pow(2.,v);
}

const adouble ldexp (const double v, const adouble &a) {
    return v*pow(2.,a);
}

const double frexp (const adouble &a, int* v) {
    return ::frexp(a.val, v);
}

#if defined(ATRIG_ERF)
const adouble erf (const adouble &a) {
    adouble tmp;
    tmp.val=::erf(a.val);
    double tmp2=2.0/::sqrt(::acos(-1.0))*::exp(-a.val*a.val);
    FOR_I_EQ_0_LT_NUMDIR
    tmp.ADVAL_I=tmp2*a.ADVAL_I;
    return tmp;
}
#endif


/*******************  nontemporary results  *********************************/
void adouble::operator = (const double v) {
    val=v;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=0.0;
}

void adouble::operator = (const adouble& a) {
    val=a.val;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=a.ADVAL_I;
}

void adouble::operator += (const double v) {
    val+=v;
}

void adouble::operator += (const adouble& a) {
    val=val+a.val;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I+=a.ADVAL_I;
}

void adouble::operator -= (const double v) {
    val-=v;
}

void adouble::operator -= (const adouble& a) {
    val=val-a.val;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I-=a.ADVAL_I;
}

void adouble::operator *= (const double v) {
    val=val*v;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I*=v;
}

void adouble::operator *= (const adouble& a) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=ADVAL_I*a.val+val*a.ADVAL_I;
    val*=a.val;
}

void adouble::operator /= (const double v) {
    val/=v;
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I/=v;
}

void adouble::operator /= (const adouble& a) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=(ADVAL_I*a.val-val*a.ADVAL_I)/(a.val*a.val);
    val=val/a.val;
}

// not
int adouble::operator ! () const {
    return val==0.0;
}

// comparision
int adouble::operator != (const adouble &a) const {
    return val!=a.val;
}

int adouble::operator != (const double v) const {
    return val!=v;
}

int operator != (const double v, const adouble &a) {
    return v!=a.val;
}

int adouble::operator == (const adouble &a) const {
    return val==a.val;
}

int adouble::operator == (const double v) const {
    return val==v;
}

int operator == (const double v, const adouble &a) {
    return v==a.val;
}

int adouble::operator <= (const adouble &a) const {
    return val<=a.val;
}

int adouble::operator <= (const double v) const {
    return val<=v;
}

int operator <= (const double v, const adouble &a) {
    return v<=a.val;
}

int adouble::operator >= (const adouble &a) const {
    return val>=a.val;
}

int adouble::operator >= (const double v) const {
    return val>=v;
}

int operator >= (const double v, const adouble &a) {
    return v>=a.val;
}

int adouble::operator >  (const adouble &a) const {
    return val>a.val;
}

int adouble::operator >  (const double v) const {
    return val>v;
}

int operator >  (const double v, const adouble &a) {
    return v>a.val;
}

int adouble::operator <  (const adouble &a) const {
    return val<a.val;
}

int adouble::operator <  (const double v) const {
    return val<v;
}

int operator <  (const double v, const adouble &a) {
    return v<a.val;
}

/*******************  getter / setter  **************************************/
double adouble::getValue() const {
    return val;
}

void adouble::setValue(const double v) {
    val=v;
}

ADVAL_TYPE adouble::getADValue() const {
    return adval;
}

void adouble::setADValue(ADVAL_TYPE v) {
    FOR_I_EQ_0_LT_NUMDIR
    ADVAL_I=V_I;
}

#  if defined(NUMBER_DIRECTIONS)
double adouble::getADValue(const unsigned int p) const {
    if (p>=NUMBER_DIRECTIONS) {
        fprintf(DIAG_OUT, "Derivative array accessed out of bounds"\
                " while \"getADValue(...)\"!!!\n");
        exit(-1);
    }
    return adval[p];
}

void adouble::setADValue(const unsigned int p, const double v) {
    if (p>=NUMBER_DIRECTIONS) {
        fprintf(DIAG_OUT, "Derivative array accessed out of bounds"\
                " while \"setADValue(...)\"!!!\n");
        exit(-1);
    }
    adval[p]=v;
}
#  endif

/*******************  i/o operations  ***************************************/
ostream& operator << ( ostream& out, const adouble& a) {
    out << "Value: " << a.val;
#if !defined(NUMBER_DIRECTIONS)
    out << " ADValue: ";
#else
out << " ADValues (" << numdir << "): ";
#endif
    FOR_I_EQ_0_LT_NUMDIR
    out << a.ADVAL_I << " ";
    out << "(a)";
    return out;
}

istream& operator >> ( istream& in, adouble& a) {
    char c;
    do {
        in >> c;
    } while (c!=':' && !in.eof());
    in >> a.val;
#if !defined(NUMBER_DIRECTIONS)
    do in >> c;
    while (c!=':' && !in.eof());
#else
unsigned int num;
do in >> c;
while (c!='(' && !in.eof());
in >> num;
if (num>NUMBER_DIRECTIONS) {
    cout << "ADOL-C error: to many directions in input\n";
    exit(-1);
}
do in >> c;
while (c!=')' && !in.eof());
#endif
    FOR_I_EQ_0_LT_NUMDIR
    in >> a.ADVAL_I;
    do in >> c;
    while (c!=')' && !in.eof());
    return in;
}
};

/****************************************************************************/
#endif /* ADOLC_TAPELESS */

/****************************************************************************/
/*                                                                THAT'S ALL*/
#endif /* __cplusplus */
#endif /* ADOLC_ADOUBLE_H */
