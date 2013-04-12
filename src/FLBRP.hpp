/*
 * FLBRP.hpp = 
 *
 * Author : Laurie Kell <laurence.kell@cefas.co.uk> Cefas, UK
 * Last Change: Thu Apr 04, 2013 at 04:38 PM +0200
 * $Id: FLBRP.hpp 889 2011-01-17 00:56:37Z lauriekell $
 *
 */
#ifndef _INC_FLBRP
#define _INC_FLBRP

#ifdef WIN32
   #include <windows.h>
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

#include "FLCoreClasses.h"
#include <adouble.h>

#define N_SR_PARAM 3;
   
void AB2SVB(FLRConstSRR, double, double *, double *);
void SVB2AB(FLRConstSRR, double, double *, double *);

typedef enum tagFLRConstBRPTarget 
	{
   FLRConst_BRPMSY        = 1,
   FLRConst_BRPMaxYPR     = 2,
   FLRConst_BRPYield      = 3,
   FLRConst_BRPYPR        = 4,
   FLRConst_BRPSPR        = 5,
   FLRConst_BRPSSB        = 6,
   FLRConst_BRPSPRPercMax = 7,
   FLRConst_BRPF0pt1      = 8,
   FLRConst_BRPMEY        = 9,
   FLRConst_BRPYS         = 10,
   } FLRConstBRPTarget;

class FLBRP 
{
public:                                             
   FLBRP(void);
   FLBRP(SEXP,SEXP,SEXP);
  ~FLBRP(void);
      
   void Init(   SEXP);
   void Equilibrium(void);
   void hcrYield(SEXP);
   SEXP brp(SEXP);

   SEXP Return(SEXP);
   SEXP ReturnStk(SEXP);
   SEXP ReturnStockN(void);
   SEXP ReturnLandingsN(void);
   SEXP ReturnDiscardsN(void);
   SEXP ReturnCatchN(void);

   SEXP ReturnYpr(void);
   SEXP ReturnSpr(void);
   
   int minage, maxage,   minfbar,   maxfbar,  plusgrp,
       minyr,  maxyr,
       nunits, nseasons, nareas, niters;

   FLRConstSRR   *sr_model;
   FLQuant       sr_params;

   FLQuant      fbar,
                stock_n,        
                landings_n,     
                discards_n,     
                landings_sel,   
                discards_sel,   
                harvest,        
                bycatch_harvest,
                stock_wt,       
                landings_wt,    
                discards_wt,    
                bycatch_wt,     
                m,              
                mat,            
                harvest_spwn,   
                m_spwn,         
                availability,         
                cost_fix,         
                cost_var,         
                price;          

   void setSR(SEXP,SEXP);

   SEXP newBrp(      SEXP);
   void setRefpts(   SEXP);
   void calcRefpts(      );
   SEXP returnRefpts(SEXP);

protected:

   bool isFLBRP(SEXP);
   void Equilibrium(int);

   FLRConstBRPTarget TargetType;
   double            Target;
   
   double ***aRefpts;
   int       nRefpts;
   SEXP      dRefpts;
      
   double F0pt1(int);
   double FMax( int);
   double FMSY( int);
   double FMEY( int);
   double FSPRPercMax(double, int);

   double TargetSSB(double, int);
   double TargetYPR(double, int);
   double TargetYS( double, int);
   double TargetSPR(double, int);

   double QuadSearch(int);
   double QSGetNewx(double x[], double func[]);
   double QSGetFunc(double x,  int);
   void   QSBracket(double *x, int);

   double Recruits(double, int);
   double Recruits(double, int, int);

   double SPR(     double, int);
   double SPR(     double, int, int);

   double BPR(     double, int);
   double BPR(     double, int, int);

   double YPR(     double, int);
   double YPR(     double, int, int);
   adouble YPR(   adouble, int, int);
   
   double RPR(     double, int);
   double RPR(     double, int, int);
   adouble RPR(   adouble, int, int);
   
   double PPR(     double, int);
   double PPR(     double, int, int);
   adouble PPR(   adouble, int, int);
   
   double YS(      double, int);

   double    SSB( double, int);
   double ad_SSB( double, int);
   
   double    Biomass( double, int);
   double ad_Biomass( double, int);
   
   double yield(   double, int);

   double Cost(    double, int);
   double Profit(  double, int);
   double Revenue( double, int);

   double YPRGrad(double, int);
   double RPRGrad(double, int);
   double PPRGrad(double, int);

   double YieldGrad(  double FMult, int iIter);
   double RecGrad(    double FMult, int iIter);
   double SSBGrad(    double FMult, int iIter);
   double BiomassGrad(double FMult, int iIter);
   double ProfitGrad( double FMult, int iIter);
   adouble Recruits( adouble FMult, int iUnit, int iIter);
   adouble SPR(      adouble FMult, int iUnit, int iIter);
   adouble BPR(      adouble FMult, int iUnit, int iIter);
};                  

#endif /* _INC_FLBRP */
                                                  



