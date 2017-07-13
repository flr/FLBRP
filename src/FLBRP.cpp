double t1,t2;

/*
 * FLBRP.cpp = 
 *
 * Author : Laurie Kell <laurence.kell@cefas.co.uk> Cefas, UK
 * Last Change: Mon Dec 09, 2013 at 12:54 PM +0100
 * $Id: FLBRP.cpp 994 2011-06-03 14:30:21Z lauriekell $
 *
 */
#include <FLBRP.h>

int RP_harvest=0,                
    RP_yield  =1,                
    RP_rec    =2,                
    RP_ssb    =3,                
    RP_biomass=4,                
    RP_revenue=5,                
    RP_cost   =6,                
    RP_profit =7;               

#define QS_ITS     500     
#define QS_TOL     1e-10
#define QS_INC     1.75
                                
extern "C" SEXPDLLExport Adolc_gr_tapeless(SEXP xX)
   {
   // Rosenbrock Banana function   
   if (!isVector(xX) || !isNumeric(xX)) 
      return R_NilValue;

   SEXP Grad = R_NilValue;
    
   adouble x1, x2;    
   adouble  val_ad = 0.0;

   PROTECT(Grad = Rf_duplicate(xX)); 

   x1 = REAL(xX)[0];
   x2 = REAL(xX)[1];
   double seed1=1.0;
   double seed0=0.0;
   double seed12=1.0;

   x1.setADValue(&seed1);
   val_ad = 100.0*(x2-x1*x1)*(x2-x1*x1)+(1.0-x1)*(1.0-x1);
   REAL(Grad)[0] = *(val_ad.getADValue());

   x1.setADValue(&seed0);
   x2.setADValue(&seed12);
   val_ad = 100.0*(x2-x1*x1)*(x2-x1*x1)+(1.0-x1)*(1.0-x1);
   REAL(Grad)[1] = *(val_ad.getADValue());
 
   UNPROTECT(1);

   return Grad;
   }

extern "C" SEXPDLLExport equilibrium(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.Equilibrium();
 
   return brp.Return(xbrp); 
   }

extern "C" SEXPDLLExport brp2stk(SEXP stk, SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.Equilibrium();
 
   return brp.ReturnStk(stk); 
   }

extern "C" SEXPDLLExport stock_n(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);
   brp.Equilibrium();
 
   return brp.ReturnStockN(); 
   }

extern "C" SEXPDLLExport landings_n(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.Equilibrium();
 
   return brp.ReturnLandingsN(); 
   }

extern "C" SEXPDLLExport discards_n(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.Equilibrium();
 
   return brp.ReturnDiscardsN(); 
   }

extern "C" SEXPDLLExport ypr(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   return brp.ReturnYpr(); 
   }

extern "C" SEXPDLLExport spr(SEXP xbrp, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   return brp.ReturnSpr(); 
   }

extern "C" SEXPDLLExport hcrYield(SEXP xbrp, SEXP xSR, SEXP xPar, SEXP xFbar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.hcrYield(xFbar);
 
   return brp.ReturnLandingsN(); 
   }

extern "C" SEXPDLLExport computeRefpts(SEXP xbrp, SEXP xref, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   return brp.brp(xref); 
   }

extern "C" SEXPDLLExport brp(SEXP xbrp, SEXP xref, SEXP xSR, SEXP xPar)
   {
   FLBRP brp(xbrp, xSR, xPar);

   brp.Equilibrium();
   
   SEXP RtnVal = brp.Return(xbrp);

   SET_SLOT(RtnVal, install("refpts"), brp.brp(xref));       

   return RtnVal; 
   }

FLBRP::FLBRP(void)
   {
   ; 
   }

FLBRP::FLBRP(SEXP x, SEXP xSR, SEXP xPar)
   {
   Init( x);

   setSR(xSR, xPar);

//   UNPROTECT(1);
   }

bool FLBRP::isFLBRP(SEXP x)
   {
   const char *s = CHAR(STRING_ELT(GET_CLASS(x), 0));

   if (strcmp(s, "FLBRP")==0)
      return TRUE;
   else
      return FALSE;   
   }

void FLBRP::Init(SEXP x)
   {
   if (!isFLBRP(x)) return; 
                         
   SEXP      range = GET_SLOT(x, install("range"));
   SEXP RangeNames = GET_NAMES(range);
	  
   int n = length(RangeNames);

   nunits   = 
   nseasons = 
   nareas   = 0;

   plusgrp = R_NaReal;

   for (int i=0; i<n; i++)
      {
      const char *s = CHAR(STRING_ELT(RangeNames, i));

      if (      strcmp(s, "min")==0 || strcmp(s, "minage")==0 || strcmp(s, "minquant")==0)
         minage     = (short)(REAL(range)[i]);
      else  if (strcmp(s, "max")==0 || strcmp(s, "maxage")==0 || strcmp(s, "maxquant")==0)
         maxage     = (short)(REAL(range)[i]);
      else if ( strcmp(s, "minfbar")==0)
         minfbar    = (short)(REAL(range)[i]);
      else if ( strcmp(s, "maxfbar")==0)
         maxfbar    = (short)(REAL(range)[i]);
      else if ( strcmp(s, "minyear")==0)
         minyr      = (short)(REAL(range)[i]);
      else if ( strcmp(s, "maxyear")==0)
         maxyr      = (short)(REAL(range)[i]);
      else  if (strcmp(s, "plusgroup")==0){
         if (R_IsNA(REAL(range)[i])){
            plusgrp = maxage+1;
         }
         else {
            plusgrp = (short)(REAL(range)[i]);
         }
      }
      }

   fbar.Init(           GET_SLOT(x, install("fbar")));       

   landings_sel.Init(   GET_SLOT(x, install("landings.sel")));  
   discards_sel.Init(   GET_SLOT(x, install("discards.sel")));  
   bycatch_harvest.Init(GET_SLOT(x, install("bycatch.harvest")));
   stock_wt.Init(       GET_SLOT(x, install("stock.wt")));      
   landings_wt.Init(    GET_SLOT(x, install("landings.wt")));   
   discards_wt.Init(    GET_SLOT(x, install("discards.wt")));   
   bycatch_wt.Init(     GET_SLOT(x, install("bycatch.wt")));    
   m.Init(              GET_SLOT(x, install("m")));             
   mat.Init(            GET_SLOT(x, install("mat")));           
   harvest_spwn.Init(   GET_SLOT(x, install("harvest.spwn")));  
   m_spwn.Init(         GET_SLOT(x, install("m.spwn")));        
   availability.Init(   GET_SLOT(x, install("availability")));         
   cost_var.Init(       GET_SLOT(x, install("vcost")));         
   cost_fix.Init(       GET_SLOT(x, install("fcost")));         
   price.Init(          GET_SLOT(x, install("price")));         

   minage   = m.minquant();
   maxage   = m.maxquant();
   minyr    = m.minyr();
   maxyr    = m.maxyr();
   nunits   = m.nunits();
   nseasons = m.nseasons();
   nareas   = m.nareas();
   
   niters   = fbar.niters();          
   niters   = __max(niters,landings_sel.niters());  
   niters   = __max(niters,discards_sel.niters());  
   niters   = __max(niters,bycatch_harvest.niters());
   niters   = __max(niters,stock_wt.niters());      
   niters   = __max(niters,landings_wt.niters());   
   niters   = __max(niters,discards_wt.niters());   
   niters   = __max(niters,bycatch_wt.niters());    
   niters   = __max(niters,m.niters());             
   niters   = __max(niters,mat.niters());           
   niters   = __max(niters,harvest_spwn.niters());  
   niters   = __max(niters,m_spwn.niters());        
   niters   = __max(niters,availability.niters());     
   niters   = __max(niters,cost_fix.niters());      
   niters   = __max(niters,cost_var.niters());      
   niters   = __max(niters,price.niters());         

   stock_n.Init(   minage,maxage,fbar.minyr(),fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);
   discards_n.Init(minage,maxage,fbar.minyr(),fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);
   landings_n.Init(minage,maxage,fbar.minyr(),fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);
   harvest.Init(   minage,maxage,fbar.minyr(),fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);

   // ensure relative abundance
   int iIter, iAge, iYr, iUnit, iSeason, iArea;  
   for (iIter=1; iIter<=availability.niters(); iIter++)
     for (iAge=availability.minquant(); iAge<=availability.maxquant(); iAge++)
       for (iYr=availability.minyr(); iYr<=availability.maxyr(); iYr++)
         for (iUnit=1; iUnit<=availability.nunits(); iUnit++)
           for (iSeason=1; iSeason<=availability.nseasons(); iSeason++)
             {
             double sum = 0.0;
             for (iArea=1; iArea<=availability.nareas(); iArea++)
               sum += availability(iAge,iYr,iUnit,iSeason,iArea,iIter);

             availability(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;
             }

   // scale so mean catch.sel = 1.0
   for (iIter=1; iIter<=discards_sel.niters(); iIter++)
     for (iYr=discards_sel.minyr(); iYr<=discards_sel.maxyr(); iYr++)
       for (iUnit=1; iUnit<=discards_sel.nunits(); iUnit++)
         for (iArea=1; iArea<=discards_sel.nareas(); iArea++)
            for (iSeason=1; iSeason<=discards_sel.nseasons(); iSeason++)
             {
             double sum = 0.0;
             for (iAge=minfbar; iAge<=maxfbar; iAge++)
                sum += discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) +
                       landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter);

             sum /= (maxfbar-minfbar+1);

             for (iAge=discards_sel.minquant(); iAge<=discards_sel.maxquant(); iAge++)
               {
               discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;
               landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;
               }
             }
   }

FLBRP::~FLBRP(void)      
   {
   //unalloc sr   

/*
   for (int i=1; i<=3; i++)
      {
      for (int j=1; j<=nunits; j++)
        delete [] (sr_params[i][j]+1);

      delete [] (sr_params[i]+1);
      }
    delete [] (sr_params+1);
*/

   delete [] (sr_model+1);
   }                               

void FLBRP::Equilibrium(void)
   {
   for (int iYr=fbar.minyr(); iYr<=fbar.maxyr(); iYr++)
      Equilibrium(iYr);
   }

void FLBRP::Equilibrium(int iYr)
   {
   int iIter, iAge, iUnit, iSeason, iArea;  
   
   for (iIter=1; iIter<=niters; iIter++)
     for (iUnit=1; iUnit<=nunits; iUnit++)
       {
       double N = 1.0;
       for (iAge=minage; iAge<=maxage; iAge++)
         {
         for (iSeason=1; iSeason<=nseasons; iSeason++)
           {
           double F         = 0.0,
                  Z         = 0.0,
                  expZ      = 0.0,
                  catch_n   = 0.0,
                  catch_sel = 0.0;

          for (iArea=1; iArea<=nareas; iArea++)
             {
             F     += fbar(1,iYr,iUnit,iSeason,iArea,iIter)*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
             Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
             expZ   = exp(-Z);
             }

          if (iAge == plusgrp && iSeason==nseasons)
             N  *= (-1.0/(expZ-1.0));

          for (iArea=1; iArea<=nareas; iArea++)
             {
             double FMult    = fbar(1,iYr,iUnit,iSeason,iArea,iIter); 
             double recruits = Recruits(FMult, iIter);

             catch_sel  = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
             
			 if (catch_sel==0.0) catch_sel=1.0;

             harvest(   iAge,iYr,iUnit,iSeason,iArea,iIter) = F;
             stock_n(   iAge,iYr,iUnit,iSeason,iArea,iIter) = recruits*N*availability(iAge,minyr,iUnit,iSeason,iArea,iIter);
             catch_n                                        = stock_n(iAge,iYr,iUnit,iSeason,iArea,iIter)*(F/Z)*(1-expZ);

			 landings_n(iAge,iYr,iUnit,iSeason,iArea,iIter) = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
             discards_n(iAge,iYr,iUnit,iSeason,iArea,iIter) = catch_n*discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;            
             }
       
         N  *= expZ;
         }
       }
     }
   }

void FLBRP::hcrYield(SEXP xfbar)
   {
   int iYr, iIter, iAge, iUnit, iSeason, iArea;  

   for (iYr=fbar.minyr(); iYr<=fbar.maxyr(); iYr++)
      Equilibrium(iYr);

   FLQuant _fbar(xfbar);

   for (iYr=_fbar.minyr(); iYr<=_fbar.maxyr(); iYr++)
      for (iIter=1; iIter<=niters; iIter++)
        for (iUnit=1; iUnit<=nunits; iUnit++)
          {
          for (iAge=minage; iAge<=maxage; iAge++)
            {
            for (iSeason=1; iSeason<=nseasons; iSeason++)
              {
              double F         = 0.0,
                     Z         = 0.0,
                     expZ      = 0.0,
                     catch_n   = 0.0,
                     catch_sel = 0.0;

            for (iArea=1; iArea<=nareas; iArea++)
              {
              F     += _fbar(1,iYr,iUnit,iSeason,iArea,iIter)*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
              Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
              expZ   = exp(-Z);
              }

            if (iAge == plusgrp && iSeason==nseasons)
              stock_n(iAge,iYr,iUnit,iSeason,iArea,iIter) *= (-1.0/(expZ-1.0));

            for (iArea=1; iArea<=nareas; iArea++)
              {
              double FMult    = _fbar(1,iYr,iUnit,iSeason,iArea,iIter);
              double recruits = Recruits(FMult, iIter);
              
			  harvest(iAge,iYr,iUnit,iSeason,iArea,iIter) = F;
              catch_n = stock_n(iAge,iYr,iUnit,iSeason,iArea,iIter)*(F/Z)*(1-expZ);
              catch_sel = discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter);

			  if (catch_sel==0.0) catch_sel=1.0;

			  discards_n(iAge,iYr,iUnit,iSeason,iArea,iIter) = catch_n*discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
              landings_n(iAge,iYr,iUnit,iSeason,iArea,iIter) = catch_n-discards_n(  iAge,minyr,iUnit,iSeason,iArea,iIter);
              }
            }
          }
        }
   }


double FLBRP::Recruits(double FMult, int iUnit, int iIter)
   {
   double spr      = SPR(FMult,iUnit,iIter),
          recruits = 1;

   //SSB as a function of SPR
   double ssb=1.0;
   switch(sr_model[iUnit]) 
      {
      case FLRConst_BevHolt: 
         ssb      = spr*sr_params(1,1,iUnit,1,1,iIter)-sr_params(2,1,iUnit,1,1,iIter);
         recruits = sr_params(1,1,iUnit,1,1,iIter)*ssb/(ssb+sr_params(2,1,iUnit,1,1,iIter));
      break;

      case FLRConst_Ricker:
         ssb      = log(spr*sr_params(1,1,iUnit,1,1,iIter))/sr_params(2,1,iUnit,1,1,iIter);
         recruits = sr_params(1,1,iUnit,1,1,iIter)*ssb*exp(-sr_params(2,1,iUnit,1,1,iIter)*ssb);
      break;
      
  	  case FLRConst_Cushing:
         ssb      =pow(1.0/(sr_params(1,1,iUnit,1,1,iIter)*spr),(1.0/(sr_params(2,1,iUnit,1,1,iIter)-1.0)));
         recruits =sr_params(1,1,iUnit,1,1,iIter)*pow(ssb,sr_params(2,1,iUnit,1,1,iIter));
      break;
      
      case FLRConst_Shepherd:
         ssb      =sr_params(2,1,iUnit,1,1,iIter)*pow(sr_params(1,1,iUnit,1,1,iIter)*spr-1.0,1/sr_params(3,1,iUnit,1,1,iIter));
         recruits =sr_params(1,1,iUnit,1,1,iIter)*ssb/(1.0+pow(ssb/sr_params(2,1,iUnit,1,1,iIter),sr_params(3,1,iUnit,1,1,iIter)));
      break;
              
      case FLRConst_SegReg:
       //ssb      = (spr < 1/sr_params(2,1,iUnit,1,1,iIter) ? 0.0 : spr*sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter));
       //recruits = (ssb < 1/sr_params(2,1,iUnit,1,1,iIter) ? 0.0 : ssb*sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter));
      if (1/spr > sr_params(1,1,iUnit,1,1,iIter)) 
	      recruits = 0.0; 
	   else 
		   recruits = sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter);
       break;

	  case FLRConst_Mean: default:
         recruits = sr_params(1,1,iUnit,1,1,iIter);
      break;
      }

   return recruits;
   }

SEXP FLBRP::Return(SEXP x)
   {       
   SET_SLOT(x, install("stock.n"),      stock_n.Return());       
   SET_SLOT(x, install("landings.n"),   landings_n.Return());    
   SET_SLOT(x, install("discards.n"),   discards_n.Return());    
   SET_SLOT(x, install("landings.sel"), landings_sel.Return());  
   SET_SLOT(x, install("discards.sel"), discards_sel.Return());  
   SET_SLOT(x, install("harvest"),      harvest.Return());       
  
   return x;
   }

SEXP FLBRP::ReturnStk(SEXP x)
   { 
   FLQuant _stock(   GET_SLOT(x, install("stock")));
   FLQuant _catch(   GET_SLOT(x, install("catch")));
   FLQuant _landings(GET_SLOT(x, install("landings")));
   FLQuant _discards(GET_SLOT(x, install("discards")));
   FLQuant _catch_wt(GET_SLOT(x, install("catch.wt")));
   FLQuant catch_n(minage,maxage,fbar.minyr(),fbar.maxyr(),nunits,nseasons,nareas,niters,0);
      
   SET_SLOT(x, install("stock.n"),    stock_n.Return());       
   SET_SLOT(x, install("landings.n"), landings_n.Return());    
   SET_SLOT(x, install("discards.n"), discards_n.Return());    
   SET_SLOT(x, install("catch.n"),    catch_n.Return());    
   SET_SLOT(x, install("harvest"),    harvest.Return());       
  
   for (int iIter=1; iIter<=niters; iIter++)
     for (int iUnit=1; iUnit<=nunits; iUnit++)
       for (int iSeason=1; iSeason<=nseasons; iSeason++)
         for (int iArea=1; iArea<=nareas; iArea++)
           for (int iYr=fbar.minyr(); iYr<=fbar.maxyr(); iYr++){

              _stock(   _stock.minquant(),iYr,iUnit,iSeason,iArea,iIter) =  
              _catch(   _stock.minquant(),iYr,iUnit,iSeason,iArea,iIter) =  
              _landings(_stock.minquant(),iYr,iUnit,iSeason,iArea,iIter) =
              _discards(_stock.minquant(), iYr,iUnit,iSeason,iArea,iIter) =
              _catch_wt(_stock.minquant(),iYr,iUnit,iSeason,iArea,iIter) = 0.0;

              for (int iAge=minage; iAge<=maxage; iAge++){
                 _stock(iAge,iYr,iUnit,iSeason,iArea,iIter)    += stock_n(   iAge,iYr,iUnit,iSeason,iArea,iIter)*stock_wt(   iAge,iYr,iUnit,iSeason,iArea,iIter);
                 _landings(iAge,iYr,iUnit,iSeason,iArea,iIter) += landings_n(iAge,iYr,iUnit,iSeason,iArea,iIter)*landings_wt(iAge,iYr,iUnit,iSeason,iArea,iIter);
                 _discards(iAge,iYr,iUnit,iSeason,iArea,iIter) += discards_n(iAge,iYr,iUnit,iSeason,iArea,iIter)*discards_wt(iAge,iYr,iUnit,iSeason,iArea,iIter);
                 catch_n(iAge,iYr,iUnit,iSeason,iArea,iIter)   += landings_n(iAge,iYr,iUnit,iSeason,iArea,iIter)*discards_n( iAge,iYr,iUnit,iSeason,iArea,iIter);
                
				 double catch_sel = landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter);
                 
				 if (catch_sel==0.0) catch_sel=1.0;
				 _catch_wt(iAge,iYr,iUnit,iSeason,iArea,iIter) +=(landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter)*landings_wt(iAge,iYr,iUnit,iSeason,iArea,iIter)+
                                                                  discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter)*discards_wt(iAge,iYr,iUnit,iSeason,iArea,iIter))/catch_sel;
                  }
           }
   
   return x;
   }

SEXP FLBRP::ReturnStockN(void)
   {       
   return stock_n.Return();
   }

SEXP FLBRP::ReturnLandingsN(void)
   {       
   return landings_n.Return();
   }

SEXP FLBRP::ReturnDiscardsN(void)
   {       
   return discards_n.Return();
   }

SEXP FLBRP::ReturnSpr(void)
   {       
   Equilibrium();

double t1,t2,t3,t4,t5,t6,t7;
 
   FLQuant x(1, 1, fbar.minyr(), fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);

   for (int iIter=1; iIter<=niters; iIter++)
     for (int iUnit=1; iUnit<=nunits; iUnit++)
       for (int iSeason=1; iSeason<=nseasons; iSeason++)
         for (int iArea=1; iArea<=nareas; iArea++)
           for (int iYr=fbar.minyr(); iYr<=fbar.maxyr(); iYr++){
              x(1,iYr,iUnit,iSeason,iArea,iIter)=0.0;
              for (int iAge=minage; iAge<=maxage; iAge++){
                 x(1,iYr,iUnit,iSeason,iArea,iIter) +=stock_n( iAge,iYr,iUnit,iSeason,iArea,iIter)*
                                                      stock_wt(iAge,1,  iUnit,iSeason,iArea,iIter)*
                                                      mat(     iAge,1,  iUnit,iSeason,iArea,iIter)*
                                                  exp(-harvest(iAge,iYr,iUnit,iSeason,iArea,iIter)*harvest_spwn(iAge,  1,iUnit,iSeason,iArea,iIter)
                                                      -m(      iAge,1,  iUnit,iSeason,iArea,iIter)*m_spwn(      iAge,  1,iUnit,iSeason,iArea,iIter));

                 t1 =stock_n( iAge,iYr,iUnit,iSeason,iArea,iIter);
                 t2 =stock_wt(iAge,1,  iUnit,iSeason,iArea,iIter);
                 t3 =mat(     iAge,1,  iUnit,iSeason,iArea,iIter);
                 t4 =harvest( iAge,iYr,iUnit,iSeason,iArea,iIter);
                 t5 =harvest_spwn(iAge,  1,iUnit,iSeason,iArea,iIter);
                 t6 =m(           iAge,1,  iUnit,iSeason,iArea,iIter);
                 t7 =m_spwn(      iAge,  1,iUnit,iSeason,iArea,iIter);

                  t1=harvest(iAge,iYr,iUnit,iSeason,iArea,iIter);
                  //x(1,iYr,iUnit,iSeason,iArea,iIter)=stock_n( 10,iYr,iUnit,iSeason,iArea,iIter);
}
                  }

   return x.Return();
   }

SEXP FLBRP::ReturnYpr(void)
   {       
   Equilibrium();
 
double t1=0;

   FLQuant x(1, 1, fbar.minyr(), fbar.maxyr(), nunits, nseasons, nareas, niters, 0.0);

   for (int iIter=1; iIter<=niters; iIter++)
     for (int iUnit=1; iUnit<=nunits; iUnit++)
       for (int iSeason=1; iSeason<=nseasons; iSeason++)
         for (int iArea=1; iArea<=nareas; iArea++)
           for (int iYr=fbar.minyr(); iYr<=fbar.maxyr(); iYr++){
             x(1,iYr,iUnit,iSeason,iArea,iIter)=0.0;
             for (int iAge=minage; iAge<=maxage; iAge++){
                x(1,iYr,iUnit,iSeason,iArea,iIter) +=landings_n( iAge,iYr,iUnit,iSeason,iArea,iIter)*
                                                     landings_wt(iAge,  1,iUnit,iSeason,iArea,iIter);

                t1=x(1,iYr,iUnit,iSeason,iArea,iIter);}
                }

   return x.Return();
   }


double  FLBRP::QuadSearch(int iIter)
   {
   double x[3],
          F[3],
          Newx,
          Newf;
    
   QSBracket(x, iIter);

   short Iter = 0;
   do {
      for(short i = 0; i <= 2; i++)
         F[i] = QSGetFunc(x[i], iIter);

      Newx = QSGetNewx(x, F);
      Newf = QSGetFunc(Newx, iIter);
    
      //Check for one sided convergence
      if (fabs(Newx - x[1]) < 0.000000001){
         if (x[1] - x[0] >= x[2] - x[1]){
            Newx = x[1] - (x[2] - x[1]);
         }
         else{
            Newx = x[1] + (x[1] - x[0]);
         }
      }
        
      if (Newx > x[1])
         if (Newf > F[1])
            {
            x[0] = x[1];
            x[1] = Newx;
            }
         else
            x[2] = Newx;        
      else
         if (Newf > F[1])
            {
            x[2] = x[1];
            x[1] = Newx;
            }
         else
            x[1] = Newx;

//Rprintf("QS \t%f\t%f\t%f\t%f",   Newx,x[0],x[1],x[2]);
//Rprintf("   \t%f\t%f\t%f\t%f\n", Newf,F[0],F[1],F[2]);

//library(FLBRP);data(ple4);t.<-brp(FLBRP(ple4))
      }
   while ((fabs(x[2] - x[0]) > QS_TOL) && (fabs(x[0] - x[1])) > QS_TOL  && (fabs(x[1] - x[2]) > QS_TOL) &&
          (fabs(F[2] - F[0]) > QS_TOL) && (fabs(F[0] - F[1])) > QS_TOL  && (fabs(F[1] - F[2]) > QS_TOL) &&Iter++ < QS_ITS);

   return x[1];
   }

double  FLBRP::QSGetNewx(double x[], double func[])
   {
   double bottom = ((x[1]-x[2])*func[0] + (x[2]-x[0])*func[1] + (x[0]-x[1])*func[2]);  

   if (fabs(bottom) < QS_TOL) return(x[1]);

   return 0.5*(x[0]+x[2]) -0.5*(func[0] -func[2])*(x[2]-x[1])*(x[1]-x[0]) / bottom;
   }

double  FLBRP::QSGetFunc(double x, int iIter)
   {
   double t = 0.0;

   switch (TargetType) {
      case FLRConst_BRPMSY:
         return yield(x, iIter);
      case FLRConst_BRPMEY:
         return Profit(x, iIter);
      case FLRConst_BRPMaxYPR:
         return YPR(x, iIter);
      case FLRConst_BRPF0pt1: 
         t = YPRGrad(x, iIter)/YPRGrad(0.0, iIter)-0.1;
         return -t*t;
      case FLRConst_BRPYield:
         t = yield(x, iIter) - Target;
         return -t*t;
      case FLRConst_BRPYPR:
         t = YPR(x, iIter) - Target;
         return -t*t;
      case FLRConst_BRPYS:
         t = YS(x, iIter) - Target;
         return -t*t;
      case FLRConst_BRPSPR:
         t = SPR(x, iIter) - Target;
         return -t*t;
      case FLRConst_BRPSSB:
         t = SSB(x, iIter) - Target;
         return -t*t;
      case FLRConst_BRPSPRPercMax:
         t = SPR(x, iIter)/SPR(0.0, iIter)-Target;
         return -t*t;
      default:
         return 0.0;
      }
   }

void  FLBRP::QSBracket(double *x, int iIter)
   {
   int Iter=0;

   x[0] = 0.0;
   x[2] = 0.11;
 
   double t, t2, slopeOrig;
   
   switch (TargetType) {
      case FLRConst_BRPMSY:
         do { 
            x[2] *= QS_INC;
//            t  = yield(x[2], iIter);
            }
         while (YieldGrad(x[2], iIter)>=0.0 && Iter++ < QS_ITS);
//         while (t2>t && Iter++ < QS_ITS);
         break;

//      case FLRConst_BRPMEY:     
//         do { 
//            t=Profit(    x[2], iIter);
//           x[2] *= QS_INC;
//            }
//         while (ProfitGrad(x[2], iIter)>=0.0 && yield(x[2], iIter)>t && Iter++ < QS_ITS);
//         x[0] =  x[2]/QS_INC;
//         x[1] = (x[0]+x[2])/2.0;
         do { 
            t = yield(x[2], iIter);
            x[2] *= QS_INC;
            }
         while (YieldGrad(x[2], iIter)>=0.0 && yield(x[2], iIter)>t && Iter++ < QS_ITS);
         break;
 
      case FLRConst_BRPMaxYPR:
         do {
            t = YPR(x[2], iIter);

            x[2] *= QS_INC;
            }
         while (YPRGrad(x[2], iIter)>=0.0 && YPR(x[2], iIter)>t && Iter++ < QS_ITS);
         break;

      case FLRConst_BRPF0pt1: 
         slopeOrig = YPRGrad(x[0], iIter);
         do {
            x[2] *= QS_INC;
            t     = YPRGrad(x[2], iIter);
            }
         while (t >= slopeOrig*0.1 && Iter++ < QS_ITS);
         break;

      case FLRConst_BRPSPRPercMax:
         do 
            x[2] *= QS_INC;
         while (SPR(x[2], iIter)/SPR(0.0, iIter)-Target > 0.0);
         break;

      case FLRConst_BRPYield:
         do 
            x[2] *= QS_INC;
         while (yield(x[2], iIter)-Target > 0.0);
         break;
      
	  case FLRConst_BRPYS:
         do 
            x[2] *= QS_INC;
         while (yield(x[2], iIter)/SSB(x[2], iIter)-Target > 0.0);
         break;

      case FLRConst_BRPYPR:
         do 
            x[2] *= QS_INC;
         while (YPR(x[2], iIter)-Target > 0.0);
         break;

      case FLRConst_BRPSPR:
         do 
            x[2] *= QS_INC;
         while (SPR(x[2], iIter)-Target > 0.0);
         break;

      case FLRConst_BRPSSB:
         do 
            x[2] *= QS_INC;
         while (SSB(x[2], iIter)-Target > 0.0);
         break;
     
      default:
         break;
      }

   x[0] = 0.0/QS_INC;
   x[1] = (x[0]+x[2])/2;
   }

double FLBRP::SPR(double FMult, int iUnit, int iIter)
   {
   double ReturnValue = 0.0;

   int iAge, iSeason, iArea;  
   
   double N = 1.0;
   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
         {
         double F         = 0.0,
                Z         = 0.0,
                expZ      = 0.0;

         for (iArea=1; iArea<=nareas; iArea++)
            {
            F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                            availability(iAge,minyr,iUnit,iSeason,iArea,iIter);
            Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
            expZ   = exp(-Z);
            }
             
         if (iAge == plusgrp && iSeason==nseasons)
            N  *= (-1.0/(expZ-1.0));
      
         for (iArea=1; iArea<=nareas; iArea++)
            {
            ReturnValue +=  availability(  iAge,minyr,iUnit,iSeason,iArea,iIter)*
                            N*exp(-m(      iAge,minyr,iUnit,iSeason,iArea,iIter)*m_spwn(      iAge,minyr,iUnit,iSeason,iArea,iIter)     
                      -FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                              harvest_spwn(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                                  stock_wt(iAge,minyr,iUnit,iSeason,iArea,iIter)*mat(          iAge,minyr,iUnit,iSeason,iArea,iIter);
            }
         
         N *= expZ;
         }
      }

   return ReturnValue;
   }

double FLBRP::BPR(double FMult, int iUnit, int iIter)
   {
   double ReturnValue = 0.0;

   int iAge, iSeason, iArea;  
   
   double N = 1.0;
   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
         {
         double F         = 0.0,
                Z         = 0.0,
                expZ      = 0.0;

         for (iArea=1; iArea<=nareas; iArea++)
            {
            F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                            availability(iAge,minyr,iUnit,iSeason,iArea,iIter);
            Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
            expZ   = exp(-Z);
            }
             
         if (iAge == plusgrp && iSeason==nseasons)
            N  *= (-1.0/(expZ-1.0));
      
         for (iArea=1; iArea<=nareas; iArea++)
            {
            ReturnValue +=  availability(  iAge,minyr,iUnit,iSeason,iArea,iIter)*
                            N*exp(-m(      iAge,minyr,iUnit,iSeason,iArea,iIter)*m_spwn(      iAge,minyr,iUnit,iSeason,iArea,iIter)     
                      -FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                              harvest_spwn(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                                  stock_wt(iAge,minyr,iUnit,iSeason,iArea,iIter);
            }
         
         N *= expZ;
         }
      }

   return ReturnValue;
   }

double FLBRP::YPR(double FMult, int iUnit, int iIter)
   {
   double ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   double N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        double F          = 0.0,
               Z          = 0.0,
               expZ       = 0.0,
               catch_n    = 0.0,
			   catch_sel  = 0.0,
               landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n   = N*(F/Z)*(1-expZ);
          catch_sel =(landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter); 
          }    

      N *= expZ;
      }
    }

   return ReturnValue;
   }

adouble FLBRP::YPR(adouble FMult, int iUnit, int iIter)
   {
   adouble ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   adouble N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_n    = 0.0,
                catch_sel  = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/(landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          ReturnValue += landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter); 
          }    

      N *= expZ;
      }
    }

   return ReturnValue;
   }

double FLBRP::RPR(double FMult, int iUnit, int iIter)
   {
   double ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   double N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        double F          = 0.0,
               Z          = 0.0,
               expZ       = 0.0,
               catch_sel  = 0.0,
               catch_n    = 0.0,
               landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          
		  catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;

		  ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter); 
          }

      N *= expZ;
      }
    }

   return ReturnValue;
   }

adouble FLBRP::RPR(adouble FMult, int iUnit, int iIter)
   {
   adouble ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   adouble N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_sel  = 0.0,
                catch_n    = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
		  catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter); 
          }

      N *= expZ;
      }
    }

   return ReturnValue;
   }

double FLBRP::PPR(double FMult, int iUnit, int iIter)
   {
   double ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   double N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        double F          = 0.0,
               Z          = 0.0,
               expZ       = 0.0,
               catch_sel  = 0.0,
               catch_n    = 0.0,
               landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter) - FMult*cost_var(1,minyr,iUnit,iSeason,iArea,iIter) - cost_fix(1,minyr,iUnit,iSeason,iArea,iIter); 
          }
    
      N *= expZ;
      }
    }

   return ReturnValue;
   }

adouble FLBRP::PPR(adouble FMult, int iUnit, int iIter)
   {
   adouble ReturnValue = 0.0;

   int iAge, iSeason, iArea;     
   adouble N = 1.0;

   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_sel  = 0.0,
                catch_n    = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }
              
       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter) - FMult*cost_var(1,minyr,iUnit,iSeason,iArea,iIter) - cost_fix(1,minyr,iUnit,iSeason,iArea,iIter); 
          }
    
      N *= expZ;
      }
    }

   return ReturnValue;
   }

double FLBRP::YPRGrad(double FMult, int iIter)
  {
  adouble ReturnValue=0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

  int iAge, iUnit, iSeason, iArea;
  for (iUnit=1; iUnit<=nunits; iUnit++)
    {
    adouble N = 1.0;
    for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_n    = 0.0,
                catch_sel  = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult_ad*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }

       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter);
          }


       N *= expZ;
       }
     }
   }

   double RtnVal = *(ReturnValue.getADValue());

   return RtnVal;
   }

double FLBRP::RPRGrad(double FMult, int iIter)
  {
  adouble ReturnValue = 0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

  int iAge, iUnit, iSeason, iArea;
  for (iUnit=1; iUnit<=nunits; iUnit++)
    {
    adouble N = 1.0;
    for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_sel  = 0.0,
                catch_n    = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult_ad*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }

       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          catch_sel    =(landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter);
          }

       N *= expZ;
       }
     }
   }

   double RtnVal = *(ReturnValue.getADValue());

   return RtnVal;
   }

double FLBRP::PPRGrad(double FMult, int iIter)
  {
  adouble ReturnValue = 0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

  int iAge, iUnit, iSeason, iArea;
  for (iUnit=1; iUnit<=nunits; iUnit++)
    {
    adouble N = 1.0;
    for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
        {
        adouble F          = 0.0,
                Z          = 0.0,
                expZ       = 0.0,
                catch_sel  = 0.0,
                catch_n    = 0.0,
                landings_n = 0.0;

       for (iArea=1; iArea<=nareas; iArea++)
          {
          F     += FMult_ad*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
          Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
          expZ   = adtl::exp(-Z);
          }

       if (iAge == plusgrp && iSeason==nseasons)
          N  *= (-1.0/(expZ-1.0));

       for (iArea=1; iArea<=nareas; iArea++)
          {
          catch_n      = N*(F/Z)*(1-expZ);
          catch_sel    = (landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter));
		  if (catch_sel==0.0) catch_sel=1.0;

		  landings_n   = catch_n*landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)/catch_sel;
          ReturnValue += price(iAge,minyr,iUnit,iSeason,iArea,iIter)*landings_n*landings_wt(iAge,minyr,iUnit,iSeason,iArea,iIter) - FMult*cost_var(iAge,minyr,iUnit,iSeason,iArea,iIter) - cost_fix(iAge,minyr,iUnit,iSeason,iArea,iIter);
          }

       N *= expZ;
       }
     }
   }

   double RtnVal = *(ReturnValue.getADValue());

   return RtnVal;
   }

double  FLBRP::F0pt1(int iIter)
   {
   TargetType = FLRConst_BRPF0pt1;

   return QuadSearch(iIter);
   }

double  FLBRP::FMax(int iIter)
   {
   TargetType = FLRConst_BRPMaxYPR;

   return QuadSearch(iIter);
   }

double  FLBRP::FSPRPercMax(double _Target, int iIter)
   {
   TargetType = FLRConst_BRPSPRPercMax;
   Target     = __max(0.0,_Target);

   return QuadSearch(iIter);
   }

double  FLBRP::TargetSSB(double _Target, int iIter)
   {
   TargetType = FLRConst_BRPSSB;
   Target     = __max(0.0,_Target);

   return QuadSearch(iIter);
   }

double  FLBRP::TargetYPR(double _Target, int iIter)
   {
   TargetType = FLRConst_BRPYPR;
   Target     = __max(0.0,_Target);

   return QuadSearch(iIter);
   }

double  FLBRP::TargetYS(double _Target, int iIter)
   {
   TargetType = FLRConst_BRPYS;
   Target     = __max(0.0,_Target);

   return QuadSearch(iIter);
   }

double  FLBRP::TargetSPR(double _Target, int iIter)
   {
   TargetType = FLRConst_BRPSPR;
   Target     = __max(0.0,_Target);

   return QuadSearch(iIter);
   }

double  FLBRP::FMSY(int iIter)
   {
   TargetType = FLRConst_BRPMSY;

   return QuadSearch(iIter);
   }

double  FLBRP::FMEY(int iIter)
   { 
   if (R_IsNA(Profit(0.1, iIter)))
	  return R_NaReal;
  
   TargetType = FLRConst_BRPMEY;

   return QuadSearch(iIter);
   }

SEXP FLBRP::brp(SEXP Object)
   {
   SEXP v        = PROTECT(duplicate(GET_SLOT(Object, install(".Data")))),
        dims     = GET_DIM(v),
        dimnames = GET_DIMNAMES(v),
        v3;

   double *a     = NUMERIC_POINTER(v);
   char   name[12];
   const char *name_ = "";

   double ***D;

   short dim[3], n = length(dims);

   if (n != 3)
      {
      UNPROTECT(1);
  
			return ScalarLogical(FALSE);
      }

   if (INTEGER(dims)[1] != 8)
      {
      UNPROTECT(1);
  
			return ScalarLogical(FALSE);
      }

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];

   short iRef, iIter, i, j, k, l=0;

   //alloc      
   D = new double**[dim[0]];
   for(i=0; i<dim[0]; i++) {
      D[i]  = new double*[dim[1]];
      for(j=0; j<dim[1]; j++) 
         D[i][j] = new double[dim[2]];}

   for (k = 0; k < dim[2]; k++)
     for (j = 0; j < dim[1]; j++)
       for (i = 0; i < dim[0]; i++)
         D[i][j][k] = (a)[l++];       

   for (iIter=0; iIter<dim[2]; iIter++)//iter
     for (iRef=0; iRef<dim[0]; iRef++)//refpts
       {
       if (dimnames != R_NilValue) 
         if (TYPEOF(dimnames) == VECSXP) 
            name_ = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), iRef));

       name[0] = '\0'; 
       strcpy(name,name_);   
       for (int i=0; i< (signed)strlen(name); i++)
         name[i] = (signed)toupper(name[i]);

       //F ref pt
       if (strcmp(name, "MSY") == 0)
         D[iRef][RP_harvest][iIter] = FMSY(iIter+1);
       else if (strcmp(name, "MEY") == 0)
         D[iRef][RP_harvest][iIter] = FMEY(iIter+1);
   	   else if (strcmp(name, "F0.1") == 0)
         D[iRef][RP_harvest][iIter] = F0pt1(iIter+1);
       else if (strcmp(name, "SPR0") == 0)
         D[iRef][RP_harvest][iIter] = 0.0;
       else if (strcmp(name, "FMAX") == 0)
         D[iRef][RP_harvest][iIter] = FMax(iIter+1);
       else if (strcmp(name, "FPA") == 0 || strcmp(name, "FLIM") == 0)
         D[iRef][RP_harvest][iIter] = D[iRef][RP_harvest][iIter];
       else if (strcmp(name, "BPA") == 0 || strcmp(name, "BLIM") == 0 || strcmp(name, "CRASH") == 0){
          D[iRef][RP_harvest][iIter] =
	      D[iRef][RP_yield  ][iIter] =
	      D[iRef][RP_rec    ][iIter] =
	      D[iRef][RP_biomass][iIter] =
	      D[iRef][RP_revenue][iIter] =
	      D[iRef][RP_cost   ][iIter] =
	      D[iRef][RP_profit ][iIter] = R_NaReal;
	      if (strcmp(name, "CRASH") == 0) D[iRef][RP_ssb][iIter] = 0.0; 
         
         int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(D[iRef][RP_ssb][iIter]-SSB(x,iIter+1),2);
           dgdx = -2*(D[iRef][RP_ssb][iIter]-SSB(x,iIter+1))*SSBGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         //D[iRef][RP_harvest][iIter] = TargetSSB(D[iRef][RP_ssb     ][iIter],iIter);
         D[iRef][RP_harvest][iIter] = x;
         }
       else if (strncmp(name, "SPR.", 4) == 0)
         {
         const char *t;

         t = strtok(name, ".\t\n\0");
         t = strtok(NULL, ".\t\n\0");
         if (t != NULL)
            {
	         double val = atof(t)/100.0;

            D[iRef][RP_harvest][iIter] = FSPRPercMax(__max(0.0,__min(1.0,val)), iIter+1);
            }
         }
       //SPR ref pt
       else if (!R_IsNA(D[iRef][RP_rec][iIter]) && !R_IsNA(D[iRef][RP_ssb][iIter]))
         D[iRef][RP_harvest][iIter] = TargetSPR(D[iRef][RP_ssb    ][iIter]/D[iRef][RP_rec][iIter],iIter+1);
       //YPR ref pt
       else if (!R_IsNA(D[iRef][RP_yield][iIter]) && !R_IsNA(D[iRef][RP_rec][iIter]))
         D[iRef][RP_harvest][iIter] = TargetYPR(D[iRef][RP_yield][iIter]/D[iRef][RP_rec][iIter],iIter+1);
       //Y/S or exploitation rate ref pt
       else if (!R_IsNA(D[iRef][RP_yield][iIter]) && !R_IsNA(D[iRef][RP_ssb][iIter]) && R_IsNA(D[iRef][RP_harvest][iIter]) && R_IsNA(D[iRef][RP_rec][iIter]))
         D[iRef][RP_harvest][iIter] = TargetYS(D[iRef][RP_yield  ][iIter]/D[iRef][RP_ssb][iIter],iIter+1);
       //ssb
       else if (R_IsNA(D[iRef][RP_harvest][iIter]) &&
                R_IsNA(D[iRef][RP_yield  ][iIter]) &&
                R_IsNA(D[iRef][RP_rec    ][iIter]) &&
                R_IsNA(D[iRef][RP_biomass][iIter]) &&
                R_IsNA(D[iRef][RP_revenue][iIter]) &&
                R_IsNA(D[iRef][RP_cost   ][iIter]) &&
                R_IsNA(D[iRef][RP_profit ][iIter]) && !R_IsNA(D[iRef][RP_ssb][iIter]))
         {
         int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(D[iRef][RP_ssb][iIter]-SSB(x,iIter+1),2);
           dgdx = -2*(D[iRef][RP_ssb][iIter]-SSB(x,iIter+1))*SSBGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         //D[iRef][RP_harvest][iIter] = TargetSSB(D[iRef][RP_ssb     ][iIter],iIter);
         D[iRef][RP_harvest][iIter] = x;
         }
       //rec  
       else if (R_IsNA(D[iRef][RP_harvest][iIter]) &&
                R_IsNA(D[iRef][RP_yield  ][iIter]) &&
                R_IsNA(D[iRef][RP_ssb    ][iIter]) &&
                R_IsNA(D[iRef][RP_biomass][iIter]) &&
                R_IsNA(D[iRef][RP_revenue][iIter]) &&
                R_IsNA(D[iRef][RP_cost   ][iIter]) &&
                R_IsNA(D[iRef][RP_profit ][iIter]) && !R_IsNA(D[iRef][RP_rec][iIter]))
         {
         int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(D[iRef][RP_rec][iIter]-Recruits(x,iIter+1),2);
           dgdx = -2*(D[iRef][RP_rec][iIter]-Recruits(x,iIter+1))*RecGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         D[iRef][RP_harvest][iIter] = x;
         }
       //biomass  
       else if (R_IsNA(D[iRef][RP_harvest][iIter]) && 
                R_IsNA(D[iRef][RP_yield  ][iIter]) && 
                R_IsNA(D[iRef][RP_rec    ][iIter]) && 
                R_IsNA(D[iRef][RP_ssb    ][iIter]) && 
                R_IsNA(D[iRef][RP_revenue][iIter]) && 
                R_IsNA(D[iRef][RP_cost   ][iIter]) && 
                R_IsNA(D[iRef][RP_profit ][iIter]) && !R_IsNA(D[iRef][RP_biomass][iIter]))
         {
         int Iters=0;
		     double x=0.1,f,dgdx;
		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(D[iRef][RP_biomass][iIter]-Biomass(x,iIter+1),2);
           dgdx = -2*(D[iRef][RP_biomass][iIter]-Biomass(x,iIter+1))*BiomassGrad(x,iIter+1);

           x = x - f / dgdx;
           }
         while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         D[iRef][RP_harvest][iIter] = x;
         }
      
      if (!R_IsNA(D[iRef][RP_harvest][iIter])){
	      D[iRef][RP_yield  ][iIter] = yield(   D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_rec    ][iIter] = Recruits(D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_ssb    ][iIter] = SSB(     D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_biomass][iIter] = Biomass( D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_revenue][iIter] = Revenue( D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_cost   ][iIter] = Cost(    D[iRef][RP_harvest][iIter],iIter+1);
          D[iRef][RP_profit ][iIter] = Profit(  D[iRef][RP_harvest][iIter],iIter+1);}
       }

    //Allocate memory
    PROTECT(v3 = Rf_allocArray(REALSXP, dims)); 
    
    //Create names for dimensions
    setAttrib(v3, R_DimNamesSymbol, dimnames);

   l = 0;
   for (k = 0; k < dim[2]; k++)
     for (j = 0; j < dim[1]; j++)
       for (i = 0; i < dim[0]; i++)
          REAL(v3)[l++] = D[i][j][k];    

   //unalloc      
   for (i=0; i<dim[0]; i++){
      for (j=0; j<dim[1]; j++)
        delete [] (D[i][j]);

      delete [] (D[i]);
      }
    delete [] (D);

   SEXP refpts;

   PROTECT(refpts = NEW_OBJECT(MAKE_CLASS("FLPar")));

   refpts = R_do_slot_assign(refpts, install(".Data"), v3);
   
   UNPROTECT(3);

   //return v3;   
   return refpts;   
   }

void FLBRP::setSR(SEXP xModel, SEXP xPar)
   {
   if (!isVector(xModel) || !isNumeric(xModel))
      return;

   sr_model    = new FLRConstSRR [nunits]-1;
   sr_model[1] = (FLRConstSRR)INTEGER(xModel)[0];

   sr_params.Init(xPar);
    }

double FLBRP::SPR(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += SPR(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::BPR(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += BPR(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::YPR(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += YPR(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::YS(double FMult, int iIter)
   {
   double result = yield(FMult,iIter)/SSB(FMult,iIter);

   return result;
   }

double FLBRP::RPR(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += RPR(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::PPR(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += RPR(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::SSB(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += SPR(FMult,iUnit,iIter)*Recruits(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::Biomass(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += BPR(FMult,iUnit,iIter)*Recruits(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::yield(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += YPR(FMult,iUnit,iIter)*Recruits(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::Revenue(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += RPR(FMult,iUnit,iIter)*Recruits(FMult,iUnit,iIter);

   return result;
   }

double FLBRP::Cost(double FMult, int iIter)
   {
   double result = 0.0;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
     for (int iSeason=1; iSeason<=nseasons; iSeason++)
       for (int iArea=1; iArea<=nareas; iArea++)
         result += FMult*cost_var(1,minyr,iUnit,iSeason,iArea,iIter)+cost_fix(1,minyr,iUnit,iSeason,iArea,iIter);

   return result;
   }

double FLBRP::Profit(double FMult, int iIter)
   {
   double result = Revenue(FMult, iIter)-Cost(FMult, iIter);

   return result;
   }

double FLBRP::Recruits(double FMult, int iIter)
   {
   double result = 0.0;

   for (int iUnit=1; iUnit<=nunits; iUnit++)
       result += Recruits(FMult, iUnit, iIter);

   return result;
   }

adouble FLBRP::SPR(adouble FMult, int iUnit, int iIter)
   {
   adouble ReturnValue = 0.0;

   int iAge, iSeason, iArea;  
   
   adouble N = 1.0;
   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
         {
         adouble F         = 0.0,
                Z         = 0.0,
                expZ      = 0.0;

         for (iArea=1; iArea<=nareas; iArea++)
            {
            F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                            availability(iAge,minyr,iUnit,iSeason,iArea,iIter);
            Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
            expZ   = adtl::exp(-Z);
            }
             
         if (iAge == plusgrp && iSeason==nseasons)
            N  *= (-1.0/(expZ-1.0));
      
         for (iArea=1; iArea<=nareas; iArea++)
           {
            ReturnValue +=  availability(  iAge,minyr,iUnit,iSeason,iArea,iIter)*
                            N*adtl::exp(-m(      iAge,minyr,iUnit,iSeason,iArea,iIter)*m_spwn(      iAge,minyr,iUnit,iSeason,iArea,iIter)     
                      -FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                              harvest_spwn(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                                  stock_wt(iAge,minyr,iUnit,iSeason,iArea,iIter)*mat(          iAge,minyr,iUnit,iSeason,iArea,iIter);
            }
         
         N *= expZ;
         }
      }

   return ReturnValue;
   }

adouble FLBRP::BPR(adouble FMult, int iUnit, int iIter)
   {
   adouble ReturnValue = 0.0;

   int iAge, iSeason, iArea;  
   
   adouble N = 1.0;
   for (iAge=minage; iAge<=maxage; iAge++)
      {
      for (iSeason=1; iSeason<=nseasons; iSeason++)
         {
         adouble F         = 0.0,
                Z         = 0.0,
                expZ      = 0.0;

         for (iArea=1; iArea<=nareas; iArea++)
            {
            F     += FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                            availability(iAge,minyr,iUnit,iSeason,iArea,iIter);
            Z     += F+m( iAge,minyr,iUnit,iSeason,iArea,iIter)+bycatch_harvest(iAge,minyr,iUnit,iSeason,iArea,iIter);
            expZ   = adtl::exp(-Z);
            }
             
         if (iAge == plusgrp && iSeason==nseasons)
            N  *= (-1.0/(expZ-1.0));
      
         for (iArea=1; iArea<=nareas; iArea++)
           {
            ReturnValue +=  availability(  iAge,minyr,iUnit,iSeason,iArea,iIter)*
                            N*adtl::exp(-m(      iAge,minyr,iUnit,iSeason,iArea,iIter)*m_spwn(      iAge,minyr,iUnit,iSeason,iArea,iIter)     
                      -FMult*(discards_sel(iAge,minyr,iUnit,iSeason,iArea,iIter)+landings_sel(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                              harvest_spwn(iAge,minyr,iUnit,iSeason,iArea,iIter))*
                                  stock_wt(iAge,minyr,iUnit,iSeason,iArea,iIter);
            }
         
         N *= expZ;
         }
      }

   return ReturnValue;
   }

adouble FLBRP::Recruits(adouble FMult, int iUnit, int iIter)
   {
   adouble spr     = SPR(FMult,iUnit,iIter),
          recruits = 1;

   //SSB as a function of SPR
   adouble ssb=1.0;
   switch(sr_model[iUnit]) 
      {
      case FLRConst_BevHolt: 
         ssb      = spr*sr_params(1,1,iUnit,1,1,iIter)-sr_params(2,1,iUnit,1,1,iIter);
         recruits = sr_params(1,1,iUnit,1,1,iIter)*ssb/(ssb+sr_params(2,1,iUnit,1,1,iIter));
      break;
         
      case FLRConst_Ricker:
         ssb      = adtl::log(spr*sr_params(1,1,iUnit,1,1,iIter))/sr_params(2,1,iUnit,1,1,iIter);
         recruits = sr_params(1,1,iUnit,1,1,iIter)*ssb*adtl::exp(-sr_params(2,1,iUnit,1,1,iIter)*ssb);
      break;
      
  	  case FLRConst_Cushing:
         ssb      =pow(1.0/(sr_params(1,1,iUnit,1,1,iIter)*spr),(1.0/(sr_params(2,1,iUnit,1,1,iIter)-1.0)));
         recruits =sr_params(1,1,iUnit,1,1,iIter)*pow(ssb,sr_params(2,1,iUnit,1,1,iIter));
      break;
      
      case FLRConst_Shepherd:
         ssb      =sr_params(2,1,iUnit,1,1,iIter)*pow(sr_params(1,1,iUnit,1,1,iIter)*spr-1.0,1/sr_params(3,1,iUnit,1,1,iIter));
         recruits = sr_params(1,1,iUnit,1,1,iIter)*ssb/(1.0+pow(ssb/sr_params(2,1,iUnit,1,1,iIter),sr_params(3,1,iUnit,1,1,iIter)));
      break;
              
      case FLRConst_SegReg:

//ifelse(ssb<=b,a*ssb,a*b)

       //ssb      = (spr < 1/sr_params(2,1,iUnit,1,1,iIter) ? 0.0 : spr*sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter));
       //recruits = (ssb < 1/sr_params(2,1,iUnit,1,1,iIter) ? 0.0 : ssb*sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter));
	   if (1/spr > sr_params(1,1,iUnit,1,1,iIter)) 
	      recruits = 0.0; 
	   else 
		   recruits = sr_params(1,1,iUnit,1,1,iIter)*sr_params(2,1,iUnit,1,1,iIter);
       break;
  
      case FLRConst_Mean: default:
         recruits = sr_params(1,1,iUnit,1,1,iIter);
      break;
      }

   return recruits;
   }

double FLBRP::YieldGrad(double FMult, int iIter)
   {
   adouble FMult_ad;
   FMult_ad = FMult;
   double seed=1.0;
   FMult_ad.setADValue(&seed);

   adouble ReturnValue = YPR(FMult_ad, 1, iIter)*Recruits(FMult_ad, 1, iIter);

   double RtnVal = *(ReturnValue.getADValue());
   double t      = ReturnValue.getValue();
   return RtnVal;
   }

double FLBRP::ProfitGrad(double FMult, int iIter)
   {
   adouble FMult_ad;
   FMult_ad = FMult;
    double seed=1.0;
   FMult_ad.setADValue(&seed);

   adouble ReturnValue = RPR(FMult_ad, 1, iIter)*Recruits(FMult_ad, 1, iIter);

   int iAge, iUnit, iSeason, iArea;
   for (iUnit=1; iUnit<=nunits; iUnit++)
       for (iSeason=1; iSeason<=nseasons; iSeason++)
         for (iArea=1; iArea<=nareas; iArea++)
            ReturnValue += FMult_ad*cost_var(1,minyr,iUnit,iSeason,iArea,iIter)-cost_fix(1,minyr,iUnit,iSeason,iArea,iIter);

   double RtnVal = *(ReturnValue.getADValue());
   double t      = ReturnValue.getValue();
   return RtnVal;
   }

double FLBRP::SSBGrad(double FMult, int iIter)
  {
  adouble result=0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += SPR(FMult_ad,iUnit,iIter)*Recruits(FMult_ad,iUnit,iIter);

   double RtnVal = *(result.getADValue());

   return RtnVal;
   }

double FLBRP::RecGrad(double FMult, int iIter)
  {
  adouble result=0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += Recruits(FMult_ad,iUnit,iIter);

   double RtnVal = *(result.getADValue());

   return RtnVal;
   }

double FLBRP::BiomassGrad(double FMult, int iIter)
  {
  adouble
  result=0.0;
  adouble FMult_ad;
  FMult_ad = FMult;
  double seed=1.0;
  FMult_ad.setADValue(&seed);

   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += BPR(FMult_ad,iUnit,iIter)*Recruits(FMult_ad,iUnit,iIter);

   double RtnVal = *(result.getADValue());

   return RtnVal;
   }

double FLBRP::ad_SSB(double FMult, int iIter)
   {
   adouble result = 0.0;
   adouble FMult_ad;
   FMult_ad = FMult;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += SPR(FMult_ad,iUnit,iIter)*Recruits(FMult_ad,iUnit,iIter);

   return *(result.getADValue());
   }

double FLBRP::ad_Biomass(double FMult, int iIter)
   {
   adouble result = 0.0;
   adouble FMult_ad;
   FMult_ad = FMult;
   for (int iUnit=1; iUnit<=nunits; iUnit++)
      result += BPR(FMult_ad,iUnit,iIter)*Recruits(FMult_ad,iUnit,iIter);

   return *(result.getADValue());}

extern "C" SEXPDLLExport InitialCond(SEXP xStk, SEXP xSRModel, SEXP xSRPar, SEXP xCtrl){
   FLStock stock(xStk);
   
   FLBRP   brp;
   
   brp.setSR(xSRModel,xSRPar);

   brp.minage   = stock.minquant;
   brp.maxage   = stock.maxquant;
   brp.minyr    =
   brp.maxyr    = 1;
   brp.nunits   = stock.nunits;
   brp.nseasons = stock.nseasons;
   brp.nareas   = stock.nareas;
   brp.niters   = stock.niters;

   brp.fbar.Init(           1,         1,         brp.minyr,  brp.maxyr, brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);

   brp.landings_sel.Init(   brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.discards_sel.Init(   brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.bycatch_harvest.Init(brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.stock_wt.Init(       brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.landings_wt.Init(    brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.discards_wt.Init(    brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.bycatch_wt.Init(     brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.m.Init(              brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.mat.Init(            brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.harvest_spwn.Init(   brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.m_spwn.Init(         brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.availability.Init(   brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.cost_var.Init(       brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.cost_fix.Init(       brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.price.Init(          brp.minage,brp.maxage,1,          1,         brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);

   brp.stock_n.Init(        brp.minage,brp.maxage,brp.minyr,  brp.maxyr, brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.discards_n.Init(     brp.minage,brp.maxage,brp.minyr,  brp.maxyr, brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.landings_n.Init(     brp.minage,brp.maxage,brp.minyr,  brp.maxyr, brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);
   brp.harvest.Init(        brp.minage,brp.maxage,brp.minyr,  brp.maxyr, brp.nunits, brp.nseasons, brp.nareas, brp.niters, 0.0);

   // Read in Biol Parameters from 1st FLStock year
   int iIter, iAge, iYr, iUnit, iSeason, iArea;
   for (iIter=1;             iIter<=brp.niters;        iIter++)
     for (iAge=brp.minage;     iAge<=brp.maxage;         iAge++)
       for (iUnit=1;             iUnit<=brp.nunits;        iUnit++)
         for (iSeason=1;           iSeason<=brp.nseasons;    iSeason++)
           for (iArea=1;             iArea<=brp.nareas;        iArea++){
             brp.landings_sel(   iAge,1,iUnit,iSeason,iArea,iIter) = stock.landings_n(  iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.discards_sel(   iAge,1,iUnit,iSeason,iArea,iIter) = stock.discards_n(  iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.stock_wt(       iAge,1,iUnit,iSeason,iArea,iIter) = stock.stock_wt(    iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.landings_wt(    iAge,1,iUnit,iSeason,iArea,iIter) = stock.landings_wt( iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.discards_wt(    iAge,1,iUnit,iSeason,iArea,iIter) = stock.discards_wt( iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.m(              iAge,1,iUnit,iSeason,iArea,iIter) = stock.m(           iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.mat(            iAge,1,iUnit,iSeason,iArea,iIter) = stock.mat(         iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.harvest_spwn(   iAge,1,iUnit,iSeason,iArea,iIter) = stock.harvest_spwn(iAge,stock.minyr,iUnit,iSeason,iArea,iIter);
             brp.m_spwn(         iAge,1,iUnit,iSeason,iArea,iIter) = stock.m_spwn(      iAge,stock.minyr,iUnit,iSeason,iArea,iIter);}


   for (iIter=1;             iIter<=brp.niters;        iIter++)
     for (iYr=brp.minyr;       iYr<=brp.maxyr;           iYr++)
       for (iUnit=1;             iUnit<=brp.nunits;        iUnit++)
         for (iSeason=1;           iSeason<=brp.nseasons;    iSeason++)
           for (iArea=1;             iArea<=brp.nareas;        iArea++)
               brp.fbar(1,iAge,iUnit,iSeason,iArea,iIter) = 0.1;

   // ensure relative abundance
   for (iIter=1; iIter<=brp.availability.niters(); iIter++)
     for (iAge=brp.availability.minquant(); iAge<=brp.availability.maxquant(); iAge++)
       for (iYr=brp.availability.minyr(); iYr<=brp.availability.maxyr(); iYr++)
         for (iUnit=1; iUnit<=brp.availability.nunits(); iUnit++)
           for (iSeason=1; iSeason<=brp.availability.nseasons(); iSeason++){
             double sum = 0.0;
             for (iArea=1; iArea<=brp.availability.nareas(); iArea++)
               sum += brp.availability(iAge,iYr,iUnit,iSeason,iArea,iIter);

             brp.availability(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;}

   // scale so mean catch.sel = 1.0
   for (iIter=1; iIter<=brp.discards_sel.niters(); iIter++)
     for (iYr=brp.discards_sel.minyr(); iYr<=brp.discards_sel.maxyr(); iYr++)
       for (iUnit=1; iUnit<=brp.discards_sel.nunits(); iUnit++)
         for (iArea=1; iArea<=brp.discards_sel.nareas(); iArea++)
            for (iSeason=1; iSeason<=brp.discards_sel.nseasons(); iSeason++)
             {
             double sum = 0.0;
             for (iAge=brp.minfbar; iAge<=brp.maxfbar; iAge++)
                sum += brp.discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) +
                       brp.landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter);

             sum /= (brp.maxfbar-brp.minfbar+1);

             for (iAge=brp.discards_sel.minquant(); iAge<=brp.discards_sel.maxquant(); iAge++){
               brp.discards_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;
               brp.landings_sel(iAge,iYr,iUnit,iSeason,iArea,iIter) /= sum;}}

   //brp.brp(SEXP Object);

   return(stock.Return());}
/*   
SEXP FLBRP::newBrp(SEXP Object){
   setRefpts(Object);
   
   calcRefpts();

   return(returnRefpts());}


SEXP FLBRP::setRefpts(SEXP Object, double ***D){
   SEXP v        = PROTECT(duplicate(GET_SLOT(Object, install(".Data")))),
        nRefpts  = GET_DIM(v),
        dRefpts = GET_DIMNAMES(v),
        v3;

   double *a     = NUMERIC_POINTER(v);
   char   name[12];
   const char *name_ = "";

   short dim[3], n = length(dims);

   if (n != 3)
      {
      UNPROTECT(1);

      return FALSE;

      }

   if (INTEGER(dims)[1] != 8)
      {
      UNPROTECT(1);

      return FALSE;
      }

   dim[0] = INTEGER(dims)[0];
   dim[1] = INTEGER(dims)[1];
   dim[2] = INTEGER(dims)[2];

   short iRef, iIter, i, j, k, l=0;

   //alloc
   aRefpts = new double**[dim[0]];
   for(i=0; i<dim[0]; i++) {
      aRefpts[i]  = new double*[dim[1]];
      for(j=0; j<dim[1]; j++)
         aRefpts[i][j] = new double[dim[2]];}

   for (k = 0; k < dim[2]; k++)
     for (j = 0; j < dim[1]; j++)
       for (i = 0; i < dim[0]; i++)
         aRefpts[i][j][k] = (a)[l++];

   return(dimnames);
   }

void FLBRP::calcRefpts(SEXP dimnames, double ***D){
   for (int iIter=0; iIter<dim[2]; iIter++)//iter
     for (int iRef=0; iRef<dim[0]; iRef++)//refpts
       {
       if (dimnames != R_NilValue)
         if (TYPEOF(dimnames) == VECSXP)
            name_ = CHAR(STRING_ELT(VECTOR_ELT(dimnames, 0), iRef));

       name[0] = '\0';
       strcpy(name,name_);
       for (int i=0; i< (signed)strlen(name); i++)
         name[i] = (signed)toupper(name[i]);

       //F ref pt
       if (strcmp(name, "MSY") == 0)
         aRefpts[iRef][RP_harvest][iIter] = FMSY(iIter+1);
       else if (strcmp(name, "MEY") == 0)
         aRefpts[iRef][RP_harvest][iIter] = FMEY(iIter+1);
   	   else if (strcmp(name, "F0.1") == 0)
         aRefpts[iRef][RP_harvest][iIter] = F0pt1(iIter+1);
       else if (strcmp(name, "SPR0") == 0)
         aRefpts[iRef][RP_harvest][iIter] = 0.0;
       else if (strcmp(name, "FMAX") == 0)
         aRefpts[iRef][RP_harvest][iIter] = FMax(iIter+1);
       else if (strcmp(name, "FPA") == 0 || strcmp(name, "FLIM") == 0)
         aRefpts[iRef][RP_harvest][iIter] = aRefpts[iRef][RP_harvest][iIter];
       else if (strcmp(name, "BPA") == 0 || strcmp(name, "BLIM") == 0 || strcmp(name, "CRASH") == 0){
         aRefpts[iRef][RP_harvest][iIter] =
	       aRefpts[iRef][RP_yield  ][iIter] =
	       aRefpts[iRef][RP_rec    ][iIter] =
	       aRefpts[iRef][RP_biomass][iIter] =
	       aRefpts[iRef][RP_revenue][iIter] =
	       aRefpts[iRef][RP_cost   ][iIter] =
	       aRefpts[iRef][RP_profit ][iIter] = R_NaReal;
	      if (strcmp(name, "CRASH") == 0) aRefpts[iRef][RP_ssb][iIter] = 0.0;

        }

        int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(aRefpts[iRef][RP_ssb][iIter]-SSB(x,iIter+1),2);
           dgdx = -2*(aRefpts[iRef][RP_ssb][iIter]-SSB(x,iIter+1))*SSBGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         //aRefpts[iRef][RP_harvest][iIter] = TargetSSB(aRefpts[iRef][RP_ssb     ][iIter],iIter);
         aRefpts[iRef][RP_harvest][iIter] = x;
         }
       else if (strncmp(name, "SPR.", 4) == 0)
         {
         const char *t;

         t = strtok(name, ".\t\n\0");
         t = strtok(NULL, ".\t\n\0");
         if (t != NULL)
            {
	         double val = atof(t)/100.0;

            aRefpts[iRef][RP_harvest][iIter] = FSPRPercMax(__max(0.0,__min(1.0,val)), iIter+1);
            }
         }
       //SPR ref pt
       else if (!R_IsNA(aRefpts[iRef][RP_rec][iIter]) && !R_IsNA(aRefpts[iRef][RP_ssb][iIter]))
         aRefpts[iRef][RP_harvest][iIter] = TargetSPR(aRefpts[iRef][RP_ssb    ][iIter]/aRefpts[iRef][RP_rec][iIter],iIter+1);
       //YPR ref pt
       else if (!R_IsNA(aRefpts[iRef][RP_yield][iIter]) && !R_IsNA(aRefpts[iRef][RP_rec][iIter]))
         aRefpts[iRef][RP_harvest][iIter] = TargetYPR(aRefpts[iRef][RP_yield][iIter]/aRefpts[iRef][RP_rec][iIter],iIter+1);
       //Y/S or exploitation rate ref pt
       else if (!R_IsNA(aRefpts[iRef][RP_yield][iIter]) && !R_IsNA(aRefpts[iRef][RP_ssb][iIter]) && R_IsNA(aRefpts[iRef][RP_harvest][iIter]) && R_IsNA(aRefpts[iRef][RP_rec][iIter]))
         aRefpts[iRef][RP_harvest][iIter] = TargetYS(aRefpts[iRef][RP_yield  ][iIter]/aRefpts[iRef][RP_ssb][iIter],iIter+1);
       //ssb
       else if (R_IsNA(aRefpts[iRef][RP_harvest][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_yield  ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_rec    ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_biomass][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_revenue][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_cost   ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_profit ][iIter]) && !R_IsNA(aRefpts[iRef][RP_ssb][iIter]))
         {

        int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(aRefpts[iRef][RP_ssb][iIter]-SSB(x,iIter+1),2);
           dgdx = -2*(aRefpts[iRef][RP_ssb][iIter]-SSB(x,iIter+1))*SSBGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         //aRefpts[iRef][RP_harvest][iIter] = TargetSSB(aRefpts[iRef][RP_ssb     ][iIter],iIter);
         aRefpts[iRef][RP_harvest][iIter] = x;
         }
       //rec
       else if (R_IsNA(aRefpts[iRef][RP_harvest][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_yield  ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_ssb    ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_biomass][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_revenue][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_cost   ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_profit ][iIter]) && !R_IsNA(aRefpts[iRef][RP_rec][iIter]))
         {
         int Iters=0;
 	      double x=0.1,f,dgdx;

		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(aRefpts[iRef][RP_rec][iIter]-Recruits(x,iIter+1),2);
           dgdx = -2*(aRefpts[iRef][RP_rec][iIter]-Recruits(x,iIter+1))*RecGrad(x,iIter+1);

           x = x - f / dgdx;
           }
        while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         aRefpts[iRef][RP_harvest][iIter] = x;
         }
       //biomass
       else if (R_IsNA(aRefpts[iRef][RP_harvest][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_yield  ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_rec    ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_ssb    ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_revenue][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_cost   ][iIter]) &&
                R_IsNA(aRefpts[iRef][RP_profit ][iIter]) && !R_IsNA(aRefpts[iRef][RP_biomass][iIter]))
         {
         int Iters=0;
		     double x=0.1,f,dgdx;
		     do
           {
           Iters++;
           //do Newton Raphson to estimate N

           f    = pow(aRefpts[iRef][RP_biomass][iIter]-Biomass(x,iIter+1),2);
           dgdx = -2*(aRefpts[iRef][RP_biomass][iIter]-Biomass(x,iIter+1))*BiomassGrad(x,iIter+1);

           x = x - f / dgdx;
           }
         while (fabs(f) >= QS_TOL && Iters <= QS_ITS);

         aRefpts[iRef][RP_harvest][iIter] = x;
         }

      if (!R_IsNA(aRefpts[iRef][RP_harvest][iIter])){
	      aRefpts[iRef][RP_yield  ][iIter] = yield(   aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_rec    ][iIter] = Recruits(aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_ssb    ][iIter] = SSB(     aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_biomass][iIter] = Biomass( aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_revenue][iIter] = Revenue( aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_cost   ][iIter] = Cost(    aRefpts[iRef][RP_harvest][iIter],iIter+1);
          aRefpts[iRef][RP_profit ][iIter] = Profit(  aRefpts[iRef][RP_harvest][iIter],iIter+1);}
       }
   }

void FLBRP::returnRefpts(SEXP Object){

    //Allocate memory
    PROTECT(v3 = Rf_allocArray(REALSXP, dims));

    //Create names for dimensions
    setAttrib(v3, R_DimNamesSymbol, dimnames);

   l = 0;
   for (k = 0; k < niters; k++)
     for (j = 0; j < dim[1]; j++)
       for (i = 0; i < nRefpts; i++)
          REAL(v3)[l++] = aRefpts[i][j][k];

   //unalloc
   for (i=0; i<dim[0]; i++){
      for (j=0; j<dim[1]; j++)
        delete [] (aRefpts[i][j]);

      delete [] (aRefpts[i]);
      }
    delete [] (aRefpts);

   SEXP refpts;

   PROTECT(refpts = NEW_OBJECT(MAKE_CLASS("refpts")));

   refpts = R_do_slot_assign(refpts, install(".Data"), v3);

   UNPROTECT(3);

   //return v3;
   return refpts;}
*/
