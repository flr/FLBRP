##############################################################
#' noise
#'
#' A noise generator
#' 
#' @param   \code{n}, length of time series to generate
#' @param   \code{B}, autocorrelation, a real number in [0,1]
#' @param   \code{trunc}, get rid of first values equal to trunc, i.e. to allow burn in
#' @param   \code{sd}, standard deviation
#'
#' @export
#' @docType methods
#' @rdname noise
#'
#' @return A vector with autocorrelation equal to B and of length \code{n-trunc}
#' 
#' @references Ranta and Kaitala 2001 Proc. R. Soc.
#' vt = B * vt-1 + s * sqrt(1 - B^2)
#' s is normally distributed random variable with mean = 0, variance = 1
#' B is the autocorrelation parameter
#' @export
#' 
#' @examples
#' white <- noise(1000,B=0)
#' plot(white)
#' acf(white)
#' 
#' red <- noise(1000,B=0.8)
#' plot(red)
#' acf(white)
#' 
#' blue <- noise(1000,B=-0.8)
#' plot(blue)
#' acf(blue)
#' 
noise <- function(len,B=0,trunc=0,sd=1){
    mn <- 0
    x <- rep(0, len+1) # going to hack off the first value at the end
    s <- rnorm(len,mean=mn,sd=1)
    
    for(i in 1:len){
      x[i+1] <- B * x[i] + s[i] * sqrt(1 - B^2)
      if(trunc>0){
        if (x[i+1] > (1-trunc))  x[i+1] <- ( 1-trunc)
        if (x[i+1] < (-1+trunc)) x[i+1] <- (-1+trunc)}
        }
    
    x<-x[-1]
    
    return(x)}

## cohort effects
coEff=function(x,sd=1,B=0){
  
  dev        =log(rlnorm(length(dmns$cohort),0,cv))   
  for(i in 2:(length(dev)))
    dev[i]=dev[i]+dev[i-1]*rho
  parC[var]=parC[var]*exp(dev)
  
  tmp=len2wt(parC,vonB(parC[c("linf","t0","k")],ages(FLCohort(m(stk)))))
  res=window(as(tmp,"FLQuant"),start=1,end=dims(m(stk))$year)
  
  res}


cEff=function(stk,par,sigma,rho=0,var="k"){
  
  cv=sigma #getS(sigma,rho)
  dmns       =dimnames(par)
  dmns$cohort=dimnames(FLCohort(m(stk)))$cohort
  parC       =FLPar(rep(c(par),length(dmns$cohort)),dimnames=dmns[c(1,3,2)])
  units(parC)=""  
  
  dev        =log(rlnorm(length(dmns$cohort),0,cv))   
  for(i in 2:(length(dev)))
       dev[i]=dev[i]+dev[i-1]*rho
  parC[var]=parC[var]*exp(dev)
  
  tmp=len2wt(parC,vonB(parC[c("linf","t0","k")],ages(FLCohort(m(stk)))))
  res=window(as(tmp,"FLQuant"),start=1,end=dims(m(stk))$year)
  
  res}


#Spectral analysis function
##############################################################
#' spectra
#'
#' A noise generator
#' 
#' @param   \code{n}, length of time series to generate
#' @param   \code{B}, autocorrelation, a real number in [0,1]
#' @param   \code{trunc}, get rid of first values equal to trunc, i.e. to allow burn in
#' @param   \code{sd}, standard deviation
#'
#' @export
#' @docType methods
#' @rdname noise
#'
#' @return A vector with autocorrelation equal to B and of length \code{n-trunc}
#' 
#' @references Ranta and Kaitala 2001 Proc. R. Soc.
#' vt = B * vt-1 + s * sqrt(1 - B^2)
#' s is normally distributed random variable with mean = 0, variance = 1
#' B is the autocorrelation parameter
#' @export
#' 
#' @examples
#' white <- noise(1000,B=0)
#' plot(white)
#' acf(white)
#' 
#' red <- noise(1000,B=0.8)
#' plot(red)
#' acf(white)
#' 
#' blue <- noise(1000,B=-0.8)
#' plot(blue)
#' acf(blue)
#' 
spectra <- function(x,fs=1,norm = FALSE, pl = TRUE,omit=-(1:5))
{
  # Pad x with zeroes to make it's length a power of 2, i.e. length should be 2^something
  # This makes the fft faster
  oldx <- x # keep for later
  if(norm == TRUE) x <- x - mean(x)
  nfft <- (2^ceiling(log2(length(x))))
  x[(length(x)+1): nfft] <- 0
  fftx <- fft(x)
  # It's symmetrical so throw away second half. Only first 1 + nfft points are unique
  NoUnPo <- ceiling((nfft+1)/2) # Number of unique points
  fftx <- fftx[1:NoUnPo]
  # First element is DC component, last is the Nyquist component
  
  # Take magnitude of fft of x and scale the fft so that it is not a function of length
  mx <- abs(fftx) / length(x)
  # Take square of magnitude
  mx <- mx^2
  
  # As we dropped the first half of fft so multiply by 2 to keep same energy
  # DC component (first element) and Nyquist component (last element if even
  # number points (should be)) should not be multiplied by 2
  mx[2:(length(mx)-1)] <- mx[2:(length(mx)-1)] * 2
  
  f <- seq(from =0, to = NoUnPo-1) * fs/nfft # frequency axis for plot

  return(as.data.frame(list(mx = mx, f = f)))}

#ggplot(spectra(x))+geom_line(aes(f,mx))

