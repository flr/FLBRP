# FLBRP 2.6.0

## NEW FEATURES

- FLBRP(FLStockR) and FLBNRP(FLStock, predictModel) added to methods list.

# FLBRP 2.5.8

## BUG FIXES

- set outputs if <0 in case -ve RPs.

## NEW FEATURES

- New properties method.
- Able to field refpts if yield is available.

# FLBRP 2.5.7

## NEW FEATURES

- Method "+" to add new reference points to an FLBRP.


# FLBRP 2.5.4

## USER-VISIBLE CHANGES

- plot(FLBRP) will now by default plot all refpts in object@refpts


# FLBRP 2.5.3

## NEW FEATURES

- New code for fwdWindow(FLStock, FLBRP)
- spr(FLBRP) and spr0(FLBRP) are now implemented
- A bare vignette now points at FLBRP/FLBRP tutorial in http://flr-project.org

## USER-VISIBLE CHANGES

- Many life history-related functions and methods have been moved to the FLife package.

## BUG FIXES

## UTILITIES

## DOCUMENTATION

## DEPRECATED & DEFUNCT
- fwdwindow(FLStock, FLBRP) only has 'end' argument for call to window(FLStock)
  as 'start' and 'frequency' are set to the first year of the FLStock object and
  1 respectively.
