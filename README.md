# IRTF

This a package of functions for applying time series filtering techniques to IRT 
(Item Response Theory) models.  The aim is to integrate performance measures captured over 
weeks or months where the the assumption of consistency of the latent variable is not a 
reasonable assumption.

The key idea is that we are introducing a weight function _w(t(j),T)_ where _t(i)_ is the 
time of Observation _j_ , and _T_ is the current time.  The log-likelihood of the observation
is weighted by _w()_.  
