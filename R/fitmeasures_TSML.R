## fit measures for TSML
## robust (adjusted for missing data) fit indices for TSML
## These are aleady implemented for method="two.stage" in lavaan,
## With equations essentially unchanged
## Let's try to pull code from there
## Update: I cannot actually find any specific code for this implementation
## Maybe because the equation is the same, it is just that Gamma gets
## Replaced with what it was in Stage1 (or for us, Stage1a), so no
## Special computations are necessary
## In that case, in the twostage object, we may chose to override the slot storing ## Gamma

## two stage vcov matrix is computed in:
## lav_model_vcov.R
