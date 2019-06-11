
## Identify optimal budget allocation


## Load models -----------------------------------------#


## Optimization inputs ---------------------------------#

B = 100000 # budget

PDR_base <- 0.02138792
PDR_PatH <- 0.05 ## THIS NUMBER IS MADE UP ##


## Define response function ----------------------------#
# Inputs for optimization: Proportion allocated to each intervention
# Outputs: Infections averted (using prediction model)

f_response <- function(budget,outcomeModel,costModel){

  function(u1,u2){
    if (u1 < 0 | u1 >1){
      warning("u1 not in valid [0,1] region.")
      return(NA)}
    if (u2 <0 | u2 >1){
      warning("u2 not in valid [0,1] region.")
      return(NA)}
    if ((u1+u2)>1){
      warning("u1+u2 is greater than 1 (invalid allocation).")
      return(NA)
    }

    u3 <- B-u1*B-u2*B
  }

}
