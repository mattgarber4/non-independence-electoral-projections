library(myUtils) # to use, run devtools::install_github("mattgarber4/myUtils")
library(Rcpp)
library(ggplot2)
library(fiftystater)
setwd("~/Projections")
sourceCpp("cpp/simFuns.cpp")
source("viewer/simulation_utils.R")

# shifts a vector of probs via function f: R -> R f(t) = {t if 0<t<1, 0 if t<0, 1 if t>1}
rectifiedShift <- function(vec, shift) {
    (vec + shift) %>% pmax(0) %>% pmin(1)
}

# import modelled probs
data <- rio::import("modelled_cook_probs.csv")

# initialize a map
simMapLazy <- refClassMap("electionSim")

# for the give coefs and shifts, simulate 20000 elections and store
for (coef in dependenceCoefs) {
    for (shift in shifts) {
        simMapLazy$set(coef, shift, 
                         electionSim(
                             simulator = simulateLazy,
                             trials = 20000, 
                             dependence = coef, 
                             initialProbs = rectifiedShift(data$prob, shift), 
                             initialVotes = data$votes, stateKeys = data$state)
        )
        print(paste0("coef: ", coef, ", shift: ", shift))
    }
}

# save output map
saveRDS(simMapLazy, file = "viewer/simsLazy.rds")
