library(myUtils) # to use, run devtools::install_github("mattgarber4/myUtils")
library(Rcpp)
library(ggplot2)
library(fiftystater)
setwd("C:/Users/mattg/Documents/Projections")
sourceCpp("cpp/simFuns.cpp")
source("viewer/simulation_utils.R")

# State label function
translateToState <- function(states) {
    out <- state.name[match(states, state.abb)]
    out[states == "DC"] <- "district of columbia"
    tolower(out)
}

# For maps - gives color on line in 3-space between demBlue and gopRed. Parameter t = 1 gives demBlue, t = 0 gives gopRed
linearParam <- function(t) {
    val <- sapply(t, function(m) col2rgb(demBlue) + (1 - m) * (col2rgb(gopRed) - col2rgb(demBlue)))
    rgb(val[1,], val[2, ], val[3, ], maxColorValue = 255)
}

# Maps dem percent along split graph - more blue for about 50%, more red for below
colorMapper <- function(demPct) {
    ifelse(demPct < .5, linearParam(3 * demPct / 4), linearParam(5 / 8 + 3 * (demPct - .5) / 4))
}

# shifts a vector of probs via function f: R -> R f(t) = {t if 0<t<1, 0 if t<0, 1 if t>1}
rectifiedShift <- function(vec, shift) {
    (vec + shift) %>% pmax(0) %>% pmin(1)
}

# import modelled probs
data <- rio::import("modelled_cook_probs.csv")

# initialize a map
#simMapGreedy <- refClassMap("electionSim")
simMapLazy <- refClassMap("electionSim")

# for the give coefs and shifts, simulate 20000 elections and store
for (coef in dependenceCoefs) {
    for (shift in shifts) {
        # simMapGreedy$set(coef, shift, 
        #            electionSim(
        #                simulator = simulateGreedy,
        #                trials = 20000, 
        #                dependence = coef, 
        #                initialProbs = rectifiedShift(data$prob, shift), 
        #                initialVotes = data$votes, stateKeys = data$state)
        #            )
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
#saveRDS(simMapGreedy, file = "viewer/simsGreedy.rds")
saveRDS(simMapLazy, file = "viewer/simsLazy.rds")
