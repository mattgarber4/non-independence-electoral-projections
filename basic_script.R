library(myUtils)
library(Rcpp)
library(ggplot2)
library(fiftystater)
library(aoos)
library(hash)
setwd("C:/Users/mattg/Documents/Projections")
sourceCpp("cpp/simFuns.cpp")

translateToState <- function(states) {
    out <- state.name[match(states, state.abb)]
    out[states == "DC"] <- "district of columbia"
    tolower(out)
    
}

linearParam <- function(t) {
    val <- sapply(t, function(m) col2rgb(demBlue) + (1 - m) * (col2rgb(gopRed) - col2rgb(demBlue)))
    rgb(val[1,], val[2, ], val[3, ], maxColorValue = 255)
}

colorMapper <- function(demPct) {
    ifelse(demPct < .5, linearParam(3 * demPct / 4), linearParam(5 / 8 + 3 * (demPct - .5) / 4))
}

rectifiedShift <- function(vec, shift) {
    (vec + shift) %>% pmax(0) %>% pmin(1)
}

electionSim <- setRefClass("electionSim",
                           fields = list(
                               trials = "numeric",
                               dependence = "numeric",
                               average = "numeric",
                               winPct = "numeric",
                               initialProbs = "numeric",
                               initialVotes = "numeric",
                               stateKeys = "character",
                               stateMeans = "numeric",
                               elections = "numeric"
                           ),
                           methods = list(
                               initialize = function(trials, dependence, initialProbs, initialVotes, stateKeys) {
                                   stateKeys <<- stateKeys
                                   initialProbs <<- initialProbs
                                   initialVotes <<- initialVotes
                                   trials <<- trials
                                   dependence <<- dependence
                                   s <- lapply(1:trials, 
                                               function(n) simulate(initialProbs, initialVotes, dependence)
                                               ) %>% myUtils::myBind()
                                   if (trials > 1) {
                                       colnames(s) <- stateKeys
                                       stateMeans <<- apply(s, 2, mean)
                                       elections <<- apply(s, 1, function(row) sum(initialVotes * row))
                                   } else {
                                       vec <- 1 * s[,1]
                                       names(vec) <- names
                                       stateMeans <<- vec
                                       elections <<- sum(initialVotes * s[,1])
                                   }
                                   
                                   average <<- mean(elections)
                                   winPct <<- sum(elections >= 270) / trials
                                   
                               },
                               
                               map = function(colorMap) {
                                   plotData <- data.frame(state = translateToState(stateKeys),
                                                          val = stateMeans)
                                   plotData <- plotData[!is.na(plotData$state),]
                                   plotData <- merge(fiftystater::fifty_states, plotData, by.x = "id", by.y = "state")
                                   ggplot(data = plotData) +
                                       geom_polygon(aes(long, lat, group = group), fill = colorMap(plotData$val)) +
                                       geom_polygon(aes(long, lat, group = group), fill = NA, col = "white", lwd = .15) +
                                       coord_quickmap() + 
                                       theme_void()
                                   
                               },
                               
                               plot = function(sub = NULL) {
                                   hist(
                                       elections,
                                       breaks = c(0, seq(4, 268, 8), seq(270, 538, 8), 538),
                                       col = c(rep(gopRed, 34), "black", rep(demBlue, 34)),
                                       axes = F,
                                       xlim = c(0, 538),
                                       xlab = "Democratic Electoral Votes",
                                       ylab = "",
                                       main = "Simulated Elections",
                                       sub = paste0(sub, 
                                                    " | SD = ", 
                                                    round(sd(elections), 2), 
                                                    " | Dem Win % = ", 
                                                    100 * winPct %>%
                                                        round(1), 
                                                    "%")
                                   )
                                   
                                   abline(v = 270)
                                   
                               }
                           ))
electionSim$lock(names(electionSim$fields()))

refClassMap <- setRefClass("refClassMatrix",
                           fields = list(
                               .hashTable = "environment",
                               .class = "character"
                           ),
                           contains = "Private",
                           methods = list(
                               set = function(row, col, ref) {
                                   key <- paste(row, col, sep = ",")
                                   if (is.null(ref)) {
                                       .hashTable$del(key)
                                       return(out)
                                   }
                                   
                                   if (class(ref) != .class) {
                                       stop(paste0("May only add ", class, " instances"))
                                   }
                                   
                                   out <- .hashTable[[key]]
                                   .set(.hashTable, key, ref)
                                   return(out)
                                   
                               },
                               get = function(row, col) {
                                   key <- paste(row, col, sep = ",")
                                   if (length(key) > 1) {
                                       return(sapply(key, function(k) .hashTable[[k]]))
                                   } else {
                                       return(.hashTable[[key]])
                                   }
                                   
                               },
                               size = function() {
                                   return(.hashTable$length())
                               },
                               initialize = function(class) {
                                   .class <<- class
                                   .hashTable <<- hash()
                               }
                               
                           ))


data <- rio::import("modelled_cook_probs.csv")
simMat <- refClassMap("electionSim")
dependenceCoefs <- c(0, 
                     seq(0.0001, 0.001, 0.0001), 
                     seq(0.002, 0.01, 0.0005), 
                     seq(0.015, 0.04, 0.005),
                     seq(.05, .1, .01),
                     seq(0.2, 1, 0.1), 
                     seq(2, 4, 1))
shifts <- c(-1, -.5, -.25, -.1, -.075, seq(-.05, .05, .01), .075, .1, .25, .5, 1)
for (coef in dependenceCoefs) {
    for (shift in shifts) {
        simMat$set(coef, shift, 
                   electionSim(trials = 25000, 
                               dependence = coef, 
                               initialProbs = rectifiedShift(data$prob, shift), 
                               initialVotes = data$votes, 
                               stateKeys = data$state))
        print(paste0("coef: ", coef, ", shift: ", shift))
    }
}

save(simMat, file = "simulations_map.Rdata")
mapRandomIndependent <- function() {
    electionSim(trials = 1, 
                initialProbs = data$prob, 
                initialVotes = data$votes, 
                dependence = 0, 
                stateKeys = data$state)$map(
                    function(v) {
                        ifelse(v > .5, demBlue, gopRed)
                        }
                    )
}

