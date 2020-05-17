library(myUtils)
library(Rcpp)
library(ggplot2)
library(fiftystater)
setwd("C:/Users/mattg/Documents/Projections")
sourceCpp("cpp/simFuns.cpp")

simulate <- function(probs, electoralCollege, coef) {
    stateID <- sample(1:length(probs), length(probs), prob = electoralCollege / 538, replace = F)
    simInOrder(stateID, probs, electoralCollege, coef)
}

multiSim <- function(n, probs, electoralCollege, coef, names = NULL) {
    s <- lapply(1:n, function(n) simulate(probs, electoralCollege, coef)) %>% myUtils::myBind()
    if (n > 1) {
        colnames(s) <- names
        list(stateMean = apply(s, 2, mean),
             elections = apply(s, 1, function(row) sum(electoralCollege * row))
        )
    } else {
        vec <- 1 * s[,1]
        names(vec) <- names
        list(stateMean = vec,
             elections = sum(electoralCollege * s[,1]))
    }
}

plotElection <- function(sims, sub = NULL) {
    hist(
        sims$elections,
        breaks = c(0, seq(4, 268, 8), seq(270, 538, 8), 538),
        col = c(rep(gopRed, 34), "black", rep(demBlue, 34)),
        axes = F,
        xlim = c(0, 538),
        xlab = "Democratic Electoral Votes",
        #ylim = c(0, .008),
        ylab = "",
        main = "Simulated Elections",
        sub = paste0(sub, 
                     " | SD = ", 
                     round(sd(sims$elections), 2), 
                     " | Dem Win % = ", 
                     100 * sum(sims$elections >= 270) / length(sims$elections) %>%
                         round(1), 
                     "%")
    )
    
    abline(v = 270)
    
}


translateToState <- function(states) {
    out <- state.name[match(states, state.abb)]
    out[states == "DC"] <- "district of columbia"
    tolower(out)
    
}

mapElection <- function(sim, colorMap) {
    plotData <- data.frame(state = translateToState(names(sim$stateMean)),
                           val = sim$stateMean)
    plotData <- plotData[!is.na(plotData$state),]
    plotData <- merge(fifty_states, plotData, by.x = "id", by.y = "state")
    ggplot(data = plotData) +
         geom_polygon(aes(long, lat, group = group), fill = colorMap(plotData$val)) +
         geom_polygon(aes(long, lat, group = group), fill = NA, col = "white", lwd = .15) +
         coord_quickmap() + 
         theme_void()
    
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


data <- rio::import("modelled_cook_probs.csv")

dependenceCoefs <- c(0, seq(0.0001, 0.001, 0.0001), seq(0.002, 0.01, 0.001), seq(0.02, 0.1, 0.005), seq(0.2, 1, 0.1), seq(2, 4, 1))
sims <- lapply(dependenceCoefs,
               function(n) {print(n); multiSim(10000, data$prob, data$votes, n, data$state)})

for (i in length(dependenceCoefs):1) {
    #plotElection(sims[[i]], sub = paste0("Dependence Coef: ", dependenceCoefs[i]))
    mapElection(sims[[i]], colorMapper)
}


mapElection(sims[[1]], function(v) ifelse(v > .52, demBlue, ifelse(v < .48, gopRed, "gray")))
mapRandomIndependent <- function() {
    mapElection(multiSim(1, data$prob, data$votes, 0), function(v) ifelse(v > .5, demBlue, gopRed), data$state)
}

mapRandomIndependent()



