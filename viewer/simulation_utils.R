dependenceCoefs <- c(0, 
                     seq(0.0001, 0.001, 0.0001),
                     seq(0.0015, 0.01, 0.0005),
                     seq(0.015, 0.045, 0.005),
                     seq(.05, .1, .01),
                     seq(0.2, 1, 0.1),
                     seq(2, 4, 1))
shifts <- c(-1, -.5, -.25, -.1, -.075, seq(-.05, .05, .01), .075, .1, .25, .5, 1)


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
                               elections = "numeric",
                               simulator = "function"
                           ),
                           methods = list(
                               initialize = function(simulator, trials, dependence, initialProbs, initialVotes, stateKeys) {
                                   simulator <<- simulator
                                   stateKeys <<- stateKeys
                                   initialProbs <<- initialProbs
                                   initialVotes <<- initialVotes
                                   trials <<- trials
                                   dependence <<- dependence
                                   s <- lapply(1:trials, 
                                               function(n) simulator(initialProbs, initialVotes, dependence)
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
                               
                               plot = function(sub = NULL, main = TRUE) {
                                   if (main == TRUE) {
                                       m <- "Simulated Elections"
                                   } else if (main == FALSE) {
                                       m <- NULL
                                   } else {
                                       m <- main
                                   }
                                   par(lwd = .1)
                                   hist(
                                       elections,
                                       breaks = c(0, seq(4, 268, 8), seq(270, 538, 8), 538),
                                       col = c(rep(gopRed, 34), "black", rep(demBlue, 34)),
                                       axes = F,
                                       xlim = c(0, 538),
                                       xlab = "Democratic Electoral Votes",
                                       ylab = "",
                                       main = m,
                                       sub = sub
                                   )
                                   
                                   text(x = c(0, 269, 538),
                                        y = 0,
                                        pos = 1,
                                        xpd = T,
                                        labels = c("0", "269", "538"))
                                   segments(x0 = 269, y0 = 0, y1 = 10)
                                   
                                   
                               },
                               
                               plotMeans = function(main = NULL) {
                                   idx <- order(sim$stateMeans)
                                   len <- length(sim$stateMeans)
                                   barplot(rbind(sim$stateMeans[idx],1 - sim$stateMeans[idx]),
                                           ylim = c(0, 1),
                                           col = c(demBlue, gopRed),
                                           width = 1,
                                           space = c(0, rep(.1, length(sim$stateMeans) - 1)),
                                           las = 2,
                                           axes = F,
                                           border = F,
                                           main = main
                                   )
                                   segments(
                                       x0 = 0,
                                       x1 = len * 1.1 - .1,
                                       y0 = sim$winPct)
                                   text(len * 1.1 + .75,
                                        sim$winPct,
                                        paste0(round(100 * sim$winPct, 1), "%"), 
                                        pos = 4, 
                                        xpd = T)
                                   Arrowhead(x0 = len * 1.1 + .2, 
                                                    y0 = sim$winPct, 
                                                    arr.adj = 1, 
                                                    angle = 180,
                                                    arr.type = 'triangle',
                                                    xpd = T)
                               }
                           ))
electionSim$lock(names(electionSim$fields()))

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
