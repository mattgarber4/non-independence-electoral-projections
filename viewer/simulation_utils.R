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
                                       sub = sub
                                   )
                                   
                                   abline(v = 270)
                                   
                               }
                           ))
electionSim$lock(names(electionSim$fields()))
