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
                               
                               plot = function(sub = NULL, main = TRUE, as.dem = T) {
                                   if (main == TRUE) {
                                       m <- "Simulated Elections"
                                   } else if (main == FALSE) {
                                       m <- NULL
                                   } else {
                                       m <- main
                                   }
                                   
                                   if (as.dem) {
                                       els <- elections
                                       cols <- c(rep(gopRed, 34), "black", rep(demBlue, 34))
                                       lab <- "Democratic Electoral Votes"
                                   } else {
                                       els <- 538 - elections
                                       cols <- c(rep(demBlue, 34), "black", rep(gopRed, 34))
                                       lab <- "Republican Electoral Votes"
                                   }
                                   
                                   par(lwd = .1)
                                   hist(
                                       els,
                                       breaks = c(0, seq(4, 268, 8), seq(270, 538, 8), 538),
                                       col = cols,
                                       axes = F,
                                       xlim = c(0, 538),
                                       xlab = lab,
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
                               
                               plotMeans = function(main = NULL, as.dem = T) {
                                   if (as.dem) {
                                       vals <- stateMeans
                                       cols <- c(demBlue, gopRed)
                                       pct <- winPct
                                   } else {
                                       vals <- 1 - stateMeans
                                       cols <- c(gopRed, demBlue)
                                       pct <- 1 - winPct
                                   }
                                   idx <- order(vals)
                                   len <- length(stateMeans)
                                   barplot(rbind(vals[idx], 1 - vals[idx]),
                                           ylim = c(0, 1),
                                           col = cols,
                                           width = 1,
                                           space = c(0, rep(.1, len - 1)),
                                           las = 2,
                                           axes = F,
                                           border = F,
                                           main = main
                                   )
                                   segments(
                                       x0 = 0,
                                       x1 = len * 1.1 - .1,
                                       y0 = pct)
                                   text(len * 1.1 + .75,
                                        pct,
                                        paste0(round(100 * pct, 1), "%"), 
                                        pos = 4, 
                                        xpd = T)
                                   Arrowhead(x0 = len * 1.1 + .2, 
                                             y0 = pct, 
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

plotMeanLine <- function(simMap, bias, as.dem = T, highlighted = NULL) {
    if (as.dem) {
        prim <- demBlue
        alt <- demBlueAlt
        white <- demBlueWhite
        party <- "Democrats"
    } else {
        prim <- gopRed
        alt <- gopRedAlt
        white <- gopRedWhite
        party <- "Republicans"
    }
    d <- sapply(dependenceCoefs, function(coef) {
        sim <- simMap$get(coef, bias)
        c(sim$average, quantile(sim$elections, c(.25, .75)), sim$winPct)
    }) %>% t() %>% as.data.frame()
    colnames(d) <- c('avg', 'q1', 'q3', 'winPct')
    
    if (!as.dem) {
        d$avg <- 538 - d$avg
        d$winPct <- 1 - d$winPct
        tmp <- 538 - d$q1
        d$q1 <- 538 - d$q3
        d$q3 <- tmp
    }
    
    d$pos <-  c(0, 
                1:10 / 10, 
                1 + 1:18 / 18, 
                2 + 1:7 / 7, 
                3 + 1:6 / 6, 
                4 + 1:9 / 9, 
                5 + 1:3 / 6)
    # canvas
    plot(d$pos, d$avg, 
         xaxt = 'n', 
         yaxt = 'n',
         ylim = c(0, 538),
         type = 'n', 
         pch = 16,
         cex = 1.5,
         bty = 'l',
         xlab = 'Dependence Coefficient',
         ylab = '',
         col = prim)
    
    # grid
    segments(x0 = -.25, 
             x1 = 5.75, 
             y0 = seq(0, 1, .125) * 538, 
             lty = 'dotted', 
             col = 'lightgray')
    segments(x0 = seq(0, 5.5, .5), 
             y0 = 538 * -1 * .25 * .2, 
             y1 = 538, 
             lty = 'dotted', 
             col = 'lightgray')
    
    # conf interval
    polygon(x = c(d$pos, rev(d$pos)), 
            y = c(d$q1, rev(d$q3)),
            border = NA,
            col = withTrans(prim, .15))
    
    # EC means
    points(d$pos, d$avg, 
           pch = 16,
           cex = 2,
           col = prim)
    lines(d$pos, d$avg,
          col = prim, 
          lwd = 2)
    
    if (!is.null(highlighted)) {
        idx <- which(near(dependenceCoefs, highlighted))
        points(d$pos[idx], d$avg[idx], pch = 21, bg = white, col = prim, cex = 2.5, lwd = 5)
    }
    
    points(d$pos, 538 * d$winPct, pch = 15, cex = 1.5, col = alt)
    lines(d$pos, 538 * d$winPct, lwd = 1.5, col = alt)
    
    if (!is.null(highlighted)) {
        points(d$pos[idx], 538 * d$winPct[idx], pch = 22, bg = white, col = alt, cex = 2.5, lwd = 5)
    }
    axis(1, at = d$pos, labels = paste0(round(100 * dependenceCoefs, 2), "%"))
    axis(2, at = c(0, 134.5, 269, 403.5, 538), 
         labels = c("0", "", "50%", "", "100%"),
         las = 1)
    legend(x = 0,
           y = 600, 
           pt.cex = 1.5,
           pt.lwd = 1.5,
           legend = c(paste0("Average % of electoral votes for ", party, "\t\t\t"), 
                      paste0("% of simulated elections ", party, " win")), 
           bty = 'n', 
           pch = c(16, 15), 
           lty = 'solid', 
           col = c(prim, alt), 
           xpd = T, 
           horiz = T)
}


withTrans <- function(color, transp) {
    col <- col2rgb(color)
    rgb(col[1,1], col[2, 1], col[3, 1], alpha = transp * 255, maxColorValue = 255)
}
