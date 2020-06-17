library(myUtils)
setwd("~/Projections/model_data/demos")

dta <- rio::import("sc-est2018-alldata6.csv")



# I want, for each state, percent of population male, age 18-34, age 35-64, age 64+,
# percent white non-hispanic, percent hispanic, percent black

stateSum <- function(state) {
    sub <- dta[dta$NAME == state, ]
    totIdx <- sub$SEX == 0 & sub$ORIGIN == 0
    
    youngIdx <- 18 <= sub$AGE & sub$AGE < 35 & totIdx
    midIdx <- 35 <= sub$AGE & sub$AGE < 65 & totIdx
    oldIdx <- 65 <= sub$AGE & totIdx
    
    whiteNonHispIdx <- sub$ORIGIN == 1 & sub$RACE == 1 & sub$SEX == 0
    blackIdx <- totIdx & sub$RACE == 2
    hispIdx <- sub$SEX == 0 & sub$ORIGIN == 2
    
    totals <- subSum(sub, totIdx)
    
    percent <- data.frame(rbind(
        subSum(sub, youngIdx) / totals,
        subSum(sub, midIdx) / totals,
        subSum(sub, oldIdx) / totals,
        subSum(sub, whiteNonHispIdx) / totals,
        subSum(sub, blackIdx) / totals,
        subSum(sub, hispIdx) / totals
    ))
    
    percent$GROUP <- c("age1834", "age3564", "age65plus", "whiteNonHisp", "black", "hisp")
    percent$STATE <- state
    percent <- tidyr::gather(percent, year, percent, POPESTIMATE2010:POPESTIMATE2018)
    percent$year <- gsub("[[:alpha:]]", "", percent$year) %>% as.numeric()
    colnames(percent) <- c("group", "state", "year", "pct")
    percent[,c("state", "group", "year", "pct")]
    
}




subSum <- function(data, idx) {
    apply(data[idx, grepl("POPEST", colnames(data))], 2, sum)
}


demos <- lapply(unique(dta$NAME), stateSum) %>% myBind()
save(demos, file = "demos_final.Rdata")



