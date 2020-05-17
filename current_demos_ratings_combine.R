library(myUtils)
setwd("C:/Users/mattg/Documents/Projections")

states <- rio::import("current_data/cook_ratings.xlsx")
pviTrans <- function(pvi) {
    if (pvi == "EVEN") {
        0
    } else {
        ifelse(substr(pvi, 1, 1) == "D", 1, -1) * as.numeric(gsub("[^0-9]", "", pvi))
    }
}
states$pvi <- sapply(states$pvi, pviTrans)


load("current_data/2020_demos.Rdata")

states <- merge(states, demos2020, by = "state")
for (i in 2:dim(states)[2]) {
    states[,i] <- as.numeric(states[,i])
}


save(states, file = "current_data/ratings_with_demos.Rdata")
