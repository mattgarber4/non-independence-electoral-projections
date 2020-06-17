library(myUtils)
setwd("model_data")

load("demos/demos_final.Rdata")
load("ratings/pres.Rdata")
load("ratings/sen.Rdata")
load("ratings/gov.Rdata")

dta <- rbind(dta.gov, dta.sen, dta.pres)
dta <- dta[dta$loc %in% state.abb, ]
colnames(dta)[colnames(dta) == "loc"] <- "state"
demos <- tidyr::spread(demos, key = group, value = pct)
for (i in rev(1:length(state.abb))) {
    demos$state <- gsub(state.name[i], state.abb[i], demos$state)
}

dta <- merge(dta, demos, by = c("state", "year"))

save(dta, file = "final.Rdata")
