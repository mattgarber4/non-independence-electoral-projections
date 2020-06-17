setwd("model_data/demos")
library(rvest)


convertToPct <- function(str) {
    idx <- gregexpr("(?=[0-9])", str, perl = T)[[1]]
    as.numeric(substr(str, idx[1], idx[length(idx)])) / 100
}

stateData <- function(state) {
    cat(state)
    cat("\n")
    html <- read_html(paste0("https://www.census.gov/quickfacts/fact/table/", state, "/PST045219"))
    t <- html_table(html)[[2]]
    age <- t[grepl("Persons 65 years and over, percent", t$Population),2]
    white <- t[grepl("White alone, not Hispanic or Latino, percent", t$Population),2]
    black <- t[grepl("Black or African American alone, percent", t$Population), 2]
    hisp <- t[grepl("^Hispanic or Latino, percent", t$Population), 2]
    
    out <- data.frame(
        state = state, 
        age65plus = convertToPct(age), 
        whiteNonHisp = convertToPct(white), 
        black = convertToPct(black),
        hisp = convertToPct(hisp)
        )
    return(out)
}

stateDemos <- lapply(state.abb, stateData) %>% myBind()
cds <- rio::import("cd_demos.xlsx")

cds$age65plus <- cds$age65plus / cds$tot
cds$whiteNonHisp <- cds$whiteNonHisp / cds$tot
cds$black <- cds$black / cds$tot
cds$hisp <- cds$hisp / cds$tot
colnames(cds)[1] <- "state"
cds$tot <- NULL

demos2020 <- rbind(stateDemos, cds)
demos2020$state <- as.character(demos2020$state)
demos2020[dim(demos2020)[1] + 1, ] <- c("DC", .119, .362, .469, .109)

save(demos2020, file = "~/non-independence-electoal-projections/current_data/2020_demos.Rdata")
