library(myUtils)
setwd("~/Projections/Cook Results")

toStateAbr <- function(s) {
    for (i in rev(1:length(state.name))) {
        s <- gsub(state.name[i], state.abb[i], s)
    }
    
    s
}

pviTrans <- function(pvi) {
    if (pvi == "EVEN") {
        0
    } else {
        ifelse(substr(pvi, 1, 1) == "D", 1, -1) * as.numeric(gsub("[^0-9]", "", pvi))
    }
}



cleanToModel <- function(dta, type) {
    dta$Loc <- toStateAbr(dta$Loc)
    dta$dem2pct <- dta$Dem_Pct / (dta$Dem_Pct + dta$Gop_Pct)
    dta$PVI <- sapply(dta$PVI, pviTrans)
    
    data.frame(loc = dta$Loc, 
               year = dta$Year,
               rating = dta$Code,
               pvi = dta$PVI, 
               demPct = dta$dem2pct,
               type = type)
}


for (f in c("sen", "pres", "gov")) {
    eval(parse(text = paste0('dta.', f, ' <- cleanToModel(rio::import(paste0(f, ".xlsx")), f)')))
    eval(parse(text = paste0('save(dta.', f, ', file = paste0("C:/Users/mattg/Documents/Projections/model_data/ratings/", f, ".Rdata"))')))
}



