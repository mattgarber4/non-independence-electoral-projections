library(myUtils)
load("model_data/final.Rdata")
load("current_data/ratings_with_demos.Rdata")
library(MASS)

# build model
dta <- merge(dta, data.frame(state = states$state, pop = states$votes), all.x = T, all.y = F)

model <- rlm(demPct ~ factor(rating) + pvi + age65plus + black + hisp + whiteNonHisp, data = dta[dta$type != "gov", ], weights = log(pop))
summary(model)



stateProbs <- data.frame(predict(model, newdata = states, interval = "predict"))
stateProbs$state <- states$state
stateProbs$sd <- (stateProbs$upr - stateProbs$lwr) / 2
stateProbs$prob <- pnorm(.5, mean = stateProbs$fit, sd = stateProbs$sd, lower.tail = F)

write.csv(data.frame(state = stateProbs$state, votes = states$votes, prob = stateProbs$prob), 
          file = "modelled_cook_probs.csv",
          row.names = F)
