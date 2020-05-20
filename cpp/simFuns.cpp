#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double updateProb(bool demWin, int state, double depCoef, NumericVector weights, NumericVector probs) {
    double stateWgt = log(weights[state]);
    double probWgt = 1.5 - 4 * pow((probs[state] - .5), 2.0);
    double base = 0.0;
    if (demWin) {
        base = 1.0;
    }
    
    return (base - 0.5) * probWgt * stateWgt * depCoef * 0.9179403;
}


// [[Rcpp::export]]
LogicalVector simInOrder(NumericVector stateID, NumericVector baseProbs, NumericVector baseVotes, double coef) {
    LogicalVector out(stateID.size());
    NumericVector probs(baseProbs.size());
    NumericVector votes(baseVotes.size());
    for (int i = 0; i < probs.size(); ++i) {
        probs[i] = baseProbs[i];
        votes[i] = baseVotes[i];
    }
    
    
    for (int i = 0; i < out.size(); ++i) {
        int thisState = stateID[i] - 1;
        NumericVector res = Rcpp::rbinom(1, 1, probs[thisState]);
        out[thisState] = res[0] == 1;
        double adjustment = updateProb(out[thisState], thisState, coef, votes, probs);
        
        for (int j = 0; j < probs.size(); ++j) {
            double newVal = probs[j] + adjustment;
            probs[j] = newVal < 0.0 ? 0.0 : newVal > 1.0 ? 1.0 : newVal;
        }
    }
    
    return out;
    
}

// [[Rcpp::export]]
LogicalVector simulate(NumericVector baseProbs, NumericVector baseVotes, double coef) {
    NumericVector ss = NumericVector(baseProbs.size());
    NumericVector newProbs = NumericVector(baseProbs.size());
    for (int i = 0; i < baseProbs.size(); ++i) {
        ss[i] = i;
        newProbs[i] = baseProbs[i] / 538.0;
    }
    NumericVector stateID = Rcpp::sample(ss, baseVotes.size(), false, baseVotes);
    return simInOrder(stateID, baseProbs, baseVotes, coef);
}


