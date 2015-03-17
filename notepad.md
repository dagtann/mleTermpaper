2015/03/12 __K-Fold cross validation:__ 

1. Predictions at t+2 - t+4 are generally better behaved than
    either t+1 or t+5. Time-dependency might be more complicated then originally implied. But note 3!
2. Some folds behave weirdly: 5, 6, 7, 8. Both 7 and 8 predict 
    consistently worst within, but best out-of-sample. The fact that these are the smallest folds might account for that. Fold 5 always performs best within and worst out-of-sample. The performance of fold 6 is highly volatile across dependent variables. More attention should be paid to this fold.
3. Absolute loss was standardized by sample size. Is this step  
    actually meaningful?

2015/03/16 __Cluster Robust SE__

- Follow King/Roberts 2014 and calculate difference in SEs.
- Respecify the Autocorrelation and heteroscedasticity 
  components of fundamental uncertainty
- Comparing relative differences of classical and robust se:
    + Current ER Repression 1.5 to 2.0 increase -> Functional form violated?
    + Co-optation stable
    + Past leader fails and Past coups increase 2.0 and more -> Variance inflation?

2015/03/16 __Next Steps__
- Close up lagged dependent variables -> Use ordered logit, but lagged dv entered as continuous predictors
- Create separation plots -> Authors did not specify discriminatory power of their analyses.
- Check parallel regressions assumption
- Create 1st Difference simulations -> How big is the predicted effect of co-optation?

