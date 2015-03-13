2015/03/12 __K-Fold cross validation:__ 

1. Predictions at t+2 - t+4 are generally better behaved than
    either t+1 or t+5. The modeled process might be time-dependent. This was not demonstrated by the original authors and might constitute a genuinely new aspect of their analysis. But note 3!
2. Some folds behave weirdly: 5, 6, 7, 8. Both 7 and 8 predict 
    consistently worst within, but best out-of-sample. The fact that these are the smallest folds might account for that. Fold 5 always performs best within and worst out-of-sample. The performance of fold 6 is highly volatile across dependent variables. More attention should be paid to this fold.
3. Absolute loss was standardized by sample size. Is this step  
    meaningful?

2015/03/13 __Cluster Robust SE__

- Follow King/Roberts 2014 and calculate difference in SEs.
- Respecify the Autocorrelation and heteroscedasticity 
  components of fundamental uncertainty

