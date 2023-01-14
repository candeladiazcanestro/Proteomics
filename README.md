# Proteomics
Calculating paired Wilcoxon rank-sum test, regression models, random forest 

###############
1. step1(Script1_Linear_Regression_Normalised_by_Covariates.R): performing linear regression analyses. Covariates in the regression models included age, foldchange of body weight, and body fat percentage.

2. step2(Script2_Paired_Wilcoxon_Test.R): Perfomring pairwised Wilcoxon test between OLINK abundance before and after exercise.

3. step3(Script3_Random_Forest_Model.R): Constructing random forest model by using the baseline OLINK abundance in the discovery cohort and further validated in the validation cohort.

See the example in example/run.sh with simulated testing datasets.
