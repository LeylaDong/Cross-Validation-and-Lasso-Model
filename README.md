# Cross-Validation-and-Lasso-Model
1. Data Preprocessing
2.  split the data into test and training sets randomly such that only 15% of the records are in the training set. Then fit a simple linear regression model to predict the overall score of a player and test the model against the test set.
3.  Calculate the R^2 for the predictions you made on the test set.
4.  Using the same training and test sets, fit a simple regression model but with 5-fold cross-validation and predict the overall scores of players in the test set. 
5.  Calculate R^2 for the predictions and compare with the R^2 from step 3.
6.  Using the training data from question 2, fit a Lasso regression to predict the overall scores of players in the test set. Use the default value of alpha (alpha used to tune the penalty parameter), which is usually 1.
7.  Use cross-validation to obtain the optimal value of lambda for the above lasso regression.
