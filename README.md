## LinearRegressionModelSelect
Model Selection Functions in R based on ISLR

## LinearRegressionModelSelect Project Overview
This project is an exercise in model selection using algorithm 6.1 and algorithm 6.2 from "An Introduction to Statistical Learning" by Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani. For the project, I create a function in R that a user can select between the Best Subset Selection (Algorithm 6.1) and Forward Selection (Algorithm 6.2) along with measure of fit criteria Adjusted r^2, AIC, BIC and cross validation.

After developing the algorithm, a short sample is given using it on the KC house data set selecting the Foward Selection method with the adjusted r^2 as the measure of fit. After the best model is selected it is again fit and residual diagnostics are displayed

## LinearRegressionModelSelect Project Files
* peru.txt (Peru BP data)
* kc_house_data.csv (KC House Data)
* SubsetSelectFunction.R
* SubsetSelectCodeandExamples.pdf (Markdown Output for Demonstrating Code)

## LinearRegressionModelSelect Technology Used
* R
* tidyverse
* gtools

## LinearRegressionModelSelect Project Takeaways
* Using vectorization in the algorithm drastically improves runtime
* residual diagnostics of the final model on the KC housing data indicate a decent fit


