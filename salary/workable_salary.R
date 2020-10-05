library(dplyr)
     #for binarize()

# 1 Load the data and check the formatting of the variables.

raw_salary <- read.csv(file = "adult.csv", sep = ";", row.names = 1, header = TRUE)

# str(raw_salary)

# 2 Rename the last column (currently called “NA”) containing the dichotomous salary information. Assign the name salary to it.

colnames(raw_salary)[15] <- "salary"

# 3 The values in salary have a space in front of them (e.g. " >50K"): remove the space from all values (hint: you can use substring()).
# 
# # because it is a leading space, I choose to use trimws().
# could be `which = c("right")` for trailing spaces,
#  `which = c("both")` for leading and trailing
#  and to remove all spaces, I could use gsub()
#    raw_salary$salary <- gsub("\s+", "", raw_salary$salary)

raw_salary$salary <- trimws(raw_salary$salary, which = c("left"))

# or substring()
raw_salary$salary <- substring(raw_salary$salary, first = 2)

# 4 Again, in the variable salary, replace “>50K” with the value 1 and “<=50K” with 0. Make sure to format it as a factor with two levels.

raw_salary$salary <- factor(raw_salary$salary, levels = c("<=50K",">50K"), labels = c(0,1))

# this also works: (requires `dplyr` and `correlationfunnel` packages)
# # but generates the binary in two separate columns, which is not good rn
raw_salary$salary <- raw_salary %>%
     select(salary) %>%
     binarize()

## comofassss
## raw_salary$salary <- c("<=50K",">50K") [raw_salary$salary + 1]

# 5 Use the glm() function to estimate this logistic model. Only specify the arguments formula = salary ~ ., data and family = binomial. Save the result of the estimation in an object called fit.

# y = g(X \cdot \beta) 
     # y: dichotomous variable with two levels
     # g(): known function 
     # X: matrix/df containing the independent  variables
     # \beta: coeff vector determining how each variable contributes to det y
     
# binary response: salary
# predictor variables: salary_df
# online.stat.psu.edu/stat504/node/216/
# stats.idre.ucla.edu/r/dae/logit-regression/
# statmethods.net/advstats/glm.html
#glm: salary ~ salary_df(all), family = binomial, raw_salary

fit <- glm(salary ~ salary_df(all),data=raw_salary,family=binomial(link="logit"))
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals




 
# 6 Using the information in fit (hint: you can use the functions coef() and summary() to extract information), create a dataframe collecting:
     #The names of the variables (names of the rows of the dataframe);
     #The value of the coefficients (first column);
     #A logical vector stating which coefficients are positive (second column);
     #The p-values (third column). .

     
     
# 7Subset the dataframe created in the previous question to only show the rows where the p-values are strictly smaller than 0.05. Knowing that (i) the remaining rows are statistically significant and that (ii) positive coefficients contribute to increase the probability of a salary larger than $50,000 (the opposite for negative values), comment on those variables that negatively contribute to salary.
