#Test statistic : Goodness of Fit and Test of Independence using chisquared test, Monte Carlo simulation.
getwd()

#Multiways to name the variables/obs of a dataset
#Way 1:
data <- c(1,2,3,4)
data
class(data)
str(data)
names(data) <- c('A','B','C','D')
#************************************************************
#Way 2:
data2 <- rbind(c(1,21), c(37,40)) #forming a table like data
data2
dim(data2)
class(data2)
str(data2)

names(data2)
dimnames(data2) <- list(Voting = c('ABC', 'DEF'),
                        Gender = c('Male','Female')
                        )
#************************************************************
#Way 3:
data2 <- rbind(c(1,21), c(37,40)) #forming a table like data
dimnames(data2) <- NULL
dimnames(data2) <- data.frame(Voting = c('ABC', 'DEF'), Gender = c('Male','Female'))
dimnames(data2)

#****************************************************************************************
#Goodness of fit test: Chisquared test
#****************************************************************************************
Jobs <- c(11091, 11282, 15378, 12696)
Jobs
names(Jobs) <- c('Project Management', 'Supply Chain','Service', 'Quality')
probability <- c(0.25,0.25,0.25,0.25)

#H0:Null Hypothesis : Proportion of jobs in each category is same (0.25 each)
#Ha: Alternate Hypothesis: Proportion of jobs in each category is NOT same
chisq.test(Jobs, p = probability)

#Conclusion: Since the p-value is <= 0.05 (if conclusion is based on 95% confidence level),
#hence we reject the Null hypothesis, and
#conclude that Proportion of jobs in each category is NOT same
#Also, if conclusion is to be based on 90% of confidence level,
#then alpha(also called, significance level) = 0.1 (or 10%).
#In both cases, we can reject H0.
#Note: df(degrees of freedom) for a vector = n-1
#So, df 4-1 = 3

#******************************************************************************************
#Test of Independence : It is done, when we have 2 variables
#******************************************************************************************
Voter <- rbind(c(2792, 3591),c(1486,2131))
dimnames(Voter) <- list(Voting = c('voted', "didn't vote"),
                        Gender = c('Male','Female')

                        )
Voter
#H0:Null Hypothesis: Gender is independent of voting
#Ha: Alternate Hypothesis: Gender is NOT independent of voting
chisq.test(Voter)

#Conclusion: Since the p-value is <= 0.05 (if conclusion is based on 95% confidence level),
#hence we reject the Null hypothesis.
#Note: df(degrees of freedom in tabular data) = (rows-1) * (columns-1)
#So, df = (2-1) * (2-1) = 1

#***************************************************************************************
#Risk Analysis using Monte Carlo simulation:
#***************************************************************************************

Auto <- rbind(c(2,130,150), c(3,20,100))
chisq.test(Auto)
#Note: if any of the expected frequency fall below '5',
#then we get the warning:'Chi-squared approximation may be incorrect',
#which raises concerns about the calculated p-value.
#So, we'll simulate sampling distribution of the chisquared test statistic, using Monte Carlo method
#When resamlping is done from non-theoretical distributions, we use Monte Carlo Simulation

chisq.test(Auto, simulate.p.value = TRUE, B = 10000)
#Note: B is the no. of replicates used in Monte Carlo test.
