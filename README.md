---
title: "Modern Data Mining - HW 1"
author:
- Cheshta Dhingra
date: "29 January 2017"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.height=5, fig.width=11, warning = F)

# constants for homework assignments
hw_num <- 1
hw_due_date <- "29 January 2017"
```

## Overview / Instructions

This is homework #`r paste(hw_num)` of STAT 471/571/701. It will be *due on `r paste(hw_due_date)` by 11:59 PM* on Canvas. You can directly edit this file to add your answers. Submit a zip file containing the Rmd file, a PDF or HTML version, and all data files necessary with only 1 submission per HW team. If you intend to work on separate problems separately, compile your answers into 1 Rmd file before submitting. Additionally, ensure that you can 'knit' or compile your Rmd file. It is also likely that you need to configure Rstudio to properly convert files to PDF. [These instructions](http://kbroman.org/knitr_knutshell/pages/latex.html#converting-knitrlatex-to-pdf) should be helpful.

You may choose to not use r markdown for now. We hope you will pick it up soon. If that is the case fill in your work in the word file, and attach all your r-code at the end of your word file then. 

In general, be as concise as possible while giving a fully complete answer. All necessary data is available in the `Data` folder on Canvas. Make sure to document your code so the teaching fellows can follow along. R Markdown is particularly useful because it follows a 'stream of consciousness' approach: as you write code in a code chunk, make sure to explain what you are doing outside of the chunk. 

Remember that the [Code of Academic Integrity](http://www.upenn.edu/academicintegrity/ai_codeofacademicintegrity.html) strictly applies to this course. Any questions you have on the homework should be directed to [Piazza](https://piazza.com/class/isg7at4xibc1m8). If you have questions that would reveal part of the solution, ask them in 'private to instructors' mode. 

Solutions will be posted after the deadline. Make sure to compare your answers to and understand the solutions.

## Question 0

Review the code and concepts covered during lecture. 

## Question 1: Exploratory Data Analysis with Sirius XM

This question is about estimating audience size and is designed as an tutorial on the data exploration process of data cleaning, data summary and data visualization. No formal statistical inference is necessary for this question. First time R users may want to defer or skip this question.

*Background:* Wharton launched a talk show called "Business Radio Powered by the Wharton School" through the Sirius Radio station in January of 2014. Within a short period of time the general reaction had been overwhelmingly positive. To find out the audience size for the show, we designed a survey and collected a data set via MTURK in May of 2014. Our goal was to estimate the audience size. There were 51.6 million Sirius Radio listeners then. One approach is to estimate the proportion of the Wharton listeners to that of the Sirius listeners, $p$, so that we will come up with an audience size estimate of approximately 51.6 times $p$. 

To do so, a simple survey was launched via Amazon Mechanical Turk (MTurk) on May 24, 2014 and we set it to be run for 6 days with a target maximum sample size of 2000 as our goal. Most of the observations came in within the first two days. The main questions of interest are "Have you ever listened to Sirius Radio" and "Have you ever listened to Sirius Business Radio by Wharton?". A few demographic features used as control variables were also collected; these include Gender, Age and Household Income.  

We requested that only people in United States answer the questions. Each person can only fill in the questionnaire once to avoid duplicates. Aside from these restrictions, we opened the survey to everyone in MTurk with a hope that the sample would be more randomly chosen. 

The raw data is stored as `Survey_results_final.csv` on Canvas.

### Q1.1

Load the data into R. 

```{r}
dir <- "/Users/che/Documents/STAT471" # my laptop
setwd(dir)
radio <- read.csv("Data/Survey_results_final.csv") #loading the data 
```

For each of the following 2 questions, there is a `dplyr` solution and a `base` R solution. Provide *both* ways of doing so. 

i. We need to clean and select only the variables of interest. Select only the variables Age, Gender, Education Level, Household Income in 2013, Sirius Listener?, Wharton Listener? and Time used to finish the survey.

```{r}
# base
radio <- radio[c("Answer.Age", "Answer.Gender", "Answer.Education", "Answer.HouseHoldIncome", "Answer.Sirius.Radio", "Answer.Wharton.Radio", "WorkTimeInSeconds")] #selecting certain variables by name 

# dplyr
require(dplyr)
radio <- select(radio, Answer.Age, Answer.Gender, Answer.Education, Answer.HouseHoldIncome, Answer.Sirius.Radio, Answer.Wharton.Radio, WorkTimeInSeconds) #selecting certain variables by name 
```

ii. Change the variable names to be "age", "gender", "education", "income", "sirius", "wharton", "worktime".

```{r}
#base 
new_names <- c("age", "gender", "education", "income", "sirius", "wharton", "worktime") #creating a vector of names 
names(radio) <- new_names #replacing names with new names 

#dplyr
#radio <- dplyr::rename(radio, age = Answer.Age, gender = Answer.Gender, education = Answer.Education, income = Answer.HouseHoldIncome, sirius = Answer.Sirius.Radio, wharton = Answer.Wharton.Radio, worktime = WorkTimeInSeconds) #renaming variables and storing in dataframe
```


### Q1.2

As in real world data with user input, the data is incomplete, missing values, and has incorrect responses. There is no general rule for dealing with these problems beyond "use common sense." In whatever case, explain what the problems were and how you addressed them. Do not use Excel, however tempting it might be.
```{r}
head(distinct(select(radio, age))) #some values that don't make sense
radio$age <- as.character(radio$age)
radio$age[radio$age == "Eighteen (18)"] <- "18"
similar <- filter(radio, radio$gender == "Female", education == "Some college, no diploma; or Associate’s degree", income == "Above $150,000")
radio$age[radio$age == "female"] <- round(mean(c(35,20)),0)
radio$age[radio$age == "27`"] <- "27"
radio$age[radio$age == "223"] <- "23"
radio$age[radio$age == "4"] <- "40"
radio$age[radio$age == ""] <- NA
radio$age <- as.numeric(radio$age)

distinct(select(radio, gender)) #missing values
radio$gender[radio$gender == ""] <- NA
radio$gender[is.na(radio$gender)] <- NA

distinct(select(radio, education)) #this has "select one" in it  
radio$education[radio$education == "select one"] <- NA
radio$education[is.na(radio$education)] <- NA

distinct(select(radio, income)) #fine
radio$income[radio$income == ""] <- NA
radio$income[is.na(radio$income)] <- NA

#unbinning income data 
levels(radio$income) <- c(levels(radio$income), (mean(c(15000,30000))), mean(c(30000,50000)), mean(c(50000,75000)), mean(c(75000,150000)), 150000, 15000)
radio$income[radio$income == "$15,000 - $30,000"] <- mean(c(15000,30000))
radio$income[radio$income == "$30,000 - $50,000"] <- mean(c(30000,50000))
radio$income[radio$income == "$50,000 - $75,000"] <- mean(c(50000,75000))
radio$income[radio$income == "$75,000 - $150,000"] <- mean(c(75000,150000))
radio$income[radio$income == "Above $150,000"] <- 150000
radio$income[radio$income == "Less than $15,000"] <- 15000
radio$income <- as.numeric(as.character(radio$income))

distinct(select(radio, sirius)) #fine
radio$sirius[radio$sirius == ""] <- NA
radio$sirius[is.na(radio$sirius)] <- NA

distinct(select(radio, wharton)) #fine 
radio$wharton[radio$wharton == ""] <- NA
radio$wharton[is.na(radio$wharton)] <- NA

head(distinct(select(radio, worktime))) #fine 
radio$worktime[radio$worktime == ""] <- NA
radio$worktime[is.na(radio$worktime)] <- NA
radio$worktime <- as.numeric(radio$worktime)

radio <- na.omit(radio)
sum(is.na(radio))
```
For each variable, I first looked at the distinct values in the data, to find any outliers or incorrect/missing values, then I tackled each variable separately. For the "age" variable, I chose to manually replace the values that were incorrect or in the wrong format, like "Eighteen (18)" or "223". I also found one response "female", so I chose to find all other instances where a female with "Some college, no diploma; or Associate’s degree", and with income "Above $150,000" had responded (I found 2 such people), and filled this individual's age as the average of the others' ages. I also then unbinned the income variable and repaced the income ranges with the mean of that range. For the rest of the variables, I simply replaced missing or blank values with NA, and then removed all the rows containing NA from the dataset. I thought this would be appropriate, as it only reduced the number of rows by (1764-1730)/1764 = 1.9%. 

### Q1.3

Write a brief report to summarize all the variables collected. Include both summary statistics (including sample size) and graphical displays such as histograms or bar charts where appropriate. Comment on what you have found from this sample. (For example - it's very interesting to think about why would one work for a job that pays only 10cents/each survey? Who are those survey workers? The answer may be interesting even if it may not directly relate to our goal.)
```{r}
require(ggplot2)
summary(radio)
head(radio)
count(radio) #sample size = 1730 
table <- table(radio$education)
prop_edu <- round(prop.table(table),2)
prop_edu
radio %>% #plotting age
  ggplot(aes(x = as.numeric(radio$age))) + geom_histogram(binwidth = 1) + xlab("age") + ylab("count") + ggtitle("Histogram of sample ages")
radio %>% #plotting age
  ggplot(aes(x = as.numeric(radio$worktime))) + geom_histogram(binwidth = 1) + xlab("age") + ylab("count") + ggtitle("Histogram of sample worktimes") #plot of worktimes 
```
The sample size = 1730 respondents. The mean age = 30.3. There are 730 females and 1000 males. The mean income = $54,743. 1339 people listened to Sirius and 69 listened to Wharton's show. Mean worktime = 22.5 seconds. The proportions of people who achieved each level of education is displayed in  prop_edu. The distribution of ages can be seen in the first histogram above. 43% of these respondents have completed "Some college, no diploma; or Associate’s degree", which may explain why they are willing to complete surveys for just 10 cents each. 

### Q1.4 Sample property questions

i. Does this sample appear to be a random sample from the general population of the USA?

Using https://en.wikipedia.org/wiki/Educational_attainment_in_the_United_States, I will compare the distribution of education levels in the sample, among those 25 and older, and those who are 25-29 to the distribution in this source. 

```{r}
radio_25 <- filter(radio, radio$age > 24)
table_25 <- table(radio_25$education)
table_25 <- round(prop.table(table_25),2)
table_25

radio_25_29 <- filter(radio, radio$age > 24 | radio$age < 30)
table_25_29 <- table(radio_25_29$education)
table_25_29 <- round(prop.table(table_25_29),2)
table_25_29 
#age comparison
br = seq(15,80,by=5)
br[1] <- 18
br[length(br)] <- 76
ranges = paste(head(br,-1), br[-1]-1, sep=" - ")
ages <- radio$age
freq_sample <- hist(as.numeric(as.character(ages)), br, main = paste("Proportion of subjects in each age category"), xlab = ("age"), ylab = ("Proportion"))
age_df <- data.frame(range = ranges, frequency = freq_sample$counts/1730)
```
Looking at table_25 it is clear that this sample is not representative of the USA population when it comes to educational attainment. The proportion of Americans aged 25 and older who got a high school diploma = 88% while for this sample it is 10%. The proportion of Americans aged 25 and older who got a high school diploma = 88% while for this sample it is 10%. The proportions get closer to the US population if you filter for those between ages 25 and 29, but the sample is still not representative. I also took a look at the age frequencies for our sample and found that it is heavily skewed towards younger individuals, compared ti the entire US population. 

ii. Does this sample appear to be a random sample from the MTURK population?
From http://demographics.mturk-tracker.com/#/householdIncome/all, we can see some demographics of the MTURK population. The gender participation seems to be balanced, with roughly 50% males and 50, which is not the case for our sample, where males comprise about 57% of the sample. Looking at the dataframe of ages binned into ranges of 5 years, we can see that about 55% of our sample is under age 29. For MTURK, this proportion is larger. Roughly 50% of the workers are born in the 1980's and are around 30 yrs old. In our sample, about 17% is over age 30, while this proportion is negligible in MTURK. In MTURK,Roughly 50% of the workers are born in the 1980's and are around 30 yrs old, approximately 20% of the workers are born in the 1990's, and another 20% are born in the 1970's. Our median income is $40,000 while MTURK's is about $50,000. So our sample seems fairly representative with some limitations at a broad glance. 
```{r}
age_df
summary(radio)
```

iii. Assume that the proportion of Wharton listeners vs. that of Sirius listeners remains the same in the general population as it is in the MTURK population. Use the data to provide an estimate of the number of Wharton listeners in the USA. In order to make this estimate do you need to break down the proportion of Wharton to Sirius by gender (or by income.)? Provide some graphical or numerical evidence to support your reasoning.
```{r}
sirius_listeners <- filter(radio, radio$sirius == "Yes")
length(sirius_listeners[,1]) #1339
wharton_listeners <- filter(sirius_listeners, sirius_listeners$wharton == "Yes")

length(wharton_listeners[,1]) #67
head(filter(sirius_listeners, sirius_listeners$wharton == "Yes" & sirius_listeners$gender == "Male")) #48
head(filter(sirius_listeners, sirius_listeners$gender == "Male")) #783 6%

filter(sirius_listeners, sirius_listeners$wharton == "Yes" & sirius_listeners$gender == "Female") #19 
head(filter(sirius_listeners, sirius_listeners$gender == "Female")) #556 3.4%
```
The proportion of respondents who listen to sirius who also listen to wharton = 69/1339 = 5.1%. However, it is clear that men who listen to sirius also tend to listen to wharton, as compared to women. Of the men who listened to sirius, 6% listened to wharton while this number dropped to 3.4% for women. Thus, gender seems to be a predictor of whether or not a sirius listener will listen to wharton. 

### Q1.5

4. Give a final estimate of the Wharton audience size in January 2014. Assume that the sample is a random sample of the MTURK population, and that the proportion of Wharton listeners vs. Sirius listeners remains the same in the general population as it is in the MTURK population. Briefly summarize your findings and how you came to that conclusion.

51.6 million * 5.1% = 2,889,600 or about 2.9 million wharton listeners. This number comes from multiplying the total Sirius listeners with the proportion of those listeners who also listen to Wharton according to our sample, which is assumed representative. 

## Question 2

This exercise is designed to help you understand the linear model and see everything through simulations.

Presume that $x$ and $y$ are linearly related with a normal error, such that $y = 1 + 1.2x + \epsilon$. The standard deviation of the error is $\sigma = 2$. 

Note: we can create a sample input vector ($n = 40$) for $x$ with the following code:

```{r, eval = F}
x <- seq(0, 1, length = 40)
```


### Q2.1

Create a corresponding output vector for $y$ according to the equation given above. Then, create a scatterplot with $\left(x, y\right)$ pairs. Base R plotting is acceptable, but if you can, attempt to use `ggplot2` to create the plot.

```{r}
x <- seq(0, 1, length = 40)
set.seed(1)
y = 1 + 1.2*x + rnorm(40, 0, 2)
data <- data.frame(x,y)
library(ggplot2)
ggplot(data, aes(x=x, y=y)) + geom_point(shape=1)  
```

### Q2.2

Find the LS estimates of $\beta_0$ and $\beta_1$, using the `lm()` function. 
```{r}
fit <- lm(y~x, data = data)
hat_beta_0 <- round(fit$coe[1], 2)
hat_beta_1 <- round(fit$coe[2], 2)
hat_beta_0
hat_beta_1
```
The LS estimate of $\beta_0$ = 1.33 and $\beta_1$ = 0.91. 

### Q2.3 

Overlay the LS estimates onto a copy of the scatterplot you made above.
```{r}
library(ggplot2)
ggplot(data, aes(x=x, y=y)) + geom_point(shape=1) + 
    geom_smooth(method = "lm") + 
    geom_smooth(method = "lm", formula = y ~ x, colour = "red")
```


### Q2.4

What is the 95% confidence interval for $\beta_1$? Does this confidence interval capture the true $\beta_1$?
```{r}
upper_bound <- hat_beta_1 + qt(0.975, 38) * summary(fit)$coefficients[2,2]
lower_bound <- hat_beta_1 - qt(0.975, 38) * summary(fit)$coefficients[2,2]
#check
round(confint(fit), 2)
round(confint(fit)[2,1], 2)
round(confint(fit)[2,2], 2)
```
The 95% Confidence Interval for $\beta_1$ = (-1.03, 2.85) and it captures the true $\beta_1$ of 1.2. 

### Q2.5

What is your RSE for this linear model fit? Is it close to $\sigma = 2$?
```{r}
RSS <- sum((fit$residuals)^2) # residual sum of squares
RSE <- sqrt(RSS/fit$df.residual) # residual standard error
RSE
#check 
summary(fit)$sigma
```
This value of 1.79 is not extremely close to 2. 

### Q2.6

This part aims to help understand the notion of sampling statistics, confidence intervals. Let's concentrate on estimating the slope only.  

Generate 100 samples of size $n = 40$, and estimate the slope coefficient from each sample. We include some sample code below, which should aim you in setting up the simulation. Note: this code is written clearly but suboptimally; see the appendix for a more R-like way to do this simulation.
```{r}
x <- seq(0, 1, length = 40) 
n_sim <- 100
b1 <- numeric(n_sim)   # nsim many LS estimates of beta1 (=1.2)
upper_ci <- numeric(n_sim)  # upper bound
lower_ci <- numeric(n_sim)  # lower bound
t_star <- qt(0.975, 38)

# Carry out the simulation
for (i in 1:n_sim){
  y <- 1 + 1.2 * x + rnorm(40, sd = 2)
  lse <- lm(y ~ x)
  lse_out <- summary(lse)$coefficients
  se <- lse_out[2, 2]
  b1[i] <- lse_out[2, 1]
  upper_ci[i] <- b1[i] + t_star * se
  lower_ci[i] <- b1[i] - t_star * se
}
results <- cbind(x, y, se, b1, upper_ci, lower_ci)
#rm(se, b1, upper_ci, lower_ci, x, n_sim, b1, t_star, lse, lse_out)
hat_beta_1 <- results[,4]
summary(hat_beta_1)
denom <- sum((x-mean(x))^2) 
mean(results[,1])
SE_theory <- (2^2)/denom #sigma = 2
SE_theory #theoretical 1.14
sd(hat_beta_1) #actual 1.10

#true beta 1 = 1.2
count <- 0
for (i in 1:100){ 
  if (results[i,5] > 1.2 && results[i,6] < 1.2)
    count <- count + 1
}  
count

#displaying graphically 
require(ggplot2)
results_df <- data.frame(results)

ggplot2::ggplot(results_df, aes(x = x, y = b1)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +  geom_abline(intercept = 1.2, slope = 0) + ggtitle("95% Confidence Intervals for b1")

```

i. Summarize the LS estimates of $\beta_1$ (in the above, `sim_results$b1`). Does the sampling distribution agree with the theory?

Yes, the sampling distribution agrees with the theory. The theoretical SE = 1.14 while actual SE  of beta 1 = 1.10.

ii.  How many times do your 95% confidence intervals cover the true $\beta_1$? Display your confidence intervals graphically. 

The confidence interval captures the true $\beta_1$ 96 times out of 100.


## Question 3

This question is about Major League Baseball (MLB) and payrolls - how do salaries paid affect wins? How could we model win propensity?

We have put together a data set consisting of the winning records and the payroll of all 30 MLB teams from 1998 to 2014. The variables include the aggregated percentage of wins over the 17-year period, total payroll (in billions), winning percentage and payroll (in millions) broken down for each year. The data is stored as `MLPayData_Total.csv` on Canvas.

```{r}
salary <- read.csv("Data/MLPayData_Total.csv")
```

### Q3.1 Exploratory questions

For each of the following questions, there is a `dplyr` solution that you should try to answer with.

i. Which 5 teams spent the most total money between in years 2000 through 2004?
```{r}
data1 <- salary[, -(21:37)] # take X1998 to X2014 out
salary <- salary[, sort(names(data1)[21-37])] # sort the col names
salary_00_04 <- select(salary, c(Team.name.2014, p2001:p2004)) #subset of relevant years 
salary_00_04 <- mutate(salary_00_04, total = p2001 + p2002 + p2003 + p2004) #sum pay over years 
head(arrange(salary_00_04, desc(total))) #arrange in descending order of pay 
most_paid <- slice(salary_00_04, c(1:5)) #slice the top 5 teams 
most_paid$Team.name.2014 #output teamnames 
```

ii. Between 1999 and 2000, which team(s) "improved" the most? That is, had the biggest percentage gain in wins. 
```{r}
salary_99_00 <- select(salary, c(Team.name.2014, X1999.pct,X2000.pct))
salary_99_00 <- mutate(salary_99_00, gain = X2000.pct - X1999.pct)
salary_99_00 <- arrange(salary_99_00, desc(gain))
most_improved <- slice(salary_99_00, c(1,2))
most_improved$Team.name.2014 #both have same improvement 
```

iii. Using `ggplot`, pick a single year, and plot the games won vs payroll for that year (payroll on x-axis). You may use any 'geom' that makes sense, such as a scatterpoint or a label with the point's corresponding team name.
```{r}
salary_plot <- select(salary, c(Team.name.2014, p2000, X2000.pct))
names(salary_plot) <- c("name", "2000 payroll", "2000 win percent")

library(ggplot2)
ggplot(salary_plot, aes(x=salary_plot$`2000 payroll`, y=salary_plot$`2000 win percent`)) + geom_point(shape=4) + xlab("Payroll") + ylab("Win Percentage") + ggtitle("Games won vs. Payroll for all teams in 2000") + geom_text(aes(label=salary_plot$name))

```


### Q3.2
For a given year, is payroll a significant variable to predict the winning percentage of that year? Choose a single year and run a regression to examine this. You may try this for a few different years. You can do this programmatically (i.e. for every year) if you are interested, but it is not required.
```{r}
#2000
fit2000 <- lm(salary$X2000.pct ~ salary$p2000)
summary(fit2000)
summary(fit2000)$coefficient[2,1] #insignificant 
#2005
fit2005 <- lm(salary$X2005.pct ~ salary$p2005)
summary(fit2005)
summary(fit2005)$coefficient[2,1] #significant at alpha = 0.01
#2011
fit2011 <- lm(salary$X2011.pct ~ salary$p2011)
summary(fit2011)
summary(fit2011)$coefficient[2,1] #significant at alpha = 0.05
```
I picked the years 2000, 2005 and 2011 at random to check whether payroll was a significant predictor of the winning percentage for the teams. It turned out that it was significant in 2005 at alpha = 0.01, in 2011 at alpha = 0.05 but insignificant in 2000.

### Q3.3
With this aggregated information, use regression to analyze total payroll and overall winning percentage. Run appropriate model(s) to answer the following questions:

i. In this analysis do the [Boston Red Sox](http://darkroom.baltimoresun.com/wp-content/uploads/2013/10/SP.BOSTON28P2.jpg) perform reasonably well given their total payroll? [Use a 95% interval.]  
```{r}
myfit <- lm(avgwin~payroll, data=salary)
summary(myfit)  # Tests and CI for the coefficients ##interpret these numbers! 
confint(myfit)  # Pull out the CI for the coefficients

new <- data.frame(payroll=c(1.97))  
CImean <- predict(myfit, new, interval="confidence", se.fit=TRUE)  #predict gives yhat and SE 
CImean
brs <- filter(salary, Team.name.2014 == "Boston Red Sox")
brs$avgwin
```
Boston Red Sox win 54.8% of the time on average. This number is captured by the 95% CI of (52.3%, 56.3%). So their win rate is reasonable as per the expectation based on payroll. 

ii. In view of their winning percentage, how much payroll should the Oakland A's have spent? [Use a 95% interval.]
```{r}
tidy <- read.csv("Data/tidy_mlb.csv")
myfit1 <- lm(tidy$Payroll~tidy$Winrate, data=tidy)
summary(myfit1)

library(ggplot2)
ggplot(tidy, aes(x=tidy$Winrate, y=tidy$Payroll)) + geom_point(shape=1) + 
    geom_smooth(method = "lm") + 
    geom_smooth(method = "lm", formula = y ~ x, colour = "red") + ylab("Payroll") + xlab("Winrate") + ggtitle("Plot of regression of Winrate on Payroll")

confint(myfit1) #(136.26826, 222.91100)
beta1 <- myfit1$coefficients[2] #179.6
```
The 95% confidence Interval for the payroll that Oakland A should have spent given their winrate, is (136.27, 222.91). 

## Appendix

This is code that is roughly equivalent to what we provide above in Question 2 (simulations).

```{r, eval = F}
simulate_lm <- function(n) {
  # note: `n` is an input but not used (don't worry about this hack)
  x <- seq(0, 1, length = 40) 
  y <- 1 + 1.2 * x + rnorm(40, sd = 2)
  t_star <- qt(0.975, 38)
  lse <- lm(y ~ x)
  lse_out <- summary(lse)$coefficients
  se <- lse_out[2, 2]
  b1 <- lse_out[2, 1]
  upper_CI = b1 + t_star * se
  lower_CI = b1 - t_star * se
  return(data.frame(se, b1, upper_CI, lower_CI))
}

# this step runs the simulation 100 times, 
# then matrix transposes the result so rows are observations 
sim_results <- data.frame(t(sapply(X = 1:100, FUN = simulate_lm)))
```
