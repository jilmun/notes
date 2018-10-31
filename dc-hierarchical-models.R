# Hierarchical and Mixed Effect Models
# October 2018, Richard Erickson

library(lme4)


# studentData -------------------------------------------------------------

# Mixed effect model
# Predict mathgain based on sex, mathprep, mathknow
# add classid and schoolid as random effects
lmerModel <- lmer(mathgain ~ sex + 
                  mathprep + mathknow + (1|classid) +
                  (1|schoolid), data = studentData, na.action = "na.omit",
                  REML = TRUE)
summary(lmerModel)

# Extract and plot 
extractAndPlot(lmerModel)


# countyBirthsData --------------------------------------------------------

# First, build a lmer with state as a random effect. Then look at the model's summary and the plot of residuals. 
birthRateStateModel <- lmer(BirthRate ~ (1 | State), data =countyBirthsData)
summary(birthRateStateModel)
plot(birthRateStateModel)

# Next, plot the predicted values from the model on top of the plot shown during the video.
countyBirthsData$birthPredictState <-  predict(birthRateStateModel, countyBirthsData)
ggplot() + theme_minimal() +
  geom_point(data =countyBirthsData,
             aes(x = TotalPopulation, y = BirthRate)) + 
  geom_point(data = countyBirthsData,
             aes(x = TotalPopulation, y = birthPredictState),
             color = 'blue', alpha = 0.5) 

# Include the AverageAgeofMother as a fixed effect within the lmer and state as a random effect
ageMotherModel <- lmer( BirthRate ~ AverageAgeofMother + (1|State),
                        countyBirthsData)
summary(ageMotherModel)

# Compare the random-effect model to the linear effect model 
summary(lm(BirthRate ~ AverageAgeofMother, data = countyBirthsData))

# Include the AverageAgeofMother as a correlated random-effect slope parameter
ageMotherModelRandomCorrelated <- lmer( BirthRate ~ AverageAgeofMother + (AverageAgeofMother | State),
                                        countyBirthsData)
summary(ageMotherModelRandomCorrelated)

# Include the AverageAgeofMother as an uncorrelated random-effect slope parameter
ageMotherModelRandomUncorrelated <- lmer( BirthRate ~ AverageAgeofMother + (AverageAgeofMother || State),
                                          countyBirthsData)
summary(ageMotherModelRandomUncorrelated)


# Checking results --------------------------------------------------------

# Extract the fixed-effect coefficients
fixef(out)

# Extract the random-effect coefficients
ranef(out)

# Estimate the confidence intervals 
confint(out)

# Extract out the parameter estimates and confidence intervals and manipulate the data
dataPlot <- data.frame(cbind( fixef(out), confint(out)[ 3:4, ]))
rownames(dataPlot)[1] <- "Intercept"
colnames(dataPlot) <- c("mean", "l95", "u95")
dataPlot$parameter <- rownames(dataPlot)

# Print the new dataframe
print(dataPlot)

# Plot the results using ggplot2
ggplot(dataPlot, aes(x = parameter, y = mean,
                     ymin = l95, ymax = u95)) +
  geom_hline( yintercept = 0, color = 'red' ) +
  geom_linerange() + geom_point() + coord_flip() + theme_minimal()


# Maryland crime dataset --------------------------------------------------

# Plot the change in crime through time by County
plot1 <- ggplot(data = MDcrime, aes(x = Year, y = Crime, group = County)) +
  geom_line() + 
  theme_minimal() +
  ylab("Major crimes reported per county")
print(plot1)

# Add the trend line for each county
plot1 + geom_smooth(method="lm", se=FALSE)

# Use lmerTest to extract p-values
## Load lmerTest
library(lmerTest)

## Fit the model with Year as both a fixed and random-effect
lmer(Crime ~ Year + (1 + Year | County) , data = MDCrime)  # doesn't converge

## Fit the model with Year2 rather than Year
out <- lmer(Crime ~ Year2 + (1 + Year2 | County) , data = MDCrime)  # rescale year

## Examine the model's output
summary(out)

# Use anova to test models
## Build the Null model with only County as a random-effect
null_model <- lmer(Crime ~ (1 | County) , data = MDCrime)

## Build the Year2 model with Year2 as a fixed and random slope and County as the random-effect
year_model <- lmer(Crime ~ Year2 + (1 + Year2 | County) , data = MDCrime)

## Compare the two models using an anova
anova(null_model, year_model)  # if Year is significant, p-val will be < 0.05 


# GLM crash course --------------------------------------------------------

# Fit a glm using data in a long format
## Each row is one observation
fitLong <- glm( mortality ~ dose, data = dfLong, family = "binomial")
summary(fitLong)

# Fit a glm using data in a short format with two columns
## Each row is a treatment (6 successes, 4 failures)
fitShort <- glm( cbind(mortality , survival ) ~ dose , data = dfShort, family = "binomial")
summary(fitShort)

# Fit a glm using data in a short format with weights
# Predict probability of mortality
fitShortP <- glm( mortalityP  ~ dose , data = dfShort, weights = nReps , family = "binomial")
summary(fitShortP)
# All three methods have same coef but different degrees of freedom

# Fiting a poisson regression on a different data set
summary(glm(y~x, family = "poisson"))

# Plot the data using jittered points and the default stat_smooth
ggplot(data = dfLong, aes(x = dose, y = mortality)) + 
  geom_jitter(height = 0.05, width = 0.1) +
  stat_smooth(fill = 'pink', color = 'red') 

# Plot the data using jittered points and the the glm stat_smooth
ggplot(data = dfLong, aes(x = dose, y = mortality)) + 
  geom_jitter(height = 0.05, width = 0.1) +
  stat_smooth(method = 'glm',  
              method.args = list(family = "binomial"))


# More fun with glmer -----------------------------------------------------

# Load lmerTest
library(lmerTest)

# Fit glmerOut and look at its summary
glmerOut <- glmer(mortality ~ dose + (1|replicate), family = 'binomial', data = df)
summary(glmerOut)  # if estimated effect for dose is different than zero, then dose has an effect on mortality

# Fit the model and look at its summary 
## cbind because this is aggregate data
modelOut <- glmer( cbind(Purchases, Pass) ~ friend + ranking + (1 | city), data = allData, family = 'binomial')
summary( modelOut) 

# Compare outputs to a lmer model, first create ratio
summary(lmer( Purchases/( Purchases + Pass) ~ friend + ranking + (1|city), data = allData))

# Run the code to see how to calculate odds ratios
summary( modelOut) 
exp(fixef(modelOut)[2])  # extract coef for friends
exp(confint(modelOut)[3,])  # extract confidence interval for friends

# Fit a Poisson glmer
summary( glmer(clicks ~ webpage + (1|group), family = 'poisson', data = userGroups))

# Another Poisson example
modelOut <- glmer(count ~ age + year + (year|county), family = 'poisson',
                  data = ILdata)  # include year as random-effect of county
summary(modelOut)

# Extract out fixed effects
fixef(modelOut)

# Extract out random effects 
ranef(modelOut)

# Run code to see one method for plotting the data
ggplot(data = ILdata2, aes(x = year, y = count, group = county)) +
  geom_line() +
  facet_grid(age ~ . ) +
  stat_smooth( method = 'glm',  # glm results won't exactly be the same as glmer, but helps display results
               method.args = list( family = "poisson"), se = FALSE,
               alpha = 0.5) +
  theme_minimal()


# Repeated measures -------------------------------------------------------

# Run a standard, non-paired t-test
t.test(y[treat == "before"], y[treat == "after"], paired = FALSE)

# Run a standard, paired t-test (does not assume constant variance of groups)
t.test(y[treat == "before"], y[treat == "after"], paired = TRUE)

# Run a repeated-measures ANOVA 
## Paired t-test is a special case of repeated-measures ANOVA
anova(lmer(y ~ treat + (1|x)))


# Sleep study data example ------------------------------------------------

# Modeling approach
# * Visualize data
# * Build a simple model
# * Build model of interest
# * Extract information of interest
# * Visualize results

# Plot the data
ggplot(data = sleepstudy) +
  geom_line(aes(x = Days, y = Reaction, group = Subject)) +
  stat_smooth(aes(x = Days, y = Reaction),
              method = 'lm', size = 3, se = FALSE)

# Build a lm 
lm( Reaction ~ Days, data = sleepstudy)
# Build a lmer
lmer( Reaction ~ Days + (1| Subject), data = sleepstudy)

# Run an anova
anova(lmerOut)
# Look at the regression coefficients
summary(lmerOut)  # non-zero could mean significant


# Hate in NY state --------------------------------------------------------

# Plot hate crimes in NY by Year, grouped by County
## Different trend lines for county suggests using random intercept
ggplot( data = hate, aes(x = Year, y = TotalIncidents, group = County)) +
  geom_line() + geom_smooth(method = 'lm', se = FALSE) 

# Load lmerTest
library(lmerTest)

# glmer with raw Year, fails to converge
glmer( TotalIncidents ~ Year + (Year|County),
       data = hate, family = "poisson")

# glmer with scaled Year, Year2 (0,1,2,etc.)
glmerOut <- glmer( TotalIncidents ~ Year2 + (Year2|County),
                   data = hate, family = "poisson")
summary(glmerOut)

# Extract and manipulate data
countyTrend <- ranef(glmerOut)$County
countyTrend$county <- factor(row.names(countyTrend), levels = row.names(countyTrend)[order(countyTrend$Year2)])

# Plot results 
ggplot(data = countyTrend, aes(x = county, y = Year2)) + geom_point() +
  coord_flip() + ylab("Change in hate crimes per year")  +
  xlab("County")
