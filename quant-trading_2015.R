# Notes for Quantitative Trading with R (2015)
# Author: Harry Georgakopoulos


# GENERAL -----------------------------------------------------------------

check_symbol_name <- function(symbols) {
  symbols <- toupper(symbols)
  valid <- regexpr("^[A-Z]{2,4}$", symbols)  # 2 to 4 letters
  if (length(sort(symbols[valid == -1])) > 0)
    cat("Invalid:", symbols[valid == -1], "\n")
  return(sort(symbols[valid == 1]))
}

calculate_spread <- function(x, y, beta) {
  return(y - beta * x)
}

calculate_beta_and_level <- function(x, y, start_date, end_date) {
  require(xts)
  time_range <- paste0(start_date, "::", end_date)
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp(~ dx + dy)
  
  beta <- r$rotation[2,1] / r$rotation[1,1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm=TRUE)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}

calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold) {
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)
  
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  return(output)
}

run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data=as.data.frame(dF))))
}

rolling_beta <- function(z, width) {
  rollapply(z, width=width, FUN=run_regression,
            by.column=F, align="right")
}


# 3 WORKING WITH DATA -----------------------------------------------------

# read and create Excel files with XLConnect
library(XLConnect)
book <- loadWorkbook("path/file.xlsx")
sheet1 <- readWorksheet(book, sheet="sheet1", header=TRUE)

book_new <- loadWorkbook("book_new.xlsx", create=TRUE)
createSheet(book_new, name="sheet1")
writeWorksheet(book_new, data=df, sheet="sheet1", header=TRUE)
saveWorkbook(book_new, file="path/book_new.xlsx")

# working with xts objects
#tmp <- read.table("clipboard")
library(quantmod)
aapl <- getSymbols("AAPL", auto.assign=FALSE)
summary(Ad(aapl))
class(aapl)
str(aapl)

plot(Ad(aapl), main="AAPL plot")
plot(aapl, main="AAPL plot", type="candles")
plot(Ad(aapl)['2007-01-01/2012-01-01'], main="AAPL plot subset")

start_date <- "2010-01-01"
end_date <- "2012-01-01"
plot(Ad(aapl)[paste0(start_date, "::", end_date)], main="AAPL")

difftime(index(aapl)[2],index(aapl)[1])

chartSeries(aapl, subset = '2010::2010-04',
            theme = chartTheme('white'),
            TA = "addVo(); addBBands()")
reChart(subset = '2011-08::2012-01')  # keeps last parameters
addDPO()

# custom technical indicator (add 90 to close price)
my_indicator <- function(x) return(x + 90)
add_my_indicator <- newTA(FUN=my_indicator, preFUN=Cl, legend.name="fancy indicator", on=1)
chartSeries(aapl, subset='2017-01-01/', theme=chartTheme('white'), TA=NULL)
add_my_indicator()  # doesn't draw line?

# using dplyr
library(tidyverse)
df <- aapl[, c("AAPL.Adjusted", "AAPL.Volume")]
names(df) <- c("price", "volume")
df$return <- diff(log(df$price))
df <- df[-1,]
df$cuts <- cut(abs(df$return), 
               breaks = c(0, 0.02, 0.04, 0.25),  # buckets for returns
               include.lowest = TRUE)
df$means <- NA
for (i in 1:3) {
  group <- which(df$cuts == i)
  if (length(group) > 0)
    df$means[group] <- mean(df$volume[group])
}
head(df)
ggplot(df) + 
  geom_histogram(aes(x=volume)) +
  facet_grid(cuts ~.) +
  geom_vline(aes(xintercept=means), linetype="dashed")


# 4 BASIC STATISTICS AND PROBABILITY --------------------------------------

set.seed(100)
X <- rnorm(1000000, mean=2.33, sd=0.5)
mu <- mean(X)
sd <- sd(X)
hist(X, breaks=100)
abline(v=mu, lwd=3, lty=2)

mean_list <- list()
for (i in 1:10000) {
  mean_list[[i]] <- mean(sample(X, 10, replace=TRUE))
}
hist(unlist(mean_list), breaks=500)
abline(v=mu, lwd=3, col="white", lty=2)


# 5 INTERMEDIATE STATISTICS AND PROBABILITY -------------------------------

library(quantmod)
SPY <- getSymbols("SPY", auto.assign=FALSE)
prices <- Ad(SPY)
mean_prices <- round(mean(prices), 2) 
sd_prices <- round(sd(prices), 2)

hist(prices, breaks=100, prob=T, cex.main=0.9)
abline(v=mean_prices, lwd=2)
legend("topright", cex=0.8, border=NULL, bty="n",
       paste0("mean=", mean_prices, "; sd=", sd_prices))

returns <- diff(log(prices))
par(mfrow = c(2, 2))
hist(returns["2008"], breaks=100)
hist(returns["2009"], breaks=100)
hist(returns["2010"], breaks=100)
hist(returns["2011"], breaks=100)
sd(returns["2008"])
sd(returns["2010"])
sd(returns["2012"])
sd(returns["2014"])
par(mfrow = c(1, 1))

# test stationarity with urca, null = stationary
spy <- Ad(SPY)
library(urca)
test <-ur.kpss(as.numeric(spy))
str(test)
test@teststat
test@cval  # cv is large enough to reject null hypothesis

test_returns <-ur.kpss(as.numeric(returns))
test_returns@teststat
test_returns@cval

# test normality assumption
mu <- mean(returns, na.rm=TRUE)
sigma <- sd(returns, na.rm=TRUE)
x <- seq(-5*sigma, 5*sigma, length=nrow(returns))  # x axis is 5 std dev
hist(returns, breaks=100, 
     main = "Histogram of returns for SPY",
     cex.main = 0.8, prob=TRUE)
lines(x, dnorm(x, mu, sigma), col="red", lwd=2)

# using qqnorm and qqline
par(mfrow = c(1, 2))
qqnorm(as.numeric(returns),
       main = "SPY empirical returns qqplot",
       cex.main = 0.8)
qqline(as.numeric(returns), lwd=2)
grid()
normal_data <- rnorm(nrow(returns), mean=mu, sd=sigma)
qqnorm(normal_data, main="Normal returns", cex.main=0.8)
qqline(normal_data, lwd=2)
grid()

# shapiro-wilk normality test
answer <- shapiro.test(as.numeric(returns))
answer
answer[[2]]  # prob that data is from normal distribution
# sensitive to outliers

# correlations
VXX <- getSymbols("VXX", auto.assign=FALSE)
cor(Cl(SPY)["2009-01-30/"], Cl(VXX))
plot(diff(log(Cl(VXX))))
cor(Ad(SPY)["2009-01-30/"], Ad(VXX))
plot(as.numeric(diff(log(Ad(SPY)["2009-01-30/"]))[-1]), 
     as.numeric(diff(log(Ad(VXX)))[-1]))

sv <- as.xts(merge(diff(log(Ad(SPY)["2009-01-30/"]))[-1],
                   diff(log(Cl(SPY)["2009-01-30/"]))[-1],
                   diff(log(Ad(VXX)))[-1],
                   diff(log(Cl(VXX)))[-1]))
head(sv)
reg <- lm(VXX.Adjusted ~ SPY.Adjusted, data=sv)
summary(reg)

# check residuals
par(mfrow = c(2, 2))
plot(reg$residuals,
     main = "Residuals through time",
     xlab = "Days", ylab = "Residuals")
hist(reg$residuals, breaks=100, 
     main = "Distribution of residuals",
     xlab = "Residuals")
qqnorm(reg$residuals)
qqline(reg$residuals)
acf(reg$residuals, main="Autocorrelation")

vxx_lag_1 <- lag(Cl(VXX), k=1)
# assume SPY returns leads VXX returns
sv <- merge(sv, lag(sv))
plot(as.numeric(sv$SPY.Adjusted.1), as.numeric(sv$VXX.Adjusted),
     main = "Scatter plot SPY lagged vs. VXX",
     cex.main = 0.8,
     cex.axis = 0.8,
     cex.lab = 0.8)
grid()  # no discernible relationship

reg2 <- lm(VXX.Close ~ SPY.Close.1, data=sv)
summary(reg2)

# cross correlation function (like acf, but for two vars)
ccf(as.numeric(sv$SPY.Adjusted), as.numeric(sv$VXX.Adjusted),
    main = "Cross correlation between SPY and VXX")

# auto corr with squared returns
z <- rnorm(1000)
par(mfrow=c(2,1))  # corr does not exist with iid var
acf(z); grid(); acf(z^2); grid()

par(mfrow=c(1,1))  # corr does exist with real returns
acf(sv$SPY.Adjusted ^ 2); grid()

par(mfrow=c(1,2))  # corr exists with other fns too
acf(sv$SPY.Adjusted ^ 3)
acf(abs(sv$SPY.Adjusted))


# 6 SPREADS, BETAS, AND RISK ----------------------------------------------

pepsi <- getSymbols("PEP", from = "2013-01-01", to = "2014-01-01",
                    adjust = TRUE, auto.assign = FALSE)
coke <- getSymbols("COKE", from = "2013-01-01", to = "2014-01-01",
                   adjust = TRUE, auto.assign = FALSE)
Sys.setenv(TZ = "UTC")

prices <- cbind(pepsi[,6], coke[,6])
price_changes <- apply(prices, 2, diff)
plot(price_changes[,1], price_changes[,2],
     main = "Pepsi vs. Coke",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

ans <- lm(price_changes[,1] ~ price_changes[,2])
beta <- ans$coefficients[2]

ans2 <- lm(price_changes[,2] ~ price_changes[,1])
beta2 <- ans2$coefficients[2]

# SPY and AAPL total least squares regression with PCA
SPY <- getSymbols("SPY", from="2007-01-01", to="2016-12-31", adjust=T, auto.assign=F)
AAPL <- getSymbols("AAPL", from="2007-01-01", to="2016-12-31", adjust=T, auto.assign=F)
x <- diff(as.numeric(SPY[,4]))  # SPY close
y <- diff(as.numeric(AAPL[,4]))  # AAPL close

plot(x, y, main="Scatter plot of SPY v AAPL")
abline(lm(y~x))
abline(lm(x~y), lty=2)
grid()

r <- prcomp(~ x + y)
slope <- r$rotation[2,1] / r$rotation[1,1]
intercept <- r$center[2] - slope * r$center[1]
abline(a=intercept, b=slope, lty=3)

# test functions in general section
start_date <- "2011-01-01"
end_date <- "2011-12-31"
x <- SPY[,6]
y <- AAPL[,6]

results <- calculate_beta_and_level(x, y, start_date, end_date)
results
plot(results$spread, ylab="Spread Value")

# out of sample test
start_date_out_sample <- "2012-01-01"
end_date_out_sample <- "2012-10-22"
range <- paste0(start_date_out_sample, "::", end_date_out_sample)
spread_out_of_sample <- calculate_spread(x[range], y[range], results$beta)
plot(spread_out_of_sample, main="AAPL-beta*SPY")
abline(h=results$level, lwd=2)

# rolling 10 day beta
window_length <- 10
start_date <- "2011-01-01"
end_date <- "2011-12-31"
range <- paste0(start_date, "::", end_date)

x <- SPY[range, 6]
y <- AAPL[range, 6]

dF <- cbind(x,y)
names(dF) <- c("x", "y")

betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF) / dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <- diff(data$y) / data$y - 
  return_beta * diff(data$x) / data$x

tail(data)
threshold <- sd(data$spread, na.rm=TRUE)

plot(data$spread, main="AAPL v SPY")
abline(h=threshold, lty=2)
abline(h=-threshold, lty=2)

# out of sample test with rolling beta trading strategy
start_date <- "2012-01-01"
end_date <- "2013-12-31"
range <- paste0(start_date, "::", end_date)

x <- SPY[range, 6]
y <- AAPL[range, 6]

dF <- cbind(x,y)
names(dF) <- c("x", "y")

beta_out_of_sample <- rolling_beta(diff(dF), 10)

data_out <- merge(beta_out_of_sample, dF)
data_out$spread <- data_out$y -
  lag(beta_out_of_sample, 1) * data_out$x

plot(data_out$spread, main="AAPL v SPY out of sample")
abline(h=threshold, lty=2)
abline(h=-threshold, lty=2)


# 7 BACKTESTING WITH QUANTSTRAT -------------------------------------------
#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
#install.packages("blotter", repos="http://R-Forge.R-project.org")
library(quantstrat)
library(blotter)

# house cleaning
rm(list=ls(.blotter), envir=.blotter)
currency("USD")
Sys.setenv(TZ="UTC")

# define ETF's before 2003
symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)
require(quantstrat)
require(PerformanceAnalytics)
initDate <- "1990-01-01"
from <- "2003-01-01" 
to <- "2013-12-31"
options(width = 70)

if(!"XLB" %in% ls()) {
  suppressMessages(getSymbols(symbols, 
                              from=from, to=to, 
                              src="google", adjust=TRUE))  # yahoo not working
}
stock(symbols, currency="USD", multiplier=1)

"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
  ATR <- ATR(HLC, n = n, maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}
"osDollarATR" <- function(orderside, tradeSize, pctATR,
                          maxPctATR = pctATR, data, timestamp,
                          symbol, prefer = "Open", portfolio, integerQty = TRUE,
                          atrMod = "", rebal = FALSE, ...) {
  if(tradeSize > 0 & orderside == "short") {
    tradeSize <- tradeSize * -1
  }
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0("atr", atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  if(length(atrCol) == 0) {
    stop(paste("Term", atrString,
               "not found in mktdata column names."))
  }
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if(is.na(atrTimeStamp) | atrTimeStamp == 0) {
    stop(paste("ATR corresponding to", atrString,
               "is invalid at this point in time. Add a logical
               operator to account for this."))
  }
  dollarATR <- pos * atrTimeStamp
  desiredDollarATR <- pctATR * tradeSize
  remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR
  if(orderside == "long") {
    qty <- min(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  } else {
    qty <- max(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  }
  if(integerQty) {
    qty <- trunc(qty)
  }
  if(!rebal) {
    if(orderside == "long" & qty < 0) {
      qty <- 0
    }
    if(orderside == "short" & qty > 0) {
      qty <- 0
    }
  }
  if(rebal) {
    if(pos == 0) {
      qty <- 0
    }
  }
  return(qty)
}

tradeSize <- 10000
initEq <- tradeSize * length(symbols)  # initial equity
strategy.st <- "Clenow_Simple"
portfolio.st <- "Clenow_Simple"
account.st <- "Clenow_Simple"
rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols, 
          initDate = initDate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, 
         initDate = initDate, currency = "USD", initEq = initEq)
initOrders(portfolio.st, initDate = initDate)
strategy(strategy.st, store = TRUE)

nLag <- 252     # 1 year
pctATR <- 0.02  # 2% risk on capital
period <- 10    # 10 day running ATR

namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
  out <- lag(x, k = k, na.pad = na.pad, ...)
  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "namedLag"
  return(out)
}

add.indicator(strategy.st, name = "namedLag",
              arguments = list(x = quote(Cl(mktdata)), k = nLag),
              label = "ind")
add.indicator(strategy.st, name = "lagATR",
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label = "atrX")
test <- applyIndicators(strategy.st, mktdata = OHLC(XLB))  # errors out
HLC(OHLC(XLB))[!complete.cases(HLC(OHLC(XLB)))]  # missing vals
head(round(test, 2), 253)

# 8 HIGH-FREQUENCY DATA
# 9 OPTIONS
# 10 OPTIMIZATION
# 11 SPEED, TESTING, AND REPORTING
