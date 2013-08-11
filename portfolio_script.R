# Our Investment Portfolio
# Adam Duncan 2013

# What follows is a largely complete implemenation for the PerformanceAnalytics package by
# authors Peter Carl, Brian G. Peterson, Kris Boudt, Eric Zivot. I have not used everything in the 
# package, but nearly all the major functions are here. Some helper functions have not been
# used.

# Data is sourced from yahoo. Change the data.source variable to suit.

library(quantmod,warn.conflicts=FALSE,quietly=TRUE)
library(PerformanceAnalytics,warn.conflicts=FALSE,quietly=TRUE)
library(knitr,warn.conflicts=FALSE,quietly=TRUE)
library(tseries, warn.conflicts=FALSE,quietly=TRUE)
library(fPortfolio, warn.conflicts=FALSE,quietly=TRUE)
library(RQuantLib)

# FUNCTION DEFS:
getData<-function(tickers,datasrc){
  for (i in 1:length(tickers)){
    cat(tickers[i],i,"\n")
    getSymbols(tickers[i],src=datasrc,
               auto.assign=getOption("getSymbols.auto.assign",TRUE),
               env=parent.frame())
  }
}
makeIndex<-function(x,inv,ret){
  # Takes an xts object x and returns an index starting at 100 and evolving as the log returns of x.
  # The inv flag tells whether or not to invert the series before calculating returns.
  # The ret flag tells whether or not we have been passed a series of returns already.
  init.val<-100
  dts<-index(x,0)
  if (inv==TRUE) data<-1/x else data<-x
  if (ret==TRUE){ # we have a series of returns...
    ret.series<-x
  } else {
    ret.series<-periodReturn(data,period="daily",subset=NULL,type="log")
    dts<-index(ret.series,0)
  }
  n<-length(ret.series)
  new.series<-ret.series
  new.series[1]<-init.val
  
  for (i in 2:n){
    new.series[i]<-(1+ret.series[i-1])*new.series[i-1]
  }
  names(new.series)<-c("index")
  return(new.series)
} # My custom index funtion for converting indices to 100 based at inception.
calcWeights<-function(prices,numshares,initial){
  ret<-NULL
  for (i in 1:length(numshares)){
    sh<-numshares[i]
    ret<-cbind(ret,sh*prices[,i]/initial)
  }
  return(ret)
}
getOHLC<-function(assets,OHLC){
  # Takes a list of assets and returns either the Open, High, Low, or Close depending
  # on the passed value of HLOC. Return value is of type xts/zoo.
  ret<-NULL
  for (i in 1:length(assets)){
    if (OHLC=="O" || OHLC=="Open"){
      ret<-cbind(ret,assets[[i]][,1])
    } else {
      if (OHLC=="H" || OHLC=="High"){
        ret<-cbind(ret,assets[[i]][,2])
      } else {
        if (OHLC=="L" || OHLC=="Low"){
          ret<-cbind(ret,assets[[i]][,3])
        } else {
          if (OHLC=="C" || OHLC=="Close"){
            ret<-cbind(ret,assets[[i]][,4])
          }
        }
      }
    }
  }
  return(ret)
}

# Begin code:
data.source = c("yahoo")
tickers.etf=c("VGSIX","VUSTX","VGTSX","VFISX","VTSMX","VFITX","VEIEX","VIPSX")
tickers.human=c("Real Est","USTSY Long","For Eq","USTSY Short",
                "All Eq","USTSY Int.","EM Eq","Infl Sec.")
tickers.bench=c("DJIA","^GSPC")
tsy.2y<-c("DGS2")

suppressWarnings(getData(tickers.etf,data.source))
suppressWarnings(getData(tickers.bench,data.source))
suppressWarnings(getData(tsy.2y,datasrc="FRED"))

etfs<-list(VGSIX,VUSTX,VGTSX,VFISX,VTSMX,VFITX,VEIEX,VIPSX)
first.date<-c("2012-10-17/") # inception data for portfolio...


etfs_close<-getOHLC(etfs,"C")
etfs_close<-etfs_close[first.date]

shares<-c(2347.447,977.96,867.798,1167.414,2086.846,1080.06,1409.844,2615.382)
init.invest<-250000

# Now that we have the relevant data and some information about the number of shares
# purchased and the original investment amount, we can set up a portfolio. First we need
# to sort out some basic mechanics. For example, we need to have the LAST price for each
# of the tickers we imported. We need this in order to mark-to-market the portfolio today.

mtm_last<-sum(last(etfs_close)*shares)
mtm_last
port_value<-as.xts(apply(etfs_close,1,FUN=function(x) sum(x*shares)))
port.eqwt<-as.xts(apply(etfs_close,1,FUN=function(x) sum(x*(1/length(shares)))))
plot.xts(port_value,las=1)

hist.len<-60 # length of rolling window in business days...

# Calculate Portfolio and Benchmark returns...The benchmark portfolio will be a 60/40 stock and intermediate bond
# portfolio.
bench.stocks<-periodReturn(GSPC[first.date][,4],period="daily",subset=NULL,type="log")
bench.bonds<-periodReturn(VFITX[first.date][,4],period="daily",subset=NULL,type="log")
bench.w.stocks<-.60
bench.w.bonds<-.40
bench_ret<-bench.w.stocks*bench.stocks+bench.w.bonds*bench.bonds
port_ret<-periodReturn(port_value,period="daily",subset=NULL,type="log")
port.eqwt.ret<-periodReturn(port.eqwt,period="daily",subset=NULL,type="log")

# Compare our portfolio with 1) an all stock portfolio, 2) an all bond portfolio, and 3) Equal Weighted
# portfolio
# Make an portoflio index starting at 100...and a benchmark index...
port_index<-makeIndex(port_ret,inv=FALSE,ret=TRUE)
bench_index<-makeIndex(bench_ret,inv=FALSE,ret=TRUE)
bench.allstock<-makeIndex(bench.stocks,inv=FALSE,ret=TRUE)
bench.allbond<-makeIndex(bench.bonds,inv=FALSE,ret=TRUE)
port.eqwt.index<-makeIndex(port.eqwt.ret,inv=FALSE,ret=TRUE)

comp.frame<-as.xts(data.frame(bench.stocks,bench.bonds,bench_ret,port.eqwt.ret,port_ret))
names(comp.frame)<-c("All_Stock","All_Bond","SixtyForty","Equal_Wt","Our_Port")
comp.dts<-as.Date(index(comp.frame,0))
comp.frame<-xts(comp.frame,comp.dts)

comp.index.frame<-as.xts(data.frame(bench.allstock,bench.allbond,bench_index,port.eqwt.index,port_index))
names(comp.index.frame)<-c("All_Stock","All_Bond","SixtyForty","Equal_Wt","Our_Port")
comp.index.dts<-as.Date(index(comp.index.frame,0))
comp.index.frame<-xts(comp.index.frame,comp.index.dts)

# Chart the performance relative to the 60/40 benchmark...
chart.RelativePerformance(comp.frame,as.vector(comp.frame$Equal_Wt),
                          main="Relative Performace vs. Benchmark",
                          colorset=c(1:8),
                          xaxis=TRUE,
                          ylog=FALSE,
                          legend.loc="topleft",
                          cex.legend=.8)

# Chart all of the series, indexed to 100 at inception, over time.
chart.TimeSeries(comp.index.frame, auto.grid = TRUE, xaxis = TRUE, yaxis = TRUE, yaxis.right = FALSE, type = "l", lty = 1,
                 lwd=1, main="Performance Over Time",legend.loc="topleft",las=1)
abline(h=last(comp.index.frame$Our_Port),lty=2,col="blue")

# Setting up some variables to mach the PerformanceAnalytics lexicon...
Ra<-port_ret
Rb<-bench_ret
dts<-index(Ra,0)
Rb<-xts(Rb,dts)
Rf<-as.numeric(last(DGS2)/100/252)

# What follows is a collection of metrics from PerformanceAnalytics that describe
# various aspects of the portfolio. Some have been tailored by me to meet specific 
# objectives.

# The performance of our portfolio relative to the benchmark:
chart.RelativePerformance(Ra,as.vector(Rb),
                         main="Relative Performace vs. Benchmark",
                         xaxis=TRUE)

# How much our portfolio outperforms the benchmark on an anuualized basis.
act.premium<-ActivePremium(Ra,Rb,scale=252)
act.premium 

# The weights of the portfolio at inception:
weights_init<-(first(etfs_close)*shares)/init.invest
names(weights_init)<-tickers.human
weights_init

# The weights of the portfolio now:
weights_last<-(last(etfs_close)*shares)/init.invest
names(weights_last)<-tickers.human
weights_last

# Change in weights since inception:
weights_chg<-as.vector(weights_last)-as.vector(weights_init)
names(weights_chg)<-tickers.human
weights_chg

# Display the weights of each asset over time:
port_weights<-calcWeights(etfs_close,shares,init.invest)
par(mfrow=c(2,2))
for (i in 1:length(tickers.etf)){
  plot.xts(port_weights[,i],las=1,type="l",main=paste(tickers.human[i]))
}
names(port_weights)<-tickers.human

# Or, we can use a stacked bar chart to represent the changin weights in 1 graph:
chart.StackedBar(port_weights, colorset = 1:8, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1,
                 cex.main=1,las=3)

# Rebalance schedule to get back to policy:
cat("Rebalancing Schedule:","\n")
round(weights_chg,4)
round(-1*weights_chg*shares,4)

# Display the prices of each sector over time:
par(mfrow=c(2,2))
for (i in 1:length(tickers.etf)){
  plot.xts(makeIndex(etfs_close[,i],inv=FALSE,ret=FALSE),las=1,type="l",main=paste(tickers.human[i]))
}

etfs.ret<-NULL # A data frame that holds all of the etf return streams...
for (i in 1:length(tickers.etf)){
  temp<-as.xts(periodReturn(etfs_close[,i],period="daily",type="log"))
  etfs.ret<-cbind(etfs.ret,temp)
}
names(etfs.ret)<-tickers.human
head(etfs.ret)

par(mfrow=c(1,1))
chart.RelativePerformance(etfs.ret,as.vector(Rb),
                          main="Relative Performace vs. Benchmark",
                          colorset=c(1:8),
                          xaxis=TRUE,
                          ylog=FALSE,
                          legend.loc="bottomleft",
                          cex.legend=.8)

chart.Correlation(etfs.ret,histogram=TRUE)

par(mfrow=c(1,1))
charts.BarVaR (etfs.ret[,1:8],
              width = 20, gap = 0,
              methods = "StdDev",  #c("none", "ModifiedVaR", "GaussianVaR", "HistoricalVaR", "StdDev", "ModifiedES", "p = 0.95"),
              p=.95,
              clean = "none",            
              all = TRUE,
              show.clean = FALSE,
              show.horizontal = TRUE,
              show.symmetric = FALSE,
              show.endvalue = TRUE,
              show.greenredbars = TRUE,
              #legend.loc = "bottomleft",
              ylim = NA,
              #lwd = 2,
              colorset = 1:12,
              lty = 1,
              ypad = 0,
              legend.cex = 0.8)

# Calculate the CAPM Alpha:
CAPM.alpha(Ra,Rb,Rf)

# Calculate the CAPM Beta:
CAPM.beta(Ra,Rb,Rf)

# ...or us this nice CAPM Table:
table.CAPM(Ra, Rb, scale = 252, Rf = Rf, digits = 4)

par(mfrow=c(1,1))
# Compare our CDF to the Normal CDF:
chart.ECDF(Ra)

# Risk/Return Scatter - See PerformanceAnalytics
chart.RiskReturnScatter(Ra,Rf,scale=252,main="Annualized Risk and Return",
                        add.boxplots=TRUE,
                        ylim=c(0,.20),
                        geometric=TRUE,
                        add.sharpe=TRUE,
                        add.names=TRUE,
                        method="calc")

# Rollling Correlation with benchmark:
chart.RollingCorrelation(Ra,as.vector(Rb),width=20)

# VaR sensitivity of the portfolio:
chart.VaRSensitivity(Ra,methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))

# Performance Summary with Wealth Index:
charts.PerformanceSummary(Ra,Rf,main="Performance Summary",methods="HistoricalVaR",
                          wealth.index=TRUE,width=20,gap=60)

# Rolling Performance. Change FUN= to whatever function you want to examine.:
chart.RollingPerformance(Ra,width=60,FUN="Return.annualized",
                         main="Rolling Annualized Return",
                         las=1)

# Plot rolling aspects of the CAPM regression (beta, alpha, etc.)
chart.RollingRegression(Ra, Rb, width = 20, Rf,
                        attribute="Beta",
                        main="Rolling CAPM Parameters",
                        las=1)

# Snail Trail chart:
chart.SnailTrail(Ra, Rf, main = "Annualized Return and Risk")

# Co-moments between asset Ra and Rb (see PerformanceAnalytics for refs):
# In this case, you might let Rb be our portfolio and Ra some asset we are
# considering adding to the portfolio. These metrics give instight into how
# such an addition will affect the moments of the portfolio.
CoVariance(Ra,Rb)
CoSkewness(Ra,Rb)
CoKurtosis(Ra,Rb)

# Downside Deviation (or Potential if flagged): (multipliy by sqrt(252) to
# annualize and compare to annualized standard deviation etc.)
DownsideDeviation(Ra, MAR = 0, method="subset", potential=FALSE )*sqrt(252)

# Expected Shortfall or Conditional Value at Risk:
ES(Ra,p=.95,
   method="historical",# or modified, gaussian, or kernel
   clean="none", # none, boudt, or geltner
   portfolio_method="single", # or 'component' or 'marginal'
   weights=NULL,
   mu=NULL,
   sigma=NULL,
   m3=NULL, #skewness,
   m4=NULL, #excess kurtosis,
   invert=TRUE,
   ) 

# Find drawdowns, sortDrawdowns, and chartDrawdowns:
dd<-sortDrawdowns(findDrawdowns(Ra, geometric = TRUE))
chart.Drawdown(Ra, geometric=TRUE, colorset = (1:12))
mean(dd$return) # Average drawdown
min(dd$return) # Max drawdown
mean(dd$length) # Average length of drawdown episode (days)
mean(dd$recovery) # Average time to restore peak equity

# Information Ratio of Portfolio: (I don't think the PerfAnalytics version works quite right):
sqerr<-(Ra-Rb)^2
msqerr<-mean(sqerr)
rmsqerr<-sqrt(msqerr) # This is now a daily mean squared error...
rmsqerr<-rmsqerr*sqrt(252)

info_ratio<-act.premium/tracking_err
info_ratio

# Kelly Ratio or bet size=ratio of edge/odds (think that is log(odds),actually:
KellyRatio(Ra, Rf, method="half")

# Omega Ratio: Essentially C(L)/P(L), representing the price of a European Call
# on the investment divided by the price of the similar Put. A steep Omega ratio
# above the mean (omega=1 at mean of dist) implies limited potential for further gain.
# L = Loss threshold and can be any specified value.
omega_ratio<-Omega(Ra, L = 0, method = "simple", output = "point",Rf=Rf)
omega_ratio

# Cumualtive and Relative, Annualized Returns and Volatilities:
cr<-Return.cumulative(Ra, geometric = TRUE) # Cumulative, annualized return since inception
ar.port<-Return.annualized(Ra, scale = 252, geometric = TRUE) # Annualized return of portfolio
ar.bench<-Return.annualized(Rb, scale = 252, geometric = TRUE) # Annualized return of benchmark
rr<-Return.relative(Ra,Rb) # Relative return Ra vs Rb
sig.port<-sd.annualized(Ra, scale = 252) # Annualized vol of portfolio
sig.bench<-sd.annualized(Rb, scale=252) # Annualized vol of benchmark
ir.port<-ar.port/sig.port
ir.bench<-ar.bench/sig.bench

cat("Cumulative Return: ",cr,"\n",
    "Ann. Return [Portfolio]: ",ar.port,"\n",
    "Ann. Return [Benchmark]: ",ar.bench,"\n",
    "Avg. Relative Return Ratio: ",mean(rr),"\n",
    "Ann. Volatility [Portfolio]: ",sig.port,"\n",
    "Ann. Volatility [Benchmark]: ",sig.bench,"\n",
    "Info. Ratio [Portfolio]: ",ir.port,"\n",
    "Info. Ratio [Benchmark]: ",ir.bench,"\n")

# or use this nice table...
table.AnnualizedReturns(Ra, scale = 252, Rf = Rf, geometric = TRUE, digits = 4)

# Sharpe Ratio: fun ="StdDev, "VaR", or "ES"
SharpeRatio.annualized(Ra, Rf = Rf, scale = 252, geometric=TRUE) # Same as Info. Ratio for portfolio above when Rf==0.

# Sortino Ratio:
SortinoRatio(Ra, MAR = 0,weights=NULL)

# Capture Ratios:
table.CaptureRatios(Ra, Rb, digits = 4)

# Calmar Ratio: (prefer Sharpe or modified Sharpe)
CalmarRatio(Ra, scale = 252)

# Sterling Ratio: (prefer Sharpe or modified Sharpe)
SterlingRatio(Ra, scale = 252, excess=.1)

# Downside Risks:
table.DownsideRisk(Ra, ci = 0.95, scale = 252, Rf = Rf, MAR = 0, p = 0.95, digits = 4)

# Table of Drawdowns:
table.Drawdowns(Ra, top= 5, digits = 4)

# Table of Higher Momements (also called S'Systematic Moments')
table.HigherMoments(Ra, Rb, scale = 252, Rf = Rf, digits = 4, method = "moment")

# UpDown Ratios: method = one of "Capture", "Number", "Percent",
UpDownRatios(Ra, Rb, method = c("Capture", "Number", "Percent"), side = c("Up", "Down"))

# Upside Potential Ratio:
UpsidePotentialRatio(Ra, MAR = 0, method=c("full"))

#VaR:
VaR(R = Ra,
    p = 0.95,
    method = "historical", #"modified", "gaussian","kernel"
    clean = "none",
    portfolio_method = "single", # "component","marginal"),
    weights = NULL, 
    mu = NULL, # The following only needed if you are calculating a parametric VaR
    sigma = NULL,
    m3 = NULL, # skewness
    m4 = NULL, # excess kurtosis
    invert = TRUE)

