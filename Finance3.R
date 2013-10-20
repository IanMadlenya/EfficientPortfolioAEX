# load package quantmod
require("quantmod")

aexcomp <- read.csv("C:/Users/NB/Google Drive/Data Analysis/Finance/cfa.csv")
head(aexcomp)

symbols <- as.vector(aexcomp[[1]])

date_begin <- as.Date("2013-01-01")
#date_end <- as.Date("")

# retrieve data of all stocks
tickers <- getSymbols(symbols, src = "yahoo", from = date_begin, auto.assign = TRUE)

dataset <- Ad(get(tickers[1]))
for (i in 2:length(tickers)) {
  dataset <- merge(dataset, Ad(get(tickers[i])))
}

# calculate returns
# return_lag <- 5  # (crude) weekly returns
data_spline <- na.locf(dataset)
data <- na.omit(ROC(data_spline, type = "discrete"))
names(data) <- symbols

library("PerformanceAnalytics")

wdata = window(data)
charts.PerformanceSummary(wdata, colorset=rich6equal)

png("CFAChart.png")
charts.PerformanceSummary(wdata, colorset=rich6equal)
dev.off()

# required packages: financeR, tseries

require("xts")
require("fPortfolio")

# convert xts to TimeSeries for fPortfolio
data_ts <- as.timeSeries(data)

# fPortfolio specification: solver and efficient fronier
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 25

# fPortfolio constraints
constraints <- c("LongOnly")
portfolioConstraints(data_ts, spec, constraints)

# perform optimization
frontier <- portfolioFrontier(data_ts, spec, constraints)
print(frontier)

# plot efficient frontier
png("CFAEffFront.png")
tailoredFrontierPlot(object = frontier)
dev.off()

# plot weights
assets <- dim(data)[2]
png("CFAPort.png")
weightsPlot(frontier, col = rainbow(assets))
dev.off()
