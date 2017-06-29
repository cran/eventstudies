### R code from vignette source 'eventstudies.Rnw'
### Encoding: ASCII

###################################################
### code chunk number 1: show-the-events
###################################################
library(eventstudies)
data(SplitDates)                        # The sample
str(SplitDates)                         # Just a data frame
head(SplitDates)


###################################################
### code chunk number 2: show-the-events
###################################################
data(StockPriceReturns)                 # The sample
str(StockPriceReturns)                  # A zoo object
head(StockPriceReturns,3)               # Time series of dates and returns.


###################################################
### code chunk number 3: no-adjustment
###################################################
es <- eventstudy(firm.returns = StockPriceReturns,
                 event.list = SplitDates,
                 event.window = 5,
                 type = "None",
                 to.remap = TRUE,
                 remap = "cumsum",
                 inference = TRUE,
                 inference.strategy = "bootstrap")


###################################################
### code chunk number 4: the-es-object
###################################################
class(es)
str(es)


###################################################
### code chunk number 5: plot-es
###################################################
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es)


###################################################
### code chunk number 6: mm-adjustment
###################################################
data(OtherReturns)
es.mm <- eventstudy(firm.returns = StockPriceReturns,
                    event.list = SplitDates,
                    event.window = 5,
                    type = "marketModel",
                    to.remap = TRUE,
                    remap = "cumsum",
                    inference = TRUE,
                    inference.strategy = "bootstrap",
                    model.args = list(market.returns=OtherReturns$NiftyIndex)
                    )


###################################################
### code chunk number 7: plot-es-mm
###################################################
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es.mm)


###################################################
### code chunk number 8: amm-adjustment
###################################################
es.amm <- eventstudy(firm.returns = StockPriceReturns,
                    event.list = SplitDates,
                    event.window = 5,
                    type = "lmAMM",
                    to.remap = TRUE,
                    remap = "cumsum",
                    inference = TRUE,
                    inference.strategy = "bootstrap",
                    model.args = list(
                        market.returns=OtherReturns$NiftyIndex,
                        others=OtherReturns$USDINR,
                        market.returns.purge=TRUE
                        )
                    )


###################################################
### code chunk number 9: efficiency-comparison
###################################################
tmp <- rbind(es$result[5, ],
             es.mm$result[5, ],
             es.amm$result[5, ]
             )[, c(1, 3)]
rownames(tmp) <- c("None", "MM", "AMM")

print(tmp["MM", ] - tmp["None", ])
print(tmp["AMM", ] - tmp["None", ])


###################################################
### code chunk number 10: intraday-example
###################################################
data(AggregateReturns) # Data used
data(RateCuts)
data(IndexReturns)


###################################################
### code chunk number 11: intraday-example
###################################################
                                        # IntraDay eventstudy
                                        # For 35 minutes pre and post event

intraday.es <- eventstudy(firm.returns = AggregateReturns,
                          event.list = RateCuts,
                          event.window = 35,
                          type = "marketModel",
                          to.remap = TRUE,
                          remap = "cumsum",
                          inference = TRUE,
                          inference.strategy = "bootstrap",
                          model.args = list(market.returns=IndexReturns)
                          )


###################################################
### code chunk number 12: plot-intraes
###################################################
par(mai=c(.8,.8,.2,.2), cex=.7)
plot(intraday.es)


