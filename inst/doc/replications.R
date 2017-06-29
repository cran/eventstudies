## ----terrorism-example---------------------------------------------------
                                     # For each market with 
                                     # different time zones
library(eventstudies)
data(TerrorIndiceReturns)
data(TerrorAttack)

## ----terrorism-example2--------------------------------------------------

TerrorIndiceCAR <- lapply(1: ncol(TerrorIndiceReturns), function(x){

                                        # 10-day window around the event
     event <- phys2eventtime(na.omit(TerrorIndiceReturns[ , x, 
                                                         drop = FALSE]),
                             TerrorAttack, 
                             10)
                                
                                        # Estimate ARs
     esMean <- constantMeanReturn(event$z.e[which(attributes(event$z.e)$index 
                                                  %in% -30:-11), ], 
                                  residual = FALSE)
     ar <- event$z.e - esMean
     ar <- window(ar, start = 0, end = 10)
     
                                        # CAR 
     car <- remap.cumsum(ar, base = as.numeric(ar[1, 1]))
     names(car) <- colnames(TerrorIndiceReturns[ , x, 
                                                drop = FALSE])
     return(car)
 })
names(TerrorIndiceCAR) <- colnames(TerrorIndiceReturns)
   
                                        # Compile for all indices
TerrorIndiceCAR <- do.call(cbind, TerrorIndiceCAR)
                                        
                                        # 11-day CAR
TerrorIndiceCAR[11, ]
                                        
                                        # 6-day CAR
TerrorIndiceCAR[6, ]

## ----earnings-example----------------------------------------------------
  
                                        # For each market with 
                                        # different time zones
data(KGStockReturns)
data(KGMarketReturns)
data(KGSurpriseCategory)

## ----earnings-example2---------------------------------------------------
es.categories <- function(stock, market, surprise, option){
                                        # Categorising returns for                                                                               
                                        # each category    

    surprise <- surprise[which(surprise$Category %in% option), ]
    stock <- stock[ , which(names(stock) %in% 
                                as.character(surprise$Company))]
    market <- market[ , which(names(market) %in% 
                                  as.character(surprise$Company))]
    
                                        # ARs
    ar <- lapply(1:NCOL(stock), function(x){
                     output <- excessReturn(stock[ , x],
                                            market[ , x])
                     return(output)
                 })
    names(ar) <- names(stock)
    ar <- do.call("merge", ar)
                   
                                        # CARs
    car <- lapply(1:NCOL(ar), function(x){
                      tmp <- remap.cumsum(z = ar[ , x],
                                          base = as.numeric(ar[1, x]))
                      return(tmp)
                  })
    names(car) <- names(ar)
    car <- do.call("merge", car)
                                      
                                        # CAARs 
    caar <- round(apply(car, 1, mean), 10)
    
    return(list(ar, car, caar))
}

                                        # Calling for each category
                                        # Good
goodCompanies <- es.categories(KGStockReturns, KGMarketReturns,
                               KGSurpriseCategory, "good")
                                        # Medium
mediumCompanies <- es.categories(KGStockReturns, KGMarketReturns,
                                 KGSurpriseCategory, "medium")
                                        # Bad
badCompanies <- es.categories(KGStockReturns, KGMarketReturns,
                              KGSurpriseCategory, "bad")

