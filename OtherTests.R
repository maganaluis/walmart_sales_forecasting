## If you have already unzipped the files, 
train=read.csv("train.csv")
test=read.csv("test.csv")

library(plyr)  # join
library(reshape)  # cast
library(forecast)  # stlf

# model
all.stores = unique(test$Store)
num.stores = length(all.stores)
train.dates = unique(train$Date)
num.train.dates = length(train.dates)
train.frame = data.frame(Date=rep(train.dates, num.stores),
                         Store=rep(all.stores, each=num.train.dates))

# Dimension reduction using SVD.
preprocess.svd = function(train, n.comp){
  train[is.na(train)] = 0
  z = svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s = diag(z$d[1:n.comp])
  train[, 2:ncol(train)] = z$u %*% s %*% t(z$v)
  train
}

n.comp = 12 # keep first 12 components

d = 1  # first department
tr.d = train.frame
tr.d = join(tr.d, train[train$Dept==d, c('Store','Date','Weekly_Sales')])  # perform a left join.
tr.d = cast(tr.d, Date ~ Store)  # row is Date, col is Store, entries are the sales
# apply SVD for tr.d
tr.d = preprocess.svd(tr.d, n.comp)   

test.dates = unique(test$Date)
num.test.dates = length(test.dates)
forecast.frame = data.frame(Date=rep(test.dates, num.stores),
                            Store=rep(all.stores, each=num.test.dates))
fc.d = forecast.frame
fc.d$Weekly_Sales = 0
fc.d = cast(fc.d, Date ~ Store)  # similar as tr.d

horizon = nrow(fc.d)  # number of steps ahead to forecast
for(j in 2:ncol(tr.d)){ # loop over stores
  s = ts(tr.d[, j], frequency = 52)  # convert sales to time series. 
  fc = stlf(s, h=horizon, s.window=3, method='arima', ic='bic')
  pred = as.numeric(fc$mean)
  fc.d[, j] = pred
}
