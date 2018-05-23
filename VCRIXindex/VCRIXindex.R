rm(list = ls(all = TRUE))
options(scipen = 999)
# please change your working directory
#setwd("C:/...")

#Load data

load("vcrix.RData")

# install and load packages
libraries = c("lubridate", "zoo", "reshape2", "plyr", "MTS", "ggplot2")
lapply(libraries, function(x)
  if (!(x %in% installed.packages())) {
    install.packages(x)
  })
lapply(libraries,
       library,
       quietly = TRUE,
       character.only = TRUE)


#Prepare data
data2 <- data[, c("crypto_symbol", "price_usd", "date")]
data2$date <- as.Date(data2$date)
weights$date <- as.Date(weights$date)


#Ensure correct variable names
colnames(weights) <- c("crypto", "weights", "date")
colnames(data2) <- c("crypto", "price", "date")


#Select relevant components

data <- data2[data2$crypto %in% weights$crypto, ]

w <- weights[order(weights$crypto), ]
p <- data[order(data$crypto), ]


#Prepare the time series format

wtable <- reshape(  #for weights
  w,
  v.names = "weights",
  idvar = "date",
  timevar = "crypto",
  direction = "wide"
)
ptable <- reshape(  #for prices
  p,
  v.names = "price",
  idvar = "date",
  timevar = "crypto",
  direction = "wide"
)
wtable <- wtable[order(wtable$date), ]
ptable <- ptable[order(ptable$date), ]


#Ensure there are no NAs

ptable[, -1] <- na.locf(ptable[, -1])


#Convert prices into returns

ret <- function(x) {
  diff(log(x))
}

returns <- colwise(ret)(ptable[, -1])
returns$date <- ptable$date[-1]

returns$dat <- as.yearmon(returns$date, "%y-%m")
wtable$dat <- as.yearmon(wtable$date, "%y-%m")
wtable$date <- NULL


#Merge price and weight tables into the main dataset

ts <- merge(returns, wtable, by = "dat")
ts$dat <- NULL


# Double check for missing values to avoid NA in var-covar matrix estimation

ts[, grepl("price", names(ts))] <-
  na.locf(ts[, grepl("price", names(ts))])
is.na(ts) <- do.call(cbind, lapply(ts, is.infinite))
ts[is.na(ts)] <- 0 #otherwise EWMA will return NAs


#Estimating variance covariance matrix with EWMA
elem <-
  length(grep(x = colnames(ts), pattern = "price")) #number of cryptos

#select lambda
volaest <- EWMAvol(ts[, grepl("price", names(ts))], lambda = 0.82)
#estimation takes around takes 5 min


#reorganise the list of var-covar matrices
vol <- volaest[1]
v <- vol[[1]]
var <- c(1:nrow(ts))

vv <-
  lapply(1:nrow(v), function(x)
    matrix(
      v[x, ],
      nrow = elem,
      ncol = elem,
      byrow = TRUE
    ))

ww <-
  as.matrix(ts[, grep(x = colnames(ts), pattern = "weights")]) #selecting weights


#Plugging weights and var-covar matrix into formula

for (i in 1:nrow(ts)) {
  var[i] = as.matrix(t(ww[i, ])) %*% vv[[i]] %*% ww[i, ]
}


#Assembling vcrix dataset
index <- data.frame("vola" = sqrt(var), "date" = ts$date)
index$vcrix = c(1:nrow(index))
index$divisor <- c(1:nrow(index))
index$day = lubridate::day(index$date)
index$vcrix[1] = 1000
index$divisor[1] = index$vola[1] / index$vcrix[1]
index$divisor[2:92] = index$divisor[1]
index$vcrix[2:92] = index$vola[2:92] / index$divisor[2:92]
index$vcrix[93] = index$vcrix[92]

index$divisor[93:184] = index$vola[93] / index$vcrix[93]
index$vcrix[94:184] = index$vola[94:184] / index$divisor[94:184]

index$vcrix[185] = index$vcrix[184]
index$divisor[185:276] = index$vola[185] / index$vcrix[185]
index$vcrix[186:276] = index$vola[186:276] / index$divisor[186:276]

index$vcrix[277] = index$vcrix[276]
index$divisor[277:365] = index$vola[277] / index$vcrix[277]
index$vcrix[277:365] = index$vola[277:365] / index$divisor[277:365]

index$vcrix[366] = index$vcrix[365]
index$divisor[366:457] = index$vola[366] / index$vcrix[366]
index$vcrix[366:457] = index$vola[366:457] / index$divisor[366:457]

index$vcrix[458] = index$vcrix[457]
index$divisor[458:549] = index$vola[458] / index$vcrix[458]
index$vcrix[458:549] = index$vola[458:549] / index$divisor[458:549]

index$vcrix[550] = index$vcrix[549]
index$divisor[550:641] = index$vola[550] / index$vcrix[550]
index$vcrix[550:641] = index$vola[550:641] / index$divisor[550:641]

index$vcrix[642] = index$vcrix[641]
index$divisor[642:730] = index$vola[642] / index$vcrix[642]
index$vcrix[642:730] = index$vola[642:730] / index$divisor[642:730]


#Plot VCRIX

plot(
  index$date,
  index$vcrix,
  type = "l",
  col = "blue",
  lwd = 2,
  xlab = "Date",
  ylab = "VCRIX"
)
