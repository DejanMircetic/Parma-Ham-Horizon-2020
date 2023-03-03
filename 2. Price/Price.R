
price1 <- read.table("~/2022/1. Capanna/Capanna/2. Price/price1.txt", quote="\"", comment.char="")
View(price1)

library(forecast)

p <- ts(price1, frequency = 12, start=c(2017,1))

plot(forecast(p))

#Druga cijene iz prezentacije ali bez 2016 godine koja fali.
price2 <- read.table("~/2022/1. Capanna/Capanna/2. Price/price2.txt", quote="\"", comment.char="")
p2 <- ts(price2, frequency = 12, start=c(2011,1))
plot(p2)

ts.plot(window(p2, start=c(2017,1)),p) #jako su slične ali su izgleda smanjene za neki porez.

# Zdravko da vidi sa Gordjom da nadju ove sto nedostaju!