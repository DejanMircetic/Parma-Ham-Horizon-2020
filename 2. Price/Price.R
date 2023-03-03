
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

purchase <- read.table("~/2023/1. Capanna/Parma-Ham-Horizon-2020/2. Price/purchase.txt", quote="\"", comment.char="")
View(purchase)
purchase <- ts(purchase,frequency = 12, start=c(2012,1))
plot(purchase)

library(fpp2)

ggseasonplot(purchase)



pro <- forecast(purchase, h=24)
plot(pro, ylab="Price in euro")

pro$mean


