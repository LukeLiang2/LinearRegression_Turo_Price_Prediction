#1-------------------
turo.df <- readRDS('turo.data.5140')

#2--------------
Turoset <- subset(turo.df, car.state =="co", select = car.city:host.verified.phone)
new.df <- na.omit(Turoset) 

#3---------------------------
colnames(new.df)

continuous.df <-new.df[c(3,5:13,18:19,29,33,35,
                         43,47,49)]
categorical.df <- new.df[c(1,2,4,14:17,20:28,30:32,
                           34,36:42,44:46,48,50:53)]

#continuous:
install.packages('psych')
summary(continuous.df)
describe(continuous.df)

hist(new.df$car.deliver.hotel.num, xlab = "number of hotels", main = "Histogram of hotels the car can be delivered to")
hist(new.df$car.displayed.turo.review.num, xlab = "number of reviews", main = "Histogram of review in total")
hist(new.df$car.displayed.turo.review.num.past.12m, xlab = "number of reviews", main = "Histogram of hosts reviews in past 12 months")
hist(new.df$car.displayed.turo.review.num.past.18m, xlab = "number of reviews", main = "Histogram of reviews in past 18 months")
hist(new.df$car.displayed.turo.review.num.past.6m, xlab = "number of reviews", main = "Histogram of reviews in past 6 months")
hist(new.df$car.displayed.user.review.num, xlab = "number of reviews", main = "Histogram of guest reviews in total")
hist(new.df$car.displayed.user.review.num.past.12m, xlab = "number of users reviews", main = "Histogram of guest reviews in past 12 months")
hist(new.df$car.displayed.user.review.num.past.18m,xlab =  "number of reviews", main = "Histogram of guest reviews in past 12 months")
hist(new.df$car.displayed.user.review.num.past.6m,xlab = "number of reviews", main = "Histogram of guest reviews in past 6 months")
hist(new.df$car.extra.mile.fee,xlab = "number of extra miles", main = "Histogram of extra miles fee")
hist(new.df$car.extra.num, xlab = "number of car extra num", main = "Histogram of car extra num")
hist(new.df$car.faq.num, xlab = "number of car faq num", main = "Histogram of car faq num")
hist(new.df$car.miles.included, xlab = "number of car mile included", main = "Histogram of car mile included")
hist(new.df$car.photo.num, xlab = "number of car photo", main = "Histogram of car photo")
hist(new.df$car.trip.price, xlab = "number of car trip price", main = "Histogram of car trip price")
hist(new.df$host.car.num, xlab = "number of host", main = "Histogram of host")
hist(new.df$host.tenure.in.weeks, xlab = "number of host tenure in weeks", main = "Histogram of host tenure in weeks")

#categorical:
airportnum.freq = data.frame(table(continuous.df[1]));airportnum.freq$Relfreq <- airportnum.freq$Freq/1325
barplot(airportnum.freq$Freq, names.arg = airportnum.freq$Var1, xlab = 'CNumber of airports the car can be delivered to', ylab = "Frequency", main = "Bar Chart of CNumber of airports the car can be delivered to")

c1.freq = data.frame(table(categorical.df[1]));c1.freq$Relfreq <- c1.freq$Freq/1325
barplot(c1.freq$Freq, names.arg = c1.freq$Var1, xlab = 'Car location (city)', ylab = "Frequency", main = "Bar Chart of Car location (city)")

c2.freq = data.frame(table(categorical.df[2]));c2.freq$Relfreq <- c2.freq$Freq/1325
barplot(c2.freq$Freq, names.arg = c2.freq$Var1, xlab = 'Whether the car can be delivered to your address or not', ylab = "Frequency", main = "Bar Chart of whether the car can be delivered to your address or not")

c3.freq = data.frame(table(categorical.df[3]));c3.freq$Relfreq <- c3.freq$Freq/1325
barplot(c3.freq$Freq, names.arg = c3.freq$Var1, xlab = 'Car door number', ylab = "Frequency", main = "Bar Chart of Car door number")

c4.freq = data.frame(table(categorical.df[4]));c4.freq$Relfreq <- c4.freq$Freq/1325
barplot(c4.freq$Freq, names.arg = c4.freq$Var1, xlab = 'Extra service availability – Beach Gear', ylab = "Frequency", main = "Bar Chart of Extra service availability – Beach Gear")

c5.freq = data.frame(table(categorical.df[5]));c5.freq$Relfreq <- c5.freq$Freq/1325
barplot(c5.freq$Freq, names.arg = c5.freq$Var1, xlab = 'Extra service availability – Child Safety Seat', ylab = "Frequency", main = "Bar Chart of Extra service availability – Child Safety Seat")

c6.freq = data.frame(table(categorical.df[6]));c6.freq$Relfreq <- c6.freq$Freq/1325
barplot(c6.freq$Freq, names.arg = c6.freq$Var1, xlab = 'Extra service availability – Cooler', ylab = "Frequency", main = "Bar Chart of Extra service availability – Cooler")

c7.freq = data.frame(table(categorical.df[7]));c7.freq$Relfreq <- c7.freq$Freq/1325
barplot(c7.freq$Freq, names.arg = c7.freq$Var1, xlab = 'Extra service availability – One Way Trip', ylab = "Frequency", main = "Bar Chart of Extra service availability – One Way Trip")

c8.freq = data.frame(table(categorical.df[8]));c8.freq$Relfreq <- c8.freq$Freq/1325
barplot(c8.freq$Freq, names.arg = c8.freq$Var1, xlab = 'Extra service availability – Pet allowed for a fee', ylab = "Frequency", main = "Bar Chart of Extra service availability – Pet allowed for a fee")

c9.freq = data.frame(table(categorical.df[9]));c9.freq$Relfreq <- c9.freq$Freq/1325
barplot(c9.freq$Freq, names.arg = c9.freq$Var1, xlab = 'Extra service availability – Phone Mount', ylab = "Frequency", main = "Bar Chart of Extra service availability – Phone Mount")

c10.freq = data.frame(table(categorical.df[10]));c10.freq$Relfreq <- c10.freq$Freq/1325
barplot(c10.freq$Freq, names.arg = c10.freq$Var1, xlab = 'Extra service availability – Portable GPS', ylab = "Frequency", main = "Bar Chart of Extra service availability – Portable GPS")

c11.freq = data.frame(table(categorical.df[11]));c11.freq$Relfreq <- c11.freq$Freq/1325
barplot(c11.freq$Freq, names.arg = c11.freq$Var1, xlab = 'Extra service availability – Post Trip Cleaning', ylab = "Frequency", main = "Bar Chart of Extra service availability – Post Trip Cleaning")

c12.freq = data.frame(table(categorical.df[12]));c12.freq$Relfreq <- c12.freq$Freq/1325
barplot(c12.freq$Freq, names.arg = c12.freq$Var1, xlab = 'Extra service availability – Prepaid EV Recharge', ylab = "Frequency", main = "Bar Chart of Extra service availability – Prepaid EV Recharge")

c13.freq = data.frame(table(categorical.df[13]));c13.freq$Relfreq <- c13.freq$Freq/1325
barplot(c13.freq$Freq, names.arg = c13.freq$Var1, xlab = 'Extra service availability – Prepaid Refuel', ylab = "Frequency", main = "Bar Chart of Extra service availability – Prepaid Refuel")

c14.freq = data.frame(table(categorical.df[14]));c14.freq$Relfreq <- c14.freq$Freq/1325
barplot(c14.freq$Freq, names.arg = c14.freq$Var1, xlab = 'Extra service availability – Stroller', ylab = "Frequency", main = "Bar Chart of Extra service availability – Stroller")

c15.freq = data.frame(table(categorical.df[15]));c15.freq$Relfreq <- c15.freq$Freq/1325
barplot(c15.freq$Freq, names.arg = c15.freq$Var1, xlab = 'Extra service availability – Unlimited Mileage', ylab = "Frequency", main = "Bar Chart of Extra service availability – Unlimited Mileage")

c16.freq = data.frame(table(categorical.df[16]));c16.freq$Relfreq <- c16.freq$Freq/1325
barplot(c16.freq$Freq, names.arg = c16.freq$Var1, xlab = 'Instant booking availability', ylab = "Frequency", main = "Bar Chart of Instant booking availability")

c17.freq = data.frame(table(categorical.df[17]));c17.freq$Relfreq <- c17.freq$Freq/1325
barplot(c17.freq$Freq, names.arg = c17.freq$Var1, xlab = 'Car insurance provider', ylab = "Frequency", main = "Bar Chart of Car insurance provider")

c18.freq = data.frame(table(categorical.df[18]));c18.freq$Relfreq <- c18.freq$Freq/1325
barplot(c18.freq$Freq, names.arg = c18.freq$Var1, xlab = 'Car make', ylab = "Frequency", main = "Bar Chart of Car make")

c19.freq = data.frame(table(categorical.df[19]));c19.freq$Relfreq <- c19.freq$Freq/1325
barplot(c19.freq$Freq, names.arg = c19.freq$Var1, xlab = 'Car model', ylab = "Frequency", main = "Bar Chart of Car model")

c20.freq = data.frame(table(categorical.df[20]));c20.freq$Relfreq <- c20.freq$Freq/1325
barplot(c20.freq$Freq, names.arg = c20.freq$Var1, xlab = 'Car photos verified or not', ylab = "Frequency", main = "Bar Chart of Car photos verified or not")

c21.freq = data.frame(table(categorical.df[21]));c21.freq$Relfreq <- c21.freq$Freq/1325
barplot(c21.freq$Freq, names.arg = c21.freq$Var1, xlab = 'Car power type', ylab = "Frequency", main = "Bar Chart of Car power type")

c22.freq = data.frame(table(categorical.df[22]));c22.freq$Relfreq <- c22.freq$Freq/1325
barplot(c22.freq$Freq, names.arg = c22.freq$Var1, xlab = 'Car rental type', ylab = "Frequency", main = "Bar Chart of Car rental type")

c23.freq = data.frame(table(categorical.df[23]));c23.freq$Relfreq <- c23.freq$Freq/1325
barplot(c23.freq$Freq, names.arg = c23.freq$Var1, xlab = 'Price for self pick up', ylab = "Frequency", main = "Bar Chart of Price for self pick up")

c24.freq = data.frame(table(categorical.df[24]));c24.freq$Relfreq <- c24.freq$Freq/1325
barplot(c24.freq$Freq, names.arg = c24.freq$Var1, xlab = 'Whether the car can be self picked up or not ', ylab = "Frequency", main = "Bar Chart of Whether the car can be self picked up or not ")

c25.freq = data.frame(table(categorical.df[25]));c25.freq$Relfreq <- c25.freq$Freq/1325
barplot(c25.freq$Freq, names.arg = c25.freq$Var1, xlab = 'Car location (state)', ylab = "Frequency", main = "Bar Chart of Car location (state) ")

c26.freq = data.frame(table(categorical.df[26]));c26.freq$Relfreq <- c26.freq$Freq/1325
barplot(c26.freq$Freq, names.arg = c26.freq$Var1, xlab = 'Car transmission type', ylab = "Frequency", main = "Bar Chart of Car transmission type")

c27.freq = data.frame(table(categorical.df[27]));c27.freq$Relfreq <- c27.freq$Freq/1325
barplot(c27.freq$Freq, names.arg = c27.freq$Var1, xlab = 'Whether the car can be unlocked using Turo Go App', ylab = "Frequency", main = "Bar Chart of Whether the car can be unlocked using Turo Go App")

c28.freq = data.frame(table(categorical.df[28]));c28.freq$Relfreq <- c28.freq$Freq/1325
barplot(c28.freq$Freq, names.arg = c28.freq$Var1, xlab = 'Car model year', ylab = "Frequency", main = "Bar Chart of Car model year")

c29.freq = data.frame(table(categorical.df[29]));c29.freq$Relfreq <- c29.freq$Freq/1325
barplot(c29.freq$Freq, names.arg = c29.freq$Var1, xlab = 'Whether the host is an all star host or not', ylab = "Frequency", main = "Bar Chart of Whether the host is an all star host or not")

c30.freq = data.frame(table(categorical.df[30]));c30.freq$Relfreq <- c30.freq$Freq/1325
barplot(c30.freq$Freq, names.arg = c30.freq$Var1, xlab = 'Whether host disclose his/her location or not', ylab = "Frequency", main = "Bar Chart of Whether the host is an all star host or not")

c31.freq = data.frame(table(categorical.df[31]));c31.freq$Relfreq <- c31.freq$Freq/1325
barplot(c31.freq$Freq, names.arg = c31.freq$Var1, xlab = 'Whether host was approved to drive or not', ylab = "Frequency", main = "Bar Chart of Whether host was approved to drive or not")

c32.freq = data.frame(table(categorical.df[32]));c32.freq$Relfreq <- c32.freq$Freq/1325
barplot(c32.freq$Freq, names.arg = c32.freq$Var1, xlab = 'Whether host email was verified or not', ylab = "Frequency", main = "Bar Chart of Whether host email was verified or not")

c33.freq = data.frame(table(categorical.df[33]));c33.freq$Relfreq <- c33.freq$Freq/1325
barplot(c33.freq$Freq, names.arg = c33.freq$Var1, xlab = 'Whether host facebook was verified or not', ylab = "Frequency", main = "Bar Chart of Whether host facebook was verified or not")

c34.freq = data.frame(table(categorical.df[34]));c34.freq$Relfreq <- c34.freq$Freq/1325
barplot(c34.freq$Freq, names.arg = c34.freq$Var1, xlab = 'Whether host phone number was verified or not', ylab = "Frequency", main = "Bar Chart of Whether host phone number was verified or not")

#4-------------------------------
Q <- quantile(continuous.df$car.deliver.hotel.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.deliver.hotel.num)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.deliver.train.station.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.deliver.train.station.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.turo.review.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.turo.review.num)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.turo.review.num.past.12m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.turo.review.num.past.12m)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.turo.review.num.past.18m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.turo.review.num.past.18m)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.turo.review.num.past.6m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.turo.review.num.past.6m)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.user.review.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.user.review.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.user.review.num.past.12m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.user.review.num.past.12m)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.user.review.num.past.18m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.user.review.num.past.18m)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.displayed.user.review.num.past.6m, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.displayed.user.review.num.past.6m)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.extra.mile.fee, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.extra.mile.fee)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.extra.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.extra.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.faq.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.faq.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.miles.included, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.miles.included)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.photo.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.photo.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$car.trip.price, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$car.trip.price)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$host.car.num, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$host.car.num)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr
Q <- quantile(continuous.df$host.tenure.in.weeks, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(continuous.df$host.tenure.in.weeks)
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr

eliminated<- subset(continuous.df, continuous.df$car.deliver.hotel.num > (Q[1] - 1.5*iqr) & continuous.df$car.deliver.hotel.num < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.deliver.train.station.num > (Q[1] - 1.5*iqr) & continuous.df$car.deliver.train.station.num < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.turo.review.num > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.turo.review.num < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.turo.review.num.past.12m > (Q[1] - 1.5*iqr) &car.displayed.turo.review.num.past.12m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.turo.review.num.past.18m > (Q[1] - 1.5*iqr) & car.displayed.turo.review.num.past.18m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.turo.review.num.past.6m > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.turo.review.num.past.6m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.user.review.num > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.user.review.num < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.user.review.num.past.12m > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.user.review.num.past.12m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.user.review.num.past.18m > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.user.review.num.past.18m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.displayed.user.review.num.past.6m > (Q[1] - 1.5*iqr) & continuous.df$car.displayed.user.review.num.past.6m < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.extra.mile.fee > (Q[1] - 1.5*iqr) & continuous.df$car.extra.mile.fee < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.extra.num> (Q[1] - 1.5*iqr) & continuous.df$car.extra.num < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.faq.num > (Q[1] - 1.5*iqr) & continuous.df$car.faq.num  < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.miles.included > (Q[1] - 1.5*iqr) & continuous.df$car.miles.included < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.photo.num > (Q[1] - 1.5*iqr) & continuous.df$car.photo.num< (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$car.trip.price > (Q[1] - 1.5*iqr) & continuous.df$car.trip.price < (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$host.car.num > (Q[1] - 1.5*iqr) & continuous.df$host.car.num< (Q[2]+1.5*iqr))
eliminated<- subset(continuous.df, continuous.df$host.tenure.in.weeks > (Q[1] - 1.5*iqr) & continuous.df$host.tenure.in.weeks< (Q[2]+1.5*iqr))


#5------------------
car.df<-merge(data.frame(categorical.df, row.names=NULL), data.frame(eliminated, row.names=NULL), by = 0, all = TRUE)[-1]
car2.df<- na.omit(car.df)


#6--------------
Turoset <- subset(turo.df, car.state =="co", select = car.city:host.verified.phone)

#7--------------
car2.df$car.extra.prepaid.refuel <- factor(car2.df$car.extra.prepaid.refuel, levels= c("TRUE", "FALSE"))
car2.df$car.extra.cooler <- factor(car2.df$car.extra.cooler ,levels= c("TRUE", "FALSE"))
car2.df$car.extra.prepaid.refuel <- factor(car2.df$car.extra.prepaid.refuel, levels= c("TRUE", "FALSE"))
car.df$host.all.star <- factor(car.df$host.all.star, levels= c("TRUE", "FALSE"))
car2.df$car.deliver.airport.num <- factor(car2.df$car.deliver.airport.num, levels= c(0,1,2,3))


#Model 1: 
car2.lm <- lm(car.trip.price ~ car.deliver.airport.num  + car.extra.mile.fee + car.displayed.user.review.num + car.transmission + car.extra.prepaid.refuel, data = car2.df)
car2.lm.summary <- summary(car2.lm); print(car2.lm.summary) 

#Model 2: 
  car2.df$car.extra.cooler <- factor(car2.df$car.extra.cooler ,levels= c("TRUE", "FALSE"))
car2.lm <- lm(car.trip.price ~ car.displayed.turo.review.num + car.extra.mile.fee + car.extra.cooler + host.all.star + car.photo.num, data = car2.df)
car2.lm.summary <- summary(car2.lm); print(car2.lm.summary)

#Model 3: 
car2.lm <- lm(car.trip.price ~ car.deliver.airport.num + car.extra.mile.fee + car.displayed.user.review.num.past.6m + car.displayed.user.review.num + car.extra.prepaid.refuel, data = car2.df)
car2.lm.summary <- summary(car2.lm); print(car2.lm.summary)

#Model 4: 
car2.lm <- lm(car.trip.price ~ car.deliver.airport.num  + car.extra.mile.fee + host.all.star + car.transmission + car.extra.prepaid.refuel, data = car2.df)
car2.lm.summary <- summary(car2.lm); print(car2.lm.summary)

#Model 5: 
car2.lm <- lm(car.trip.price ~ car.deliver.airport.num  + car.extra.mile.fee + host.all.star + car.displayed.turo.review.num + car.displayed.user.review.num, data = car2.df)
car2.lm.summary <- summary(car2.lm); print(car2.lm.summary)



