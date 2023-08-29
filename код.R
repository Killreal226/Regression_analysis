library(tseries)
library(uroot)
library(lmtest)
library(forecast)
library(tsDyn)
library(vars)
library(urca)

#загружаем данные
path1 <- "D:/User/Desktop/Kirill/projects/Regression_analysis/inflation.csv"
data1 <- read.csv2(file = path1,
                   header = T,
                   sep = ",",
                   stringsAsFactors = F)

path2 <- "D:/User/Desktop/Kirill/projects/Regression_analysis/gdp.csv"
data2 <- read.csv2(file = path2,
                   header = F,
                   sep = ",",
                   stringsAsFactors = F)

#создаем временные ряды
inf <- ts(as.numeric(data1[,3]), frequency = 12, start = c(2013,3))
gdp <- ts(as.numeric(data2[,1]), frequency = 4, start = c(2011,1))

#графики исходных рядов
plot(inf)
plot(gdp)



#АНАЛИЗ РЯДА ИНФЛЯЦИИ

#коррелограммы

#acf
plot(acf(inf,plot=F,lag=24),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0,2,length.out=25),
     labels = seq(0,24,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#pacf
plot(pacf(inf,plot=F,lag=24),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0.083,2,length.out=24),
     labels = seq(1,24,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#тест Льюнг-Бокса, fitdf= p + q, lag > fitdf
Box.test(inf, lag = 2, type = "Ljung-Box", fitdf = 1)

#оценка модели

#арима с параметрами (1,0,0)
inf.m1 <- Arima(inf,
                order = c(1,0,0),
                include.constant = T,
                include.drift = F)

#автоматическая арима
inf.m2 <- auto.arima(inf)

#график остатков
inf.residuals <- ts(as.numeric(inf.m1$residuals), frequency = 12, start = c(2013,3)) 
plot(inf.residuals)

#тестирование остатков модели

#тест Льюнг-Бокса, fitdf= p + q, lag > fitdf
Box.test(inf.residuals, lag = 2, type = "Ljung-Box", fitdf = 1)

#тест Бройша-Годфри
bgtest(inf.residuals ~ 1 + lag(inf.residuals), order = 12)

#ARCH тест
bptest(inf.residuals^2 ~ const + lag(inf.residuals,1)^2 + lag(inf.residuals,2)^2)

#тест Харке-Бера
jarque.bera.test(inf.residuals)
jarque.bera.test(inf.residuals[40:100])

#прогнозирование

#значение прогнозов на последние 12 периодов
inf.f12 <- forecast(Arima(inf[1:103],
                          order = c(1,0,0),
                          include.constant = T,
                          include.drift = F),
                    h = 12)

#график прогнозов
plot(inf.f12,
     xaxt = "n",
     ylim = c(-1,9))
par(new = T)
plot(data1$value,
     type = "l",
     xlab = "",
     ylab = "",
     xaxt = "n",
     ylim = c(-1,9))
axis(side = 1,
     at = seq(from = 1, to = 107, length.out = 10),
     labels = seq(from = 2013, to = 2022, by = 1),
     las = 2)
abline(v = 104,
       lty = 2)

#показатели качества прогноза
accuracy(inf.f12)



#АНАЛИЗ РЯДА ВВП

#коррелограммы

#acf
plot(acf(gdp,plot=F,lag=16),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0,4,length.out=17),
     labels = seq(0,16,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#pacf
plot(pacf(gdp,plot=F,lag=16),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0.25,4,length.out=16),
     labels = seq(1,16,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#тест на сезонные единичные корни
hegy.test(diff(gdp,4), deterministic = c(1,1,0), lag.method = 'fixed', maxlag = 4)

#тест на сезонные единичные корни после взятия сезонной разности
hegy.test(diff(gdp,4), deterministic = c(1,0,0), lag.method = 'fixed', maxlag = 4)

#график сезонных разностей ВВП
plot(diff(gdp,4))

#acf для сезонных разностей ряда ВВП
plot(acf(diff(gdp,4),plot=F,lag=16),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0,4,length.out=17),
     labels = seq(0,16,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#pacf для сезонных разностей ряда ВВП
plot(pacf(diff(gdp,4),plot=F,lag=16),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n")
axis(side = 1,
     at = seq(0.25,4,length.out=16),
     labels = seq(1,16,1))
axis(side = 2,
     at = seq(-1,1,by=0.2))

#тест Льюнг-Бокса, fitdf= p + q, lag > fitdf
Box.test(diff(gdp,4), lag = 3, type = "Ljung-Box", fitdf = 2)

#оценка моделей

#арима с параметрами (2,0,0)(0,1,0)[12]
gdp.m1 <- Arima(gdp,
                order = c(2,0,0),
                seasonal = list(order = c(0, 1, 0), period = 4),
                include.constant = T,
                include.drift = T)

#автоматическая арима
gdp.m2 <- auto.arima(gdp)

#графики остатков
gdp.m1.residuals <- ts(as.numeric(gdp.m1$residuals), frequency = 4, start = c(2011,1)) 
gdp.m2.residuals <- ts(as.numeric(gdp.m2$residuals), frequency = 4, start = c(2011,1)) 
plot(gdp.m1.residuals)
plot(gdp.m2.residuals)

#тестирование остатков моделей

#тест Льюнг-Бокса, fitdf= p + q, lag > fitdf
Box.test(gdp.m1.residuals, lag = 2, type = "Ljung-Box", fitdf = 1)
Box.test(gdp.m2.residuals, lag = 2, type = "Ljung-Box", fitdf = 1)

#тест Бройша-Годфри
bgtest(gdp.m1.residuals ~ 1 + lag(gdp.m1.residuals), order = 4)
bgtest(gdp.m2.residuals ~ 1 + lag(gdp.m2.residuals), order = 4)

#ARCH тест
bptest(gdp.m1.residuals^2 ~ 1 + lag(gdp.m1.residuals,1)^2 + lag(gdp.m1.residuals,2)^2)
bptest(gdp.m2.residuals^2 ~ 1 + lag(gdp.m2.residuals,1)^2 + lag(gdp.m2.residuals,2)^2)

#тест Харке-Бера
jarque.bera.test(gdp.m1.residuals)
jarque.bera.test(gdp.m2.residuals)

#прогнозирование

#значение прогнозов по моделям на последние 8 периодов
gdp.m1.f8 <- forecast(Arima(gdp[1:40],
                            order = c(2,0,0),
                            seasonal = list(order = c(0,1,0), period = 4),
                            include.constant = T,
                            include.drift = F),
                      h = 8)
gdp.m2.f8 <- forecast(Arima(gdp[1:40],
                            order = c(0,1,1),
                            seasonal = list(order = c(0,1,2), period = 4),
                            include.constant = T,
                            include.drift = F),
                      h = 8)

#график прогнозов по первой модели
plot(gdp.m1.f8,
     xaxt = "n",
     ylim = c(10000,45000))
par(new = T)
plot(data2,
     type = "l",
     xlab = "",
     ylab = "",
     xaxt = "n",
     ylim = c(10000,45000))
axis(side = 1,
     at = seq(from = 1, to = 45, length.out = 12),
     labels = seq(from = 2011, to = 2022, by = 1),
     las = 2)
abline(v = 41,
       lty = 2)

#график прогнозов по второй модели
plot(gdp.m2.f8,
     xaxt = "n",
     ylim = c(10000,45000))
par(new = T)
plot(data2,
     type = "l",
     xlab = "",
     ylab = "",
     xaxt = "n",
     ylim = c(10000,45000))
axis(side = 1,
     at = seq(from = 1, to = 45, length.out = 12),
     labels = seq(from = 2011, to = 2022, by = 1),
     las = 2)
abline(v = 41,
       lty = 2)

#показатели качества прогнозов
accuracy(gdp.m1.f8)
accuracy(gdp.m2.f8)

#тест диболда мариано на остатках прогнозов
e1 <- gdp[41:48]-gdp.m1.f8[[4]]
e2 <- gdp[41:48]-gdp.m2.f8[[4]]
dm.test(e1, e2, "greater", h = 8)

sum(e1)/length(e1)
sum(e2)/length(e2)

#ПОСТРОЕНИЕ ВЕКТОРНОЙ МОДЕЛИ

#перевод инфляции в квартальные данные
inf.q <- ts(c(inf[1], 
              aggregate(ts(inf[2:115],
                           frequency = 12,
                           start = c(2013,4)),
                        4,
                        function(x) tail(x, 1))),
            frequency = 4,
            start = c(2013,1))

#укорачивание ряда ВВП
gdp.q <- ts(gdp[9:47], frequency = 4, start = c(2013,1))

#графики рядов
plot(inf.q)
plot(gdp.q)

#тестирование стационарности

#тест Дики-Фуллера для исходных рядов
summary(ur.df(inf.q,type="trend",selectlags = "AIC"))
summary(ur.df(gdp.q,type="trend",selectlags = "AIC"))

#тест Филлипса-Перрона для исходных рядов
PP.test(inf.q)
PP.test(gdp.q)

#график первых разностей ВВП
plot(diff(gdp.q))

#тест Дики-Фуллера для первых разностей ВВП
summary(ur.df(diff(gdp.q),type="drift",selectlags = "AIC"))

#тест Филлипса-Перрона для первых разностей ВВП
PP.test(diff(gdp.q))

#тест на сезонный единичный корень для разностей ИПП
hegy.test(diff(gdp.q), deterministic = c(1,0,1), lag.method = 'fixed', maxlag = 2)

#слияние данных
data.diff <- cbind(inf = inf.q[2:39], gdp = diff(gdp.q))

#тест Грейнжера

grangertest(data.diff[,'inf'] ~ data.diff[,'gdp'], order = 1)
grangertest(data.diff[,'gdp'] ~ data.diff[,'inf'], order = 1)

#тест Йохансена

#выбор количества лагов для VAR
VARselect(data.diff, lag.max = 8, type = "both")$selection

summary(ca.jo(data.diff, type = "trace", ecdet = "const", K=8))
summary(ca.jo(data.diff, type = "eigen", ecdet = "const", K=8))

#оценка модели

summary(VAR(data.diff, 
            p = 7,
            type = "const",
            season = 4))

