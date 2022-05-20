library(car)
library(lmtest)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(dplyr)
library(xts)
library(tidyr)
library(reshape)
library(readxl)
#загрузим данные

full_data <- read_excel("Уни/8 семестр/Диплом/full_data.xlsx")
View(full_data)

#отберем переменные, необходимые для постоения модели

data <- full_data[,c(1,2,3,4,5,6,7,8,10,13,14,15,16,34,47,58,66)]
View(data)
summary(data)
names(data)

data$Industry <- as.factor(data$Industry)
data$`Company Name` <- as.factor(data$`Company Name`)
data$`Identifier (RIC)` <- as.factor(data$`Identifier (RIC)`)
data$Date <- as.factor(data$Date)

#скачаем цены акций

?getSymbols
sym <- data$`Identifier (RIC)`
s <- unique(sym)

#построим цикл
D <- new.env()
sDate <- as.Date("2012-10-30")
eDate <- as.Date("2021-10-30")
ticker <- s[-c(61,71,90)]
num_ticker <- length(ticker)

Temp_D <- list()
counter <- 1L

for(i in ticker)  {  
  
  getSymbols(
    i, 
    env = D, 
    reload.Symbols = FALSE, 
    from = sDate, 
    to = eDate,
    verbose = FALSE,
    warnings = TRUE,
    src = "yahoo",
    symbol.lookup = TRUE) 
  print(counter)
   
  Temp_D[[i]] <- Cl(D[[i]])  
  
  if (counter == length(ticker))
  { 
    #Merge all the objects of the list into one object. 
    Daily_Price   <- do.call(merge, Temp_D)
    Monthly_Price <- Daily_Price[endpoints(Daily_Price,'months')]
  }
  else
  {
    counter <- counter + 1
  }
  
}
View(Monthly_Price)
#обработам полученные данные для того, чтобы было удобно с ними дальше работать
Price <- Monthly_Price[c(1,13,25,37,49,61,73,85,97,109),]
View(Price)
Price <- as.data.frame(Price)

library("writexl")
write_xlsx(Price, "Price.xlsx")

library(readxl)
P <- read_excel("C:/Users/valer/OneDrive/Desktop/Price.xlsx")
View(P)
Pr <- gather(P, key = "Ticker")
Pr <- Pr[-c(1:10),]
View(Pr)

#Соединим данные по котировкам с финансовыми данными
View(data)
data <- data[-c(601:610,701:710,891:900),]
data$Price <- Pr$value

#выведем описательные статистики
#data <- na.omit(data)
summary(data)

#почистим сразу от выбросов
low_b_ESG <- quantile(data$ESG_score,0.005)
low_b_ESG
up_b_ESG <- quantile(data$ESG_score,0.995)
up_b_ESG 
ind_ESG <- which(data$ESG_score < low_b_ESG | data$ESG_score > up_b_ESG)
ind_ESG

low_b_S <- quantile(data$Social_score,0.005)
low_b_S
up_b_S <- quantile(data$Social_score,0.995)
up_b_S 
ind_S <- which(data$Social_score < low_b_S | data$Social_score > up_b_S)
ind_S

low_b_G <- quantile(data$Gov_score,0.005)
low_b_G
up_b_G <- quantile(data$Gov_score,0.995)
up_b_G 
ind_G <- which(data$Gov_score < low_b_G | data$Gov_score > up_b_G)
ind_G

low_b_E <- quantile(data$Env_score,0.005)
low_b_E
up_b_E <- quantile(data$Env_score,0.995)
up_b_E 
ind_E <- which(data$Env_score < low_b_E | data$Env_score > up_b_E)
ind_E

low_b_BVPS <- quantile(data$BVPS,0.005)
low_b_BVPS
up_b_BVPS <- quantile(data$BVPS,0.995)
up_b_BVPS 
ind_BVPS <- which(data$BVPS < low_b_BVPS | data$BVPS > up_b_BVPS)
ind_BVPS

low_b_TA <- quantile(data$Total_assets,0.005)
low_b_TA
up_b_TA <- quantile(data$Total_assets,0.995)
up_b_TA 
ind_TA <- which(data$Total_assets <= low_b_TA | data$Total_assets >= up_b_TA)
ind_TA

low_b_NI <- quantile(data$Net_income,0.005)
low_b_NI
up_b_NI <- quantile(data$Net_income,0.995)
up_b_NI 
ind_NI <- which(data$Net_income < low_b_NI | data$Net_income > up_b_NI)
ind_NI

low_b_Sh <- quantile(data$Shares,0.005)
low_b_Sh
up_b_Sh <- quantile(data$Shares,0.995)
up_b_Sh 
ind_Sh <- which(data$Shares <= low_b_Sh | data$Shares >= up_b_Sh)
ind_Sh

low_b_FL <- quantile(data$FNCL_LVRG,0.005, na.rm = TRUE)
low_b_FL
up_b_FL <- quantile(data$FNCL_LVRG,0.995, na.rm = TRUE)
up_b_FL 
ind_FL <- which(data$FNCL_LVRG < low_b_FL | data$FNCL_LVRG > up_b_FL)
ind_FL

low_b_P <- quantile(data$Price,0.005, na.rm = TRUE)
low_b_P
up_b_P <- quantile(data$Price,0.995, na.rm = TRUE)
up_b_P 
ind_P <- which(data$Price < low_b_P | data$Price > up_b_P)
ind_P

low_roe <- quantile(data$ROE,0.005, na.rm = TRUE)
low_roe
up_roe <- quantile(data$ROE,0.995, na.rm = TRUE)
up_roe 
ind_roe <- which(data$ROE < low_roe | data$ROE > up_roe)
ind_roe

low_roa <- quantile(data$RETURN_ON_ASSET,0.005, na.rm = TRUE)
low_roa
up_roa <- quantile(data$RETURN_ON_ASSET,0.995, na.rm = TRUE)
up_roa 
ind_roa <- which(data$RETURN_ON_ASSET < low_roa | data$RETURN_ON_ASSET > up_roa)
ind_roa

low_ag <- quantile(data$ASSET_GROWTH,0.005, na.rm = TRUE)
low_ag
up_ag <- quantile(data$ASSET_GROWTH,0.995, na.rm = TRUE)
up_ag 
ind_ag <- which(data$ASSET_GROWTH < low_ag | data$RETURN_ON_ASSET > up_ag)
ind_ag

low_qr <- quantile(data$QUICK_RATIO,0.005, na.rm = TRUE)
low_qr
up_qr <- quantile(data$QUICK_RATIO,0.995, na.rm = TRUE)
up_qr 
ind_qr <- which(data$QUICK_RATIO < low_qr | data$QUICK_RATIO > up_qr)
ind_qr

outlier <- c(ind_BVPS,ind_E,ind_ESG,ind_FL,ind_G,ind_NI,ind_P,ind_S,ind_Sh,ind_TA, ind_roe, ind_roa, ind_qr, ind_ag)
outlier2 <- unique(outlier)
data_out <- data[-c(outlier2),]
data_out$NIPS <- data_out$Net_income/data_out$Shares
data_out <- data_out[,-c(10,11)]
#outlier <- c(ind_BVPS,ind_E,ind_ESG,ind_FL,ind_G,ind_NI,ind_P,ind_S,ind_Sh,ind_TA)
#outlier1 <- unique(outlier)
#data_out <- data[-c(outlier1),]
View(data_out)
summary(data_out)
data_out <- as.data.frame(data_out)
unique(data_out$`Identifier (RIC)`)
summary(data_out$Date)
min(summary(data_out$Industry))
stargazer(data_out, type = "text", column.labels=names(data_out[,-c(1,2,3,10)]),
          df=FALSE, digits=3, out = "summary1.doc")

####Графики####
View(data_out)


ggplot(data_out, aes(x = Industry)) +geom_bar()
install.packages("RColorBrewer")                   
library("RColorBrewer")
display.brewer.all(colorblindFriendly = TRUE)

ggplot(data_out, aes(x=factor(Date), y=ESG_score, fill=Date)) +
  geom_boxplot(outlier.size=1.5, outlier.shape=21)+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black")+
  xlab("")+ylab("ESG_score")+labs(fill="Год")+scale_fill_brewer(palette="PiYG")


ggplot(data_out, aes(x=factor(Date), y=Gov_score, fill=Date)) +
  geom_boxplot(outlier.size=1.5, outlier.shape=21)+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black")+
  xlab("")+ylab("Gov_score")+scale_fill_brewer(palette="PiYG")

ggplot(data_out, aes(x=factor(Date), y=Social_score, fill=Date)) +
  geom_boxplot(outlier.size=1.5, outlier.shape=21)+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black")+
  xlab("")+ylab("Social_score")+scale_fill_brewer(palette="PiYG")

ggplot(data_out, aes(x=factor(Date), y=Env_score, fill=Date)) +
  geom_boxplot(outlier.size=1.5, outlier.shape=21)+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="black")+
  xlab("")+ylab("Env_score")+scale_fill_brewer(palette="PiYG")
#скрипичные графики
install.packages("gcookbook")
library(gcookbook) 

p <- ggplot(data_out, aes(x=factor(Date), y=ESG_score))
p + geom_violin()
install.packages("vioplot")
library(vioplot)
col <- c("Grey","Purple","Lightblue", "Lightgreen")
n <- c("ESG_score","Social_score", "Gov_score","Env_score")
vioplot(data_out$ESG_score, data_out$Social_score, data_out$Gov_score, data_out$Env_score, 
        names = c("ESG_score","Social_score", "Gov_score","Env_score"),col = col
            )
#гистограммы
ggplot(data_out, aes(x=BVPS)) + geom_histogram(binwidth=4,fill="darkgreen", col="black" )+xlab("Балансовая стоимость на акицию")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=Total_assets)) + geom_histogram(binwidth = 20000000000, fill="darkgreen", col="black" )+xlab("Активы")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=ROE)) + geom_histogram(binwidth=0.1,fill="darkgreen", col="black" )+xlab("Рентабельность собственного капитала")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=RETURN_ON_ASSET)) + geom_histogram(binwidth=1,fill="darkgreen", col="black" )+xlab("Рентабельность активов")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=QUICK_RATIO)) + geom_histogram(binwidth=0.2,fill="darkgreen", col="black" )+xlab("Коэффициент быстрой ликвидности")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=ASSET_GROWTH)) + geom_histogram(binwidth=12,fill="darkgreen", col="black" )+xlab("Темп роста активов")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=FNCL_LVRG)) + geom_histogram(binwidth=2,fill="darkgreen", col="black" )+xlab("Финансовый рычаг")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=NIPS)) + geom_histogram(binwidth=2,fill="darkgreen", col="black" )+xlab("Чистая прибыль на акицию")+ylab("Количество наблюдений")
ggplot(data_out, aes(x=Price)) + geom_histogram(binwidth=20,fill="darkgreen", col="black" )+xlab("Цена акции")+ylab("Количество наблюдений")


#sp <- ggplot(data_out, aes(x=ESG_score, y=ROE))
#sp + geom_point() + stat_smooth(method=lm)

#sp2 <- ggplot(data_out, aes(x=ESG_score, y=RETURN_ON_ASSET))
#sp2 + geom_point() + stat_smooth(method=lm)

#sp3 <- ggplot(data_out, aes(x=ESG_score, y=Price))
#sp3 + geom_point() + stat_smooth(method=lm, se=TRUE)


summary(data_out$Industry)
View(a)
a <- aggregate(data_out$ESG_score, by=list(data_out$Industry), FUN=mean)
ggplot(a, aes(x=x, y=reorder(Group.1,x))) +
  geom_point(size=1.5) +
  theme_bw() + xlab("ESG_score")+ylab("Сектор")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

summary(data_out$Date)
View(a)
a <- aggregate(data_out$ESG_score, by=list(data_out$Date), FUN=mean)
ggplot(a, aes(x=x, y=reorder(Group.1,x))) +
  geom_point(size=2)+
   xlab("ESG_score")+ylab("Год")

a <- aggregate(data_out$Social_score, by=list(data_out$Date), FUN=mean)
ggplot(a, aes(x=x, y=reorder(Group.1,x))) +
  geom_point(size=2)+
  xlab("Социальный фактор")+ylab("Год")
 
a <- aggregate(data_out$Gov_score, by=list(data_out$Date), FUN=mean)
ggplot(a, aes(x=x, y=reorder(Group.1,x))) +
  geom_point(size=2)+
  xlab("Фактор корпоративного управления")+ylab("Год")

a <- aggregate(data_out$Env_score, by=list(data_out$Date), FUN=mean)
ggplot(a, aes(x=x, y=reorder(Group.1,x))) +
  geom_point(size=2)+
  xlab("Экологический фактор")+ylab("Год")

View(summary(data_out$Industry))
b <- summary(data_out$Industry)
c <- sort(b,decreasing = TRUE)
View(c)
sum(c[1:10])
industry <- c("Equity Real Estate Investment Trusts (REITs)","IT Services", "Capital Markets", "Oil, Gas & Consumable Fuels", "Health Care Providers & Services",
              "Insurance", "Health Care Equipment & Supplies", "Machinery", "Banks", "Semiconductors & Semiconductor Equipment")
id <- filter(data_out,data_out$Industry==industry)
View(id)

#корреляционная матрица
data_cl <- na.omit(data_out)

mat1 <- as.dist(round(cor(data_cl[,-c(1,2,3,10)]),3))

library(GGally)
ggcorr(data_cl[,-c(1,2,3,10)], nbreaks = 6,
       low = "steelblue",
       mid = "white",
       high = "darkred",
       label = TRUE,
       label_size = 3,
       legend.size = 9,
       legend.position = "right",
       nudge_x=-1.2, 
       layout.exp = 2)


#построим модель
#модель с фиксированными эффектами

View(data_out)
mod <- lm(data = data_out, 
          formula = data_out$Price~data_out$BVPS+data_out$NIPS+data_out$Total_assets + 
            data_out$FNCL_LVRG+data_out$ESG_score+data_out$Date)
summary(mod)
plot(mod, which = 1)
#пропущена какая-то степень
#тест Бокса-кокса
boxCox(mod)
summary(powerTransform(mod))
mod2 <- lm(data = data_out, 
           formula = log(data_out$Price)~data_out$BVPS+data_out$NIPS+data_out$Total_assets + 
             data_out$FNCL_LVRG+data_out$ESG_score+data_out$Date)
summary(mod2)
plot(mod2,1)
boxCox(mod2)

library(car)
crPlots(mod2)
#тест Рамсея для пропуска степени, Н0 - степени не пропущены
library(lmtest)
resettest(mod2, power=2)
resettest(mod2, power=3)
#p-value < любого уровня значимости,  значить H0 отвергается, степени пропущены

mod3 <- lm(data = data_out, 
           formula = log(data_out$Price)~data_out$BVPS+I(data_out$NIPS^3)+data_out$Total_assets + 
             data_out$FNCL_LVRG+data_out$ESG_score+data_out$Date)
summary(mod3)
plot(mod3,1)
crPlots(mod3)
#стало хуже по R2 и по графикам
mod3 <- lm(data = data_out, 
           formula = log(data_out$Price)~data_out$BVPS+I(data_out$NIPS^2)+data_out$Total_assets + 
             data_out$FNCL_LVRG+data_out$ESG_score+data_out$Date)
summary(mod3)
plot(mod3,1)
crPlots(mod3)
resettest(mod3,power=2)
#опять не лучше
#попробуем взять логарифм активов и левериджа
mod4 <- lm(data = data_out, 
          formula = data_out$Price~data_out$BVPS+data_out$NIPS+log(data_out$Total_assets) + 
            log(data_out$FNCL_LVRG)+data_out$ESG_score+data_out$Date)
summary(mod4)
crPlots(mod4)
plot(mod4,1)
#R2 увеличился, по графикам стало лучше
#посмотрим нормальность остатков
plot(mod4,2)
plot(mod2,2)
#интересно, что в модели без логарифма остатки нормальны
#мультиколлинеарность
vif(mod4)
vif(mod2)
plot(mod2,3)
plot(mod4,3)
bptest(mod2)
#есть гетероскедастичность

#удалим влиятельные наблюдения
plot(mod4, 4)
cook_mod <- cooks.distance(mod4)
K <- which(cook_mod > 4/4511)
K

e <- resid(mod4)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out2 <- data_out[-OUT,]
View(data_out2)

#модель на новой выборке
mod5 <- lm(data = data_out2, 
           formula = log(data_out2$Price)~data_out2$BVPS+data_out2$NIPS+log(data_out2$Total_assets) + 
             log(data_out2$FNCL_LVRG)+data_out2$ESG_score+data_out2$Date)
summary(mod5)
plot(mod5,1)
crPlots(mod5)
resettest(mod5, power=3)

mod6 <- lm(data = data_out2, 
           formula = log(data_out2$Price)~data_out2$BVPS+I(data_out2$NIPS^3)+data_out2$Total_assets + 
             data_out2$FNCL_LVRG+data_out2$ESG_score+data_out2$Date)
summary(mod6)
plot(mod6,1)
crPlots(mod6)
resettest(mod6, power=2)
#опять только хуже
#еще раз с логарифмом
mod7 <- lm(data = data_out2, 
           formula = log(data_out2$Price)~data_out2$BVPS+data_out2$NIPS+log(data_out2$Total_assets) + 
             log(data_out2$FNCL_LVRG)+data_out2$ESG_score+data_out2$Date)
summary(mod7)
plot(mod7,1)
crPlots(mod7)
plot(mod7,2)
plot(mod7,3)
#гетероскедастичность все еще осталась
#Лучше не становится, нет смысла улучшать, mod7 наилучшая

library(sandwich)
cov_wtite <- vcovHC(mod7, type="HC0")
coeftest(mod7,cov_wtite)
#добавим фиксированный эффект для отрасли
mod7_2 <- lm(data = data_out2, 
           formula = log(data_out2$Price)~data_out2$BVPS+data_out2$NIPS+log(data_out2$Total_assets) + 
             log(data_out2$FNCL_LVRG)+data_out2$ESG_score+data_out2$Date+data_out2$Industry)
summary(mod7_2)
cov_wtite2 <- vcovHC(mod7_2, type="HC0")
coeftest(mod7_2,cov_wtite2)
#влияние ESG значимо

##### Модель для фактора E ######
mode <- lm(data = data_out, 
          formula = data_out$Price~data_out$BVPS+data_out$NIPS+data_out$Total_assets + 
            data_out$FNCL_LVRG+data_out$Env_score+data_out$Date)
summary(mode)
plot(mode,1)
vif(mode)
boxCox(mode)
crPlots(mode)

mode2 <- lm(data = data_out, 
           formula = log(data_out$Price)~data_out$BVPS+data_out$NIPS+log(data_out$Total_assets) + 
             log(data_out$FNCL_LVRG)+data_out$Env_score+data_out$Date)
summary(mode2)
boxCox(mode2)
plot(mode2,1)
plot(mode2,2)
plot(mode2,3)

plot(mode2, 4)
cook_mod <- cooks.distance(mode2)
K <- which(cook_mod > 4/4511)
K

e <- resid(mode2)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out3 <- data_out[-OUT,]
View(data_out3)
#новая модель
mode3 <- lm(data = data_out3, 
            formula = log(data_out3$Price)~data_out3$BVPS+data_out3$NIPS+log(data_out3$Total_assets) + 
              log(data_out3$FNCL_LVRG)+data_out3$Env_score+data_out3$Date)
summary(mode3)
plot(mode3,1)
plot(mode3,2)
crPlots(mode3)
plot(mode3,3)
#введем робастные ошибки
cov_wtite3 <- vcovHC(mode3, type="HC0")
coeftest(mode3,cov_wtite3)
#добавим отрасть
mode4 <- lm(data = data_out3, 
            formula = log(data_out3$Price)~data_out3$BVPS+data_out3$NIPS+log(data_out3$Total_assets) + 
              log(data_out3$FNCL_LVRG)+data_out3$Env_score+data_out3$Date+data_out3$Industry)
summary(mode4)
cov_wtite41 <- vcovHC(mode4, type="HC0")
coeftest(mode4,cov_wtite41)

#### Модель для фактора S #####
mods <- lm(data = data_out, 
           formula = data_out$Price~data_out$BVPS+data_out$NIPS+data_out$Total_assets + 
             data_out$FNCL_LVRG+data_out$Social_score+data_out$Date)
summary(mods)
plot(mods,1)
vif(mods)
boxCox(mods)
crPlots(mods)

mods2 <- lm(data = data_out, 
            formula = log(data_out$Price)~data_out$BVPS+data_out$NIPS+log(data_out$Total_assets) + 
              log(data_out$FNCL_LVRG)+data_out$Social_score+data_out$Date)
summary(mods2)
boxCox(mods2)
plot(mods2,1)
plot(mods2,2)
plot(mods2,3)

plot(mods2, 4)
cook_mod <- cooks.distance(mods2)
K <- which(cook_mod > 4/4511)
K

e <- resid(mods2)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out4 <- data_out[-OUT,]
View(data_out4)
#новая модель
mods3 <- lm(data = data_out4, 
            formula = log(data_out4$Price)~data_out4$BVPS+data_out4$NIPS+log(data_out4$Total_assets) + 
              log(data_out4$FNCL_LVRG)+data_out4$Social_score+data_out4$Date)
summary(mods3)
plot(mods3,1)
plot(mods3,2)
crPlots(mods3)
plot(mods3,3)
#введем робастные ошибки
cov_wtite4 <- vcovHC(mods3, type="HC0")
coeftest(mods3,cov_wtite4)
#добавим отрасль
mods4 <- lm(data = data_out4, 
            formula = log(data_out4$Price)~data_out4$BVPS+data_out4$NIPS+log(data_out4$Total_assets) + 
              log(data_out4$FNCL_LVRG)+data_out4$Social_score+data_out4$Date+data_out4$Industry)
summary(mods4)
cov_wtite5 <- vcovHC(mods4, type="HC0")
coeftest(mods4,cov_wtite5)

#### Модель для фактора G####
modg <- lm(data = data_out, 
           formula = data_out$Price~data_out$BVPS+data_out$NIPS+data_out$Total_assets + 
             data_out$FNCL_LVRG+data_out$Gov_score+data_out$Date)
summary(modg)
plot(modg,1)
vif(modg)
boxCox(modg)
crPlots(modg)

modg2 <- lm(data = data_out, 
            formula = log(data_out$Price)~data_out$BVPS+data_out$NIPS+log(data_out$Total_assets) + 
              log(data_out$FNCL_LVRG)+data_out$Gov_score+data_out$Date)
summary(modg2)
boxCox(modg2)
plot(modg2,1)
plot(modg2,2)
plot(modg2,3)
crPlots(modg2)
plot(modg2, 4)
cook_mod <- cooks.distance(modg2)
K <- which(cook_mod > 4/4511)
K

e <- resid(modg2)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out5 <- data_out[-OUT,]
View(data_out5)
#новая модель
modg3 <- lm(data = data_out5, 
            formula = log(data_out5$Price)~data_out5$BVPS+data_out5$NIPS+log(data_out5$Total_assets) + 
              log(data_out5$FNCL_LVRG)+data_out5$Gov_score+data_out5$Date)
summary(modg3)
plot(modg3,1)
plot(modg3,2)
crPlots(modg3)
plot(modg3,3)
#введем робастные ошибки
cov_wtite6 <- vcovHC(modg3, type="HC0")
coeftest(modg3,cov_wtite6)
#добавим отрасль
modg4 <- lm(data = data_out5, 
            formula = log(data_out5$Price)~data_out5$BVPS+data_out5$NIPS+log(data_out5$Total_assets) + 
              log(data_out5$FNCL_LVRG)+data_out5$Gov_score+data_out5$Date+data_out5$Industry)
summary(modg4)
cov_wtite7 <- vcovHC(modg4, type="HC0")
coeftest(modg4,cov_wtite7)

###выгрузим все красиво
library(stargazer)
stargazer(coeftest(mod7,cov_wtite),coeftest(mode3,cov_wtite3),coeftest(mods3,cov_wtite4),coeftest(modg3,cov_wtite6),type = "text",column.labels=c("Модель 1", "Модель 2","Модель 3", "Модель 4"),
          df=FALSE, digits=3, out = "models1.doc" )
stargazer(mod7,mode3,mods3,modg3, type = "text", digits = 3, out = "models2.doc" )

stargazer(coeftest(mod7_2,cov_wtite2),coeftest(mode4,cov_wtite41),coeftest(mods4,cov_wtite5),coeftest(modg4,cov_wtite7),type = "text",column.labels=c("Модель 1", "Модель 2","Модель 3", "Модель 4"),
          df=FALSE, digits=3, out = "models3.doc" )

stargazer(coeftest(mod7_2,cov_wtite2),coeftest(mode4,cov_wtite41),type = "text",column.labels=c("Модель 1", "Модель 2","Модель 3", "Модель 4"),
          df=FALSE, digits=3, out = "models32.doc" )

stargazer(coeftest(mods4,cov_wtite5),coeftest(modg4,cov_wtite7),type = "text",column.labels=c("Модель 1", "Модель 2","Модель 3", "Модель 4"),
          df=FALSE, digits=3, out = "models33.doc" )
stargazer(mod7_2,mode4,mods4,modg4,type = "text",column.labels=c("Модель 1", "Модель 2","Модель 3", "Модель 4"),
          df=FALSE, digits=3, out = "models4.doc" )


######Построение портфелей#####
full_data <- read_excel("Уни/8 семестр/Диплом/full_data.xlsx")
View(full_data)
data <- full_data[,c(1,2,3,4,5,6,7,8,10,13,14,15,66)]
View(data)
data$`Identifier (RIC)` <- as.factor(data$`Identifier (RIC)`)

#Топ 25 компаний и их устойчивость в топе
data2019 <- filter(data, data$Date==2019)
data2019 <- data2019[order(-data2019$ESG_score),]
top2019 <- data2019[c(1:25),c(1,2,4)]
View(top2019)

data2018 <- filter(data, data$Date==2018)
data2018 <- data2018[order(-data2018$ESG_score),]
top2018 <- data2018[c(1:25),c(1,2,4)]
View(top2018)

data2017 <- filter(data, data$Date==2017)
data2017 <- data2017[order(-data2017$ESG_score),]
top2017 <- data2017[c(1:25),c(1,2,4)]
View(top2017)

data2016 <- filter(data, data$Date==2016)
data2016 <- data2016[order(-data2016$ESG_score),]
top2016 <- data2016[c(1:25),c(1,2,4)]
View(top2016)

data2015 <- filter(data, data$Date==2015)
data2015 <- data2015[order(-data2015$ESG_score),]
top2015 <- data2015[c(1:25),c(1,2,4)]
View(top2015)

data2014 <- filter(data, data$Date==2014)
data2014 <- data2014[order(-data2014$ESG_score),]
top2014 <- data2014[c(1:25),c(1,2,4)]
View(top2014)

x <- c(top2019$`Company Name`,top2018$`Company Name`)
View(x)
x <- unique(x)
y <- c(top2017$`Company Name`,top2016$`Company Name`)
View(y)
y <- unique(y)
y <- c(top2018$`Company Name`,top2017$`Company Name`)
View(y)
y <- unique(y)
y <- c(top2016$`Company Name`,top2015$`Company Name`)
View(y)
y <- unique(y)
y <- c(top2019$`Company Name`,top2015$`Company Name`)
View(y)
y <- unique(y)
###Отберем наилучший и наихудший кваритили по ESG
bad_q2014 <- quantile(data2014$ESG_score, 0.25)
bad_q2014
bad_q2014_num <- which(data2014$ESG_score<bad_q2014)
bad_q2014_num

best_q2014 <- quantile(data2014$ESG_score, 0.75)
best_q2014
best_q2014_num <- which(data2014$ESG_score>best_q2014)
best_q2014_num



ticker_w2014 <- data2014$`Identifier (RIC)`[bad_q2014_num]
ticker_b2014 <- data2014$`Identifier (RIC)`[best_q2014_num]
#скачаем котировки акций
D <- new.env()
sDate <- as.Date("2014-10-30")
eDate <- as.Date("2015-10-30")
num_ticker <- length(ticker_b2014)

Temp_D <- list()
counter <- 1L

for(i in ticker_b2014)  {  
  
  getSymbols(
    i, 
    env = D, 
    reload.Symbols = FALSE, 
    from = sDate, 
    to = eDate,
    verbose = FALSE,
    warnings = TRUE,
    src = "yahoo",
    symbol.lookup = TRUE) 
  print(counter)
  
  Temp_D[[i]] <- Cl(D[[i]])  
  
  if (counter == length(ticker_b2014))
  { 
    #Merge all the objects of the list into one object. 
    Daily_Price   <- do.call(merge, Temp_D)
  }
  else
  {
    counter <- counter + 1
  }
  
}

View(Daily_Price)
best2015 <- Daily_Price
#для первого квартиля
D <- new.env()
sDate <- as.Date("2014-10-30")
eDate <- as.Date("2015-10-30")

num_ticker <- length(ticker_w2014)

Temp_D <- list()
counter <- 1L

for(i in ticker_w2014)  {  
  
  getSymbols(
    i, 
    env = D, 
    reload.Symbols = FALSE, 
    from = sDate, 
    to = eDate,
    verbose = FALSE,
    warnings = TRUE,
    src = "yahoo",
    symbol.lookup = TRUE) 
  print(counter)
  
  Temp_D[[i]] <- Cl(D[[i]])  
  
  if (counter == length(ticker_w2014))
  { 
    #Merge all the objects of the list into one object. 
    Daily_Price   <- do.call(merge, Temp_D)
  }
  else
  {
    counter <- counter + 1
  }
  
}
View(Daily_Price)
worst2015 <- Daily_Price

#Построим портфель
#сначала посмотрим на какую-нибудь акцию
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(DEoptim)
library(quantmod)
best2015 <- na.omit(best2015)
Returns20151 <-Return.calculate(best2015, method = "log") 
Returns20151
A <- Returns20151$A.Close
plot(A)
chart.RollingPerformance(A, width = 30, FUN = "sd") #скольязящее среднее для дисперсии
#вывод - есть гетероскедастичность
chart.CumReturns(A) #видим как мы росли 
chart.Drawdown(A) #куммулятивные провалы
chart.Histogram(A, methods = c("add.density"))
chart.Histogram(A, methods = c("add.density", "add.normal"))

colnames(Returns20151)
Port <- portfolio.spec(assets = colnames(Returns20151))

#средние доходности
Returns20151 <- na.omit(Returns20151)
(colMeans(Returns20151))

#линейные ограничения
Port <- add.constraint(Port, type = "full_invsetment") #сумма весов 1
Port <- add.constraint(Port, type = "long_only")
#Port <- add.constraint(Port, type="diversification", div_target=1-(1/dim(Returns20191)[2]))
Port <- add.constraint(Port, type = "return", name = "mean", return_target = 0.001)
#добавим цель - минимизация дисперсии
Port <- add.objective(Port, type = "risk", name = "StdDev")

#метод оптимизации
Result <- optimize.portfolio(portfolio = Port, R = Returns20151, optimize_method = "ROI")
Result$weights
Result$objective_measures

w1 <- (Result$weights)
1-sum(w1^2)
#построим плохой портфель
worst2015 <- na.omit(worst2015)
Returns20152 <-Return.calculate(worst2015, method = "log") 
BIO <- Returns20152$BIO.Close
plot(BIO)
chart.RollingPerformance(BIO, width = 30, FUN = "sd") #скольязящее среднее для дисперсии
#вывод - есть гетероскедастичность
chart.CumReturns(BIO) #видим как мы росли 
chart.Drawdown(BIO) #куммулятивные провалы
chart.Histogram(BIO, methods = c("add.density"))
chart.Histogram(BIO, methods = c("add.density", "add.normal"))

colnames(Returns20152)
Port2 <- portfolio.spec(assets = colnames(Returns20152))

#средние доходности
Returns20152 <- na.omit(Returns20152)
mean(colMeans(Returns20152))
#линейные ограничения
Port2 <- add.constraint(Port2, type = "full_invsetment") #сумма весов 1
Port2 <- add.constraint(Port2, type = "long_only")
#Port <- add.constraint(Port, type="diversification", div_target=1-(1/dim(Returns20191)[2]))
Port2 <- add.constraint(Port2, type = "return", name = "mean", return_target = 0.001)

#добавим цель - минимизация дисперсии
Port2 <- add.objective(Port2, type = "risk", name = "StdDev")
#метод оптимизации
Result2 <- optimize.portfolio(portfolio = Port2, R = Returns20152, optimize_method = "ROI")
Result2$weights
Result2$objective_measures
w2 <- (Result2$weights)
1-sum(w2^2)

#найдем доходность портфеля для каждого периода
Day_return1 <- Return.portfolio(R = Returns20151, weights = w1)
plot(Day_return1)


Day_return2 <- Return.portfolio(R = Returns20152, weights = w2)
plot(Day_return2)

#скачаем цены акций за 2016-2018 года для хорошего портфеля
D <- new.env()
sDate <- as.Date("2015-10-30")
eDate <- as.Date("2018-10-30")
num_ticker <- length(ticker_b2014)

Temp_D <- list()
counter <- 1L

for(i in ticker_b2014)  {  
  
  getSymbols(
    i, 
    env = D, 
    reload.Symbols = FALSE, 
    from = sDate, 
    to = eDate,
    verbose = FALSE,
    warnings = TRUE,
    src = "yahoo",
    symbol.lookup = TRUE) 
  print(counter)
  
  Temp_D[[i]] <- Cl(D[[i]])  
  
  if (counter == length(ticker_b2014))
  { 
    #Merge all the objects of the list into one object. 
    Daily_Price   <- do.call(merge, Temp_D)
  }
  else
  {
    counter <- counter + 1
  }
  
}
View(Daily_Price)
best2016_2018 <- Daily_Price
# и для плохого портфеля
D <- new.env()
sDate <- as.Date("2015-10-30")
eDate <- as.Date("2018-10-30")
num_ticker <- length(ticker_w2014)

Temp_D <- list()
counter <- 1L

for(i in ticker_w2014)  {  
  
  getSymbols(
    i, 
    env = D, 
    reload.Symbols = FALSE, 
    from = sDate, 
    to = eDate,
    verbose = FALSE,
    warnings = TRUE,
    src = "yahoo",
    symbol.lookup = TRUE) 
  print(counter)
  
  Temp_D[[i]] <- Cl(D[[i]])  
  
  if (counter == length(ticker_w2014))
  { 
    #Merge all the objects of the list into one object. 
    Daily_Price   <- do.call(merge, Temp_D)
  }
  else
  {
    counter <- counter + 1
  }
  
}
View(Daily_Price)
worst2016_2018 <- Daily_Price
#построим портфели с заданными весами на этом промежутке
Returns20161 <- Return.calculate(best2016_2018, method = "log") 

ret20161 <- Return.portfolio(R = Returns20161, weights = w1)
charts.PerformanceSummary(R = ret20161)
sum(ret20161)
var(ret20161)
Table_return <- table.CalendarReturns(ret20161)
Table_return
StdDev.annualized(ret20161) 
year_ret20161 <- periodReturn(ret20161)
year_ret20161
?periodReturn

Returns20162 <- Return.calculate(worst2016_2018, method = "log") 

ret20162 <- Return.portfolio(R = Returns20162, weights = w2)
charts.PerformanceSummary(R = ret20162)
sum(ret20162)
var(ret20162)


#####Портфели с равными весами#####
library(readxl)
best_2018 <- read_excel("best_2018.xlsx")
View(best_2018)
str(best_2018)
d <- best_2018[,1]
d <- as.data.frame(d)
b <- best_2018[,-1]
str(b)
ymd(d)
best <- as.xts(b, order.by = as.Date(d$Date))
best_2018 <- na.omit(best_2018)
str(best_2018)

Ret161 <- Return.calculate(best, method = "log") 
View(Ret161)
Ret161 <- na.omit(Ret161)

P3<- Return.portfolio(Ret161, weights = rep((1/125),times = 125))

charts.PerformanceSummary(R = P3)
sum(P3)
mean(P3)
var(P3)
Table_return1 <- table.CalendarReturns(P3)
Table_return1
StdDev.annualized(P3) 

#для плохого портфеля
worst_2018 <- read_excel("worst_2018.xlsx")
View(worst_2018)
str(worst_2018)
d <- worst_2018[,1]
#d <- as.data.frame(d)
w <- worst_2018[,-1]
str(w)
worst <- as.xts(w, order.by = as.Date(d$Date))
str(worst)

Ret162 <- Return.calculate(worst, method = "log") 
View(Ret162)
Ret162 <- na.omit(Ret162)

P4<- Return.portfolio(Ret162, weights = rep((1/105),times = 105))

charts.PerformanceSummary(R = P4)
sum(P4)
mean(P4)
var(P4)
Table_return2 <- table.CalendarReturns(P4)
Table_return2
StdDev.annualized(P4) 

dim(Table_return1)
t.test(Table_return1[,13],Table_return2[,13], var.equal = FALSE)

####С короткими позициями####
best_2015 <- read_excel("best_2015.xlsx")
View(best_2015)
str(best_2015)
d <- best_2015[,1]
b <- best_2015[,-1]
str(b)
best <- as.xts(b, order.by = as.Date(d$Date))

str(best)

Ret151 <- Return.calculate(best, method = "log") 
View(Ret151)
Ret151 <- na.omit(Ret151)

colnames(Ret151)
Port5 <- portfolio.spec(assets = colnames(Ret151))

#средние доходности
max((colMeans(Ret151)))
min((colMeans(Ret151)))
#линейные ограничения
Port5 <- add.constraint(Port5, type = "full_invsetment") #сумма весов 1
Port5 <- add.constraint(Port5, type = "return", name = "mean", return_target = 0.002)
#добавим цель - минимизация дисперсии
Port5 <- add.objective(Port5, type = "risk", name = "StdDev")

#метод оптимизации
Result5 <- optimize.portfolio(portfolio = Port5, R = Ret151, optimize_method = "ROI")
Result5$weights
Result5$objective_measures

w5 <- (Result5$weights)

#для плохого портфеля
worst_2015 <- read_excel("worst_2015.xlsx")
View(worst_2015)
str(worst_2015)
d <- worst_2015[,1]
b <- worst_2015[,-1]
str(b)
worst <- as.xts(b, order.by = as.Date(d$Date))

str(worst)

Ret152 <- Return.calculate(worst, method = "log") 
View(Ret152)
Ret152 <- na.omit(Ret152)

colnames(Ret152)
Port6 <- portfolio.spec(assets = colnames(Ret152))

#средние доходности
max((colMeans(Ret152)))
min((colMeans(Ret152)))
#линейные ограничения
Port6 <- add.constraint(Port6, type = "full_invsetment") #сумма весов 1
Port6 <- add.constraint(Port6, type = "return", name = "mean", return_target = 0.002)
#добавим цель - минимизация дисперсии
Port6 <- add.objective(Port6, type = "risk", name = "StdDev")

#метод оптимизации
Result6 <- optimize.portfolio(portfolio = Port6, R = Ret152, optimize_method = "ROI")
Result6$weights
Result6$objective_measures

w6 <- (Result6$weights)

#посмотрим как вели весбя портфели на продолжительном периоде
ret20161 <- Return.portfolio(R = Ret161, weights = w5)
charts.PerformanceSummary(R = ret20161)
sum(ret20161)
var(ret20161)
Table_return3 <- table.CalendarReturns(ret20161)
Table_return3
StdDev.annualized(ret20161) 

ret20162 <- Return.portfolio(R = Ret162, weights = w6)
charts.PerformanceSummary(R = ret20162)
sum(ret20162)
var(ret20162)
Table_return4 <- table.CalendarReturns(ret20162)
Table_return4
StdDev.annualized(ret20162) 


?t.test
library(stats)
t.test(Table_return3[,13],Table_return4[,13],var.equal = FALSE)


####Регрессии с финансовыми показателями#####
View(full_data)
m <- lm(data=data_out, formula = data_out$ROE~data_out$ESG_score+data_out$Total_assets+data_out$FNCL_LVRG+data_out$QUICK_RATIO+data_out$ASSET_GROWTH+data_out$Date)
summary(m)

plot(m, which = 1)
#пропущена какая-то степень

crPlots(m)
#попробуем взять логарифм активов, ликвидности, роста и рычага
m2 <- lm(data=data_out, formula = data_out$ROE~data_out$ESG_score+log(data_out$Total_assets)+log(data_out$FNCL_LVRG)+log(data_out$QUICK_RATIO)+log(data_out$ASSET_GROWTH)+data_out$Date)
summary(m2)
stargazer(m2, type = "text")
crPlots(m2)
plot(m2,2)
#остатки не нормальны
plot(m2,3)

plot(m2, 4)
cook_mod <- cooks.distance(m2)
K <- which(cook_mod > 4/2851)
K

e <- resid(m2)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out4 <- data_out[-OUT,]
#новая модель
m3 <- lm(data=data_out4, formula = data_out4$ROE~data_out4$ESG_score+log(data_out4$Total_assets)+log(data_out4$FNCL_LVRG)+log(data_out4$QUICK_RATIO)+log(data_out4$ASSET_GROWTH)+data_out4$Date)
plot(m3,1)
plot(m3,2)
plot(m3,3)
#есть небольшая гетероскедастичность, введем робастные ошибки
cov_wtite8 <- vcovHC(m3, type="HC0")
coeftest(m3,cov_wtite8)

#####Модель для ROA#####
data_out$RETURN_ON_ASSET <- data_out$RETURN_ON_ASSET/100
m4 <- lm(data=data_out, formula = data_out$RETURN_ON_ASSET~data_out$ESG_score+log(data_out$Total_assets)+log(data_out$FNCL_LVRG)+log(data_out$QUICK_RATIO)+log(data_out$ASSET_GROWTH)+data_out$Date)
summary(m4)
stargazer(m4, type = "text")
crPlots(m4)
plot(m4,2)
#остатки не нормальны
plot(m4,3)

plot(m4, 4)
cook_mod <- cooks.distance(m4)
K <- which(cook_mod > 4/2851)
K

e <- resid(m4)  
g <- abs(e/sd(e))
barplot(g)
N <- which(g>3)
N
OUT <- c(K,N)
OUT <- unique(OUT)
data_out5 <- data_out[-OUT,]
#новая модель
m5 <- lm(data=data_out5, formula = data_out5$RETURN_ON_ASSET~data_out5$ESG_score+log(data_out5$Total_assets)+log(data_out5$FNCL_LVRG)+log(data_out5$QUICK_RATIO)+log(data_out5$ASSET_GROWTH)+data_out5$Date)
summary(m5)
plot(m5,1)
plot(m5,2)
plot(m5,3)
#есть небольшая гетероскедастичность, введем робастные ошибки
cov_wtite9 <- vcovHC(m5, type="HC0")
coeftest(m5,cov_wtite9)
#выведем все красиво
stargazer(coeftest(m3,cov_wtite8), coeftest(m5,cov_wtite9),type = "text",column.labels=c("Модель 9", "Модель 10"),
          df=FALSE, digits=3, out = "models4.doc" )
stargazer(m3,m5,type = "text",column.labels=c("Модель 1", "Модель 2"),
          df=FALSE, digits=3, out = "models5.doc" )


####Модель Фамы-Френча####
library(dplyr)
library(xts)
library(ggplot2)
library(PortfolioAnalytics)
library(readxl)
library(openxlsx)
library(lubridate)
library(car)
library(stargazer)
#загрузим данные для модели Фамы-Френча, они взяты с https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
library(readxl)
#данные
esg_score <- read_excel("esg score.xlsx")
sp <- read_excel("sp.xlsx")
FF <- read_excel("FF3.xlsx")
data_FF <- mutate_all(FF, as.numeric)
str(data_FF)

#переименую одно имя
names(data_FF)[2] <- "Mkt_RF"
names(data_FF)

#поделю на 100 чтобы получить доходности
data_FF2 <- mutate(data_FF, Mkt_RF = Mkt_RF/100, SMB = SMB/100, HML = HML/100, RF = RF/100)
View(data_FF2)
#превратим в xts
data_FF3 <- data_FF2
data_FF3$Date <- ymd(data_FF2$Date)

#обработка данных
Time <- sp$Date
data <- select(sp, - Date)
data2 <- xts(data, order.by = as.Date(Time))

#перейдем к доходностям
r <- CalculateReturns(data2, method = "log")
r <- na.omit(r)

#объединим с ФФ
data_FF4 <- select(data_FF3, -Date)
data_FF5 <- xts(data_FF4, order.by = data_FF3$Date)
data_all <- merge(r, data_FF5)

#удалим пропуски
data_all <- na.omit(data_all)
dim(data_all)
head(data_all)

#отберем 2020 год
data_2020 <- window(data_all, start = "2020-01-01", end = "2020-12-31")
dim(data_2020)

#для проверки построим обычную модель без ESG
names(data_2020)
mod <- lm(MSFT - RF.1 ~ Mkt_RF + SMB + HML, data_2020)
summary(mod)
#все хорошо

#создадим ESG фактор
esg_2020 <- filter(esg_score, Date == 2020)
up <- filter(esg_2020, ESG_score >
               quantile(esg_2020$ESG_score, 0.7))
Names_up <- up$`Identifier (RIC)`
data_2020_up <- data_2020[, colnames(data_2020) %in% Names_up]

#вычислим доходность портфеля при равенстве весов
ESG_high <- rowMeans(data_2020_up)

#аналогично для нижних 30%
down <- filter(esg_2020, ESG_score <
                 quantile(esg_2020$ESG_score, 0.3))
Names_down <- down$`Identifier (RIC)`
data_2020_down <- data_2020[, colnames(data_2020) %in% Names_down]

#вычислим доходность портфеля при равенстве весов
ESG_down <- rowMeans(data_2020_down)

#Вычислим разницу верхнего и нижнего
ESG_factor <- ESG_high - ESG_down

#переведем в форму ряда
ESG_factor <- xts(ESG_factor, order.by = date(data_2020))

#добавим в данные
data_2020_2 <- merge(data_2020, ESG_factor)
dim(data_2020_2)

#проверочная регрессия
mod2 <- lm(MSFT - RF.1 ~ Mkt_RF + SMB + HML + ESG_factor, data_2020_2)
summary(mod2)

#поищем значимые
head(names(data_2020_2))
mod3 <- lm(JNJ - RF.1 ~ Mkt_RF + SMB + HML + ESG_factor, data_2020_2)
summary(mod3)

#построим оценки для всех и выгрузим
#первые 486 - это акции
K <- matrix(0, nrow = 486, ncol = 5)
t_value <- matrix(0, nrow = 486, ncol = 5)
Stars <- matrix(0, nrow = 486, ncol = 5)
q <- qt(0.975, 247)
R2 <- NULL

#запустим цикл
for(i in 1:486) {
  mod4 <- lm(data_2020_2[,i] - RF.1 ~ Mkt_RF +
               SMB + HML + ESG_factor, data_2020_2)
  S <- summary(mod4)
  K[i,] <- S$coefficients[,1]
  t_value[i,] <- S$coefficients[,3]
  Stars[i,] <- abs(S$coefficients[,3]) > q
  R2[i] <- S$r.squared
}

#сколько раз ESG фактор значим?
sum(Stars[,5])
#значит, ESG фактор хорошо прогнозирует доходность
#сколько раз значима альфа?
sum(Stars[,1])
#ни разу

#проделаем все то же самое без R2 и посмотрим на сколько он 
#меняется при добавлении ESG
K2 <- matrix(0, nrow = 486, ncol = 4)
t_value2 <- matrix(0, nrow = 486, ncol = 4)
Stars2 <- matrix(0, nrow = 486, ncol = 4)
q <- qt(0.975, 247)
R2_2 <- NULL

#запустим цикл
for(i in 1:486) {
  mod4 <- lm(data_2020_2[,i] - RF.1 ~ Mkt_RF +
               SMB + HML, data_2020_2)
  S <- summary(mod4)
  K2[i,] <- S$coefficients[,1]
  t_value2[i,] <- S$coefficients[,3]
  Stars2[i,] <- abs(S$coefficients[,3]) > q
  R2_2[i] <- S$r.squared
}

#изменен R2
R2_delta <- R2 - R2_2

#среднее изменение R2
mean(R2_delta)

#изменение R2 в моделях, где ESG значим
R2_delta_star <- R2_delta*Stars[,5]
R2_delta_star <- R2_delta_star[R2_delta_star != 0]
mean(R2_delta_star)


#выгрузим таблицу со значимыми коэффициентами (буду ставить нули там,
#где нет значимости)
Data_result <- data.frame(K*Stars, R2_delta = R2_delta*Stars[,5])
#первый столбец не выгружаю, там нули
Data_result <- Data_result[, -1]
colnames(Data_result) <- c("Mkt_RF", "SMB", "HML", "ESG", "R2_d")
rownames(Data_result) <- colnames(r)


#выгрузка
stargazer(Data_result, type = "html", 
          summary = FALSE, out = "Data_result.html")

####ESG-граница
library(dplyr)
library(xts)
library(ggplot2)
library(PortfolioAnalytics)

#данные
esg_score <- read_excel("esg score.xlsx")
sp <- read_excel("sp.xlsx")
esg_score <- na.omit(esg_score)
sp <- na.omit(sp)

#обработка данных
Time <- sp$Date
data <- select(sp, - Date)

#переведем в формат дат
data2 <- xts(data, order.by = as.Date(Time))
#отберем 2020 год
data_2020 <- window(data2, start = "2020-01-01", end = "2020-12-31")

#составим топ ESG за 2020
esg_2020 <- filter(esg_score, Date == 2020)


#отберем 100 лучших активов
up_100 <- filter(esg_2020, ESG_score >
                   quantile(esg_2020$ESG_score, 0.8))
dim(up_100)
#получился 101

#достанем их имена
Names <- up_100$`Identifier (RIC)`

#отберем их доходности
data_2020_100 <- data_2020[, colnames(data_2020) %in% Names]
dim(data_2020_100)

#перейдем к доходностям
r <- CalculateReturns(data_2020_100, method = "log")
r <- na.omit(r)

#вектор средних
r_mean <- colMeans(r)

#ковариационная матрица
V <- var(r)

#их ESG параметры
ESG <- up_100[Names %in% colnames(data_2020), 4]

#отклонения от среднего ESG
ESG_norm <- ESG$ESG_score - mean(ESG$ESG_score)

#оптимальные параметры
SD <- mean(sqrt(diag(V)))
ESG_target <- seq(-20,
                  20, length.out = 100)
SR <- NULL

#запустим цикл
for(i in 1:100) {
  #получим оптимальные веса
  ESG_stand <- ESG_norm - ESG_target[i]
  
  #получим веса
  Pi <- - (ESG_stand %*% solve(V) %*% r_mean)/(ESG_stand %*% solve(V) %*% ESG_stand)
  Teta <- 1/SD * sqrt(r_mean %*% solve(V) %*% t(r_mean + Pi %*% ESG_stand))
  Teta <- as.numeric(Teta)
  x <- solve(V) %*% t(r_mean + Pi %*% ESG_stand)/ Teta
  SR[i] <- sum(r_mean*x)/SD}

#граница
data_2 <- data.frame(ESG_target, SR)
ggplot(data_2, aes(ESG_target, SR)) + geom_line() + theme_bw()

#рыночный порфтель: проверка
z <- solve(V) %*% r_mean
w <- z/sum(z)
SD3 <- sqrt(t(w) %*% V %*% w)
M <- t(w) %*% r_mean
M/SD3
