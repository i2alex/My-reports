---
title: "Анализ теории Дж. Арриги о системных циклах накопления капиталов"
author: Калиновский А.
output:
  html_document: default
  pdf_document: default
  word_document: default
subtitle: сквозь призму анализа счета текущих операций платежного баланса(Current account balance) стран мира
header-includes:
- \usepackage[T2A]{fontenc}
- \usepackage[utf8]{inputenc}
- \usepackage[russian]{babel}
---



##1)Вступление
В этом небольшом исследовании, автор поставил перед собой задачу подтвердить либо опровергнуть выводы, сделанные Дж. Арриги в рамках своей теории системных циклов накопления капиталов, о том, что мир находится в процессе перехода на новый, Азиатский цикл накопления. В своей книге «Долгий двадцатый век» он выделяет четыре исторических цикла накопления – Генуэзский системный цикл накопления(XV-XVI), Голландский системный цикл накопления (XVII – XVIII века), Британский системный цикл накопления(XIX - начало ХХ века) и последний, который находится в своей завершающей стадии - Американский системный цикл накопления(ХХ - начало XXI века). Уже в своей следующей и последней книге «Адам Смит в Пекине, Арриги уточняет, что новым ядром накопления является именно Китай. 
Для анализа предлагается рассмотреть несколько экономических показателей. Начнем с анализа баланса счета текущих операций стран Запада и стран АТР. 

[Счёт текущих операций][1] — раздел платёжного баланса страны, в котором фиксируются экспорт и импорт товаров и услуг, чистый доход от инвестиций и чистый объём трансфертных платежей.

Баланс текущих операций — счет платежного баланса, на котором отражаются операции с товарами, услугами и доходами. Текущий платежный баланс включает экспорт и импорт товаров и услуг, доход от иностранных инвестиций и текущие трансферты. В нём отражаются операции, завершающиеся в течение периода, за который составляется баланс, действие которых не сказывается на платежном балансе в последующие периоды. В счёте выделяются три статьи (баланса): «Товары и услуги», «Доходы» и «Текущие трансферты». Счет текущих операций имеет следующий вид.


##2)Обработка данных
###Загрузка необходимых пакетов

```r
library(dplyr)
library(ggplot2)
library(wbstats) 
```

###Загрузка данных
Используя пакет wbstats для работы с базами данных Всемирного Банка, выбираем индикатор, который относится к текущему счету платежного баланса - BN.CAB.XOKA.CD.

```r
# Выбираем индикатор, и загружаем даты с 91 по 15 года
cab_data <- wb(indicator = "BN.CAB.XOKA.CD", startdate = 1991, enddate = 2015)
```

###Очистка данных

```r
# Убираем лишние колонки
cab_data <- select(cab_data, -(indicatorID:iso2c)) 
# Значения колонок date и country делаем factor
cab_data$date <- as.numeric(cab_data$date)
cab_data$country <- as.factor(cab_data$country)
# Для более удобной работы с цифрами, уменьшаем value на миллиард, и округляем до 3 цифер после запятой
cab_data$value <- cab_data$value/1000000000
cab_data$value <- round(cab_data$value, 3)
# Даем имена колонкам
colnames(cab_data) <- c("value", "year", "country") 
```


## 3)Анализ изменений счета текущих операций платежного баланса стран Азиатско-Тихоокеанского региона 

```r
# Вытягиваем из таблицы нужные для анализа страны. Ядро АТР
atrn <- grep("India|Japan|Indonesia|Korea, Rep|Russia|Singapore|
                      |Thailand|Malasiya|Philippines|Vietnam|Bangladesh", cab_data$country)

# Создаем новую таблицу основных стран АТР (без Китая)
atr <- data.frame()
for(i in atrn){
        atr <- rbind(atr, cab_data[i,])
}
```

Учитывая что ВБ дает отдельные данные по Китаю, Гон-Конгу и Макао, придется в ручную объеденить их единую таблицу под названием Китай. 

```r
atrn <- grep("China", cab_data$country)

atrC <- data.frame()
for(i in atrn){
        atrC <- rbind(atrC, cab_data[i,])
}

# Объеденяем данные Гон-Конга и Макао с общими данными Китая

atrC[grepl("Hong|Macao", atrC$country, ignore.case=T), c("country")] <- "China"

# Убераем повоторения China по годам
chinaHM <- data.frame()
temp2 <- filter(atrC, year == "1991" , country == "China")
temp2 <- list(sum(temp2$value), as.numeric(temp2[1,2]), "China")
chinaHM <- rbind(chinaHM, temp2)

# Создаем новую таблицу, которая объеденяет данные China, Hong-Kong и Macao
for (i in c(1992:2015)){
temp <- filter(atrC, year == i , country == "China")
temp <- c(sum(temp$value), as.numeric(temp[1,2]), "China")
chinaHM <- rbind(chinaHM, temp)
}

# Корректируем названия колонок 
names(chinaHM) <- c("value", "year", "country")

chinaHM <- arrange(chinaHM, desc(year))
# Переводим колонку valuу из factor в numeric
chinaHM$value <- as.numeric(as.character(chinaHM$value))
# Добавляем Китай в таблицу выбранных стран АТР с очищенными выше данными 
atr <- rbind(atr,chinaHM)
atr <- arrange(atr, country, year) 
# Для возможности реализации графика
atr$value <- as.numeric(atr$value)
atr$year <- as.numeric(atr$year)
```

Отчет по странам АТР

```r
summary(atr)
```

```
##      value              year             country   
##  Min.   :-91.471   Min.   :1991   Bangladesh : 25  
##  1st Qu.: -1.026   1st Qu.:1998   China      : 25  
##  Median :  5.394   Median :2004   India      : 25  
##  Mean   : 29.922   Mean   :2004   Indonesia  : 25  
##  3rd Qu.: 32.125   3rd Qu.:2010   Korea, Rep.: 25  
##  Max.   :456.795   Max.   :2015   Philippines: 25  
##                                   (Other)    :112
```

### График изменений счета текущих операций платежного баланса по основным странам АТР

```r
# График изменений счета текущих операций платежного баланса ТОП6 стран АТР -> подготовка данных
atrtop6 <- atr %>% arrange(country, desc(year))
atrtop6 <- atrtop6[c(26:145,171:192),]

# График изменений счета текущих операций платежного баланса ТОП6 стран АТР -> построение графика
ggplot(data = atrtop6, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - ATR top 6") + 
        facet_grid(country~., scales = "free")
```

![plot of chunk unnamed-chunk-341](figure/unnamed-chunk-341-1.png)

```r
# График изменений счета текущих операций платежного баланса стран АТР -> подготовка данных
atr2 <- arrange(atr, year)
atr2$year <- as.numeric(as.character(atr2$year))
atr2$value <- as.numeric(atr2$value)
atrDF <- data.frame()
tempatr <- data.frame()

for(i in 1991:2015){
tempatr <- atr2 %>% filter(year == i)%>%
        summarise(value = sum(value), year = i) 
atrDF <- rbind(atrDF, tempatr)
}

# График изменений счета текущих операций платежного баланса стран АТР  в целом -> построение графика
ggplot(data = atrDF, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(colour="darkgreen")+
        geom_smooth(method = "loess")+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - ATR countries")
```

![plot of chunk unnamed-chunk-341](figure/unnamed-chunk-341-2.png)

## 4)Рассмотрим теперь изменения счета текущих операций платежного баланса по странам Запада 

```r
# Вытягиваем из таблицы нужные для анализа страны. Ядро западной цивилизации 
westn <- grep("United States|United Kingdom|Canada|Australia|Germany|France|Italy|
                      |Austria|Spain", cab_data$country)
# Заменяем ядро европейских стран, всеми странами входящими в ЕС 
westnE <- grep("United States|Euro area|Canada|Australia", cab_data$country)


# Создаем новую таблицу стран-ядра Западной цивилизации
west <- data.frame()
for(i in westn){
        west <- rbind(west, cab_data[i,])
}

# Создаем новую таблицу всей Западной цивилизации
westE <- data.frame()
for(i in westnE){
        westE <- rbind(westE, cab_data[i,])
}
```

Отчет по странам Западной цивилизация

```r
summary(west)
```

```
##      value               year           country  
##  Min.   :-806.726   Min.   :1991   Australia:25  
##  1st Qu.: -49.459   1st Qu.:1997   Canada   :25  
##  Median : -19.022   Median :2004   France   :25  
##  Mean   : -49.652   Mean   :2003   Germany  :25  
##  3rd Qu.:   7.574   3rd Qu.:2010   Italy    :25  
##  Max.   : 283.908   Max.   :2015   Spain    :25  
##                                    (Other)  :61
```

### График изменений счета текущих операций платежного баланса по странам Западной цивилизации

```r
# График изменений счета текущих операций платежного баланса стран Западного ядра
ggplot(data = west, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance of payments - West countries")+
        facet_grid(country~., scales = "free")
```

![plot of chunk unnamed-chunk-344](figure/unnamed-chunk-344-1.png)

```r
# График изменений счета текущих операций платежного баланса стран Запада  в целом -> подготовка данных
west2 <- arrange(westE, year)
west2$year <- as.numeric(as.character(west2$year))
westDF <- data.frame()

for(i in 1991:2015){
temp <- west2 %>% filter(year == i)%>%
        summarise(value = sum(value), year = i) 
westDF <- rbind(westDF, temp)
}

# График изменений счета текущих операций платежного баланса стран Запада в целом -> построение графика
ggplot(data = westDF, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(colour="darkgreen")+
        geom_smooth(method = "loess")+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - West countries")
```

![plot of chunk unnamed-chunk-344](figure/unnamed-chunk-344-2.png)

## 5) Сравнение изменений счета текущих операций платежного баланса стран Запада и АТР


```r
westDF$countries <- rep("West", 25)
atrDF$countries <- rep("ATR", 25)
s_ummery <- rbind(westDF, atrDF)

#Изменения счета текущих операций платежного баланса стран Запада и АТР по годам
s_ummery2 <- westDF
s_ummery2$ATR <- atrDF$value
s_ummery2$countries <- NULL
s_ummery2 <- s_ummery2 %>% select(Year = year, West = value, ATR = ATR)
#Сравнительная таблица Запад-АТР
s_ummery2
```

```
##    Year     West     ATR
## 1  1991  -30.610  -6.545
## 2  1992  -83.637  -4.503
## 3  1993 -116.426 -18.366
## 4  1994 -151.412   6.411
## 5  1995 -137.291 -15.106
## 6  1996 -136.704  41.889
## 7  1997 -161.620 119.544
## 8  1998 -241.818 219.505
## 9  1999 -316.484 217.948
## 10 2000 -433.620 236.213
## 11 2001 -471.391 172.949
## 12 2002 -485.801 223.723
## 13 2003 -495.690 289.704
## 14 2004 -626.914 385.862
## 15 2005 -685.651 436.445
## 16 2006 -814.864 578.188
## 17 2007 -772.055 747.386
## 18 2008 -714.852 694.384
## 19 2009 -669.567 544.803
## 20 2010 -557.371 605.519
## 21 2011 -542.305 427.587
## 22 2012 -562.275 372.459
## 23 2013 -303.480 325.939
## 24 2014 -170.435 512.873
## 25 2015 -240.700 724.837
```

```r
#Сравнительный график изменений счета текущих операций платежного баланса стран Запада и АТР 
ggplot(data = s_ummery, aes(x = year, y = value))+
        geom_line(aes(colour=countries))+
        geom_point(aes(colour=countries))+
        theme_bw()+
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - ATR and West")
```

![plot of chunk unnamed-chunk-345](figure/unnamed-chunk-345-1.png)

### Отдельное сравнение изменений счета текущих операций платежного баланса США и Китая

```r
# Для отдельного сравнения Китая и США с общей таблицы данных выделим данные по США
temp <- grep("United States", cab_data$country)

# Создаем новую таблицу США 
cabUS <- data.frame()
for(i in temp){
        cabUS <- rbind(cabUS, cab_data[i,])
}

#Создаем сводную таблицу измений счета текущих операций платежного баланса США и Китая по годам
chimerica <- rbind(cabUS, chinaHM)
chimerica2 <- cabUS  
chimerica2$China <- chinaHM$value
chimerica2$country <- NULL
chimerica2 <- chimerica2 %>% select(Year = year, USA = value, China = China)

# Сравнительная таблица США-Китай
chimerica2
```

```
##      Year      USA   China
## 6326 2015 -462.961 353.154
## 6327 2014 -392.066 300.299
## 6328 2013 -366.424 173.103
## 6329 2012 -446.527 236.433
## 6330 2011 -460.358 164.953
## 6331 2010 -441.963 264.911
## 6332 2009 -384.024 270.462
## 6333 2008 -690.789 456.795
## 6334 2007 -718.641 384.706
## 6335 2006 -806.726 258.578
## 6336 2005 -745.445 156.855
## 6337 2004 -633.768  89.284
## 6338 2003 -521.342  63.012
## 6339 2002 -458.092  50.903
## 6340 2001 -395.331  27.793
## 6341 2000 -410.762  28.063
## 6342 1999 -295.534  31.780
## 6343 1998 -215.037  34.342
## 6344 1997 -140.725  36.963
## 6345 1996 -124.727   7.243
## 6346 1995 -113.561   1.618
## 6347 1994 -121.642   6.908
## 6348 1993  -84.783 -11.609
## 6349 1992  -51.605   6.401
## 6350 1991    2.851  13.272
```

```r
# Для коректного отображения на графике, переводим year в numeric
chimerica$year <- as.numeric(chimerica$year)
```

Сравнительный график измений счета текущих операций платежного баланса США и Китая

```r
ggplot(data = chimerica, aes(x = year, y = value))+
         geom_line(aes(colour=country))+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - China and USA")
```

![plot of chunk unnamed-chunk-347](figure/unnamed-chunk-347-1.png)

## 6) ТОП 25 стран мира положительного счета текущих операций торгового баланса (с 2000 по 2015 года):

```r
# Создаем таблицу стран мира без Китая, Гон-Конга и Макао
temp <- grep("China|Hong Kong SAR, China|Macao SAR, China", cab_data$country)
alldata <- cab_data[-temp,]
# Объеденяем эту таблицу с объедененной ранее таблицы Китая, Гон-Конга и Макао
alldata <- rbind(alldata, chinaHM)
# Удаляем года 1991-1999 
alldata <- arrange(alldata, year)
alldata <- alldata[-(1:1289),]
alldata <- arrange(alldata, country, desc(year))

#Суммируем все ежегодные результаты текущих операций торгового баланса и сводим 15-ти летний баланс по каждой из стран
allDF <- data.frame(value = 0, country = "country")
l <- 1

for(i in alldata$country){
        temp2 <- grep(i, alldata$country)
        temp2 <- alldata[temp2,]
        temp2 <- temp2 %>% summarise(value = sum(value), country = i) 
        if(allDF$country[l] != temp2$country){
        allDF <- rbind(allDF, temp2)
        }else {next}
        
        l <- l+1
        }
# Сортируем по значению value и выделяем первые 25 стран + Евросоюз
top20 <- arrange(allDF, desc(value)) 
top20 <- top20[1:20,]
# Удаляем из топ 25 Niger, который попал туда из-за сходства с Нигерией
top20 <- top20[-17,]
```
Таблица топ 20 стран мира положительного счета текущих операций торгового баланса за 15 лет (00-15)

```r
# Выводим таблицу топ 20 стран
top20
```

```
##       value            country
## 1  3279.304              China
## 2  2595.688            Germany
## 3  2120.086              Japan
## 4  1097.081       Saudi Arabia
## 5   995.763 Russian Federation
## 6   829.408        Switzerland
## 7   801.955        Netherlands
## 8   733.756             Norway
## 9   686.217          Euro area
## 10  587.708          Singapore
## 11  572.417             Kuwait
## 12  493.991        Korea, Rep.
## 13  420.002             Sweden
## 14  307.553           Malaysia
## 15  237.746              Qatar
## 16  220.269            Nigeria
## 18  191.289            Denmark
## 19  172.743              Libya
## 20  158.204      Venezuela, RB
```

# Визуализация топ 20 стран положительного счета текущих операций торгового баланса за 15 лет

```r
top20df <- data.frame()
# Подготовка данных
for(i in top20$country){
        temp <- grep(i, alldata$country)
        temp <- alldata[temp,]
         
        top20df <- rbind(top20df, temp)
        }
top20df$year <- as.numeric(top20df$year)
top10 <- top20df[1:160,]
top10$year <- as.numeric(top10$year)
top10 <- top10[-(128:144),]
class(top20df$value)
```

```
## [1] "numeric"
```

```r
ggplot(data = top20df, aes(x = year, y = value))+
        geom_line(size = 0.1, aes(colour=country))+
        geom_point(aes(colour=country))+
        
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - China and USA")
```

![plot of chunk unnamed-chunk-350](figure/unnamed-chunk-350-1.png)



## 6) Выводы
Если рассматривать США и другие западные страны как единое целое, то из сравнительного графика стран Запада и АТР, можно увидеть, что их балансы почти полностью зеркальны. Запад имеет отрицательный баланс текущих операций, тогда как страны АТР положительный, что подтверждает переток денег с Запада в Азиатский регион. 
Если разделить Западную цивилизацию на ее составляющие, то мы увидим что ЕС, за счет Германии, которая заняла 2-е место в таблице положительных балансов счета текущих операций за последнии 15 лет с цифрой 3279.304 млрд, имеет положительный баланс. 
Вывод таков, что основной переток капитла происходит именно из США в Китай, как мы можем это увидеть на сравнительном графике этих двух стран. 
Интересным открытием оказалось, что в 2009 году при глобальной "просадке" Еврозоны на минус 195.198 млрд USD по балансу текущего счета, Германия за этот же год показала плюс 198.872 млрд USD, из чего можно сделать вывод, что бурный прирост капитала в Германии происходит именно за счет ЕС.   
В общем, результаты такого мини-исследования подтверждают теорию Дж. Арриги об окончании Американского цикла, и перехода на Азиатский, а точнее на Китайский системный цикл накопления капиталов. Вместе с Китаем, за счет рынка США так-же растут Япония и Ю. Корея. Страны вроде РФ и Саудовской Аравии росли в основном за счет дорогих энергоресурсов, после падения цен на которые, как видим на последнем графике, наиболее "просевшей"" страной является именно Саудовская Аравия. 


[1]: https://ru.wikipedia.org/wiki/Счёт_текущих_операций

