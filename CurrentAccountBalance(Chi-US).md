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
В этом небольшом исследовании, автор поставил перед собой задачу подтвердить либо опровергнуть выводы, сделанные Дж. Арриги в рамках своей теории системных циклов накопления капиталов, о том, что мир находится в процессе перехода на новый, Азиатский цикл накопления. В книге «Долгий двадцатый век» он выделяет четыре исторических цикла накопления – Генуэзский системный цикл накопления(XV-XVI), Голландский системный цикл накопления (XVII – XVIII века), Британский системный цикл накопления(XIX - начало ХХ века) и последний, который находится в своей завершающей стадии - Американский системный цикл накопления(ХХ - начало XXI века). Уже в следующей и последней книге "Адам Смит в Пекине", Арриги уточняет, что новым ядром накопления является именно Китай. 
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


## 3)Анализ изменений счета текущих операций платежного баланса стран Восточной Азии

```r
# Вытягиваем из таблицы нужные для анализа страны восточной Азии
## Первая волна азиатских "тигров"(без Тайваня, который ВБ объеденяет с Китаем)
firstTigersn <- grep("Korea, Rep|Singapore|Hong Kong", cab_data$country)
## Вторая волна азиатских "тигров"
secondTigersn <- grep("Malaysia|Indonesia|Thailand", cab_data$country)
## Япония
japann <- grep("Japan", cab_data$country)
## Индия 
indian <- grep("India", cab_data$country)
## Новые индустриальные страны восточной азии
restAsian <- grep("Philippines|Vietnam|Bangladesh", cab_data$country)

# Создаем таблицу стран Восточной Азии первой волны "Тигров"
firstTigers <- data.frame()
for(i in firstTigersn){
        firstTigers <- rbind(firstTigers, cab_data[i,])
}

# Создаем таблицу стран Восточной Азии второй волны "Тигров"
secondTigers <- data.frame()
for(i in secondTigersn){
        secondTigers <- rbind(secondTigers, cab_data[i,])
}

# Создаем таблицу Японии
japan <- data.frame()
for(i in japann){
       japan <- rbind(japan, cab_data[i,])
}

# Создаем таблицу Индии
india <- data.frame()
for(i in indian){
       india <- rbind(india, cab_data[i,])
}

# Создаем таблицу дргих основных стран Региона, куда относится Япония, Индия и НИС
restAsia <- data.frame()
for(i in restAsian){
        restAsia <- rbind(restAsia, cab_data[i,])
}
```

Всемирный Банк предоставляет данные по Китаю, Гонконгу и Макао в отдельности. Поэтому самостоятельно объединяем данные Макао с Китаем, оставляя Гонконг отдельным счетом, для анализа роста "Азиатских тигров", к которым он относится. Данные по другому "тигру" - Тайвань, ВБ к сожалению, не предоставляет, поэтому в анализе мы его не учитываем.  

```r
chinan <- grep("China", cab_data$country)

china <- data.frame()
for(i in chinan){
        china <- rbind(china, cab_data[i,])
}

# Удаляем данные Гонконга
china <- china[-(26:43),]

# Объединяем данные Макао с общими данными Китая
china[grepl("Macao", china$country, ignore.case=T), c("country")] <- "China"

# Убираем повторения China по годам
chinaHM <- data.frame()
temp2 <- filter(china, year == "1991" , country == "China")
temp2 <- list(sum(temp2$value), as.numeric(temp2[1,2]), "China")
chinaHM <- rbind(chinaHM, temp2)

# Создаем новую таблицу, которая объединяет данные China и Macao
for (i in c(1992:2015)){
temp <- filter(china, year == i , country == "China")
temp <- c(sum(temp$value), as.numeric(temp[1,2]), "China")
chinaHM <- rbind(chinaHM, temp)
}

# Корректируем названия колонок 
names(chinaHM) <- c("value", "year", "country")

# Создаем таблицу объединенных данных Китая и Макао
china <- arrange(chinaHM, desc(year))
# Переводим колонку valuу и year в numeric 
china$value <- as.numeric(as.character(china$value))
china$year <- as.numeric(china$year)
```

Создаем единую объединенную таблицу данных для стран Восточной Азии.

```r
# Добавляем Китай в таблицу выбранных стран восточной Азии с очищенными выше данными 
asia <- rbind(china,firstTigers, secondTigers, japan, india, restAsia)
asia <- arrange(asia, country, desc(year)) 
# Для возможности реализации графика
asia$value <- as.numeric(asia$value)
asia$year <- as.numeric(asia$year)
```


Отчет по странам Восточной Азии

```r
summary(asia)
```

```
##      value              year             country   
##  Min.   :-91.471   Min.   :1991   China      : 25  
##  1st Qu.: -1.301   1st Qu.:1998   Bangladesh : 25  
##  Median :  5.783   Median :2004   India      : 25  
##  Mean   : 25.063   Mean   :2004   Indonesia  : 25  
##  3rd Qu.: 20.953   3rd Qu.:2010   Korea, Rep.: 25  
##  Max.   :423.923   Max.   :2015   Malaysia   : 25  
##                                   (Other)    :133
```

### График изменений счета текущих операций платежного баланса по выбранным странам в. Азии

```r
# График изменений счета текущих операций платежного баланса "первых тигров"
ggplot(data = firstTigers, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - first Tigers")+ 
        facet_grid(country~., scales = "free") 
```

![plot of chunk unnamed-chunk-331](figure/unnamed-chunk-331-1.png)

```r
# График изменений счета текущих операций платежного баланса "вторых тигров"
ggplot(data = secondTigers, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - second Tigers")+ 
        facet_grid(country~., scales = "free") 
```

![plot of chunk unnamed-chunk-331](figure/unnamed-chunk-331-2.png)

```r
# График изменений счета текущих операций платежного баланса Японии, Индии и новых индустриальных стран Восточной азии 
tempdata <- rbind(japan, india, restAsia)
ggplot(data = tempdata, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - Japan, India and NICs of east Asia")+ 
        facet_grid(country~., scales = "free") 
```

![plot of chunk unnamed-chunk-331](figure/unnamed-chunk-331-3.png)

```r
# График изменений счета текущих операций платежного баланса Китая

ggplot(data = china, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point()+
        theme_bw()+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - China") 
```

![plot of chunk unnamed-chunk-331](figure/unnamed-chunk-331-4.png)

```r
# График изменений счета текущих операций платежного баланса для стран восточной Азии в целом -> подготовка данных
asiaDF <- data.frame()
tempasia <- data.frame()

for(i in 1991:2015){
tempasia <- asia %>% filter(year == i)%>%
        summarise(value = sum(value), year = i) 
asiaDF <- rbind(asiaDF, tempasia)
}
```
График изменений счета текущих операций платежного баланса стран восточной Азии в целом

```r
# График изменений счета текущих операций платежного баланса стран восточной Азии  в целом -> построение графика
ggplot(data = asiaDF, aes(x = year, y = value))+
        geom_line(colour="darkblue")+
        geom_point(colour="darkgreen")+
        geom_smooth(method = "loess")+
        xlab("Year")+
        ylab("Billions of $ USD")+
        ggtitle("Current account balance of payments - East Asia countries")
```

![plot of chunk unnamed-chunk-332](figure/unnamed-chunk-332-1.png)


## 4)Рассмотрим теперь изменения счета текущих операций платежного баланса стран Запада 

```r
# Выбираем из таблицы необходимые для анализа страны. Ядро Западной цивилизации 
westn <- grep("United States|United Kingdom|Canada|Australia|Germany|France|Italy|
                      |Austria|Spain", cab_data$country)
# Выбираем данные для второй таблицы, которая включает в себя все страны ЕС, а не только ядро
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

![plot of chunk unnamed-chunk-335](figure/unnamed-chunk-335-1.png)

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

![plot of chunk unnamed-chunk-335](figure/unnamed-chunk-335-2.png)

## 5) Сравнение изменений счета текущих операций платежного баланса стран Запада и Восточной Азии


```r
westDF$countries <- rep("West", 25)
asiaDF$countries <- rep("eastAsia", 25)
s_ummery <- rbind(westDF, asiaDF)

#Изменения счета текущих операций платежного баланса стран Запада и восточной Азии по годам
s_ummery2 <- westDF
s_ummery2$eastAsia <- asiaDF$value
s_ummery2$countries <- NULL
s_ummery2 <- s_ummery2 %>% select(Year = year, West = value, eastAsia = eastAsia)
#Сравнительная таблица Запад - восточная Азия 
s_ummery2
```

```
##    Year     West eastAsia
## 1  1991  -30.610  -10.728
## 2  1992  -83.637   -6.670
## 3  1993 -116.426  -21.357
## 4  1994 -151.412   -5.953
## 5  1995 -137.291  -30.713
## 6  1996 -136.704   26.580
## 7  1997 -161.620  114.444
## 8  1998 -241.818  228.963
## 9  1999 -316.484  207.697
## 10 2000 -433.620  199.319
## 11 2001 -471.391  148.182
## 12 2002 -485.801  203.440
## 13 2003 -495.690  269.957
## 14 2004 -626.914  342.381
## 15 2005 -685.651  372.036
## 16 2006 -814.864  512.072
## 17 2007 -772.055  704.963
## 18 2008 -714.852  629.363
## 19 2009 -669.567  526.220
## 20 2010 -557.371  563.711
## 21 2011 -542.305  362.805
## 22 2012 -562.275  317.493
## 23 2013 -303.480  303.716
## 24 2014 -170.435  470.206
## 25 2015 -240.700  664.797
```

```r
#Сравнительный график изменений счета текущих операций платежного баланса стран Запада и восточной Азии 
ggplot(data = s_ummery, aes(x = year, y = value))+
        geom_line(aes(colour=countries))+
        geom_point(aes(colour=countries))+
        theme_bw()+
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - east Asia and West")
```

![plot of chunk unnamed-chunk-336](figure/unnamed-chunk-336-1.png)

### Отдельное сравнение изменений счета текущих операций платежного баланса США и Китая

```r
# Для отдельного сравнения Китая и США с общей таблицы данных выделим данные по США
temp <- grep("United States", cab_data$country)

# Создаем новую таблицу США 
cabUS <- data.frame()
for(i in temp){
        cabUS <- rbind(cabUS, cab_data[i,])
}

#Создаем сводную таблицу изменений счета текущих операций платежного баланса США и Китая по годам
chimerica <- rbind(cabUS, china)
chimerica2 <- cabUS  
chimerica2$China <- china$value
chimerica2$country <- NULL
chimerica2 <- chimerica2 %>% select(Year = year, USA = value, China = China)

# Сравнительная таблица США-Китай
chimerica2
```

```
##      Year      USA   China
## 6326 2015 -462.961 343.522
## 6327 2014 -392.066 296.511
## 6328 2013 -366.424 168.950
## 6329 2012 -446.527 232.286
## 6330 2011 -460.358 151.145
## 6331 2010 -441.963 248.899
## 6332 2009 -384.024 249.306
## 6333 2008 -690.789 423.923
## 6334 2007 -718.641 357.151
## 6335 2006 -806.726 234.023
## 6336 2005 -745.445 135.280
## 6337 2004 -633.768  72.462
## 6338 2003 -521.342  45.603
## 6339 2002 -458.092  37.774
## 6340 2001 -395.331  17.401
## 6341 2000 -410.762  20.518
## 6342 1999 -295.534  21.115
## 6343 1998 -215.037  31.472
## 6344 1997 -140.725  36.963
## 6345 1996 -124.727   7.243
## 6346 1995 -113.561   1.618
## 6347 1994 -121.642   6.908
## 6348 1993  -84.783 -11.609
## 6349 1992  -51.605   6.401
## 6350 1991    2.851  13.272
```

```r
# Для корректного отображения на графике, переводим year в numeric
chimerica$year <- as.numeric(chimerica$year)
```

Сравнительный график изменений счета текущих операций платежного баланса США и Китая

```r
ggplot(data = chimerica, aes(x = year, y = value))+
         geom_line(aes(colour=country))+
        geom_point(aes(colour=country))+
        theme_bw()+
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - China and USA")
```

![plot of chunk unnamed-chunk-338](figure/unnamed-chunk-338-1.png)

## 6) ТОП 20 стран мира положительного счета текущих операций торгового баланса (с 2000 по 2015 года):

```r
alldata <- arrange(cab_data, country, desc(year))

# Суммируем все ежегодные результаты текущих операций торгового баланса и сводим 15-ти летний баланс по каждой из стран
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
# Сортируем по значению value и выделяем первые 20 стран + Евросоюз 
top20 <- arrange(allDF, desc(value)) 
# Объединяем результаты Китая и Макао
top20[1,1] <- top20[1,1] + top20[23,1]
# Удаляем из топ 20 Niger, который попал туда из-за сходства с Нигерией
top20 <- top20[-19,]
# Выбираем топ20
top20 <- top20[1:20,]
```
Таблица топ 20 стран мира положительного счета текущих операций торгового баланса за 15 лет (00-15)

```r
# Выводим таблицу топ 20 стран
top20
```

```
##       value              country
## 1  3528.873                China
## 2  2513.347                Japan
## 3  2381.375              Germany
## 4  1043.508   Russian Federation
## 5  1007.082         Saudi Arabia
## 6  1005.330          Switzerland
## 7   947.832          Netherlands
## 8   785.639               Norway
## 9   690.889            Singapore
## 10  686.217            Euro area
## 11  578.514               Kuwait
## 12  499.313          Korea, Rep.
## 13  441.669               Sweden
## 14  296.784             Malaysia
## 15  258.085 Hong Kong SAR, China
## 16  237.746                Qatar
## 17  218.575              Nigeria
## 18  212.397              Denmark
## 20  178.995                Libya
## 21  169.079        Venezuela, RB
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

# Построение графика топ 20 стран мира
ggplot(data = top20df, aes(x = year, y = value))+
        geom_line(size = 0.2, aes(colour=country))+
        geom_point(size = 0.2, aes(colour=country))+
        
        xlab("Year")+
        ylab("billion US dollars")+
        ggtitle("Current account balance - Top20 countries")
```

![plot of chunk unnamed-chunk-341](figure/unnamed-chunk-341-1.png)



## 6) Выводы
Из построенных сравнительных таблиц и графиков для анализа балансов текущих операций стран Западной цивилизации и восточной Азии,  четко просматривается тенденция перетока капиталов с Запада на Восток. График построенный для этих целей выглядет практически зеркальным для обоих регионов. В периоды где отрицательные показатели баланса текущего счета для Запада усиливаться, для Азии эти-же показатели растут, и наоборот.   
Если разделить Западную цивилизацию на ее составляющие, можно увидеть что ЕС, за счет положительного баланса счета текущих операций Германии, которая заняла 2-е место в таблице топ 20 стран по этому показателю за 15 лет (с 2000 по 2015)  с цифрой 3279.304 млрд, имеет положительный баланс.
Главный вывод таков, что основной переток капитла происходит не в целом с Запада на Восток, а как Дж. Арриги предполагал, из США в Китай. Эту мысль подтверждает сравнительный график баланса счета текущих операций этих двух стран.
Интересным открытием оказалось, что в 2009 году при глобальной "просадке" Еврозоны на минус 195.198 млрд USD по балансу текущего счета, Германия за этот же год показала плюс 198.872 млрд USD, из чего можно сделать вывод, что бурный прирост капитала в Германии происходит за счет ЕС. 
В общем, результаты этого мини-исследования сквозь призму показателя "Счета текущих операций платежного баланса" подтверждают теорию Дж. Арриги об окончании Американского цикла, и перехода на Азиатский, а точнее Китайский системный цикл накопления капиталов. Вместе с Китаем, за счет рынка США так-же растут Япония и Ю. Корея. Страны вроде РФ и Саудовской Аравии росли в основном за счет дорогих энергоресурсов, после падения цен на которые, как видим на последнем графике, наиболее "просевшей"" страной является Саудовская Аравия.



[1]: https://ru.wikipedia.org/wiki/Счёт_текущих_операций

