---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.


```{r}
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(openair)
library(pastecs)
library(psych)
library(Mcomp)
library(xts)
library(zoo)
library(TTR)
library(timeDate)
library(tseries)
library(rlist)
library(knitr)
library(skimr)
library(readr)
library(magrittr)
library(tidyr)
library(corrplot)
library(circlize)
library(ggpubr)
library(forecast)
library(h2o)
```


```{r}
movie_weekend <- read.delim("C:/Users/danie/Downloads/movieweekend-dat.txt", TRUE)
movie_daily <- read.delim("C:/Users/danie/Downloads/moviedaily-dat.txt", TRUE)
movie_total <- read.delim("C:/Users/danie/Downloads/movietotal-dat.txt", TRUE)
```


```{r}
head(movie_weekend, 7)
```


```{r}
head(movie_daily, 7)
```


```{r}
head(movie_total, 7)
```


```{r}
summary(movie_weekend)
```


```{r}
summary(movie_daily)
```


```{r}
summary(movie_total)
```


```{r}
movie_weekend$WEEKEND_DATE <- as.Date.character(movie_weekend$WEEKEND_DATE, "%m/%d/%Y")
movie_weekend <- na.exclude(movie_weekend)
```


```{r}
movie_daily[,c(1,4)] <- movie_daily[,c(1,4)] %>% sapply(as.integer)
movie_daily$DATE <- as.Date.character(movie_daily$DATE, "%m/%d/%Y")
movie_daily <- na.exclude(movie_daily)
movie_daily
```


```{r}
p1 <- ggplot(movie_total, aes(x = MOVIE, y = TOTAL, colour = TYPE)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(x = "Movie", y = "Revenue") + 
  scale_y_continuous(labels = comma) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
p1
```


```{r}
p2 <- ggplot(movie_total, aes(x = TYPE, y = TOTAL)) + 
  geom_boxplot() +
  geom_point(color = 'steelblue')
p2
```
```{r}
p1a <- ggplot(movie_total %>% filter(TOTAL != 0), aes(x = TOTAL/100)) +
  geom_histogram(aes(y=..density..), fill = "coral3", color = 'black', bins = 35) +
  geom_density(alpha=.4, fill="cyan1") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous() +
  labs(x = "Revenue", y = "Frequency") + 
  theme_classic()
p2a <- ggplot(movie_total %>% filter(TOTAL != 0), aes(x = log(TOTAL))) +
  geom_histogram(aes(y=..density..), fill = "darkorange1", color = 'black', bins = 35) +
  geom_density(alpha=.4, fill="darkorchid1") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous() +
  labs(x = "Revenue", y = "Frequency") + 
  theme_classic()
ggarrange(p1a, p2a)
```
```{r}
ggplot(mw, aes(x=week, y=week_total, colour = MOVIE)) + 
  geom_point() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 6.5),
        legend.box.margin = margin(1, 1, 1, 5))
```


```{r}
ggplot(movie_weekend %>% filter(WEEKEND_DATE >= as.Date("2000-01-01") & WEEKEND_DATE <= as.Date("2000-10-20")
                                & WEEKEND_PER_THEATER >= 200),  aes(x = WEEKEND_DATE, y = WEEKEND_PER_THEATER)) + 
  geom_point(color = "deeppink1", size = 1) +
  geom_line(color = "grey", size = 0.7) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", fill = "antiquewhite", size = 2) +
  theme_minimal()
```




```{r}
mw <- movie_weekend %>% 
  filter(WEEKEND_DATE >= as.Date("2000-01-01")) %>%
  select(MOVIE, WEEKEND_DATE, WEEKEND_PER_THEATER) %>%
  group_by(MOVIE) %>%
  summarise(MOVIE, week_total = sum(WEEKEND_PER_THEATER), week = WEEKEND_DATE)
mw
```
```{r}
movie_weekend %>% filter(WEEKEND_DATE >= as.Date("2000-01-01") & WEEKEND_DATE <= as.Date("2000-12-12")
                                & WEEKEND_PER_THEATER >= 200)
```


```{r}
ggplot(movie_weekend %>% filter(WEEKEND_DATE >= as.Date("2001-01-01") & WEEKEND_DATE <= as.Date("2003-10-20")),  aes(x = WEEKEND_DATE, y = WEEKEND_PER_THEATER)) + 
  geom_point(color = "brown1", size = 1) +
  geom_line(color = "grey", size = 0.7) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", fill = "cyan4", size = 2) +
  geom_smooth(method="lm", se=FALSE, size = 1.5) +
  theme_minimal()
```
```{r}
mw_ts <- ts(movie_weekend$WEEKEND_PER_THEATER, frequency=12, start = c(1977, 05), end = c(2007, 12))
str(mw_ts)
autoplot(mw_ts)
```


```{r}
df_ts <- data.frame(revenue = mw_ts, as.numeric(time(mw_ts)))
names(df_ts) <- c("revenue", "time")
fit.consMR <- tslm(
  revenue ~ season + trend - 1,
  data=df_ts)
summary(fit.consMR)
```
```{r}
my_fc <- forecast(fit.consMR, h=120)
plot(my_fc)
```
```{r}
autoplot(mw_ts, series="initial") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("Revenue") +
  ggtitle("Quarterly Revenue") + 
  theme_bw()
```

```{r}
checkresiduals(fit.consMR)
```
```{r}
h <- 10
fit.lin <- tslm(revenue ~ trend,data=df_ts)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(revenue ~ trend, data=df_ts, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(mw_ts)
t.break1 <- 1970
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1977)
tb2 <- ts(pmax(0, t - t.break2), start = 2008)

fit.pw <- tslm(revenue ~ t + tb1 + tb2, data=df_ts)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(revenue ~ t + I(t^2) + I(t^3) +
  I(tb1^3) + I(tb2^3), data=df_ts)
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(mw_ts) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Revenue") +
  ggtitle("Cinemas' Revenue") +
  guides(colour = guide_legend(title = " ")) +
  theme_pubclean()
```
```{r}
splinef(mw_ts, h=14, lambda=-0.05, method = "gcv") %>%
  autoplot(ylim = c(0, 60000)) +
  ylab("Revenue") +
  ggtitle("Smoothing splines") +
  theme_light()
```

```{r}
spl_model <- splinef(mw_ts, h=14, lambda=-0.05, method = "gcv")
accuracy(mw_fit)
```



```{r}
spl_pred <- predict(mw_ts %>% splinef(h=3, lambda=0.3), mw_ts)
autoplot(spl_pred, ylim = c(0, 30000), xlim = c(2000, 2010)) +
autolayer(mw_ts) + 
  scale_color_manual(labels = c("Actual", "Forecasted"),
                    values=c("chartreuse3", "darkred3")) +
  ggtitle("Smoothed vs Actual") +
  theme_classic2()
```
```{r}
fit <- auto.arima(mw_ts, seasonal=TRUE)
fit %>% forecast(h=10) %>% autoplot(include=90)
```




##Random Forest

```{r}
model_data_tbl <- 
  movie_weekend %>% 
  mutate(trend       = 1:nrow(movie_weekend),
         trend_sqr   = trend^2,
         rev_lag_13  = lag(WEEKEND_PER_THEATER, n = 13),
         rev_lag_52  = lag(WEEKEND_PER_THEATER, n = 52),
         season      = case_when(WEEKEND_PER_THEATER == 0 ~ 0,
                                 TRUE ~ 1)
        ) %>% 
 filter(!is.na(rev_lag_52))

train_tbl <- 
  model_data_tbl %>% 
  filter(WEEKEND_DATE <= "2007-03-19") 

test_tbl <- 
  model_data_tbl %>%
  filter(WEEKEND_DATE >= "2006-10-02" &
           WEEKEND_DATE <= "2007-03-19")

train_tbl %>% head()
```



```{r}
h2o.init(max_mem_size = "8G")
h2o.no_progress()
y <- "WEEKEND_PER_THEATER"

# predictors set: remove response variable and order_date from the set
x <- setdiff(names(train_tbl %>% as.h2o()), c(y, "weekend_date"))

rft_model <- 
  h2o.randomForest(
    x = x, 
    y = y, 
    training_frame = train_tbl %>% as.h2o(),
    nfolds = 10,
    ntrees = 500,
    stopping_metric = "RMSE",
    stopping_rounds = 10,
    stopping_tolerance = 0.005,
    seed = 1975
  )

rft_model %>% h2o.varimp_plot()
```
```{r}
rft_model@model$model_summary
h2o.performance(rft_model, newdata = test_tbl %>% as.h2o())
rft_model %>% h2o.r2()
```

##naive bayes

```{r}
splice <- h2o.uploadFile("C:/Users/danie/Downloads/movietotal-dat.txt", header = TRUE, na.strings = FALSE)

# Set the predictors and response; set the response as a factor:
splice$TYPE <- as.factor(splice$TYPE)
predictors <- c("MOVIE", "TOTAL")
response <- "TYPE"

# Build and train the model:
pros_nb <- h2o.naiveBayes(x = predictors,
                          y = response,
                          training_frame = splice,
                          laplace = 0,
                          nfolds = 5,
                          seed = 1234)

perf <- h2o.performance(pros_nb)
pred <- h2o.predict(pros_nb, newdata = splice)
pros_nb %>% h2o.r2()
```













