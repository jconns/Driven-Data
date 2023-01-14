


### libraries
library(dplyr)
library(lubridate)
library(ggplot2)

### data cleaning
air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))


air_visits %>%
  group_by(visit_date) %>%
  summarize(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line()

holidays <- holidays %>%
  mutate(calendar_date = ymd(calendar_date))
head(air_visits)

### total air visitors

air_visitors <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

class(air_visitors$visit_date)

##ggplot add on
group_by(visit_date) %>%
  summarize(all_visitors=sum(visitors)) %>%
  ggplot(aes(visit_date, all_visitors, fill=1)) +
  geom_col() +
  labs(title="Total Visitors per Day",
       x="Date", 
       y="Visitors") +
  theme_bw() +
  theme(
    legend.position = "none"
  ) 

### total hpg visitors

hpg_visitors <- hpg_reserve %>%
  mutate(reserve_datetime=ymd_hms(reserve_datetime),
         all_visitors = sum(reserve_visitors))
##ggplot add on

group_by(reserve_datetime) %>%
  summarize(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(reserve_datetime, all_visitors, fill=1)) +
  geom_col() +
  labs(title="Total Visitors per Day",
       x="Date", 
       y="Visitors") +
  theme_bw() +
  theme(
    legend.position = "none"
  ) 

colnames(hpg_visitors)
colnames(air_visitors)



#### single air visitors

foo <- air_visits %>%
  rename(date = visit_date) %>%
  distinct(date) %>%
  mutate(dset = "train")

library(tidyr)

bar <- test %>%
  separate(id, c("foo", "bar", "date"), sep = "_") %>%
  mutate(date = ymd(date)) %>%
  distinct(date) %>%
  mutate(dset = "test")

foo <- foo %>%
  bind_rows(bar) %>%
  mutate(year = year(date))
year(foo$date) <- 2017

###visualizing the length of the test set

library(forcats)
foo %>%
  filter(!is.na(date)) %>%
  mutate(year = fct_relevel(as.factor(year), c("2017","2016"))) %>%
  ggplot(aes(date, year, color = dset)) +
  geom_point(shape = "|", size = 10) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  #scale_y_reverse() +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(color = "Data set") +
  guides(color = guide_legend(override.aes = list(size = 4, pch = 15)))


### next step

air_id = "air_db4b38ebe7a7ceff"

###training length
pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

###setting date

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = 
                       seq(min(air_visits$visit_date), 
                           max(air_visits$visit_date), 1))

colnames(all_visits)

###single restaurant class = df
foo <- air_visits %>%
  filter(air_store_id == air_id)
distinct(foo) %>%
  nrow()
head(foo)
class(foo$visit_date)

library(dplyr)
library(tibble)
library(fpp3)


###superimpose all dates and fill NAs with median value

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
  rownames_to_column()


#### train and test
visits_train <- visits %>% 
  filter(visit_date <= split_date)

visits_valid <- visits %>% 
  filter(visit_date > split_date)

library(fable)
library(forecast)
arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, 
                                   frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)


arima_visits <- arima.fit %>% 
  forecast(h = pred_len, level = c(50,95))


arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
  labs(x = "Time [weeks]", y = "log1p visitors vs auto.arima predictions")


### bring holiday_flg in
library(lubridate)
library(tidyr)

colnames(holidays)[1] <- "visit_date"

colnames(holidays)
class(visits$visit_date)

### create merged df

merge <-  right_join(visits, holidays, by="visit_date") %>%
  replace_na(list(visitors=mean(foo$visitors)))
head(merge)

## train and test
merge_train <- merge %>% 
  filter(visit_date <= split_date)

merge_valid <- merge %>% 
  filter(visit_date > split_date)


###dynamic arima modeling tsibble

arima <- Arima(merge_train$visitors, xreg=merge_train$holiday_flg, 
               order=c(1,0,0))

summary(arima)

forecast <- forecast(arima, h=39, xreg=visits_train$visitors)
autoplot(forecast)
class(merge)

### master

###coalesce

data <- master %>% 
  mutate(mycol = coalesce(visitors,all_visitors))

View(data)

master <- data %>%
  mutate(date = ymd_hms(visit_datetime))
###ggplot add on
group_by(date) %>%
  summarize(mycol = sum(mycol)) %>%
  ggplot(aes(date,mycol)) +
  geom_line()

View(master)

###single ts series for modeling
library(fpp3)
library(fpp2)

h <- 39
master.ts <- ts(master$mycol, start=c(2016,1),
                frequency=364)

mastertrain<-window(master.ts,
                    end=(time(master.ts)
                         [length(master.ts)]-
                           39))

arima <- auto.arima(mastertrain)

arima.forecast <- forecast(arima, h=39)


### right join
master <- right_join(air_visitors, hpg_visitors,
                     by="all_visitors")

head(master)

data <- master %>% 
  mutate(mycol = coalesce(visitors,all_visitors))

View(data)

master %>%
  mutate(total = sum(visitors + all_visitors)) %>%
  group_by(visit_datetime) %>%
  summarize(sum = sum(total)) %>%
  ggplot(aes(visit_datetime, sum)) +
  geom_col()
