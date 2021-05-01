library(forecast)
library(MLmetrics)
library(ggplot2)
aqi_ts = delhi_aqi_by_day[c('Date','AQI')]
plot.ts(delhi_aqi_by_day$AQI)
aqi_ts$Date = as.Date(aqi_ts$Date)

delhi_aqi_by_day$Date = as.Date(delhi_aqi_by_day$Date)
AirQuality = delhi_aqi_by_day$AQI_Bucket

# Normal Plot
p = ggplot(aqi_ts,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  #geom_point(aes(color=AirQuality))
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b/%Y",date_breaks = '3 month')+ ggtitle("Delhi Air Quality Index (2015-2020)")+
  theme(plot.title = element_text(hjust = 0.5))

## Bucketed Plot
p = ggplot(aqi_ts,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=AirQuality))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

p + scale_x_date(date_labels = "%b/%Y",date_breaks = '5 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index\n")+
  theme(plot.title = element_text(hjust = 0.5))

library(lubridate)
df_ts_plot = aqi_ts
df_ts_plot['Month'] = month(aqi_ts$Date, label = TRUE)
df_ts_plot['Year'] = year(aqi_ts$Date)
is_weekday = function(timestamp){
  lubridate::wday(timestamp, week_start = 1) < 6
}
weekday = is_weekday(df_ts_plot$Date)

df_ts_plot['Weekday'] = as.factor(weekday)
## For 2015
df_2015 = df_ts_plot[df_ts_plot$Year==2015,]

p = ggplot(df_2015,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2015\n")+
  theme(plot.title = element_text(hjust = 0.5))

## For 2016
df_2016 = df_ts_plot[df_ts_plot$Year==2016,]

p = ggplot(df_2016,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2016\n")+
  theme(plot.title = element_text(hjust = 0.5))

## For 2017
df_2017 = df_ts_plot[df_ts_plot$Year==2017,]

p = ggplot(df_2017,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2017\n")+
  theme(plot.title = element_text(hjust = 0.5))

## For 2018
df_2018 = df_ts_plot[df_ts_plot$Year==2018,]

p = ggplot(df_2018,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2018\n")+
  theme(plot.title = element_text(hjust = 0.5))

## For 2019
df_2019 = df_ts_plot[df_ts_plot$Year==2019,]

p = ggplot(df_2019,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2019\n")+
  theme(plot.title = element_text(hjust = 0.5))

## For 2020
df_2020 = df_ts_plot[df_ts_plot$Year==2020,]

p = ggplot(df_2020,aes(x = Date, y = AQI)) +
  geom_line(color="steelblue") +  # Blue lines
  geom_point(aes(color=Weekday))+
  xlab("\n Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b",date_breaks = '1 month')+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "loess")+ ggtitle("Delhi Air Quality Index in 2015\n")+
  theme(plot.title = element_text(hjust = 0.5))



#### Models ####
#Create samples
data = aqi_ts
#training=window(data$Date, start = c(2015,1), end = c(2019,12))
#test=window(data$Date, start = c(2020,1))

df_ts <- ts(aqi_ts$AQI, start=c(2015,1), frequency=365)
#plot(df_ts, yax.flip = TRUE)

mean_aqi = mean(na.omit(aqi_ts$AQI))
c=1
for(val in df_ts){
  if(is.na(val)){
    if(!is.na(df_ts[c-1]) & !is.na(df_ts[c+1])){
      df_ts[c]= (df_ts[c-1]+df_ts[c+1])/2 # replace by mean of previous and next day's aqi
    }
    else{
      #replace by mean if previous or next day's aqi is missing
      df_ts[c]=mean_aqi
    }
  }
  c=c+1
}
# Check for na's
sum(is.na(df_ts))
df_ts_new = aqi_ts
df_ts_new$AQI = df_ts
write.csv(df_ts_new,"AQI_univar.csv")

# 1) Additive Decomposition
aqi.decomp <- decompose(df_ts,type = 'additive')
## plot the obs ts, trend & seasonal effect
plot(aqi.decomp, yax.flip = TRUE)

# 2) Multiplicative Decomposition
aqi.decomp <- decompose(df_ts,type = 'multiplicative')
## plot the obs ts, trend & seasonal effect
plot(aqi.decomp, yax.flip = TRUE)

# 7-day forecast 19.83% MAPE using Auto Arima
# 1-month forecast 20% MAPE using Auto Arima
training = as.ts(df_ts[1:1979])
test = df_ts[1980:2009]
## use auto.arima to choose ARIMA terms
fit <- auto.arima(training)
summary(fit)
## forecast for future time points
predictions <- forecast(fit, h = 30)
y_pred = predictions$mean
MAPE(y_pred,test)
library(caret)
R2(y_pred,test)
MAE(y_pred,test)

library(tseries)
diff(log())
adf.test(log(training), alternative="stationary", k=0)


# Difference with lag=1
plot.ts(diff(training,lag=7))

ts_diff_1 = diff(training,lag=1) 
ts_diff_7 = diff(training,lag=7) 



# Normal Plot
date_ts <-aqi_ts$Date
p = ggplot(diff(training,lag=1),aes(x=date_ts[2:1826],y =training[2:1826])) +
  geom_line(color="steelblue") +  # Blue lines
  #geom_point()
  xlab("Date") +
  ylab("Air Quality Index\n")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p + scale_x_date(date_labels = "%b/%Y",date_breaks = '3 month')+ ggtitle("Differenced Daily Air Quality Index (2015-2020)")+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method="lm", se=FALSE)

  geom_hline(yintercept=mean(ts_diff_1), linetype="dashed", 
                  color = "red", size=2)




# Difference with lag=7
plot.ts(diff(training,lag=7))


acf(training)
pacf(training)


