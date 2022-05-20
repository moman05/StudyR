# R을 이용한 공공데이터 분석
getwd()

install.packages('data.table')
install.packages('dplyr')
install.packages('lubridate')
install.packages('stringer')
install.packages('forecast')
install.packages('randtests')
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(forecast)
library(randtests)

load(file='./result_sales_dt.RData')

str(result_sales_dt)
unique(result_sales_dt$yyyyqrt)

glimpse(result_sales_dt, width=60)


# 분기별 아파트 매매건수
# 쿼터별 매매량 계산
qrt_cnts <- result_sales_dt[,.N,yyyyqrt]
str(qrt_cnts)
head(qrt_cnts)
tail(qrt_cnts)
qrt_cnts <- result_sales_dt[yyyyqrt != '2015Q2', .N, yyyyqrt]
head(qrt_cnts)
tail(qrt_cnts)

ggplot(qrt_cnts, aes(x=yyyyqrt, y=N, group=1)) +
  geom_line() + xlab('년도분기') + ylab('매매건수') +
  theme(axis.text.x=element_text(angle=90)) +
  stat_smooth(method='lm')

# 쿼터별/지역별 매매량 계산
region_cnts <- result_sales_dt[yyyyqrt !=
  '2015Q2',.N,.(yyyyqrt, region)]
ggplot(region_cnts, aes(x=yyyyqrt, y=N, group=region)) +
  geom_line() + facet_wrap(~region,scale='free_y', ncol=3) +
  stat_smooth(method = 'lm') +
  theme(axis.text.x = element_blank())

# 월별 지역별 매매량
region_cnts <- result_sales_dt[,.N,.(yyyymm,region)]
# 대표지역 추출
regions <- unique(region_cnts$region)
#각 지역별로 매매량의 랜덤성 검정 결과를 runs_p 변수에 추가
runs_p <- c()
for(reg in regions){
  runs_p <- c(runs_p, runs.test(region_cnts[region %chin% reg,N])$p.value)
}
ggplot(data.table(regions, runs_p), aes(x=regions, y=runs_p, group=1)) +
  geom_line() + geom_point() + 
  xlab('지역') + ylab('P-value') 

# 시계열 분할(서울지역)
seoul_cnts <- result_sales_dt[yyyymm != '201504' &
                                region %chin% '서울',.N,.(yyyymm)]
tot_ts <- ts(seoul_cnts$N, start = c(2006,1), frequency = 12)
plot(stl(tot_ts,s.window = 'periodic'))

#사계절 분할에 대한 모형가정
arima_mdl <- auto.arima(tot_ts)
tsdiag(arima_mdl)

plot(forecast(arima_mdl, h=12))

