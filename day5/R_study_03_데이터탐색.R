## 데이터 탐색
# 단일범주 데이터
library(carData)
tData <- Titanic
tData2 <- TitanicSurvival
str(tData2)

room.class <- TitanicSurvival$passengerClass
str(room.class)

room.class
tbl <- table(room.class)
sum(tbl)

barplot(tbl, main='선실별 탑승객', xlab='선실등급', ylab='탑승객수',
        col=c('blue','green','yellow'), names=c('1등실','2등실','3등실'))

tbl / sum(tbl)

pie3D(tbl, main = '선실별 탑승객', radius=1.5,
      labels = c('1등실','2등실','3등실'),
      col=c('blue','green','yellow'),
      labelcex = 1.0, labelcol = 'white', explode = 0.1)

# 생사여부, 성별 모두 위와 동일.

# 단일변수 수치형 
str(state.x77)
grads <- state.x77[,'HS Grad']
grads
str(grads)

summary(grads)
min(grads)
max(grads)
var(grads)
median(grads)
sd(grads)
table(grads)

hist(grads, main='주별 졸업률', xlab='졸업률', 
     ylab='주의개수', col='orange')


boxplot(grads, main='주별 졸업률', col='green')

idx <- which(grads == min(grads))
grads[idx]
idx <- which(grads == max(grads))
grads[idx]

idx <- which(grads < mean(grads))
grads[idx]

## 74~79 영국 폐질환 사망자 데이터
getwd()
ds <- read.csv('./day5/fdeaths.csv', header=T, 
               row.names='year')
ds

my.color <- c('orange','blue','red','green','purple','dark gray')
my.lty <- 1:6

plot(1:12,                        # x축 데이터
     ds[1,],                      # y축 데이터(1974)
     main = '월별 사망자 추이',   # 그래프 제목
     type = 'b',                  # 포인트 타입
     lty = my.lty[1],             # 라인 종류
     lwd = 2,                     # 라인 두께
     xlab ='월',                  # x축 라벨
     ylab ='사망자수',            # y축 라벨
     ylim = c(300, 1200),         # y축 범위
     las = 1,                     # y축 가로쓰기
     col = my.color[1],           # 선색
    )
for (i in 2:6) {
  lines(1:12,
        ds[i,],
        type = 'b',
        lty = my.lty[i],
        lwd = 2,
        col = my.color[i]
        )
} 

legend(x='topright', 
       lty = my.lty,
       lwd = 2,
       col = my.color,
       legend = rownames(ds),
       inset = c(0,0))


## LAB 미국 자동차 판매가격 데이터
library(DAAG)
str(carprice)

carprice$Price
range(carprice$Price)
summary(carprice$Price)

hist(carprice$Price, 
     main='자동차 가격 분포',
     xlab='가격대(1,000$)',
     ylab='빈도수',
     col = rainbow(8)
     )

barplot(carprice$Price)

tbl <- table(carprice$Type)
barplot(tbl,
        main='차종별 빈도수',
        xlab='차종',
        ylab='빈도수',
        col=rainbow(length(tbl))
        )
summary(tbl)

idx <- which(carprice$Price == max(carprice$Price))
carprice[idx, c('Type', 'Price')]


## 다중변수 탐색(분석)
str(pressure)

plot(pressure$temperature, 
     pressure$pressure,
     main = '온도와 기압',
     xlab = '온도(섭씨)',
     ylab = '기압',
     )






