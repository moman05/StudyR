## 외부 파일 함수 실행
getwd()
setwd('./day4')
getwd()
# 함수파일 호출 
source('myFunc.R')

result <- mydiv(109, 6)
result

## 자격증 합격여부
sub1 <- c(14,16,12,20,8,6,12,18,16,10)
sub2 <- c(18,14,14,16,10,12,10,20,14,14)
sub3 <- c(44,38,30,48,42,50,36,52,54,32)
score <- data.frame(sub1,sub2,sub3)

result <- determine(score)
source('determine.R')
result <- determine(score)
result

## 조건에 맞는 데이터 위치 리턴 함수
score.two <- c(76,84,69,50,95,60,82,71,88,84)
score.two 
which(score.two==69)
which(score.two==76)
which(score.two==84)
which(score.two>84)
max(score.two)
which.max(score.two)
which.min(score.two)

idx <- which(score.two<=60)
idx
score.two[idx]

score.two[idx] <- 61
score.two

idx <- which(score.two>=80)
idx
score.two[c(2,5,7,9,10)]
score.high <- score.two[idx]
score.high

## iris 꽃잎길이가 5.0 이상인건
idx <- which(iris$Petal.Length>5.0)
idx
iris.big.1 <- iris[idx,]
str(iris.big.1)

iris.big.2 <- subset(iris, iris$Petal.Length>5.0)


## LAB 언어발달 상황진단 프로그램
install.packages('Stat2Data')
library(Stat2Data)
ChildSpeaks
data(ChildSpeaks)
ChildSpeaks
str(ChildSpeaks)

## 말문이 트인 개월수로 등급환산 
idx <- which(ChildSpeaks$Age < 9)
idx
ChildSpeaks[idx, ]
ChildSpeaks[idx, 'm1'] <- 5
idx <- which(ChildSpeaks$Age >= 9 & ChildSpeaks$Age < 14)
idx
ChildSpeaks[idx, 'm1'] <- 4
idx <- which(ChildSpeaks$Age >= 15 & ChildSpeaks$Age < 21)
idx
ChildSpeaks[idx, 'm1'] <- 3
idx <- which(ChildSpeaks$Age >= 21 & ChildSpeaks$Age < 27)
idx
ChildSpeaks[idx, 'm1'] <- 2
idx <- which(ChildSpeaks$Age >= 27)
idx
ChildSpeaks[idx, 'm1'] <- 1

## 언어이해력 점수(Gesell) 등급환산
idx <- which(ChildSpeaks$Gesell < 70)
idx
ChildSpeaks[idx, 'm2'] <- 1
idx <- which(ChildSpeaks$Gesell >= 70 & ChildSpeaks$Gesell < 90)
idx
ChildSpeaks[idx, 'm2'] <- 2
idx <- which(ChildSpeaks$Gesell >= 90 & ChildSpeaks$Gesell < 110)
idx
ChildSpeaks[idx, 'm2'] <- 3
idx <- which(ChildSpeaks$Gesell >= 110 & ChildSpeaks$Gesell < 130)
idx
ChildSpeaks[idx, 'm2'] <- 4
idx <- which(ChildSpeaks$Gesell >= 130)
idx
ChildSpeaks[idx, 'm2'] <- 5

ChildSpeaks$total <- ChildSpeaks$m1 + ChildSpeaks$m2

idx <- which(ChildSpeaks$total < 3)
ChildSpeaks[idx, 'result'] <- '매우늦음'
idx <- which(ChildSpeaks$total >= 3 & ChildSpeaks$total < 5)
ChildSpeaks[idx, 'result'] <- '늦음'
idx <- which(ChildSpeaks$total >= 5 & ChildSpeaks$total < 7)
ChildSpeaks[idx, 'result'] <- '보통'
idx <- which(ChildSpeaks$total >= 7 & ChildSpeaks$total < 9)
ChildSpeaks[idx, 'result'] <- '빠름'
idx <- which(ChildSpeaks$total >= 9)
ChildSpeaks[idx, 'result'] <- '매우빠름'

ChildSpeaks[which.min(ChildSpeaks$total), ]


## 종업원의 팁 분석하기
install.packages('reshape2')
library(reshape2)
str(tips)

# 성별에따라 팁을주는정도비교
unique(tips$sex)

idx <- which(tips[ ,'sex'] == 'Female')
avg.female <- mean(tips[idx, 'tip'])

idx <- which(tips[ ,'sex'] == 'Male')
avg.male <- mean(tips[idx, 'tip'])

avg.female
avg.male

# 흡연유무에따라 팁을주는정도
unique(tips$smoker)

idx <- which(tips[,'smoker'] == 'Yes')
avg.smoker <- mean(tips[idx, 'tip'])
avg.smoker

idx <- which(tips[,'smoker'] == 'No')
avg.nosmoker <- mean(tips[idx, 'tip'])
avg.nosmoker

# 함수작성해서 코드중복을 줄인다
# myFunc_tip.R

source('myFunc_tip.R')
meanbycol.tip('sex')
meanbycol.tip('smoker')

# 결제금액대비 받은팁
categorize.tip <- function(tips) {
  tip_ratio <- tips$tip/tips$total_bill * 100
  
  class <- c()
  
  for(i in 1:nrow(tips)){
    if(tip_ratio[i] < 10){
      class[i] <- 1
    } else if(tip_ratio[i] < 15){
      class[i] <- 2
    } else if(tip_ratio[i] < 20){
      class[i] <- 3
    } else {
      class[i] <- 4
    }
  }
  tips.new <- cbind(tips, type = class, ratio=tip_ratio)
  return(tips.new)
}
source('myFunc_tip.R')
tips.new <- categorize.tip(tips)
head(tips.new)

res <- c()
for(i in 1:4){
  idx <- which(tips.new[,'type'] == i)
  tips.tmp <- tips.new[idx, ]
  res.tmp <- apply(tips.tmp[c('type', 'total_bill', 'tip', 'ratio')], 2, mean)
  res <- rbind(res, res.tmp)
}
rownames(res) <- 1:4
res
