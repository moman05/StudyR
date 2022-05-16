## 데이터 입출력
setwd('C:/STUDY/StudyR/day4')  # working 디렉토리 지정
print('Hello R!')

# 실행결과를 저장 append=F 파일 새로 생성 
sink('result.txt', append=T)  # append -> 이전 문서 다음 뒤, 계속 붙이기
a <- 10; b <- 20
cat('a+b=', a+b, '\n')
a + b

print('Hey')
sink()


sink('result.txt', append=T)

iris
tail(iris)

sink()  # 연 파일 닫아주기(반드시!!)

print('TEST')
sink()


## txt 파일 읽기/ 구분자 공백또는 탭
air.txt <- read.table('./airquality.txt', header = T, sep=' ')
air.txt

# csv파일도 read.table()로 로드 가능 sep=',' <<되어있어야함
air.csv <- read.table('../day3/airquality.csv', header=T, sep=',')
air.csv

## LAB 체질량 지수 계산 II
library(svDialogs)
# 마지막 Ctrl+Enter로 실행
height <- dlgInput('키 입력(cm)')$res
weight <- dlgInput('몸무게 입력(kg)')$res
height <- as.numeric(height)
weight <- as.numeric(weight)
height.unit <- height / 100
bmi <- weight / (height.unit^2)

sink('bmi.txt', append=T)
cat('height','weight','bmi\n')
cat(height.unit * 100, weight, bmi)
cat('\n')
sink()

result <- read.table('./bmi.txt', header=T, sep=' ')
result

names(result) <- c('키','몸무게','체질량지수')
result
write.table(result, './bmi2.txt', row.names=F)


## 엑셀 패키지
install.packages('readxl')  # 엑셀 읽기용 패키지
library(readxl)

air.xlsx <- read_excel('../day3/airquality.xlsx')
air.xlsx

install.packages('openxlsx')  #엑셀 쓰기용 패키지
library(openxlsx)

write.xlsx(result, 'bmi_reslut.xlsx', rowNames=F)


## 실전분석
library(svDialogs)
library(readxl)
library(openxlsx)

carprise.new <- read.csv('./carprice.csv', header=T)
carprise.new
str(carprise.new)
levels(carprise.new[,1])

levels(carprise.new[,1])

input.type <- dlgInput('차량타입 입력
                       (Compact, Small, Midsize, Large, Sporty, Van)')$res
input.city <- dlgInput('최소 시내연비 입력')$res
input.city <- as.numeric(input.city)

result <- subset(carprise.new, Type == input.type & MPG.city >= input.city)
result

write.xlsx(result, './van_result.xlsx', rowNames = F)

getwd()

## Oracle 연동
install.packages('rJava')
install.packages('RJDBC')
library(rJava)
library(RJDBC)

# DB연결 설정
jdbcDriver <- JDBC(driverClass = 'oracle.jdbc.OracleDriver',
                   classPath = './day4/ojdbc6.jar')

# PASS


