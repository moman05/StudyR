## 매트릭스 계속

z <- matrix(1:20, nrow=4, ncol=5, byrow=T)
z

x <- c(20, 25, 30, 35)
x
m1 <- cbind(z, x)  # 열기준 결합
m1

y <- 36:41
y

m2 <- rbind(m1, y)  # 행기준 결합
m2


#값추출
m2[1,3]
m2[1,6]
m2[1,7]

z[1,6]
z[1,5]
z[4,5]
z
z[,5]

z[2,1:3]
z[1,c(1,2,4)]
z
z[1:2,]
z[1:2,c(2,4)]
z[,c(1,4)]
z

# 매트릭스 이름 붙이기
score <- matrix(c(90,85,69,78,85,96,49,95,90,80,70,60),
                nrow=4)
score
rownames(score) <- c('John','Tom','Mark','Jane')
score
colnames(score) <- c('English','Math','Science')
score

score['John','Math']
score['Math','Tom'] # Error

score['Tom',]
score[,'Science']
rownames(score)
colnames(score)
colnames(score)[2]

## LAB 햄버거 영양성분 정보
burger = matrix(c(514,533,566,917,853,888,11,13,10),
                nrow = 3)
burger
rownames(burger) <- c('Macdonald','Lotteria','BurgerKing')
burger
colnames(burger) <- c('kcal','na','fat')
burger


burger['Macdonald','na']
burger['Macdonald',]
burger[,'kcal']

# 데이터프레임
physicals <- matrix(c(168.4,169.5,62.4,65.3,'M','F'), nrow=2)
physicals

city <- c('Seoul','Tokyo','Washington')
rank <- c(1,3,2)
city.info <- data.frame(city, rank)
city.info

## iris 데이터셋
iris
head(iris)
tail(iris)
str(iris)
z
str(z)
str(burger)
str(x)
str(c('John','Tom','Mark','Jane'))

iris[140,]
iris[,5]
levels(iris[,5])
iris[,c(1,2)]
iris[,c('Sepal.Length','Petal.Length','Species')]
head(iris)


val1 <- head(iris[,c('Sepal.Length','Petal.Length','Species')])
tail(iris[,c('Sepal.Length','Petal.Length','Species')])


head(iris[,c('Sepal.Length','Petal.Length','Species')])[1,]
head(iris[,c('Sepal.Length','Petal.Length','Species')])[1,3]


## LAB2 햄버거 영양정보2
kcal <- c(514,533,566)
na <- c(917,853,888)
fat <- c(11,13,10)
menu <- c('새우버거','불고기버거','치킨버거')
burger.2 <- data.frame(kcal,na,fat,menu)
burger.2
rownames(burger.2) <- c('맥도날드','롯데리아','버거킹')
burger.2

burger.sum <- burger.2[,'na'] * burger.2[,'fat']
burger.sum

burger.2['맥도날드',]

##
dim(iris)
x
dim(x)
dim(burger.2)

nrow(iris)
ncol(iris)
colnames(iris)
colnames(burger.2)
rownames(burger.2)
rownames(iris)

head(iris, n=2)
tail(iris, n=5)
head(x=iris, n=10)
str(iris)

iris$Sepal.Length
levels(iris$Species)
levels(iris[,'Species'])

# 행별, 열별 합계, 평균 산술계산
colSums(iris[,-5])
colSums(iris[,-'Species'])  # Error

colSums(iris[,c(1, 2)])  #최소 두개 이상의 열이 선택되어야 됨
colMeans(iris[,-5])
head(iris)
rowSums(iris[,-5])
rowMeans(iris[,-5])

z <- matrix(1:20, nrow=4)
z
t(z)
str(burger.2)
str(t(burger.2))

iris.subset.1 <- subset(iris, Species=='setosa')
iris.subset.1

iris.subset.2 <- subset(iris, Sepal.Length>=5.0 & Sepal.Width>3.0)
iris.subset.2
str(iris.subset.2)

iris.subset.2$Sepal.Length

iris.subset.2[,c(1,2,5)]



a <- matrix(1:20, 4, 5)
b <- matrix(21:40, 4, 5)
a + b
a * b

a.2 <- a * 2
a
a.2

iris

class(iris)
str(iris)
class(z)
str(z)
is.matrix(iris)
is.matrix(z)
is.data.frame(iris)

## LAB 벚나무 판매
trees
class(trees)
str(trees)

# 화원 나무 직경 평균
girth.mean <- mean(trees[,1])
girth.mean
girth.mean.2 <- mean(trees$Girth)
girth.mean.2

candidate <- subset(trees, Girth >= girth.mean & Height >= 80 & Volume >= 50)

candidate
nrow(candidate)

# 종업원 팁계산
install.packages('reshape2')
library(reshape2)
tips

is.matrix(tips)
is.array(tips)
is.data.frame(tips)
class(tips)
str(tips)
levels(tips$day)

head(tips)
tail(tips)

table(tips$day)
class(table(tips$day))

dinner <- subset(tips, time=='Dinner')
dinner
lunch <- subset(tips, time=='Lunch')
lunch
table(dinner$day)
table(lunch$day)

head(dinner)
dinner.data <- dinner[c('total_bill','tip','size')]
lunch.data <- lunch[c('total_bill','tip','size')]
str(dinner.data)
str(lunch.data)

colSums(dinner.data)
colMeans(dinner.data)
colSums(lunch.data)
colMeans(lunch.data)

tips.rate <- tips$tip/tips$total_bill
tips.rate
mean(tips.rate) * 100
