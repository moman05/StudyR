## 데이터 분석
# 데이터 준비
install.packages('HSAUR') 
library(HSAUR)

data("Forbes2000")
ds <- Forbes2000
ds[!complete.cases(ds),]  # 결측값 확인

str(ds)
head(ds)
View(ds)

# 국가별 기업통계
table(ds$country)
tmp <- sort(table(ds$country), decreasing = T)
top.10.contry <- tmp[1:10]
top.10.contry

par(mar=c(8,4,4,2))
barplot(top.10.contry,
        main='기업수 상위 10개국',
        col=rainbow(10),
        las=2
        )
par(mar=c(5,4,4,2))

# 업종별 기업 통계
table(ds$category)
tmp <- sort(table(ds$category), decreasing = T)
top.10.category <- tmp[1:10]
top.10.category

par(mar=c(10,4,4,2))
barplot(top.10.category,
        main='기업수 상위 10개 업종',
        col='pink',
        las=2)
par(mar=c(5,4,4,2))

# 업종별 기업자산 분포
tmp <- ds[ds$category %in% names(top.10.category),]
 # A %in% B 연산: A에 있는 값들 중 B에 속하는 값들을 찾는 역할
levels(tmp$category)
tmp$category <- factor(tmp$category)
levels(tmp$category)

par(mar=c(10,4,4,2))
boxplot(assets~category, data=tmp,
        ylim=c(0,100),
        xlab= ' ',
        las=2)
par(mar=c(5,4,4,2))

# 기업 가치 상위 10대 기업
tmp <- ds[order(ds$marketvalue, decreasing = T),]
tmp[1:10,c('name', 'country','category','marketvalue')]

# 한국 기업 정보
korea <- subset(ds, country=='South Korea')
korea[,c('rank','name','category','marketvalue')]

# 기업 가치와 타 변수와의 상관관계
tmp <- ds[,5:8]
tmp <- tmp[complete.cases(tmp),]
plot(tmp, lower.panel=NULL)
cor(tmp)

