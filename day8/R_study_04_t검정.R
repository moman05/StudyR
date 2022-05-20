## compact 자동차와 suv 자동차의 도시 연비 t 검정
# 데이터준비
mpg <- as.data.frame(ggplot2::mpg)

library(dplyr)
mpg_diff <- mpg %>%  # %>% <<이거 Ctrl+Shift+M
  select(class, cty) %>%
  filter(class %in% c('compact', 'suv'))

unique(mpg_diff$class)
head(mpg_diff)
table(mpg_diff$class)

#t-test
t.test(data=mpg_diff, cty~class, var.equal=T)


## 일반 휘발유와 고급 휘발유의 도시 연비 t 검정
# 데이터 준비
mpg_diff2 <- mpg %>% 
  select(fl, cty) %>%
  filter(fl %in% c('r','p'))

table(mpg_diff2$fl)

#t-test
t.test(data=mpg_diff2, cty~fl, var.equal=T)


## 실업자 수와 개인 소비 지출의 상관관계
# 데이터 준비
economics <- as.data.frame(ggplot2::economics)

# 상관분석
cor.test(economics$unemploy, economics$pce)


## 상관행렬 히트맵 만들기
# 데이터 준비
head(mtcars)

# 상관행렬 만들기
car_cor <- cor(mtcars)
round(car_cor, 2)

install.packages('corrplot')
library(corrplot)
  #히트맵
corrplot(car_cor)
  #숫자표기
corrplot(car_cor, method='number')

