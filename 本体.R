library(readxl);library("dplyr")
x <- read_xlsx("Book1.xlsx")
X <- x %>%
  slice(6:53) %>%
  select(3,13)
colnames(X) <- c("都道府県別","薬剤師数に対する10万人")　#ラベルの作成

y <- read_xlsx("mr_yaku0053.xlsx")
Y <- y %>%
  slice(5:52) %>%
  select(2,5,,10,19,22,25,30,31)
colnames(Y) <- c("都道府県別","薬局の従事者", "医療施設の従事者",
                 "介護保険施設の従事者", "大学の従事者",
                 "医薬品関係企業の従事者","衛生行政機関又は保健衛生施設の従事者",
                 "その他の業務の従事者")

X$都道府県別 == Y$都道府県別
z <- left_join(X, Y, by = "都道府県別") %>%
  dplyr::mutate_at(
    .vars = c("薬剤師数に対する10万人", colnames(Y)[-1]),
    .funs = as.numeric
  )


# B <- z[-7,]
# A <- na.omit(B)
ans <- lm(`薬剤師数に対する10万人` ~ 薬局の従事者 + 医療施設の従事者 +
           介護保険施設の従事者 + 大学の従事者 +
           医薬品関係企業の従事者 + 衛生行政機関又は保健衛生施設の従事者 +
           その他の業務の従事者, data = z)
summary(ans)

par(family= "HiraKakuProN-W3")
library(psych)
pairs.panels(z[,-1], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             )


#------------
#zデータの確認をするため
#https://multivariate-statistics.com/2021/12/07/r-programming-xlsx-files/
install.packages("openxlsx")
library(openxlsx)
write.xlsx(z,file="zデータの確認.xlsx",asTable = F)

