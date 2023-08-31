library(readxl);library("dplyr")
x <- read_xlsx("Book1.xlsx")
# 特定の行と列を選択する（例:6～53行目の列3.13） , 
specific_cell <- x %>%
  slice(6:53) %>%
  select(3,13)

colnames(specific_cell) <- c("都道府県別","薬剤師数/10万人")　#ラベルの作成
specific_cell #行数が 49 で、列数が 5 であることを確認した。
specific_cell[!complete.cases(specific_cell),]#欠損値（NA）を含む行を選択するためのコード
#---------------------
#https://www.e-stat.go.jp/stat-search/files?stat_infid=000032179772

y <- read_xlsx("mr_yaku0053.xlsx")
cell <- y %>%
  slice(5:52) %>%
  select(2,5,,10,19,22,25,30,31)
#cell[,1]　＝　データの列が抜き出せる
# カラム名を変更
colnames(cell) <- c("都道府県別","薬局の従事者", "医療施設の従事者",
                    "介護保険施設の従事者", "大学の従事者",
                    "医薬品関係企業の従事者","衛生行政機関又は保健衛生施設の従事者",
                    "その他の業務の従事者")
#cell[,1]。データの列を確認する
cell[!complete.cases(cell),]
# カラム名が変更されたことを確認
print(cell)
print(specific_cell)
print(z)
A <- na.omit(z)
A
#2データを1つにした。https://manamina.valuesccg.com/articles/898
z <- left_join(specific_cell,cell,by="都道府県別")
z[,4]
print(z)




summary(z)
z[!complete.cases(z),]

cell[!complete.cases(cell),]

# 10万人で判定がごちゃるので、``を入れてラベルと認識させた
ans <- lm(`薬剤師数/10万人` ~ 薬局の従事者 + 医療施設の従事者 +
            介護保険施設の従事者 + 大学の従事者 +
            医薬品関係企業の従事者 + 衛生行政機関又は保健衛生施設の従事者 +
            その他の業務の従事者, data = z)
summary(ans)


z[!complete.cases(z),]


z
B　<-for (col in colnames(z)) {
  z[, col][z[, col] == "-"] <- NA
}　
#データフレームの"-"を全てNAに変更した

A <- na.omit(B) #データフレームのNAを含む行を削除
A


ans<- lm(`薬剤師数/10万人` ~ 薬局の従事者 + 医療施設の従事者 +
           介護保険施設の従事者 + 大学の従事者 +
           医薬品関係企業の従事者 + 衛生行政機関又は保健衛生施設の従事者 +
           その他の業務の従事者, data = B)
summary(ans)

#https://epirhandbook.com/jp/cleaning.html
#na_ifの使い方について