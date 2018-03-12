

#=======================================================
#   A Big Project for Data Science in Banking Course
#=======================================================


#---------------------------------------------------------------------------------------
#  Giải thích 1: Một mô hình có mức độ chính xác toàn cục khi phân loại
#  hồ sơ xin vay tín dụng thường không phải là mô hình mà Ngân Hàng lựa chọn
#  Data Source: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
#---------------------------------------------------------------------------------------


# Đọc và xem qua dữ liệu: 

rm(list = ls())
library(tidyverse)
library(magrittr)
taiwan_default <- read.csv("D:/Teaching/new_data/taiwan_credit1.csv")

taiwan_default %>% dim()
taiwan_default %>% str()

# Viết hàm dán lại nhãn: 
recode_default <- function(x) {
  case_when(x == 1 ~ "Default", 
            x == 0 ~ "NonDefault")
}

# Chỉ lấy một số biến được cho là quan trọng để phân tích và dán lại nhãn: 
dung <- taiwan_default %>% 
  select(X1, X6, X18:X23, -id, Y) %>% 
  mutate(Y = recode_default(Y), 
         Y = as.factor(Y))

dung %>% head()

# Chỉ lấy 2000 quan sát bất kì trong bộ số liệu đầu: 
set.seed(1709)
dung_small <- dung %>% 
  sample_n(2000)

# Chia bộ dữ liệu thành hai phần bằng nhau (1000 cho mỗi phần):
library(caret)
set.seed(123) 
indxTrain <- createDataPartition(y = dung_small$Y, p = 1 / 2, list = FALSE) 
training <- dung_small[indxTrain, ]
testing <- dung_small[-indxTrain, ]

# Mô hình Logistic: 
logistic <- train(Y ~., 
                  data = training, 
                  method = "glm",
                  family = "binomial")

# Đánh giá mô hình trên test data: 
pred1 <- predict(logistic, newdata = testing %>% select(-Y))
pred1 %>% head()

confusionMatrix(data = pred1, testing$Y, positive = "Default")

# Mô hình Probit: 
probit <- train(Y ~., 
                data = training, 
                method = "glm", 
                family = "binomial"(link = "probit"))

# Đánh giá mô hình trên bộ dữ testing: 
pred2 <- predict(probit, newdata = testing %>% select(-Y))
confusionMatrix(data = pred2, testing$Y, positive = "Default")


#---------------------------------------------------------------------------------------
#  Giải thích 2: Nếu chọn một tiêu chí nào đó, chẳng hạn, mức chính xác toàn cục để 
#  lựa chọn mô hình phân loại thì tiêu chí đó phải được đánh giá trên nhiều lần chọn 
#  mẫu khác nhau chứ không phải chỉ dựa trên một mẫu hay một tình huống cụ thể nào đó. 
#---------------------------------------------------------------------------------------

set.seed(29) 
indxTrain <- createDataPartition(y = dung_small$Y, p = 1 / 2, list = FALSE) 
training <- dung_small[indxTrain, ]
testing <- dung_small[-indxTrain, ]

# Mô hình Logistic: 
logistic <- train(Y ~., 
                  data = training, 
                  method = "glm",
                  family = "binomial")

# Đánh giá mô hình trên test data: 
pred1 <- predict(logistic, newdata = testing %>% select(-Y))
confusionMatrix(data = pred1, testing$Y, positive = "Default")

# Mô hình Probit: 
probit <- train(Y ~., 
                data = training, 
                method = "glm", 
                family = "binomial"(link = "probit"))

# Đánh giá mô hình trên bộ dữ testing: 
pred2 <- predict(probit, newdata = testing %>% select(-Y))
confusionMatrix(data = pred2, testing$Y, positive = "Default")


# Đánh giá hai mô hình trên 100 mẫu khác nhau: 

acc_logistic <- c()
acc_probit <- c()

for (i in 1:100) {
  set.seed(i)
  testing <- dung %>% sample_n(1000)
  
  pred1 <- predict(logistic, newdata = testing %>% select(-Y))
  acc1 <- mean(pred1 == testing$Y)
  acc_logistic <- c(acc_logistic, acc1)
  
  pred2 <- predict(probit, newdata = testing %>% select(-Y))
  acc2 <- mean(pred2 == testing$Y)
  acc_probit <- c(acc_probit, acc2)
  
}

acc_logistic %>% mean()
acc_probit %>% mean()

all_df <- data.frame(Accuracy = c(acc_logistic, acc_probit), 
                     Model = c(rep("Logistic", 100), rep("Probit", 100)))


theme_set(theme_minimal())
all_df %>% 
  ggplot(aes(Model, Accuracy)) + 
  geom_boxplot()


# Viết hàm so sánh hai mô hình dựa trên tiêu chí Accuracy: 
evaluating_fun <- function(so_lan, kich_thuoc_mau, model1, model2) {
  acc_m1 <- c()
  acc_m2 <- c()
  
  for (i in 1:so_lan) {
    set.seed(i)
    testing <- dung %>% sample_n(kich_thuoc_mau)
    
    pred1 <- predict(model1, newdata = testing %>% select(-Y))
    acc1 <- mean(pred1 == testing$Y)
    acc_m1 <- c(acc_m1, acc1)
    
    pred2 <- predict(model2, newdata = testing %>% select(-Y))
    acc2 <- mean(pred2 == testing$Y)
    acc_m2 <- c(acc_m2, acc2)
    
  }
  my_df <- data.frame(Accuracy = c(acc_m1, acc_m2), 
                      Model = c(rep("Model1", so_lan), rep("Model2", so_lan)))
  return(my_df)
  
}

# Sử dụng hàm: 
so_sanh <- evaluating_fun(so_lan = 100, kich_thuoc_mau = 1000, logistic, probit)

# Đánh giá sơ bộ bằng công cụ hình ảnh: 

so_sanh %>% 
  ggplot(aes(Model, Accuracy)) + 
  geom_boxplot()

# Đánh giá sơ bộ bằng các tiêu chí thống kê: 
so_sanh %>% 
  group_by(Model) %>% 
  summarise_each(funs(mean, median, min, max, sd), Accuracy)


#------------------------------------------------------------------------------------
#  Giải thích 3: Lợi nhuận nên là tiêu chí đầu tiên được lựa chọn mô hình phân loại
#  và phải dựa vào nhiều mẫu số liệu (nhiều tình huống khác nhau)
#  Tham khảo: http://rpubs.com/chidungkt/320301
#------------------------------------------------------------------------------------

# Viết hàm đánh giá lợi nhuận: 

df_evaluate_profit <- function(model, so_lan_chon_mau, kich_thuoc) {
  
  ket_qua <- data.frame()
  for (j in 1:so_lan_chon_mau) {
    set.seed(j)
    testing <- dung %>% sample_n(kich_thuoc)
    du_bao <- predict(model, testing %>% select(-Y))
    u <- confusionMatrix(du_bao, testing$Y, positive = "Default")
    v <- u$table %>% as.vector()
    ket_qua <- rbind(ket_qua, v)
    names(ket_qua) <- c("BB", "GB", "BG", "GG")
  }
  
  return(ket_qua)
}


# Sử dụng hàm: 
m <- df_evaluate_profit(logistic, so_lan_chon_mau = 1000, kich_thuoc = 3000)

# Tính toán lợi nhuận: 
m %<>% mutate(Profit = 0.2*GG - BG)

# Đánh giá các thống kê về lợi nhuận: 
m$Profit %>% summary()

# Hình ảnh hóa lợi nhuận này: 

m %>% 
  ggplot(aes(Profit)) + 
  geom_density(fill = "blue", color = "blue", alpha = 0.3) + 
  geom_histogram(aes(y = ..density..), fill = "red", color = "red", alpha = 0.3)

n <- df_evaluate_profit(probit, so_lan_chon_mau = 1000, kich_thuoc = 3000) %>% 
  mutate(Profit = 0.2*GG - BG)


df_compare <- bind_rows(m %>% mutate(Model = "Logistic"), 
                        n %>% mutate(Model = "Probit")) 

df_compare %<>% mutate(Accuracy = (BB + GG) / (BB + GG + BG + GB))


df_compare %>% 
  ggplot(aes(Profit)) + 
  geom_density(fill = "blue", color = "blue", alpha = 0.3) + 
  geom_histogram(aes(y = ..density..), fill = "red", color = "red", alpha = 0.3) + 
  facet_wrap(~ Model)

df_compare %>% 
  group_by(Model) %>% 
  summarise_each(funs(mean, median, min, max, sd), Profit)

df_compare %>% 
  group_by(Model) %>% 
  summarise_each(funs(mean, median, min, max, sd), Accuracy)


#------------------------------------------------------------------------------------
#  Giải thích 4: Mức độ chính xác của mô hình phân loại còn phụ thuộc vào ngưỡng 
#  (Threshold) để phân loại nhóm (hay nhãn) của biến đích và do đó có thể ảnh hưởng
#  đến các tiêu chí khác như lợi nhuận. 
#------------------------------------------------------------------------------------

# Viết hàm re-convert nhãn: 
reconvert <- function(x) {
  case_when(x == "Default" ~ 1, 
            x == "NonDefault" ~ 0)
}

# Áp dụng hàm: 

dung10 <- dung %>% mutate(Y = reconvert(Y))

set.seed(1709)
dung_small <- dung10 %>% 
  sample_n(2000)

# Chia bộ dữ liệu thành hai phần bằng nhau (1000 cho mỗi phần):

set.seed(123) 
indxTrain <- createDataPartition(y = dung_small$Y, p = 1 / 2, list = FALSE) 
training <- dung_small[indxTrain, ]
testing <- dung_small[-indxTrain, ]

# Mô hình Logistic: 
logistic <- train(Y ~., 
                  data = training, 
                  method = "glm",
                  family = "binomial")

# Đánh giá mô hình trên test data: 
pred1 <- predict(logistic, newdata = testing %>% select(-Y))

pred1 %>% head()


#  Viết hàm phân loại hồ sơ theo ngưỡng mà chúng ta lựa chọn: 
xep_loai <- function(x, nguong) {
  case_when(x >= nguong ~ "Default", 
            x < nguong ~ "NonDefault")
  
}

# Đánh giá mô hình khi ngưỡng là 0.5: 

confusionMatrix(data = pred1 %>% xep_loai(nguong = 0.5), 
                testing$Y %>% recode_default(), positive = "Default")

# Đánh giá mô hình khi ngưỡng là 0.6: 

confusionMatrix(data = pred1 %>% xep_loai(nguong = 0.6), 
                testing$Y %>% recode_default(), positive = "Default")


# Viết hàm: 
my_fun <- function(nguong, so_lan_lap, n_sample, model) {
  ket_qua <- data.frame()
  for (i in 1:so_lan_lap) {
    set.seed(i)
    testing <- dung10 %>% 
      sample_n(n_sample)
    
  
    dubao <- predict(model, testing %>% select(-Y))
    dubao <- xep_loai(dubao, nguong)
    u <- confusionMatrix(dubao, testing$Y %>% recode_default(), positive = "Default")
    v <- u$table %>% as.vector()
    ket_qua <- rbind(ket_qua, v)
    names(ket_qua) <- c("BB", "GB", "BG", "GG")
  }
  return(ket_qua)
}


# Sử dụng hàm với ngưỡng là 0.5 và 1000 lần chạy mô hình, kích cỡ 3000: 
kq5 <- my_fun(0.5, 1000, 3000, logistic)
# Sử dụng hàm với ngưỡng là 0.6 và 1000 lần chạy mô hình, kích cỡ 3000: 
kq6 <- my_fun(0.6, 1000, 3000, logistic)

df_nguong <- bind_rows(kq5 %>% mutate(Thr = "T0.5"), 
                       kq6 %>% mutate(Thr = "T0.6"))

df_nguong %<>%  
  mutate(Profit = 0.2*GG - BG, 
         Accuracy = (BB + GG) / (BB + GG + BG + GB))

df_nguong %>% 
  group_by(Thr) %>% 
  summarise_each(funs(mean, median, min, max, sd, n()), Profit)


df_nguong %>% 
  group_by(Thr) %>% 
  summarise_each(funs(mean, median, min, max, sd, n()), Accuracy)


#---------------------------------------------------------------------------------
#  Giải thích 5: 
#  Feature Engineering - bỏ / thêm / chuyển hóa biến đầu vào có thể cải thiện
#  mức độ chính xác của mô hình phân loại đối với nhiều mô hình phân loại. 

#  References: 
#  - Mastering Feature Engineering Principles and Techniques for Data Scientists
#  - ds_breast_cancer_cancer.r
#----------------------------------------------------------------------------------

#------  Case 1: Bỏ biến tương quan cao và phương sai không  ---------#
data("GermanCredit")

df_ori <- GermanCredit
Class <- GermanCredit$Class

feature_df <- GermanCredit %>% select(-Class)

# Vị trí cột biến có phương sai zero và loại bỏ: 
zero_pos <- nearZeroVar(feature_df)
zero_va <- names(feature_df)[zero_pos]
feature_df %<>% select(-zero_va)

# Loại tương quan trên 0.75: 
tuong_quan <- cor(feature_df)
highCorr <- findCorrelation(tuong_quan, cutoff = .75)

var_name <- names(feature_df)[highCorr]

final_df <- feature_df %>% 
  select(-var_name) %>% 
  mutate(Class = Class)


# Model thứ nhất: 
set.seed(1)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 20,
                     number = 10)

logistic1 <- train(Class ~ .,
                   data = final_df,
                   method = "glm", 
                   trControl = ctrl)

# Model thứ 2: 
logistic2 <- train(Class ~ .,
                   data = GermanCredit,
                   method = "glm", 
                   trControl = ctrl)

# Đánh giá nhanh: 
logistic1$resample %>% summary()
logistic2$resample %>% summary()

# Đánh giá hình bằng hình ảnh: 

comp_df <- bind_rows(logistic1$resample %>% mutate(Model = "Logistic1"), 
                     logistic2$resample %>% mutate(Model = "Logistic2"))

comp_df %>% 
  ggplot(aes(Model, Accuracy)) + geom_boxplot()

# Đánh giá bằng các tiêu chí thống kê: 
comp_df %>% 
  group_by(Model) %>% 
  summarise_each(funs(mean, median, min, max, n()), Accuracy)
  

#------  Case 2: Chuẩn hóa dữ liệu (ds_breast_cancer_cancer)  ---------#


#---------------------------------------------------------------
#       Main Project: hmeq.csv  (to be continued in part 2)
#---------------------------------------------------------------











