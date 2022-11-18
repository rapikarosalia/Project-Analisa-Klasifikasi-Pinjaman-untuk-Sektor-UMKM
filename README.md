# Project-Analisa-Klasifikasi-Pinjaman-untuk-Sektor-UMKM

#Korelasi antar variabel data https://academy.dqlab.id/main/projectcode/299/584/2928?pr=0
library(corrplot)
library(ggcorrplot)

M = data_reduce[,8:11]

# Library corrplot
# -- Pearson correlation
par(mfrow=c(2,2))
corrplot(cor(M), type="upper", order="hclust")
corrplot(cor(M), method="square", type="upper")
corrplot(cor(M), method="number", type="lower")
corrplot(cor(M), method="ellipse")

# -- Kendall correlation
par(mfrow=c(2,2))
corrplot(cor(M, method="kendall"), type="upper", order="hclust")
corrplot(cor(M, method="kendall"), method="square", type="upper")
corrplot(cor(M, method="kendall"), method="number", type="lower")
corrplot(cor(M, method="kendall"), method="ellipse")

# Library ggcorrplot
corr = round(cor(M), 1) # Pearson correlation
ggcorrplot(round(cor(M), 1), 
		   hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Data Nasabah", 
           ggtheme=theme_bw)
           
#Pemilihan fitur/independent variabel/input https://academy.dqlab.id/main/livecode/299/587/2929?pr=0

colnames(data_reduce)
data_select = data_reduce[,c("KARAKTER","KONDISI_USAHA","KONDISI_JAMINAN","STATUS","KEWAJIBAN","OSL","KOLEKTIBILITAS","REKOMENDASI_TINDAK_LANJUT")]

data_non_na = na.omit(data_select)

#Transformasi Data https://academy.dqlab.id/main/livecode/299/587/2930?pr=0
data_select_new = data_select
data_select_new$KEWAJIBAN = scale(data_select_new$KEWAJIBAN)[, 1]
data_select_new$OSL = scale(data_select_new$OSL)[, 1]
data_select_new$KEWAJIBAN = cut(data_select_new$KEWAJIBAN, breaks = c(-0.354107,5,15,30))
data_select_new$KEWAJIBAN = as.factor(data_select_new$KEWAJIBAN)
data_select_new$OSL = cut(data_select_new$OSL, breaks = c(-0.60383,3,10,15))
data_select_new$OSL = as.factor(data_select_new$OSL)
data_select_new = na.omit(data_select_new)

#Training Data 
library(caret)
index = createDataPartition(data_select_new$REKOMENDASI_TINDAK_LANJUT,p= .95, list = FALSE)
train = data_select_new[index,]
test = data_select_new[-index,]

#Pemodelan/Modelling
train2 = train
# Setting the reference
train2$REKOMENDASI_TINDAK_LANJUT = relevel(train2$REKOMENDASI_TINDAK_LANJUT, ref = "Angsuran Biasa")
#training the model
require(nnet)
# Training the multinomial model
multinom_model = multinom(REKOMENDASI_TINDAK_LANJUT ~ ., data = train2)


# Checking the model
summary(multinom_model)
#converting the coefficients to odds by taking the exponential of the coefficients.
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))
# Predicting the values for train dataset
train2$ClassPredicted = predict(multinom_model, newdata = train2, "class")
train_prob = predict(multinom_model, newdata = train2, "probs")
df = train_prob
df$max=apply(df,1, max)
train2$score = df$max
test_prob = predict(multinom_model, newdata = test, "probs")
df2 = test_prob
df2$max=apply(df2,1, max)

tab_train = table(train2$REKOMENDASI_TINDAK_LANJUT, train2$ClassPredicted)
round((sum(diag(tab_train))/sum(tab_train))*100,4)
test$ClassPredicted = predict(multinom_model, newdata = test, "class")
test$score = df2$max
tab_test = table(test$REKOMENDASI_TINDAK_LANJUT, test$ClassPredicted)
round((sum(diag(tab_test))/sum(tab_test))*100,4)

