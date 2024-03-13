################### Datanın İçeri Aktarılması ##################################

df <- read.csv("C:\\Users\\GLB90057874\\Desktop\\Denetimli İstatistik\\hitters_eda.csv")

head(df)

################## Çoklu Doğrusal Regresyon Modelinin Kurulması ################

base_model=lm(Salary~., data=df) #Tüm değişkenler ile base modele ekliyoruz.

summary(base_model)

library(car)
vif(base_model) #VIF değerlerini yazdırır.

base_model2=lm(Salary~Walks+PutOuts+Assists+Errors+NEW_HitRatio+NEW_RunRatio+NEW_CHitRatio+NEW_CRunRatio+League_N+Division_W+NewLeague_N, data=df)
summary(base_model2)
vif(base_model2)


#5.	Değişken seçim yöntemlerini kullanarak (Değişken seçim yöntemleri ile ana etkenlere karar verilmektedir. Veri seti içinde karesel, kübik, vb. etkisi olduğunu düşündüğünüz ya da etkileşim etkisini katabileceğiniz şekilde modeli geliştirebilirsiniz) ve alternatif modeller için tahmin performanslarını karşılaştırarak final modelinize karar veriniz. 
######Model Se?me  Komutlar?####

library(olsrr)
library("ISLR")
library(car)

a=ols_step_all_possible(base_model2)
plot(a)
summary(a)

###Walks PutOuts NEW_HitRatio NEW_RunRatio NEW_CHitRatio Division_W

base_model3=lm(Salary~Walks+PutOuts+NEW_HitRatio+NEW_RunRatio+NEW_CHitRatio+Division_W, data=df)
summary(base_model3)

######Matrix Plot Çizimi#####
base_model3df <- data.frame(df$Walks, df$Salary, df$PutOuts,df$NEW_HitRatio,df$NEW_RunRatio,df$NEW_CHitRatio,df$Division_W)
pairs(base_model3df, pch=19, col='red', lower.panel = NULL)

#FArklı bir değişken seçim yöntemi
s=ols_step_both_p(base_model3)
s=ols_step_both_p(base_model3, pent = 0.05, prem = 0.1)
s
s$base_model3
plot(s)

########Değişkenlerin Normalleştirilmesi için DAğılım Grafiklerinin Kontrolü#######
# Örnek bir değişken
my_variable <- df$Walks
hist(my_variable, col = "skyblue", main = "Değişkenin Dağılımı", xlab = "Değerler", ylab = "Frekans") #log alacağız

my_variable <- df$PutOuts
hist(my_variable, col = "skyblue", main = "Değişkenin Dağılımı", xlab = "Değerler", ylab = "Frekans") #log alacağız

my_variable <- df$NEW_HitRatio
hist(my_variable, col = "skyblue", main = "Değişkenin Dağılımı", xlab = "Değerler", ylab = "Frekans") #log alacağız

my_variable <- df$NEW_RunRatio
hist(my_variable, col = "skyblue", main = "Değişkenin Dağılımı", xlab = "Değerler", ylab = "Frekans") #log alacağız

my_variable <- df$NEW_CHitRatio
hist(my_variable, col = "skyblue", main = "Değişkenin Dağılımı", xlab = "Değerler", ylab = "Frekans") #log alacağız


base_model3=lm(Salary~Walks+PutOuts+NEW_HitRatio+NEW_RunRatio+NEW_CHitRatio+Division_W, data=df)
summary(base_model3)


log_transform_vars <- c("Walks", "PutOuts", "NEW_HitRatio", "NEW_RunRatio", "NEW_CHitRatio")
df[log_transform_vars] <- log(df[log_transform_vars] + 1)  # +1 eklenmesi log(0) hatasını önler
# Log dönüşümü yapılan değişkenleri içeren modeli oluşturalım
log_transform_model <- lm(Salary ~ Walks + PutOuts + NEW_HitRatio + NEW_RunRatio + NEW_CHitRatio + Division_W, data = df)
# Modelin özetini görüntüleyelim
summary(log_transform_model)

######Matrix Plot Çizimi#####
log_transform_modeldf <- data.frame(df$Walks, df$Salary, df$PutOuts,df$NEW_HitRatio,df$NEW_RunRatio,df$NEW_CHitRatio,df$Division_W)
pairs(log_transform_modeldf, pch=19, col='red', lower.panel = NULL)

vif(log_transform_model)


#a.	Hataların normal dağıldığı varsayımını grafikle ve uygun istatistiksel test ile kontrol ediniz.


# Hataların normal dağıldığını kontrol etme
qqnorm(log_transform_model$residuals)
qqline(log_transform_model$residuals)

###Kolmogorov-Smirnov Testi 
ks.test(log_transform_model$residuals, "pnorm")

#Bu test sonuçlarına göre, regresyon modelinizin hata terimleri normal bir dağılıma uymamaktadır. 
#p değeri çok düşük olduğu için, null hipotez (veri setinin normal bir dağılıma sahip olduğu) reddedilir. 
#Bu durumda, modelinizin normalite varsayımı karşılamadığını söyleyebiliriz.

#b.	Hataların sabit varyanslı olup olmadığını grafikle ve uygun istatistiksel test ile kontrol ediniz.

# Hataların standartlaştırılmış (Studentized) değerlerini elde etmek için
log_transform_model_stand_residuals <- rstandard(log_transform_model)

# Hataların standartlaştırılmış değerlerini görselleştirmek için
plot(log_transform_model_stand_residuals, ylab = "Standartlaştırılmış Hatalar", xlab = "Gözlemler")
abline(h = 0, col = "red", lty = 2)

# Residual plot çizimi
plot(log_transform_model, which = 1)

#c.	Uç değer ve etkin gözlem olup olmadığını grafiklerle ve ilgili değerlerle belirleyiniz.

# Cook's Distance'i hesapla
cooksd <- cooks.distance(log_transform_model)

# Cook's Distance grafikle görselleştirme
plot(cooksd, pch = "o", cex = 1, main = "Cook's Distance")
abline(h = 4/length(log_transform_model$residuals), col = "red", lty = 2)

# Residuals vs. Leverage grafik
par(mfrow = c(1, 1))
plot(log_transform_model, which = 5)

#d.	VIF değerlerine bakarak yorumlayınız.

vif(log_transform_model)

#e.	Final modelin katsayılarını yorumlayınız.
summary(log_transform_model)

#f.	Katsayıların %95’lik güven aralıklarını elde ederek yorumlayınız.

###G�ven ve Tahmin Aral�klar�###

confint(log_transform_model)
confint(log_transform_model,level=0.95)

#g.	Yeni bir gözlem değeri için %95’lik güven aralığını ve kestirim aralığını bularak yorumlayınız.
Walks + PutOuts + NEW_HitRatio + NEW_RunRatio + NEW_CHitRatio + Division_W
# Yeni gözlem değeri için 95% güven aralığı
new_observation <- data.frame(Walks = 0.99, 
                              PutOuts = 1.11, 
                              NEW_HitRatio = 0.20, 
                              NEW_RunRatio = 0.29, 
                              NEW_CHitRatio = 0.20, 
                              Division_W = 1)
predict_interval <- predict(log_transform_model, newdata = new_observation, interval = "confidence", level = 0.95)

# Güven aralığını yazdırma
cat("95% Güven Aralığı:", predict_interval[1], "ile", predict_interval[2], "\n")

# Yeni gözlem değeri için tahmin ve 95% kestirim aralığı
predict_interval <- predict(log_transform_model, newdata = new_observation, interval = "prediction", level = 0.95)

# Kestirim aralığını yazdırma
cat("95% Kestirim Aralığı:", predict_interval[1], "ile", predict_interval[2], "\n")

#6.	Modeli geliştirmek üzere görüş ve öneriniz varsa belirtiniz.
