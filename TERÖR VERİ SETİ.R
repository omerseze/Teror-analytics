
library(dplyr)  
library(ggplot2)
library(tidyverse)
library(psych)
library(raster)
library(readr)
library(readxl)
library(sf)
library(maps)
library(spData)
library(magick)
library(grid)
library(tmap)
library(viridis)
library(maps)     
library(timetk)
library(mice)
library(readr)
library(magrittr)
library(e1071)
library(caret)
library(tidymodels)
## gerekli kütüphanler çağrılır.

##  R stuido içerisine enviroment ımport dataset from text(readr ) kısmından veri setini yüklüyorum
## yüklediğim ana verisetini adı Terror_Data seti bu veri setini df diye yeni bir değişkene atayım tanımlıyorum
df <- Terror_Data
## df verisetindeki country 209 olan türkiyeyi çekip turkey değişkenine atıyorum
turkey <- df[df$country== 209, ]
## boş değerleri atmak için masasüstüne atıyorum 
write_csv(turkey, "/Users/omerfaruksezer/Desktop/turkey.csv",na = "")
#sonra tekrar okutuyorum bu kısımda NA değerli tekrar r görüyor
turkey <- read_csv("/Users/omerfaruksezer/Desktop/turkey.csv")
## navie bayes 
model <- naiveBayes(turkey$nkill ~ ., data = turkey)
print(model)
#ongoru yapalim
ongoru <- predict(model,turkey)
#gercek siniflandirma degerleri ile ongoru degerlerini karsilastiralim
table(turkey$nkill,ongoru)
#iris veri setinin tum ongoru degerlerini goreyim
print(ongoru)
data.frame(gercek=turkey$nkill,ongoru=ongoru)
#karisiklik matrisi
karisiklikmatrisi <- table(turkey$nkill,ongoru)
karisiklikmatrisi
#dogruluk oranini bulalim
sum(diag(karisiklikmatrisi))/sum(karisiklikmatrisi)
# R daki diğer istatistiksel özelliklerine bakıyorum 
glimpse(turkey)
summary(turkey)
str(turkey)
describe(turkey)
view(turkey)
nrow(turkey)
is.null(turkey)
attributes(turkey)
sum(is.null(turkey$nkill))

## bu kısımdan sonra ise görselleştirmeler yapıyorum 

##türkiyedeki terör olaylarının olduğu yerleri gösteren harita
ggplot(turkey, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.4,col="firebrick1") +
  labs(title="TÜRKİYE TERÖR HARİTASI", subtitle="türkiyede gerçekleşen 1970-2016 yıllarına ait terör olaylarının gerçekleşiği koordinatları gösteren harita", y="ENLEM", x="BOYLAM", caption="Türkiye terör olaylarının gerçekleştiği yoğunluk haritası ")+
  theme_minimal() +
  theme(axis.text = element_text(size = 16),
        axis.line.x.bottom = element_line(size = 2, color = "gray40"),
        axis.line.y  = element_line(size = 2, color = "gray40"),
        axis.title = element_text(size = 20, face = "bold"))


## türkiyedeki olan terör olaylarının yıllara göre kaç kişinin öldüğünü gösteren scaatter plot uygulaması 
 ggplot(turkey, aes(x =turkey$Yıl, y = turkey$nkill))+
   labs(y = "ölüm sayısı", x = "yıllar") +
   geom_point(color = "darkturquoise")
 ## türkiyedeki olan terör olaylarının yıllara göre iller düzeyinde  kaç kişinin yaralandığını gösteren  uygulama
 
 ggplot(turkey, aes(x =Yıl, y =nwound,fill=provstate))+
   labs(y = "yaralı sayısı", x = "yıllar") +
   geom_bar(stat = "identity")
 ## türkiyede kaç kişinin öldüğünü gösteren barplot uygulaması 
 
 ggplot(data = turkey, aes(x = turkey$Yıl, y = turkey$nkill)) +
   geom_bar(stat = "identity", width = 0.4, 
            fill = "darkorange2", color = "dimgray")
 
##sadece 1996 yılı verisetinden seçilip o yıla ait aylara göre ölüm grafiği barplot ile gösterimi

   df_year <- turkey[turkey$Yıl == 1996, ]
 ## illere göre enlem ve boylamda nerde gerçekleştiğini gösteren harita uygulması 
 ggplot(df_year, aes(x =df_year$longitude, y = df_year$latitude,fill=provstate))+
   labs(y = "ölüm sayısı", x = "yıllar") +
   geom_point(color = "red")
 
 ##1996 aylara göre ölüm oranları barplot gösterimi
 ggplot(df_year, aes(x = df_year$imonth, y = df_year$nkill))+
   labs(title = "1996 yılı aylara göre ölüm oranları",x= "aylar",y = "ölüm sayısı")+
   geom_bar(stat = "identity",fill= 'darkturquoise', width=0.5,color ="darkmagenta")
   geom_point(color = "darkmagenta")

   
   
   turkey %>% explore()
   
   ## zaman serisi analizi 
   turkey %>%
     plot_time_series(Yıl, nkill, .smooth = F, .interactive = F)
   
  ## navie bayes ile kaç kişinin öleceğini tahmin etmek için öngürülen değişkenlerde gerçekleşecek ölüm sayısını hesaplamak.
   
    modell <- naiveBayes(df_turkey$nkill ~ ., data = turkey)
   
   
   kkk <- data.frame(
     Yıl = 2029,
     imonth = 7,
     iday = 20,
     country = 209,
     country_txt = "Turkey",
     region = 10,
     region_txt = "Middle East & North Africa",
     provstate = "Istanbul",
     city = "Uskudar",
     nkill = 35,
     nwound = 120
   )
   predicted_nkill <- predict(modell, newdata = kkk)
   
   # Tahminlenen nkill değerini yazdır
   print(paste("Tahminlenen ölüm sayısı:", predicted_nkill))
   
   
 ## faktöre çevirme işlevi  sadece karakter olanları faktör yapar 
     turkey<- read_csv("/Users/omerfaruksezer/Desktop/turkey.csv") %>%
       clean_names() %>%
       mutate_if(is.character, as_factor)
     
     print(turkey)