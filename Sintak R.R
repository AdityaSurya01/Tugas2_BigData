library(tidyverse)     # Pemrosesan data dan visualisasi
library(ggplot2)       # Pembuatan grafik
library(factoextra)    # Analisis klastering dan visualisasi
library(FactoMineR)    # Analisis faktor dan eksplorasi data
library(cluster)       # Analisis klastering
library(corrplot)      # Analisis korelasi
library(repr)          # Kontrol representasi grafik

fit <- prcomp(data_2019[,-1],scale = FALSE)
fviz_eig(fit)
fit
head(X2019[,-(1:2)])
fit2 <- prcomp(data_2019[,-(1:2)], scale = TRUE )
var <- get_pca_var(fit2)
corrplot(var$cor)
fviz_pca_var(fit2, axes = c(1,2) ,col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)              
data_2019 <- data_2019[order(data_2019$Country),] #Pengurutan 
df_2019_numeric <- data_2019 %>% select(c(-Country)) #Menghilangkan colom "Country"
df_2019_scaled <- scale(df_2019_numeric) #Mengubah skala data m=0, sd=1
fviz_nbclust(df_2019_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") #Evaluasi jumlah cluster
fviz_nbclust(df_2019_scaled, kmeans, method = "wss")+
  labs(subtitle = "Elbow method") #Sama hanya beda method
gap_stat <- clusGap(df_2019_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax") #Mencetak hasil
fviz_gap_stat(gap_stat)
df_final_2019 <- df_2019_scaled
rownames(df_final_2019) <- data_2019$Country
head(df_final_2019)
final_2019 <- kmeans(df_final_2019, 4, nstart = 25)
print(final_2019)
fviz_cluster(final_2019, data = df_final_2019)

cluster_data <- as.data.frame(final_2019$cluster)

head(cluster_data)
total_df_2019 <- cbind(data_2019,cluster_data)
colnames(total_df_2019)[9] <- "cluster"
head(total_df_2019)
aggregate(total_df_2019[, 2:9], list(total_df_2019$cluster), mean)
total_df_2019$cluster <- factor(total_df_2019$cluster)
total_df_2019$cluster <- recode(total_df_2019$cluster, '4' = "Senang", '3' = "Bermasalah", '2' = "Sedang",'1'= 'Sedih')
total_df_2019


