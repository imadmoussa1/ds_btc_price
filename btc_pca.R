coin_data <- read.csv("~/Desktop/btc_data_csv/price_study/imp_coin.csv")[-c(1,2,4,5,6)]

coin_data.pca <- prcomp(coin_data, center = TRUE, scale. = TRUE)
coin_data.pca

summary(coin_data.pca)
coin_data.pca$center
coin_data.pca$scale
coin_data.pca$sdev

coin_data.pca$rotation[,1:6]
coin_data.pca$x
coin_data.pca$sdev^2 / sum(coin_data.pca$sdev^2)
plot(coin_data.pca)

library("factoextra")
fviz_eig(us_health.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(us_health.pca)
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(coin_data.pca)

ggbiplot(coin_data.pca, circle=TRUE)



