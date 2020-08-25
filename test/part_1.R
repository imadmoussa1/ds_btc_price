# pca , Part 1 Data used from us health
us_health <- read.table("../data_final/Ushealth.txt", quote="\"", comment.char="")
# because pca work better with numeric value
us_health.pca <- prcomp(us_health[,c(2:14)], center = TRUE, scale. = TRUE)
us_health.pca

summary(us_health.pca)
# means input features (without any transformation).
us_health.pca$center

# standard deviations input features (without any transformation).
us_health.pca$scale

# standard deviation of projected points on PC1
# the standard deviation of projected point is in decreasing order
us_health.pca$sdev

# This defines the principal components axis. ( same as eigenvector)
us_health.pca$rotation[,1:6]
us_health.pca$x
us_health.pca$sdev^2 / sum(us_health.pca$sdev^2)
plot(us_health.pca)


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
ggbiplot(us_health.pca)

ggbiplot(us_health.pca, labels=us_health$V1, circle=TRUE)

ggbiplot(us_health.pca,ellipse=TRUE, labels=us_health$V1, groups=us_health$V13)
ggbiplot(us_health.pca,ellipse=TRUE,choices=c(3,4))
ggbiplot(us_health.pca,ellipse=TRUE,choices=c(3,4), labels=us_health$V1, groups=us_health$V13)

ggbiplot(us_health.pca,ellipse=TRUE,circle=TRUE, labels=us_health$V1, groups=us_health$V13)


us_health.pcaneg = us_health.pca
us_health.pcaneg$rotation = -us_health.pcaneg$rotation
us_health.pcaneg$x = -us_health.pcaneg$x
ggbiplot(us_health.pcaneg)
