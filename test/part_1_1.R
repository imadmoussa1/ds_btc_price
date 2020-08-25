# pca , Part 1, Data used demo_fr
demo_fr <- read.csv("../data_final/DemoFR.txt", sep="")

# because pca work better with numeric value we will exlcude the categorical varibale
demo_fr.pca <- prcomp(demo_fr[,c(4:18)], center = TRUE, scale = TRUE)

# means input features (without any transformation).
demo_fr.pca$center

# standard deviations input features (without any transformation).
demo_fr.pca$scale

# standard deviation of projected points on PC1
# the standard deviation of projected point is in decreasing order
demo_fr.pca$sdev

# This defines the principal components axis. ( same as eigenvector)

demo_fr.pca$rotation
demo_fr.pca$x
demo_fr.pca$sdev^2 / sum(demo_fr.pca$sdev^2)
plot(demo_fr.pca)

summary(demo_fr.pca)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(demo_fr.pca)

ggbiplot(demo_fr.pca, labels=rownames(demo_fr))

ggbiplot(demo_fr.pca,ellipse=TRUE,  labels=rownames(demo_fr), groups=demo_fr$region)

ggbiplot(demo_fr.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(demo_fr), groups=demo_fr$region)

ggbiplot(demo_fr.pca,ellipse=TRUE,circle=TRUE, labels=rownames(demo_fr), groups=demo_fr$region)

