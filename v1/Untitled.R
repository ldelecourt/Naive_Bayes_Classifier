library(discretization)
library(dplyr)

data(iris)
df <- cbind(iris[5], iris[5])
df <- cbind(df, iris)
colnames(df) <- c("Species1", "Species2", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species3")
print(df[1:5, ])

# condition <- which(sapply(df, class) == "numeric")
# df_disc <- df
# for (i in condition) {
#   df[i] <- mdlp(cbind(df[i], df[1]))$Disc.data[1]
# }
# print(df[1:5, ])

condition <- which(sapply(df, class) == "numeric")
df <- select(df, which(sapply(df, class) == "numeric"), names(df[1]))
print(mdlp(df))