library(discretization)

data(iris)
df <- cbind(iris[5], iris[5])
df <- cbind(df, iris)
print(df[1:5, ])


condition <- which(sapply(df, class) == "numeric")
df_disc <- df
for (i in condition) {
  df[i] <- mdlp(cbind(df[i], df[1]))$Disc.data[1]
}
print(df[1:5, ])

