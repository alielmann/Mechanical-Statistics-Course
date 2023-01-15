modulus <- read.csv(file = "C:/THT/Modulus Elasticity_13120177.csv", header = TRUE)
head(modulus)


boxplot(A)
t.test(A, mu=210, alt="less",conf=0.95)

B <- modulus$B;
boxplot(B)
t.test(B, mu=210, alt="less",conf=0.95)