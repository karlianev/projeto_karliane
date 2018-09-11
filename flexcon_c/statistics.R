setwd("flexcon_c/resultados/")

file <- list.files(pattern="S_naive")
read <- read.csv(file[1])
y1 <- read$V1
read <- read.csv(file[2])
y2 <- read$V1
read <- read.csv(file[3])
y3 <- read$V1
read <- read.csv(file[4])
y4 <- read$V1


y <- c(y1, y2, y3, y4)

n <- rep(length(y1), (length(y) / length(y1)))

group <- rep(1:length(n), n)

tmp = tapply(y, group, stem)
tmp = tapply(z, grupos, stem)

tmpfn <- function(x){
  c(sum = sum(x), mean = mean(x), var = var(x), n = length(x))
}

tapply(y, group, tmpfn)

tmpfn(y)

data <- data.frame(y = y, group = factor(group))

fit <- lm(y~group, data)

data.aov <- aov(formula = y ~ group, data = data)

summary(data$y)

summary(fit)

summary.aov(fit)

boxplot(y1, y2, y3, y4)

plot(y1, type = "l")

lines(lowess(y1), col = 4)

var.test(y1, y4)
wilcox.test(y1, y4)
ks.test(y1, y4)
t.test(y1, y4)
t.test(y1, y4, var.equal=TRUE)

# z1 <- c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
# 
# z2 <- c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
# 
# z3 <- c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)
# 
# z <- c(z1, z2, z3)
# 
# num <- rep(length(z1), (length(z)/length(z1)))
# 
# grupos <- rep(1:length(num), num)
# 
# dados <- data.frame(z = z, grupos = factor(grupos))
# 
# ajuste <-lm(z ~ grupos, dados)
# 
# anova(ajuste)
