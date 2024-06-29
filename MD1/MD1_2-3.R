set.seed(123)      
n <- 100          
a <- 2             
b <- 3            
num_sim <- 1000 
b_star_res <- numeric(num_sim)
for (i in 1:num_sim) {
  x <- runif(n, 0, 10)   
  e <- rnorm(n, 0, 2)  
  y <- a + b * x + e
  b_star <- sum(y) / sum(x)
  b_star_res[i] <- b_star
}
mean_b_star <- mean(b_star_res)
cat("Mean(b*) = ", mean_b_star, "\n")

var_b_star <- var(b_star_res)
cat("Variance(b*) = ", var_b_star, "\n")




#3uzd
prc <- c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0.95, 0.95, 0.95, 0.85, 0.85, 0.85)
qty <- c(89, 86, 74, 79, 68, 84, 139, 122, 102, 186, 179, 187)
model <- lm(qty ~ prc)
plot(prc, qty, main="1")
abline(model)
plot(predict(model), residuals(model), main="2")
abline(h=0, col="red")
summary(model)

#c
# SST
y_i <- mean(qty)
SST <- sum((qty - y_i)^2)

# SSE
y_ii <- predict(model)
SSE <- sum((qty - y_ii)^2)

# SSR
SSR <- SST - SSE

# R^2
R2 <- SSR / SST

list(SST = SST, SSE = SSE, SSR = SSR, R2 = R2)

#d
n <- length(qty)
p <- 1
sigma2 <- SSE / (n - p - 1)
sigma2

#e
model_summary <- summary(model)
p_value <- model_summary$coefficients["prc", "Pr(>|t|)"]
p_value

#f
se_beta <- summary(model)$coefficients["prc", "Std. Error"]
beta <- coef(model)["prc"]
t_crit <- qt(0.975, df = n - p - 1)

low <- beta - t_crit * se_beta
up <- beta + t_crit * se_beta
 
