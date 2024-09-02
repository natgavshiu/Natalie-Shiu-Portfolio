#[PREAMBLE]
library(xts)
library(readr)
library(viridis)
yc_raw <- read_csv("datasets/FED-SVENY.csv")

#[BOND YIELDS OVER TIME DATA VISUALISATION]
yc_all <- as.xts(yc_raw[,-1], order.by = yc_raw$Date)
yc_all_tail <- tail(yc_all[, c(1, 5, 10, 20, 30)])
yc_all_tail
#to observe the yield curve
yields <- yc_all
plot.type  <- "single"
plot.palette <- viridis(30)
asset.names <- colnames(yc_all)
plot.zoo(x = yields, plot.type = plot.type, col = plot.palette)
legend("topleft", legend = asset.names, col = plot.palette, cex = 0.45, lwd = 3)

#[DIFFERENTIATED BOND YIELDS OVER TIME DATA VISUALISATION]
ycc_all <- diff.xts(yc_all)
ycc_all_tail <- tail(ycc_all[, c(1, 5, 10, 20, 30)])
ycc_all_tail
#defining plot parameters
yield.changes <- ycc_all
plot.type <- "multiple"
plot.zoo(x = yield.changes, 
         plot.type = plot.type, 
         ylim = c(-1, 1), 
         cex.axis = 1, 
         ylab = 1:30, 
         main = "Differentiated Yield Changes Over Time", 
         col = plot.palette)

#[AUTOCORRELATION OF BOND YIELD CHANGES]
ycc <- window(ycc_all, start = as.Date("2000-01-01"))
x_1 <- ycc[, 1]   
x_20 <- ycc[, 20] 
par(mfrow=c(2,2))
acf_1 <- acf(x_1, main = "ACF of 1-Year Yield Changes")
acf_20 <- acf(x_20, main = "ACF of 20-Year Yield Changes")

#[AUTOCORRELATION OF ABOSLUTE BOND YIELD CHANGES] 
acf_abs_1 <- acf(abs(x_1), main = "ACF of Absolute 1-Year Yield Changes")
acf_abs_20 <- acf(abs(x_20), main = "ACF of Absolute 20-Year Yield Changes")

#[GARCH 1 YEAR MATURITY:YIELD CHANGES, VOLATILITIES AND STANDARDIZED RESIDUALS]
library(rugarch)
library(parallel)
#GARCH model with the skewed t-distribution
spec <- ugarchspec(distribution.model = "sstd") 
#fitting the model onto the data with the GARCH model with the skewed t-distribution 
fit_1 <- ugarchfit(x_1, spec = spec)
vol_1 <- sigma(fit_1) #volatilities
res_1 <- scale(residuals(fit_1, standardize = TRUE)) * sd(x_1) + mean(x_1) #rescaled residuals
merge_1 <- merge.xts(x_1, vol_1, res_1)
colnames(merge_1) <- c("Yield Changes", "Volatilities", "Standardized Residuals")
plot.zoo(merge_1, main = "GARCH Model Results with 1 Year Maturity: Yield Changes, Volatilities, and Standardized Residuals", cex.main = 1)

#[GARCH 20 YEAR MATURITY:YIELD CHANGES, VOLATILITIES AND STANDARDIZED RESIDUALS]
fit_20 <- ugarchfit(x_20, spec = spec)
vol_20 <- sigma(fit_20) #volatilities
res_20 <- scale(residuals(fit_20, standardize = TRUE)) * sd(x_20) + mean(x_20) #rescaled residuals
merge_20 <- merge.xts(x_20, vol_20, res_20)
colnames(merge_20) <- c("Yield Changes", "Volatilities", "Standardized Residuals")
plot.zoo(merge_20, main = "GARCH Model Results with 20 Year Maturity: Yield Changes, Volatilities, and Standardized Residuals", cex.main = 1)

#[DENSITY COMPARISON: BEFORE GARCH, AFTER GARCH AND NORMAL DISTRIBUTION]
density_x_1 <- density(x_1)
density_res_1 <- density(res_1)
plot(density_x_1, main = "Density Comparison: Before GARCH, After GARCH, and Normal Distribution", cex.main = 0.9)
lines(density_res_1, col = "red")
norm_dist <- dnorm(seq(-0.4, 0.4, by = .01), mean = mean(x_1), sd = sd(x_1))
lines(seq(-0.4, 0.4, by = .01), 
      norm_dist, 
      col = "green"
)
legend <- c("Before GARCH", "After GARCH", "Normal distribution")
legend("topleft", legend = legend, 
       col = c("black", "red", "green"), lty=c(1,1))
#[Q-Q PLOT ON DENSITY COMPARISON: BEFORE GARCH, AFTER GARCH AND NORMAL DISTRIBUTION]
data_orig <- x_1
data_res <- res_1
distribution <- qnorm
qqnorm(data_orig, ylim = c(-0.5, 0.5), main = "Q-Q Plot on Density Comparison: Before GARCH, After GARCH, and Normal Distribution", cex.main = 0.7)
qqline(data_orig, distribution = distribution, col = "black")
par(new=TRUE)
qqnorm(data_res * 0.623695122815242, col = "red", ylim = c(-0.5, 0.5), axes = FALSE, xlab = "", ylab = "", ann = FALSE)
qqline(data_res * 0.623695122815242, distribution = distribution, col = "black")
legend("topleft", c("Before GARCH", "After GARCH"), col = c("blue", "red"), pch=c(1,1))