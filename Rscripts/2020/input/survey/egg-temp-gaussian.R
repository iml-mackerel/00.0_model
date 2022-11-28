

# Fit Gaussian curve to mackerel egg density ~ temperature
library(tidyverse);library(nlstools);library(broom);library(nlstools);library(investr)


#' Geometric mean
#'
#' @param x your variable
#' @param na.rm  TRUE or FALSE
#'
#' @return a value
#' @export
#'
#' @examples gm_mean(data$variable); data %>% group_by(a, b, c, ...) %>% dplyr::summarise(g_mean = gm_mean(x, na.rm = TRUE))
gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

load("~/My Stocks/Mackerel/mackerel_sar_2021/mackerel_model_2021/mackerel_assessment_input/Rdata/mackerel_ichthyo.Rdata")

df <- mackerel_ichthyo %>%
    dplyr::select(year,doy,hour,station,`Scomber scombrus (oeuf stade 1)`,`Scomber scombrus (oeuf stade 2)`,`Scomber scombrus (oeuf stade 3)`,`Scomber scombrus (oeuf stade 4)`,`Scomber scombrus (oeuf stade 5)`,`Scomber scombrus (larva)`,N_m2,lat,lon,temperature,temp_daiss) 

# quick look at data
df %>% ggplot(aes(temperature, N_m2)) + geom_point()
df %>% ggplot(aes(temp_daiss, N_m2)) + geom_point()

# get regularly spaced temperature data and calculate geometric mean
df2 <- df %>% filter(!is.na(temperature),!is.na(N_m2))
df2 %<>% mutate(temp = lencat(temperature, w=0.1))
df2 <- df2 %>% group_by(temp) %>%
    dplyr::summarise(nm2 = gm_mean(N_m2,na.rm=T), NM2 = mean(N_m2,na.rm=T)) 

# Define x and y values
y <- df2$nm2
x <- df2$temp

# Gaussian function
# 
# f(x) = a * exp(-(x-b)^2 / 2 * c^2)
# where:
# a = the height of the curve's peak
# b = the position of the centre of the peak
# c = the standard deviation

# g(x) = 1/(√(2 π) σ) e^-((x - μ)^2/(2 σ^2))

# rearanged model
# y = b + a * exp(-0.5 * ((x - m)/s)^2) + error
# y = response variable
# x = independant variable
# m = peak 
# s > 0 quantifies the rate at which the curve tapers off
# a > 0 reflects the overall magnitudes of the relative y values 
# b = baseline

# By including the middle (m) among the parameters, the software will automatically output its estimate and a standard error for it:

f <- function(x, theta)  { 
    m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
    a*exp(-0.5*((x - m)/s)^2) + b
}

# Estimate some starting values.
m.0 <- x[which.max(y)]     # peak 
s.0 <- (max(x)-min(x))/4   # rate 
b.0 <- min(y)              # baseline
a.0 <- (max(y)-min(y))     # overall magnitudes of relative y values

# Fit the model
fit <- nls(log(y) ~ f(x, c(m, s, a, b)),
           data.frame(x, y),
           start = list(m = m.0, s = s.0, a = a.0, b = b.0))

# Get error stats on model as not available with nls by using MC simulation
newdata = data.frame(x = sort(unique(df2$temp)))
df_fit <- as.data.frame(predictNLS(fit,newdata=newdata))
df_fit %>% ggplot(aes(fit,mean))+geom_point()

# or for a specific value
predictNLS(fit, newdata = data.frame(x = 12.338))
# fit     mean sd median mad  2.5% 97.5%
# 3.973 24.58  0  24.58   0  24.58 24.58

# Display the estimated location of the peak and its SE.
summary(fit)
summary(fit)$parameters["m", 1:2] # 12.338
tidy(fit)
# term  estimate std.error statistic  p.value
#
# m         12.40   0.474    26.100  2.07e-73
# s         2.58    0.751     3.440  6.77e- 4
# a       127.00   24.300     5.210  3.98e- 7
# b        -5.00   24.400    -0.205  8.38e- 1

# sigma isConv     finTol logLik   AIC   BIC deviance df.residual  nobs

#  110. TRUE   0.00000481 -1558. 3126. 3144. 3027280.         251   255

# confidence intervals
confint(fit,"m")

plot(c(x,0),c(log(y),f(coef(fit)["m"],coef(fit))), type = "n",
     xlab = "temperature", ylab = "log density")
points(x,log(y), pch = 19)
curve(f(x, coef(fit)), add = TRUE, col = "Red", lwd = 2)
abline(v = 12.338, add = TRUE, col ="blue", lwd = 2 )
abline(v = 12.338+(0.136*1.96), add = TRUE, col ="blue", lwd = 1,lty=2 )
abline(v = 12.338-(0.136*1.96), add = TRUE, col ="blue", lwd = 1,lty=2 )
text(x = 5, y = 5, "Peak temperature = 12.338 =/- 0.136", col ="blue")

# plot the residuals
plot(x, resid(fit), main = "Residuals")

# custom function from model gaussian
fmean <- function(x) tidy(fit)$estimate[3]*exp(-0.5*((x - tidy(fit)$estimate[1])/tidy(fit)$estimate[2])^2) + tidy(fit)$estimate[4]

#  Figure
df2 %>% 
ggplot(aes(x = temp, y = log(nm2))) + geom_point() +
    stat_function(fun = fmean, col = 'red', size = 1.5) +
    scale_x_continuous(limits = c(0,25)) +
    scale_y_continuous() +
    labs(x = "Temperature", y = "log mean egg density (N*m-2)") +
    theme_minimal(base_size = 14) +
    geom_vline(xintercept  = 12.338) +
    geom_vline(xintercept = 12.338 + 2*0.136, linetype = "dashed") +
    geom_vline(xintercept = 12.338 - 2*0.136, linetype = "dashed") +
    annotate("text", x = 7, y = 5, label = "Peak = 12.34 C +/- 0.27")

# classic logistic
# f(x)  = L/(1+exp(-k*(x-x0)))
# f(x) = 1/(1+exp(-x)) = exp(x)/(exp(x)+1) = 0.5 + 0.5*tanh(x/2)
# The logistic function has the symmetry property that
# 1-f(x)=f(-x)
# thus x -> f(x) - 1/2 is an odd function

#  Richards: Y=A*(1+(B-1)*EXP(-C*(X-D)))^(1/(1-B)) 
# gsi logistic

a*exp(-0.5*((x - m)/s)^2) + b



logistic_2018 <- nls(gsi ~ y0 + (a/(1+(doy/x0)^b)),
                     data = mackerel_bio %>% dplyr::filter(year == 2018),
                     start = list(y0 = 0.4451, x0 = 170, a = 20, b = 60))

f.test <- function(a, b, m, s, x) a * exp(-0.5 * ((x - m) / s)^2) + b
#  Figure
df2 %>% 
    ggplot(aes(x = temp, y = log(nm2))) + geom_point() +
    stat_function(fun = fmean, col = 'red', size = 1.5) +
    scale_x_continuous(limits = c(0,25)) +
    scale_y_continuous() +
    labs(x = "Temperature", y = "log mean egg density (N*m-2)") +
    theme_minimal(base_size = 14) +
    geom_vline(xintercept  = 12.338) +
    geom_vline(xintercept = 12.338 + 2*0.136, linetype = "dashed") +
    geom_vline(xintercept = 12.338 - 2*0.136, linetype = "dashed") +
    annotate("text", x = 7, y = 5, label = "Peak = 12.34 C +/- 0.27")



# a quick look at logistic function params


a 14
b 24 
x0 175
y0 0.5

x = data.frame(x = seq(0,20,0.01))



gsi ~ y0 + (a/(1+(doy/x0)^b))

# Francois' 4 parameter logistic function
fg_logistic_fun <- function(a, b, c, d, x) d + (a/(1 + (x/c)^b))

# "classical" 4 parameter logistic function
classic_logistic_fun <- function(a, b, c, d, x) d + ((a - d)/(1 + (x/c)^b))

#  Figure comparing classical 4 param logistic function vs francois' ... just the max asymptote that changes...
base <- ggplot() + xlim(50, 250) + ylim(0, 40)
base + 
    geom_point(aes(mackerel_bio$doy, mackerel_bio$gsi, colour = mackerel_bio$division_group)) +
    scale_colour_viridis_d() +
    geom_function(fun = fg_logistic_fun, args = list(a = 14, b = 24, c = 175, d = 0.5), size = 2) +
       geom_function(fun = classic_logistic_fun, args = list(a = 14, b = 24, c = 175, d = 0.5), colour = "red", size = 2) 

# # instead of logistic try fitting a normal

# get regularly spaced temperature data and calculate geometric mean
df <- mackerel_bio %>% dplyr::filter(!is.na(doy),!is.na(gsi))

df %<>% mutate(doy = lencat(doy, w = 1))

df <- df %>% group_by(doy) %>%
    dplyr::summarise(gsi = gm_mean(gsi, na.rm = T))

# Define x and y values
y <- df$gsi
x <- df$doy


# By including the middle (m) among the parameters, the software will automatically output its estimate and a standard error for it:

f <- function(x, theta)  {
    m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
    a*exp(-0.5*((x - m)/s)^2) + b
}

# Estimate some starting values.
m.0 <- c(seq(0,30,1))    # peak
s.0 <- c(seq(0,10,1))    # rate
b.0 <- c(seq(-2,2,1))               # baseline
a.0 <- c(seq(0,10,1))      # overall magnitudes of relative y values

# Fit the model
fit <- nls(log(y) ~ f(x, c(m, s, a, b)),
           data.frame(x, y),
           start = list(m = m.0, s = s.0, a = a.0, b = b.0))

# Get error stats on model as not available with nls by using MC simulation
newdata = data.frame(x = sort(unique(df2$temp)))


r = df$gsi
x <- df$doy

f <- function(par)
{
    m <- par[1]
    sd <- par[2]
    k <- par[3]
    rhat <- k * exp(-0.5 * ((x - m)/sd)^2)
    sum((r - rhat)^2)
}

fit.out <- optim(c(15, 2, 1), f, method="BFGS", control=list(reltol=1e-9))



# First present the data in a data-frame
tab <- data.frame(x=x, r=r)
#Apply function nls
(res <- nls( r ~ k*exp(-1/2*(x-mu)^2/sigma^2), start=c(mu=15,sigma=5,k=1) , data = tab))

v <- summary(res)$parameters[,"Estimate"]
plot(r~x, data=tab)
plot(function(x) v[3]*exp(-1/2*(x-v[1])^2/v[2]^2),col=2,add=T,xlim=range(tab$x) )

# Display the estimated location of the peak and its SE.
summary(fit)
summary(fit)$parameters["m", 1:2] # 12.338
tidy(fit)


plot(c(x,0),c(log(y),f(coef(fit)["m"],coef(fit))), type = "n",
     xlab = "temperature", ylab = "log density")
points(x,log(y), pch = 19)
curve(f(x, coef(fit)), add = TRUE, col = "Red", lwd = 2)
abline(v = 12.338, add = TRUE, col ="blue", lwd = 2 )
abline(v = 12.338+(0.136*1.96), add = TRUE, col ="blue", lwd = 1,lty=2 )
abline(v = 12.338-(0.136*1.96), add = TRUE, col ="blue", lwd = 1,lty=2 )
text(x = 5, y = 5, "Peak temperature = 12.338 =/- 0.136", col ="blue")

# plot the residuals
plot(x, resid(fit), main = "Residuals")

# custom function from model gaussian
fmean <- function(x) tidy(fit)$estimate[3]*exp(-0.5*((x - tidy(fit)$estimate[1])/tidy(fit)$estimate[2])^2) + tidy(fit)$estimate[4]

#  Figure
df2 %>% 
    ggplot(aes(x = temp, y = log(nm2))) + geom_point() +
    stat_function(fun = fmean, col = 'red', size = 1.5) +
    scale_x_continuous(limits = c(0,25)) +
    scale_y_continuous() +
    labs(x = "Temperature", y = "log mean egg density (N*m-2)") +
    theme_minimal(base_size = 14) +
    geom_vline(xintercept  = 12.338) +
    geom_vline(xintercept = 12.338 + 2*0.136, linetype = "dashed") +
    geom_vline(xintercept = 12.338 - 2*0.136, linetype = "dashed") +
    annotate("text", x = 7, y = 5, label = "Peak = 12.34 C +/- 0.27")
