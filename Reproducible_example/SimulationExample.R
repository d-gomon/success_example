install.packages("success_0.1.2.tar.gz", repos = NULL, type="source") 
library(success)
library(survival)
set.seed(01041996)


###########################Pre-requisites####################################

#Determine Risk-adjustment Cox and GLM model on full data set:
exprfit <- as.formula("Surv(survtime, censorid) ~ age + sex + BMI")
#50 day after surgery followup for Bernoulli CUSUM
exprfitglm <- as.formula("(survtime <= 50) & (censorid == 1)~ age + sex + BMI")
coxmod <- coxph(exprfit, data= surgerydat)
glmmod <- glm(exprfitglm, data = surgerydat, family = binomial(link = "logit"))

#############################################################################



#######################Type I error restriction###############################

############################Simulation parameters############################

#Number of in-control samples - increase for more accuracy (Article: 500)
n_sim_ic <- 30
#Number of out-of-control samples - increase for better validation (Article: 500)
n_sim_oc <- 30
#Study duration (in days)
time <- 365
#arrival rate (per day)
psi <- 1
#Expected hazard rate (theta) for Bernoulli and BK-CUSUM
theta <- log(2)
#Follow-up period (only for Bernoulli CUSUM)
followup <- 50
#Required Type I error when determining control limits
alpha <- 0.05

#############################################################################


#Find control limits restricting type I error to 0.05 over 1 year for BK-, 
#CGR- and Bernoulli CUSUM
bk_control <- bk_control_limit(time = time, alpha = alpha, psi = psi, 
                               n_sim = n_sim_ic, theta = theta, coxphmod = coxmod, 
                               baseline_data = surgerydat, pb = TRUE)

cgr_control <- cgr_control_limit(time = time, alpha = alpha, psi = psi, 
                                 n_sim = n_sim_ic, coxphmod = coxmod, 
                                 baseline_data = surgerydat, pb = TRUE)

bernoulli_control <- bernoulli_control_limit(time = time, alpha = alpha, 
                                             psi = psi, n_sim = n_sim_ic, 
                                             followup = followup, theta = theta,
                                             glmmod = glmmod, 
                                             baseline_data = surgerydat, 
                                             pb = TRUE)

#Generate out-of-control units with mu = log(2) (twice the failure rate)
#This means that BK- and Bernoulli CUSUM have perfect parameters!
newdat <- generate_units(time = 2*time, psi = psi, n_sim = n_sim_oc, 
                         coxphmod = coxmod, baseline_data = surgerydat, 
                         mu = theta)


#Determine control charts for each of the generated hospitals, 
#stop when control limit is surpassed.
charts <- vector(mode = "list", length = n_sim_oc)
for (i in 1:n_sim_oc){
  print(i)
  tdat <- subset(newdat, unit == i)
  charts[[i]]$bernoulli <- bernoulli_cusum(data = tdat, followup = followup,
                                           glmmod = glmmod, theta = theta,
                                           h = bernoulli_control$h)
  charts[[i]]$bk <- bk_cusum(data = tdat, theta = theta, coxphmod = coxmod, 
                             h = bk_control$h)
  charts[[i]]$cgr <- cgr_cusum(data = tdat, coxphmod = coxmod, 
                               h = cgr_control$h)
}


#Determine power over time for each of the charts:
powerber <- sapply(charts, FUN = function(x){
  runlength(x$bernoulli, h = bernoulli_control$h)})
powercgr <- sapply(charts, FUN = function(x){
  runlength(x$cgr, h = cgr_control$h)})
powerbk <- sapply(charts, FUN = function(x){
  runlength(x$bk, h = bk_control$h)})

#Determine the power of the charts in the first 100 days after the start of the study
power <- matrix(data = NA, nrow = 3, ncol = 100)
for(i in 1:ncol(power)){
  power[1,i] <- sum(powerber <= i)/n_sim_oc
  power[2,i] <- sum(powerbk <= i)/n_sim_oc
  power[3,i] <- sum(powercgr <= i)/n_sim_oc
}
rownames(power) <- c("Ber", "BK", "CGR")

#Make a simple plot displaying power over time for all 3 charts. 
#In this case, the BK-CUSUM does a bit better than the CGR-CUSUM, but with
#a control limit determined on a sample size of 30 and
#sample size of 30 out-of-control hospitals it's hard to draw conclusions.
#Increase n_sim_ic and n_sim_oc to get more reliable estimates! 
#Warning: increases computation time considerably.

plot(1:100, power[1,], col = "black", xlab = "Time(Days)", ylab = "Power",
     main = "Power of 3 charts over time", type = "l", ylim = c(0,1))
lines(1:100, power[2,], col = "blue")
lines(1:100, power[3,], col = "red")
legend("bottomright", legend=c("Ber", "BK", "CGR"),
       col=c("black", "blue", "red"), lty = 1)




###########################ARL restriction###################################

############################Simulation parameters############################

#Number of in-control samples - increase for more accuracy (Article: 500)
n_sim_ic <- 30
#Number of out-of-control samples - increase for better validation (Article: 500)
n_sim_oc <- 30
#arrival rate (per day)
psi <- 2500/1095
#Expected hazard rate (theta) for Bernoulli and BK-CUSUM
theta <- log(2)
#Follow-up period (only for Bernoulli CUSUM)
followup <- 50
#Required Type I error when determining control limits
alpha <- 0.05


#Desired in-control average run length (under the null hypothesis)
ARL_0 <- 180
#t_stoptime is the stopping time for calculating charts to reduce computation time.
#In this case, 500 suits our needs
#The value you should choose depends on the failure rate and desired ARL_0
t_stoptime <- 500
#Study time in which patients arrivals happen (must be \geq t_stoptime)
time <- 1000

#Change t_stoptime to approx 17*365 to restrict ARL to 15 years (15*365)
#We want t_stoptime to be as small as possible (reduce computation time),
#while at the same time we want each in-control chart to hit the control limit.
#Strategy: determine a few in-control charts until 17*365 and memorize their values. 
#Then you can stop all charts around those values by specifying 
#h = mean(values) + margin (about 0.5). This will greatly reduce computation time.
#This works, because Run length of the charts is a non-decreasing function 
#of the control limit. 

#############################################################################

#Generate n_sim_ic = 30 in-control hospitals with arrivals until time = 1000 and 
#approx psi = 2500/1095 = 2.28 arrivals per day with
#exponential (lambda = 0.002) failure rate
ic_hospitals <- generate_units(time = time, psi = psi, n_sim = n_sim_ic, 
                               inv_cbaseh = function(t) inv_chaz_exp(t, lambda = 0.002))

#Determine in-control charts
ic_charts <- vector(mode = "list", length = n_sim_ic)
for (i in 1:n_sim_ic){
  print(i)
  tdat <- subset(ic_hospitals, unit == i)
  ic_charts[[i]]$bk <- bk_cusum(data = tdat, theta = theta, cbaseh = function(t) chaz_exp(t, lambda = 0.002),
                                stoptime = t_stoptime, pb = TRUE)
  ic_charts[[i]]$cgr <- cgr_cusum(data = tdat, cbaseh = function(t) chaz_exp(t, lambda = 0.002), 
                                  stoptime = t_stoptime, pb = TRUE)
}

hosps <- unique(ic_hospitals$unit)
hseq <- seq(2, 20, 0.01)
ARL_bk <- vector(mode = "numeric", length = length(hseq))
ARL_cgr <- vector(mode = "numeric", length = length(hseq))
for(i in seq_along(hseq)){
  ARL_bk[i] <- mean(sapply(ic_charts, FUN = function(x) runlength(x$bk, h = hseq[i])))
  ARL_cgr[i] <- mean(sapply(ic_charts, FUN = function(x) runlength(x$cgr, h = hseq[i])))
}

id_bk <- which.min(abs(ARL_bk - ARL_0))
id_cgr <- which.min(abs(ARL_cgr - ARL_0))


#Determine control limits to use for BK and CGR-CUSUM
h_bk <- hseq[id_bk]
h_cgr <- hseq[id_cgr]



#Generate out-of-control charts with exp(mu) = 2
#This procedure can be repeated for different value of mu in correspondence to the article

oc_hospitals_log2 <- generate_units(time = 500, psi = 2500/1095, n_sim = 100, 
                               inv_cbaseh = function(t) inv_chaz_exp(t, lambda = 0.002),
                               mu = log(2))


oc_charts_log2 <- vector(mode = "list", length = 100)
for (i in 1:100){
  print(i)
  tdat <- subset(oc_hospitals_log2, unit == i)
  oc_charts_log2[[i]]$bk <- bk_cusum(data = tdat, theta = log(2), cbaseh = function(t) chaz_exp(t, lambda = 0.002),
                                stoptime = t_stoptime, pb = TRUE, h = h_bk)
  oc_charts_log2[[i]]$cgr <- cgr_cusum(data = tdat, cbaseh = function(t) chaz_exp(t, lambda = 0.002), 
                                  stoptime = t_stoptime, pb = TRUE, h = h_cgr)
}


ARL_log2 <- c(mean(sapply(oc_charts_log2, FUN = function(x) runlength(x$bk, h = h_bk))), 
              mean(sapply(oc_charts_log2, FUN = function(x) runlength(x$cgr, h = h_cgr))))
names(ARL_log2) <- c("bk", "cgr")

ARL_log2


#So CGR has smaller oc ARL with same ic ARL















