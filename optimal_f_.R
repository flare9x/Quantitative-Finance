# Optimal F Simulation In R 
# Make random returns 
# Adjust $ return output by varying fractional (f)
#######################################################################
# Calculations 
# Calculation 1 = Optimal F calculation Start calculation = 1 + fractional amount * trade gain/loss (-/+) / abs(worst.loss)
# Calculation 2 = Optimal F Calculation = prev equity * (1+fractional (f) amount * trade gain/loss (-/+) / abs(worst.loss)
#####################################################################

# Required Packages 
require(reshape2)
require(PerformanceAnalytics)

# Set (f) values
f <- c(0,0.05,	0.1,	0.15,	0.2,	0.25,	0.3,	0.35,	0.4	,0.45,	0.5	,0.55,	0.6	,0.65,	0.7,	0.75,	0.8,	0.85,	0.9	,0.95	,1)

# Create emptry matrix's for each (f) iteration
f.0.00 <- matrix()
f.0.05 <- matrix()
f.0.10 <- matrix()
f.0.15 <- matrix()
f.0.05 <- matrix()
f.0.20 <- matrix()
f.0.25 <- matrix()
f.0.30 <- matrix()
f.0.35 <- matrix()
f.0.40 <- matrix()
f.0.45 <- matrix()
f.0.50 <- matrix()
f.0.55 <- matrix()
f.0.60 <- matrix()
f.0.65 <- matrix()
f.0.70 <- matrix()
f.0.75 <- matrix()
f.0.80 <- matrix()
f.0.85 <- matrix()
f.0.90 <- matrix()
f.0.95 <- matrix()
f.1 <- matrix()

# Create random $ amount gains and losses
random.rets <- ifelse(runif(252)<0.40,-1,1)*round(252*runif(252),2)

# Sample and replace original return stream (note this may be real life gains and losses from a strategy)
# One may wish to enter actual trade results here
# and sample with or without replacement
rets <- sample(random.rets,replace=TRUE) # Remove repace=TRUE for without replacement
rets <- random.rets
i=2# Start on second location (calculation 1 is fixed)
for (i in 2:length(rets)) { 
  f.0.00[1] = (1+0.00 * rets[1] / abs(min(rets)))
  f.0.00[i] = f.0.00[i-1] * ((1+0.00* rets[i] / abs(min(rets))))
f.0.05[1] = (1+0.05 * rets[1] / abs(min(rets)))
f.0.05[i] = f.0.05[i-1] * ((1+0.05* rets[i] / abs(min(rets))))
f.0.10[1] = (1+0.10 * rets[1] / abs(min(rets)))
f.0.10[i] = f.0.10[i-1] * ((1+0.10* rets[i] / abs(min(rets))))
f.0.15[1] = (1+0.15 * rets[1] / abs(min(rets)))
f.0.15[i] = f.0.15[i-1] * ((1+0.15* rets[i] / abs(min(rets))))
f.0.20[1] = (1+0.20 * rets[1] / abs(min(rets)))
f.0.20[i] = f.0.20[i-1] * ((1+0.20* rets[i] / abs(min(rets))))
f.0.25[1] = (1+0.25 * rets[1] / abs(min(rets)))
f.0.25[i] = f.0.25[i-1] * ((1+0.25* rets[i] / abs(min(rets))))
f.0.30[1] = (1+0.30 * rets[1] / abs(min(rets)))
f.0.30[i] = f.0.30[i-1] * ((1+0.30* rets[i] / abs(min(rets))))
f.0.35[1] = (1+0.35 * rets[1] / abs(min(rets)))
f.0.35[i] = f.0.35[i-1] * ((1+0.35* rets[i] / abs(min(rets))))
f.0.40[1] = (1+0.40 * rets[1] / abs(min(rets)))
f.0.40[i] = f.0.40[i-1] * ((1+0.40* rets[i] / abs(min(rets))))
f.0.45[1] = (1+0.45 * rets[1] / abs(min(rets)))
f.0.45[i] = f.0.45[i-1] * ((1+0.45* rets[i] / abs(min(rets))))
f.0.50[1] = (1+0.50 * rets[1] / abs(min(rets)))
f.0.50[i] = f.0.50[i-1] * ((1+0.50* rets[i] / abs(min(rets))))
f.0.55[1] = (1+0.55 * rets[1] / abs(min(rets)))
f.0.55[i] = f.0.55[i-1] * ((1+0.55* rets[i] / abs(min(rets))))
f.0.60[1] = (1+0.60 * rets[1] / abs(min(rets)))
f.0.60[i] = f.0.60[i-1] * ((1+0.60* rets[i] / abs(min(rets))))
f.0.65[1] = (1+0.65 * rets[1] / abs(min(rets)))
f.0.65[i] = f.0.65[i-1] * ((1+0.65* rets[i] / abs(min(rets))))
f.0.70[1] = (1+0.70 * rets[1] / abs(min(rets)))
f.0.70[i] = f.0.70[i-1] * ((1+0.70* rets[i] / abs(min(rets))))
f.0.75[1] = (1+0.75 * rets[1] / abs(min(rets)))
f.0.75[i] = f.0.75[i-1] * ((1+0.75* rets[i] / abs(min(rets))))
f.0.80[1] = (1+0.80 * rets[1] / abs(min(rets)))
f.0.80[i] = f.0.80[i-1] * ((1+0.80* rets[i] / abs(min(rets))))
f.0.85[1] = (1+0.85 * rets[1] / abs(min(rets)))
f.0.85[i] = f.0.85[i-1] * ((1+0.85* rets[i] / abs(min(rets))))
f.0.90[1] = (1+0.90 * rets[1] / abs(min(rets)))
f.0.90[i] = f.0.90[i-1] * ((1+0.90* rets[i] / abs(min(rets))))
f.0.95[1] = (1+0.95 * rets[1] / abs(min(rets)))
f.0.95[i] = f.0.95[i-1] * ((1+0.95* rets[i] / abs(min(rets))))
f.1[1] = (1+1 * rets[1] / abs(min(rets)))
f.1[i] = f.1[i-1] * ((1+1* rets[i] / abs(min(rets))))
}

# Output
out <- cbind(f.0.00,f.0.05,f.0.10,f.0.15,f.0.20,f.0.25,f.0.30,f.0.35,f.0.40,f.0.45,f.0.50,f.0.55,f.0.60,f.0.65,f.0.70,f.0.75,f.0.80,f.0.85,f.0.90,f.0.95,f.1)
cnames <- f
colnames(out) <- cnames

######################################################
# Find cumulative return for any given fractional (f)
#######################################################
cr.list <- list()
i=1
for (i in 1:21) { 
  cr.rets <- ROC(out[,i],type = "discrete",na.pad=TRUE)
  cr.list[[i]] <- rbind(as.numeric(as.data.frame(Return.cumulative(xts(cr.rets, Sys.Date()-length(cr.rets):1)))))
}

# cbind output 
cr_out <- do.call(cbind,cr.list)
cnames <- f
colnames(cr_out) <- cnames
# Plot cumulative return 
cr_plot <- melt(cr_out)
cr_plot <- setDT(cr_plot, keep.rownames=TRUE)
cr_plot <- data.frame(cr_plot)
plot(cr_plot$Var2,cr_plot$value,type="o",col="red",ann=FALSE,xaxt="n")
title("Cumulative Return Vs Fractional (f)",xlab="Fractional (f)", ylab="Cumulative Return (%)")
axis(side = 1, at = f, las=2)

# Plot for final $ equity
#plot.df <- out[nrow(out),] # Extract last row of simulation
#plots <- melt(plot.df)
#plots <- setDT(plots, keep.rownames=TRUE)
#plots <- data.frame(plots)
# Plot
#plot(plots$rn,plots$value,type="o",col="red", ann=FALSE)
#title("Final Equity Vs Fractional (f)",xlab="Fractional (f)", ylab="Final Equity ($)")

################################################################################
# Find maximum drawdown for any given fractioanl (f)
################################################################################
dd <- data.frame(out)
cnames <- f
colnames(dd) <- cnames

# loop to pull max dd 
dd.list <- list()
i=1
for (i in 1:length(dd)) { 
 dd.rets <- ROC(dd[,i],type = "discrete",na.pad=TRUE)
  dd.list[[i]] <- rbind(as.numeric(as.data.frame(maxDrawdown(xts(dd.rets, Sys.Date()-length(dd.rets):1)))))
}

# Validation
#rets <- ROC(dd[,2],type = "discrete",na.pad=TRUE)
#maxDrawdown(xts(rets, Sys.Date()-length(rets):1))
#chart.Drawdown(xts(rets$rets, Sys.Date()-nrow(rets):1), legend.loc = NULL, colorset = (1:12))

# cbind output 
dd_out <- do.call(cbind,dd.list)
cnames <- f
colnames(dd_out) <- cnames

# Plot DD vs fractional (f)
  dd.plot <- melt(dd_out)
# Plot
plot(dd.plot$Var2,dd.plot$value,type="o",col="red", ann=FALSE,xaxt="n")
title("Maximum Drawdown Vs Fractional (f)",xlab="Fractional (f)", ylab="Maximum Draw Down (%)")
axis(side = 1, at = f, las=2)

# Combine Plots 
par(mfrow=c(2,2))
# Cumulative Return
plot(cr_plot$Var2,cr_plot$value,type="o",col="red", ann=FALSE, xaxt="n")
title("Cumulative Return Vs Fractional (f)",xlab="Fractional (f)", ylab="Cumulative Return (%)")
axis(side = 1, at = f, las=2)
# Maximum Draw down
plot(dd.plot$Var2,dd.plot$value,type="o",col="red", ann=FALSE,xaxt="n")
title("Maximum Drawdown Vs Fractional (f)",xlab="Fractional (f)", ylab="Maximum Draw Down (%)")
axis(side = 1, at = f, las=2)


# Find win / loss % of sample
pw <- sum(rets >0) / length(rets) # win
pl <- sum(rets <0)/ length(rets)  # loss
pw
pl


###############################################################
# Find Risk of ruin for any given fractional (f)
###############################################################
# Calculate daily returns
rets.list <- list()
mean.win.list <- list()
mean.loss.list <- list()
sd.ret.list <- list()
i=1
for (i in 1:21) { 
  rets.rets <- ROC(out[,i],type = "discrete",na.pad=TRUE)
  rets.list[[i]] <- rets.rets
  mean.win.list[[i]] <- mean(rets.rets[rets.rets>0],na.rm=TRUE)
  mean.loss.list[[i]] <- mean(rets.rets[rets.rets<0],na.rm=TRUE)
    sd.ret.list[[i]] <- sd(rets.rets,na.rm=TRUE)
}

# cbind output 
rets_out <- do.call(cbind,rets.list)
cnames <- f
colnames(rets_out) <- cnames

mean_win_out <- do.call(cbind,mean.win.list)
cnames <- f
colnames(mean_win_out) <- cnames

mean_loss_out <- do.call(cbind,mean.loss.list)
cnames <- f
colnames(mean_loss_out) <- cnames

sd_out <- do.call(cbind,mean.ret.list)
cnames <- f
colnames(sd_out) <- cnames

# Fixed fractional risk of ruin
# Ralph Vince 1990 (Portfolio Management Formulas : Mathematical Trading Methods for the Futures, Options, and Stock Markets)
risk_ruin.list <- list()
  i=1
q=2
g= 0.3   # Set maximum % loss willing to accept
for (i in 1:21) { 
  aw  = mean_win_out[,i]
  al  = mean_loss_out[,i]
  pw = pw
  p = .5 * (1+(z/a))
  z = (abs((aw/q)*pw)) - (abs(al/q)*(1-pw))
  a = ((pw * (aw/q)^2) + ((1-pw) * (al/q)^2)) ^(1/2)
  u = g/a
  risk_ruin.list[[i]] =  ((1-p)/p)^u *100
  }

# cbind output 
risk_ruin_out <- do.call(cbind,risk_ruin.list)
cnames <- f
colnames(risk_ruin_out) <- cnames

# Plot risk of ruin vs fractional (f)
rr.plot <- melt(risk_ruin_out)
rr.plot <- replace(rr.plot, is.na(rr.plot), 0)
#options(scipen = 0)
# Plot
plot(rr.plot$Var2,rr.plot$value,type="o",col="red", ann=FALSE,xaxt="n")
title("Risk Of Ruin Vs Fractional (f)",xlab="Fractional (f)", ylab="Risk of Ruin (%)")
axis(side = 1, at = f, las=2)

########################################################################################
# Find sharpe ratio for any given fractional (f)
########################################################################################
sr.list <- list()
i=1
for (i in 1:21) { 
  sr.rets <- ROC(out[,i],type = "discrete",na.pad=TRUE)
  sr.list[[i]] <- rbind(as.numeric(as.data.frame(SharpeRatio.annualized(xts(sr.rets, Sys.Date()-length(sr.rets):1)))))
}

# cbind output 
sr_out <- do.call(cbind,sr.list)
cnames <- f
colnames(sr_out) <- cnames
# Plot cumulative return 
sr_plot <- melt(sr_out)
sr_plot <- setDT(sr_plot, keep.rownames=TRUE)
sr_plot <- data.frame(sr_plot)
plot(sr_plot$Var2,sr_plot$value,type="o",col="red",ann=FALSE,xaxt="n")
title("Sharpe Ratio Vs Fractional (f)",xlab="Fractional (f)", ylab="Sharpe Ratio")
axis(side = 1, at = f, las=2)

############################################################
# Find optimal F 
###########################################################
find.max <- which(cr_plot$value==max(cr_plot$value))
optimal.f <- cr_plot$Var2[find.max]
cat("Optimal f = ", optimal.f)

########################################################################################################
# Highlight code and ctrl+r to run many iterations
