# Questions:
# What initial data inspection should we do?
# Was our model determination process correct?
# What discussion would you like to see for leverage/cook outliers?


swings <-1:30
ht <- c(71,71,76,77,74,69,69,76,69,72,
        73,75,69,74,73,77,76,78,68,69,
        78,72,78,69,71,73,73,74,68,69)


wt <- c(180,195,205,190,178,196,166,187,189,219,
        195,178,175,164,224,166,202,163,169,134,
        166,183,212,176,211,183,174,171,141,163)


d <- c(196,209,220,229,211,164,157,227,158,177,
       214,248,141,232,194,234,251,242,138,145,
       182,174,183,153,173,162,168,200,143,130)

shaft <- c(rep('Gr',10),rep('S1',10),rep('S2',10))

# Make indicator variables for soil quality categories
sg <- ifelse(shaft=='Gr',1,0)  # for graphite shaft
s1 <- ifelse(shaft=='S1',1,0)  # for steel 1 shaft
s2 <- ifelse(shaft=='S2',1,0)  # for steel 2 shaft 

data.set <- data.frame(ht,wt,d,sg,s1,s2)
data.set

plot(d~ht,xlab="Height",ylab="Distance",type="n",
     main="Height vs Distance")
points(ht[s1==1],          d[s1==1],          pch="1")
points(ht[s2==1],          d[s2==1],          pch="2")
points(ht[s1==0 & s2==0],  d[s1==0 & s2==0],  pch="G")

plot(d~wt,xlab="Weight",ylab="Distance",type="n",
     main="Weight vs Distance")
points(wt[s1==1],          d[s1==1],          pch="1")
points(wt[s2==1],          d[s2==1],          pch="2")
points(wt[s1==0 & s2==0],  d[s1==0 & s2==0],  pch="G")

# plot(wt, log.d)
# plot(log.wt, log.d)
# plot(ht, d)
# plot(ht, log.d)
# plot(log.ht, log.d)


#Single Mean
model.1 <- lm(d ~ 1)
summary(model.1)
aov(model.1)

#SLR
model.2 <- lm(d ~ ht + wt)
summary(model.2)
aov(model.2)

#Multi Mean (ANOVA)
model.3 <- lm(d ~ s1 + s2)
summary(model.3)
aov(model.3)

#Quadratic
model.4 <- lm(d ~ ht + I(ht^2) + wt + I(wt^2))
summary(model.4)
aov(model.4)

#SLR common slope without interaction
model.5 <- lm(d ~ ht + wt + s1 + s2)
summary(model.5)
aov(model.5)

#Quadratic without interaction
model.6 <- lm(d ~ ht + I(ht^2) + wt + I(wt^2) + s1 + s2)
summary(model.6)
aov(model.6)

#SLR with interaction
model.7 <- lm(d ~ ht + ht*s1 + ht*s2 + wt + wt*s1 + wt*s2 + s1 + s2)
summary(model.7)
aov(model.7)

# Quadratic with interaction
model.8 <- lm(d ~ ht + ht*s1 + ht*s2 + I(ht^2) + I(ht^2)*s1 + I(ht^2)*s2 + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s1 + I(wt^2)*s2 + s1 + s2)
summary(model.8)
aov(model.8)

anova(model.1,model.2)        # Single mean vs SLR
anova(model.1,model.3)        # Single mean vs ANOVA

anova(model.2,model.4)        # SLR vs Quad
anova(model.2,model.5)       # SLR vs Common Slope

anova(model.3,model.5)        # ANOVA vs Common slope

anova(model.4, model.6)       # Quad vs Quad without interaction

anova(model.5,model.7)        # Common slope vs Separate slope
anova(model.5,model.6)        # Common slope vs Quad no interaction

anova(model.6, model.8)       # Quad without interaction vs Quad with interaction
anova(model.7, model.8)       # Separate Slope vs Quad with interaction


# Quadratic with interaction
f1 <- lm(d ~ ht + ht*s1 + ht*s2 + I(ht^2) + I(ht^2)*s1 + I(ht^2)*s2 + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s1 + I(wt^2)*s2 + s1 + s2)
summary(f1)
# plot(f1,which=c(1,2),add.smooth=F)
f2 <- lm(d ~ ht + ht*s1 + ht*s2 + I(ht^2) + I(ht^2)*s2 + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s1 + I(wt^2)*s2 + s1 + s2)
summary(f2)
anova(f2, f1)
f3 <- lm(d ~ ht + ht*s1 + ht*s2 + I(ht^2) + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s1 + I(wt^2)*s2 + s1 + s2)
anova(f3, f2)
summary(f3)
f4 <- lm(d ~ ht + ht*s1 + I(ht^2) + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s1 + I(wt^2)*s2 + s1 + s2)
anova(f4, f3)
summary(f4)
# little to no evidence of difference between f4 and f5
f5 <- lm(d ~ ht + ht*s1 + I(ht^2) + wt + wt*s1 + wt*s2 + I(wt^2) + I(wt^2)*s2 + s1 + s2)
anova(f5, f4)
summary(f5)
# no evidence of difference between f5 and f6
# BEST
f6 <- lm(d ~ ht + ht*s1 + I(ht^2) + wt + wt*s2 + I(wt^2) + I(wt^2)*s2 + s1 + s2)
anova(f6, f5)
summary(f6)
# MODERATE of difference between f6 and f7 so f6 IS BEST
f7 <- lm(d ~ ht + ht*s1 + I(ht^2) + wt + wt*s2 + I(wt^2) + s1 + s2)
anova(f7, f6)
summary(f7)

# log.f6 <- lm(d ~ log.ht + log.ht*s1 + I(log.ht^2) + log.wt + log.wt*s2 + I(log.wt^2) + I(log.wt^2)*s2 + s1 + s2)
# plot(log.f6,which=c(1,2),add.smooth=F)

plot(f6,which=c(1,2),add.smooth=F)
###############################################################################
############################# TEST RESULTS ####################################
# Generate CIs
library(nlme)
confint(f6)

# Confidence Level
CI.level <- 0.95

# (1) Extract parameter estimates
beta0 <- f6$coefficients[1]
beta1 <- f6$coefficients[2]
beta2 <- f6$coefficients[3]
beta3 <- f6$coefficients[4]
beta4 <- f6$coefficients[5]
beta5 <- f6$coefficients[6]
beta6 <- f6$coefficients[7]
beta7 <- f6$coefficients[8]
beta8 <- f6$coefficients[9]
beta9 <- f6$coefficients[10]
betavec <- c(beta0, beta1, beta2, beta3, beta4, beta5, beta6, beta7, beta8, beta9)

# (2) Create vector of linear combination of coefficients with hypothesized value
GraphComb <- c(1, 72, 0, 72^2, 190, 0, 190^2, 0, 0, 0); g0 = 0 #graphite
S1Comb <- c(1, 72, 1, 72^2, 190, 0, 190^2, 72, 0, 0); g0 = 0 #S1
S2Comb <- c(1, 72, 0, 72^2, 190, 1, 190^2, 0, 190, 190^2); g0 = 0 #S2

gdist <- sum(betavec*GraphComb)
gdist
s1dist <- sum(betavec*S1Comb)
s1dist
s2dist <- sum(betavec*S2Comb)
s2dist

LinearComb <- GraphComb - S1Comb 
# t-stat:  0.6983098 
# 0   0  -1   0   0   0   0 -72   0   0

#LinearComb <- GraphComb - S2Comb
# t-stat: 6.262764e-05
# 0      0      0      0      0     -1      0      0   -190 -36100

#LinearComb <- S1Comb - S2Comb 
# t-stat: 1.533610e-04
# 0      0      1      0      0     -1      0     72   -190 -36100


# (3) Calculate estimate and standard error
covmat <- vcov(f6)
est.g <- sum(betavec*LinearComb)
se.g <- sqrt(t(LinearComb)%*%covmat%*%(LinearComb))

# (4) Generate CI for linear combination
dfE <- f1$df.residual;
tstar <- qt(1-(1-CI.level)/2, dfE)
CI.LinearComb <- est.g + c(-1,1)*tstar*se.g

# Output results
LCsummary <- c(est.g, se.g, dfE, CI.LinearComb)
names(LCsummary) <- c("Estimate", "Std Error", "df", "(CI:Left", "CI:Right)")
LCsummary

# (5) Generate test statistic and p-values for hypothesis testing
t.g <- (est.g-g0)/se.g
pvalue_2 <- 2*(1-pt(abs(t.g), dfE))
pvalue_lt <- pt(t.g, dfE)
pvalue_gt <- 1-pt(t.g, dfE)
pvalues <- c(t.g, pvalue_2, pvalue_lt, pvalue_gt)
# output p-vaues for two sided, less than, and greater than
names(pvalues) <- c("t-statistic", "Two-sided", "Less Than", "Greater Than")
pvalues
LinearComb

################################################################################
#################### Model Check and Refinement ################################ 
# Make a leverage plot with threshold horizontal line
leverage.out <- lm.influence(f6)$hat
leverage.out
sort(leverage.out)
plot(leverage.out, type="h", ylim=c(0,1),main="Leverage Plot")
threshold <- 2*length(f6$coeff)/length(leverage.out)
abline(h=threshold,lwd=2,lty=2)

# Calculate and plot studentized residuals
cbind(rstandard(f6),rstudent(f6))
rstudent(f6)
plot(f6$fit,rstudent(f6), type="n", 
     main="Externally Studentized Residuals")
text(f6$fit,rstudent(f6), swings)
abline(h=-3,lwd=1.5,lty=2) 
abline(h=-2,lwd=1.5,lty=2)
abline(h= 0,lwd=1.5,lty=2)
abline(h= 2,lwd=1.5,lty=2)
abline(h= 3,lwd=1.5,lty=2)

# Make a plot of Cook's distances
cooks.out <- cooks.distance(f6)
cooks.out
sort(cooks.out)
plot(swings,cooks.distance(f6),  type="h", 
     main="Cook's Distances")
abline(h= 1,lwd=1.5,lty=2)  

plot(leverage.out, cooks.out, type = 'n', main="Cooks Distance vs Leverages")
text(leverage.out, cooks.out, swings)
abline(h=1, lty=2)
abline(v=threshold, lty=2)

# Potential Influencers:
# 13?, 20! cook and lev, 24? student resid, 29? leverage
ht <- ht[-c(20)]
length(ht)
wt <- wt[-c(20)]
d <- d[-c(20)]
shaft <- shaft[-c(20)]








# RSS <- c(37933.5, 14350.652, 30456.1, 11629.800, 8470.952, 5751.845, 4728.033, 2087.649)
# stripchart(RSS, method = "stack", pch = 18, main = "RSS Comparison across Models", xlab = "Model RSS")
# 
# log.RSS <- log(RSS)
# stripchart(log(RSS), method = "stack", pch = 18, main = "log(RSS) Comparison across Models", xlab = "Model log(RSS)")
# 
# # Make a leverage plot with threshold horizontal line
# leverage.out <- lm.influence(model.5)$hat
# leverage.out
# sort(leverage.out)
# plot(leverage.out, type="h", ylim=c(0,1),main="Leverage Plot")
# threshold <- 2*length(model.5$coeff)/length(leverage.out)
# abline(h=threshold,lwd=2,lty=2)
# 
# # Calculate and plot studentized residuals
# cbind(rstandard(model.5),rstudent(model.5))
# rstudent(model.5)
# plot(model.5$fit,rstudent(model.5), type="n", 
#      main="Externally Studentized Residuals")
# text(model.5$fit,rstudent(model.5), swings)
# abline(h=-3,lwd=1.5,lty=2) 
# abline(h=-2,lwd=1.5,lty=2)
# abline(h= 0,lwd=1.5,lty=2)
# abline(h= 2,lwd=1.5,lty=2)
# abline(h= 3,lwd=1.5,lty=2)
# 
# # Make a plot of Cook's distances
# cooks.out <- cooks.distance(model.5)
# cooks.out
# sort(cooks.out)
# plot(swings,cooks.distance(model.5),  type="h", 
#      main="Cook's Distances")
# abline(h= 1,lwd=1.5,lty=2)  
# 
# plot(leverage.out, cooks.out, type = 'n', main="Cooks Distance vs Leverages")
# text(leverage.out, cooks.out, swings)
# abline(h=1, lty=2)
# abline(v=threshold, lty=2)
# 
