#### Script used for creating figures of N Year Risk Prediction.docx



lr <- 0.13
thr <- 1.1
thr <- qnorm(1-lr)
thr

eplr1 <- 0.25
eplr2 <- 0.67

f1 <- 1-eplr1
f2 <- 1-eplr2

vX <- seq(-3.5,3.5,by=0.01)

# liability prob density prior
vY <- dnorm( vX )

vDistToThr <- abs(vX-thr)
vix <- which(min(vDistToThr)==vDistToThr)
probDensityYAtThr <- vY[vix]



# liability prob density conditional on right censored unaffected
vbx <- vX>=thr
# at earlier age
vY1 <- vY
vY1[vbx] <- f1*vY1[vbx]
# at later age
vY2 <- vY
vY2[vbx] <- f2*vY2[vbx]



par(mfcol=c(1,1))
vXlim <- c(-3,3)
# plot earlier
sMain = paste("Liability distribution of an unaffected singleton at earlier age (age 1)",
	"\nExpressed Proportion of Lifetime Risk =", eplr1,
	"\nLifetime Risk =",lr,", Liability threshold =", signif(thr,2) )
plot( x=vX, y=vY, type="n", xlab="liability",ylab="Probability Density", main=sMain, xlim=vXlim )
abline( h=0)

# plot prior
lines( x=vX, y=vY, lty="dashed")

# plot conditional
abline( v=thr)
# at earlier age
lines( x=vX[!vbx], y=vY1[!vbx] )
lines( x=vX[vbx], y=vY1[vbx] )

text( x=0,y=0.1,'a')
text( x=1.4,y=0.05,'b')
text( x=thr-0.05,y=f1*probDensityYAtThr,'f1', pos=2 )

# plot later
sMain = paste("Liability distribution for the unaffected at age 1 singleton cohort, n years later",
	"\nExpressed Proportion of Lifetime Risk =", eplr2,
	"\nLifetime Risk =",lr,", Liability threshold =", signif(thr,2) )
plot( x=vX, y=vY, type="n", xlab="liability",ylab="Probability Density", main=sMain )
abline( h=0)

# plot prior
lines( x=vX, y=vY, lty="dashed", col="grey" )
lines( x=vX, y=vY, lty="dashed")

# plot conditional
abline( v=thr)
# at earlier age
lines( x=vX[!vbx], y=vY2[!vbx] )
lines( x=vX[vbx], y=vY2[vbx] )
lines( x=vX[vbx], y=vY1[vbx] )

text( x=1.4,y=0.08,'c')
text( x=thr-0.05,y=f1*probDensityYAtThr,'f1', pos=2 )
text( x=thr-0.05,y=f2*probDensityYAtThr,'f2', pos=2 )
