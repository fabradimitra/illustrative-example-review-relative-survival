library(relsurv)
data(colrec)
summary(colrec)
# Let us first compare all estimators in the whole population of rectum cancer patients
sub.rec <- subset(colrec, site == "rectum")
rs_e1 <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = ))

# Let us consider the subset of rectum cancer patients diagnosed in 1994 and 2000
sub.rec.94 <- subset(colrec, site == "rectum" & diag >= as.Date("1994-01-01") & diag < as.Date("1995-01-01"))
sub.rec.00 <- subset(colrec, site == "rectum" & diag >= as.Date("2000-01-01") & diag < as.Date("2001-01-01"))

rs_e2 <- rs.surv(Surv(follow.up.time.in.days,
                      Follow.up.status)~1,
                 rmap = list(age = Age.at.diagnosis*365.241,
                             sex = Sex, year = as.Date(paste(Year.of.diagnosis,
                                                             Month.of.diagnosis,
                                                             Day.of.Diagnosis,sep = "-"))), 
                 method = "ederer2",
                 ratetable = slopop,
                 data = res$data)

par(mfrow = c(4,4), oma=c(2,0,2,0), mar=c(1, 4, 4, 1) + 0.1)
plot(rs_e2, xscale=365.241, ylim=c(0,1), xlim = c(0,20*365.241))
grid(col = "black")
mtext("Period of diagnosis", line=0, side=3, outer=TRUE, cex=1,font=2)
title("1993-1997", ylab = "Age 0-54",
      font.lab=2, 
      cex.main = 1.2,
      cex.lab = 1.2)
summary(slopop)
sloop sloop 