library(relsurv)
data(colrec)
summary(colrec)
# Let us first compare all estimators in the whole population of rectum cancer patients
sub.rec <- subset(colrec, site == "rectum")
# Censor all patients at 10 years of follow-up
ind_10plus <- which(sub.rec$time > 365.241 * 10)
sub.rec$time[ind_10plus] <- 365.241 * 10
sub.rec$stat[ind_10plus] <- 0
# Compute the relative survival estimates using the different estimators
rs_e1 <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "ederer1",
  ratetable = slopop,
  data = sub.rec)
rs_e2 <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "ederer2",
  ratetable = slopop,
  data = sub.rec)
rs_hak <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "hakulinen",
  ratetable = slopop,
  data = sub.rec)
rs_pp <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "pohar-perme",
  ratetable = slopop,
  data = sub.rec)
# Plot four panels with a shared legend (Okabe-Ito palette)
par(mfrow = c(2, 2))

plot(rs_e1, xscale = 365.241,
     xlab = "Follow-up time in years",
     ylab = "Rel. surv. ratio",
     main = "Ederer I",
     ylim = c(0, 1),
     lwd = 1.5)
par(xpd = FALSE)
grid(col = "black")
plot(rs_e2, xscale = 365.241,
     xlab = "Follow-up time in years",
     ylab = "Obs. net surv.",
     main = "Ederer II",
     ylim = c(0, 1),
     lwd = 1.5)
par(xpd = FALSE)
grid(col = "black")
plot(rs_hak, xscale = 365.241,
     xlab = "Follow-up time in years",
     ylab = "Rel. surv. ratio",
     main = "Hakulinen",
     ylim = c(0, 1),
     lwd = 1.5)
par(xpd = FALSE)
grid(col = "black")
plot(rs_pp, xscale = 365.241,
     xlab = "Follow-up time in years",
     ylab = "Net surv.",
     main = "Pohar-Perme",
     ylim = c(0, 1),
     lwd = 1.5)
par(xpd = FALSE)
grid(col = "darkgray")

# Let us consider the subset of rectum cancer patients diagnosed in 1994 and 2000
sub.rec.94 <- subset(sub.rec,diag >= as.Date("1994-01-01") & diag < as.Date("1995-01-01"))
sub.rec.00 <- subset(sub.rec,diag >= as.Date("2000-01-01") & diag < as.Date("2001-01-01"))

# Relative survival ratio comparison for patients diagnosed in 1994 and 2000 using Ederer I estimator
rs_e1.94 <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "ederer1",
  ratetable = slopop,
  data = sub.rec.94)

rs_e1.00 <- rs.surv(Surv(time,stat)~1,
  rmap = list(age = age, sex = sex, year = diag),
  method = "ederer1",
  ratetable = slopop,
  data = sub.rec.00)

# Plot the two curves in the same panel
par(mfrow = c(1, 1))
plot(rs_e1.94, xscale = 365.241,
      xlab = "Follow-up time in years",
      ylab = "Rel. surv. ratio",
      ylim = c(0, 1),
      lwd = 1.5,
      lty = 1,
      conf.int = FALSE)
lines(rs_e1.00, xscale = 365.241, lwd = 1.5, lty = 2, conf.int = FALSE)
legend("topright", 
      legend = c("Patients diagnosed in 1994", "Patients diagnosed in 2000"),
      col = "black", lty = c(1, 2), lwd = 1.5, bty = "n")
grid(col = "darkgray")
# Kaplan-Meier estimates versus net survival estimates
os_km <- survfit(Surv(time,stat)~1, data = sub.rec)
# Plot the two curves in the same panel
par(mfrow = c(1, 1))
plot(rs_pp, xscale = 365.241,
      xlab = "Follow-up time in years",
      ylab = "Survival probability",
      ylim = c(0, 1),
      lwd = 1.5,
      lty = 1,
      conf.int = FALSE)
lines(os_km, xscale = 365.241, lwd = 1.5, lty = 2, conf.int = FALSE)
legend("topright", 
      legend = c("Net survival with Pohar-Perme", "Overall survival with Kaplan-Meier"),
      col = "black", lty = c(1, 2), lwd = 1.5, bty = "n")
grid(col = "darkgray")



