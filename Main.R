# In this document, we analyze the data from HoeGekisNL with the
# Extended TSO model. 

# As we are not authorized to share the data, only section 5 can be rerun with the files available
# in the git repository.

# Index:
# 0. Prepare R environment 
# 1. Load and prepare data
# 2. Descriptive statistics
# 3. Stationarity checks
# 4. Export data to Mplus
# 5. Import Mplus results of selected analysis

# 0. Prepare R environment----
# Load required packages and functions.
#remotes::install_github("wviechtb/esmpack", force = TRUE) # Install package (in development) for managing esm data 
library(esmpack)
library(foreign)                            
library(MplusAutomation)
library(car)
library(xtable)
source("R/esmfunctions.R")

# 1. Load and prepare data----

# Load data files:
# Here, we load a data file that contains the diary data.
HGIN_Diary <- read.spss("HGIN/201210_HND117_AlvarezData191219_Diary.sav", 
                        use.value.label=TRUE, 
                        to.data.frame=TRUE)
# Here, we load a data file that contains the data for the variable optimism, which was
# measured during the cross-sectional study of HoeGekisNL.
HGIN_Cross <- read.csv("HGIN/optimisme.csv", sep = ";", header = TRUE)

# Compute sum score of Optimism
HGIN_Cross$Optimism <- combitems(4:13, HGIN_Cross, fun = sum, na.rm = FALSE)

# Here, we merge the sum scores of optimism with the diary data.
HGIN_Full <- merge.esm("Optimism", id = c(2, 3), esmdata = HGIN_Diary, crossdata = HGIN_Cross)

# The daily diary study of HoeGekisNL (HGIN) was developed to study the mental health of the 
# Dutch population. Participants signed up voluntarily on the website. After this, via a mobile
# app, participants had to fill in the questionnaire three times a day during 30 days (maximum
# 90 measurements). In some cases, the daily diary study was completed multiple times by the 
# same individuals. This information is in the variables Real_period and meting. To get the 
# first valid diary data, we have to filter the data for when Real_period = 1 and meting = 1.
# Additionally, some individuals filled the daily diary for 31 days (93 measurements in total).
# We will only consider the first 90 measurements of each individual.

HGIN_FirstD <- subset(HGIN_Full, 
                      HGIN_Full$Real_period == 1 & 
                      HGIN_Full$meting == 1 & 
                      HGIN_Full$time <= 90)

# Participants that completed at least 65% (58.5 measurements) of the 90 measurements received 
# personalized feedback. Here, we also use this criterion to include participants in our 
# analyses. Hence, we further filter the data to only include participants with 59 or more 
# valid observations during their daily diary.

HGIN_65 <- HGIN_FirstD[HGIN_FirstD$Observations_valid90 >= 59, ]

# After this filters, the data got reduced from 115386 rows to 57945 rows (including NAs).
# Also the number of participants was reduced from 1396 to 644. 
nsub(HGIN_FirstD$id); nsub(HGIN_65$id)

# Now, we select only the variables that we are interested in, which are:
# Variables for the identification of the measurement: ID, the beep number (time), and
# the time at which the questionnaire was completed (as this is a date format imported 
# from SPSS, it resulted as the number of seconds elapsed since 01/10/1582 until the date
# and hour of the measurement)
# Socioeconomic variables: Gender, sex, age in years, and country.
# Daily diary items: Positive affect items, some event happened, social contact.
# Cross-sectional variables: Optimism.

HGIN <- HGIN_65[, c("id", "time", "mad_diary_completed_at", "gender", "sex", "age", "country",
                    paste0("mad_diary_", c(5, 7, 9, 11, 13, 15, 32, 34)), "Optimism")]

# Rename variables. Note that in Mplus, variables' names are limited to 8 characters.

names(HGIN) <- c("id", "time", "seconds", "gender", "sex", "age", "country",
                 "relax", "energ", "enthu", "conte", "calm", "cheer", "event", 
                 "social", "optim")

# We recode the variable event because we are only interested in the negative events. Therefore,
# "nothing happened", "something positive happened", and "something neutral happened" become 0;
# and "something negative happened" becomes 1.

HGIN$event <- recode(HGIN$event, "4 = 1; NA = NA; else = 0")

# The codes of the variables event and social are as follows:
# event: 0 = Nothing negative happened, 1 = Something negative happened.
# social: 0 = The person was alone, 1 = The person was in company.

# In the HGIN data, one can not say that the measurements are equally spaced over time.
# Not only because of the missing beeps but also because the time overnight. For this reason,
# in the analyses in Mplus, we will use the TINTERVAL command to make the measurements
# approximately equally spaced over time (by including extra NAs).
# For this, we transform the variable seconds to days elapsed since the 00:00 of the first 
# day in which the participant received a beep until the time the person finished the 
# questionnaire.

HGIN$days <- NA

ids <- unique(HGIN$id)
miss <- rep(NA, length(ids))
for (i in 1:length(ids)){
  miss[i] <- which(!is.na(HGIN$relax[which(HGIN$id == ids[i])]))[1]
  HGIN$days[which(HGIN$id == ids[i])] <- HGIN$seconds[which(HGIN$id == ids[i])] / 86400 - 
    (HGIN$seconds[which(HGIN$id == ids[i])][miss[i]] %/% 86400)
}
rm(ids, i, miss)

# Reorder the variables in the data frame.
HGIN <- mvvar("days", "seconds", data = HGIN)

# Create dummy variables for the combined situations negative event + being alone
table(HGIN$event, HGIN$social)
# Reference is NOT being alone and nothing negative happened 
HGIN$d1 <- ifelse(HGIN$event == 0 & HGIN$social == 0, 1, 0) # Being alone and nothing Negative
HGIN$d2 <- ifelse(HGIN$event == 1 & HGIN$social == 1, 1, 0) # Not being alone and something negative
HGIN$d3 <- ifelse(HGIN$event == 1 & HGIN$social == 0, 1, 0) # Being alone and something negative

# 2. Descriptive statistics----

apply(HGIN[, 9:16], 2, function(x) mean(calc.nomiss(x, HGIN$id, prop = TRUE)))

pdf(file = "Figures/boxplotevent.pdf", height = 4)
par(mfrow = c(1, 3), oma = c(0, 0, 4, 0))
boxplot(HGIN$energ ~ factor(HGIN$event, levels = c(0, 1), labels = c("NN", "SN")), 
        xlab = NULL, ylab = NULL, las = 1, main = "Energetic")
mtext("A", line = 3, at = 0, font = 2)
boxplot(HGIN$enthu ~ factor(HGIN$event, levels = c(0, 1), labels = c("NN", "SN")), 
        xlab = NULL, ylab = NULL, las = 1, main = "Enthusiastic")
mtext("B", line = 3, at = 0, font = 2)
boxplot(HGIN$cheer ~ factor(HGIN$event, levels = c(0, 1), labels = c("NN", "SN")), 
        xlab = NULL, ylab = NULL, las = 1, main = "Cheerful")
mtext("C", line = 3, at = 0, font = 2)
dev.off()

boxplot(HGIN$relax ~ HGIN$event)
boxplot(HGIN$conte ~ HGIN$event)
boxplot(HGIN$calm  ~ HGIN$event)

boxplot(HGIN$energ ~ HGIN$social)
boxplot(HGIN$enthu ~ HGIN$social)
boxplot(HGIN$cheer ~ HGIN$social)

boxplot(HGIN$relax ~ HGIN$social)
boxplot(HGIN$conte ~ HGIN$social)
boxplot(HGIN$calm ~ HGIN$social)

plot(HGIN$optim, HGIN$energ)
abline(lm(HGIN$energ ~ HGIN$optim))
plot(HGIN$optim, HGIN$enthu)
abline(lm(HGIN$enthu ~ HGIN$optim))
plot(HGIN$optim, HGIN$cheer)
abline(lm(HGIN$cheer ~ HGIN$optim))

plot(HGIN$optim, HGIN$relax)
abline(lm(HGIN$relax ~ HGIN$optim))
plot(HGIN$optim, HGIN$conte)
abline(lm(HGIN$conte ~ HGIN$optim))
plot(HGIN$optim, HGIN$calm)
abline(lm(HGIN$calm ~ HGIN$optim))

ids <- unique(HGIN$id)
set.seed(1234)
ids <- ids[sample(1:nsub(HGIN$id), 4)]

pdf(file = "Figures/tseries.pdf", width = 11)
par(mfrow = c(2, 2), mar = c(0.5, 1, 0.5, 0), oma = c(5, 3, 3, 2), xpd = NA)
ts.plot(HGIN[HGIN$id == ids[1], c(10, 11, 14)],
        gpars=list(xlab=NULL, ylab="Score", col = gray((3:1)/5), las = 1, 
                   ylim = c(-0.5, 100.5), xaxt = 'n'))
ts.plot(HGIN[HGIN$id == ids[2], c(10, 11, 14)],
        gpars=list(xlab=NULL, ylab=NULL, col = gray((3:1)/5), las = 1, 
                   ylim = c(-0.5, 100.5), xaxt = 'n', yaxt = 'n'))
ts.plot(HGIN[HGIN$id == ids[3], c(10, 11, 14)],
        gpars=list(xlab="Beep Number", ylab="Score", col = gray((3:1)/5), las = 1, ylim = c(-0.5, 100.5)))
ts.plot(HGIN[HGIN$id == ids[4], c(10, 11, 14)],
        gpars=list(xlab="Beep Number", ylab=NULL, col = gray((3:1)/5), las = 1, 
                   ylim = c(-0.5, 100.5), yaxt = 'n'))
legend("bottomright", legend = c("Energetic", "Enthusiastic", "Cheerful"), 
       col = gray((3:1)/5), lty = 1, bg = "white")
dev.off()

pdf(file = "Figures/tseriesall.pdf", width = 11)
par(mfrow = c(2, 2), mar = c(0.5, 1, 0.5, 0), oma = c(5, 3, 3, 2), xpd = NA)
ts.plot(HGIN[HGIN$id == ids[1], c(10:14)],
        gpars=list(xlab=NULL, ylab="Score", col = gray(c(3, 3, 2, 2, 1 ,1)/5), lty = c(2, 1, 1, 2, 2, 1), las = 1, 
                   ylim = c(-0.5, 100.5), xaxt = 'n'))
ts.plot(HGIN[HGIN$id == ids[2], c(10:14)],
        gpars=list(xlab=NULL, ylab=NULL, col = gray(c(3, 3, 2, 2, 1 ,1)/5), lty = c(2, 1, 1, 2, 2, 1), las = 1, 
                   ylim = c(-0.5, 100.5), xaxt = 'n', yaxt = 'n'))
ts.plot(HGIN[HGIN$id == ids[3], c(10:14)],
        gpars=list(xlab="Beep Number", ylab="Score", col = gray(c(3, 3, 2, 2, 1 ,1)/5), lty = c(2, 1, 1, 2, 2, 1), las = 1, ylim = c(-0.5, 100.5)))
ts.plot(HGIN[HGIN$id == ids[4], c(10:14)],
        gpars=list(xlab="Beep Number", ylab=NULL, col = gray(c(3, 3, 2, 2, 1 ,1)/5), lty = c(2, 1, 1, 2, 2, 1), las = 1, 
                   ylim = c(-0.5, 100.5), yaxt = 'n'))
legend("bottomright", legend = c("Relax", "Energetic", "Enthusiastic", "Content", "Calm", "Cheerful"), 
       col = gray(c(3, 3, 2, 2, 1 ,1)/5), lty = c(2, 1, 1, 2, 2, 1), bg = "white")
dev.off()

# 3. Stationarity checks ----
library(tseries)

# To check trend stationarity, we use the Kwiatkowski–Phillips–Schmidt–Shin test.

stat.test <- data.frame(matrix(NA, nrow = nsub(HGIN$id), 13))
names(stat.test) <- c("id", paste0(c("relax", "energ", "enthu", "conte", "calm", "cheer"), "_kpss"),
                      paste0(c("relax", "energ", "enthu", "conte", "calm", "cheer"), "_stat"))

stat.test[, 1] <- unique(HGIN$id) 

for (i in 1:nsub(HGIN$id)) {
  temp <- HGIN[HGIN$id == stat.test[i, 1], 9:14]
  temp <- na.omit(temp)
  stat.test[i, 2:7] <- apply(temp, 2, function(x) {
    pvalue <- kpss.test(x, null = "Trend")
    return(pvalue$p.value)
    })
  stat.test[i, 8:13] <- (stat.test[i, 2:7] <= 0.05) 
}
rm(i, temp)

# Stationarity of positive affect activation items
apply(stat.test[, c(9, 10, 13)], 1, sum)
summary(as.factor(apply(stat.test[, c(9, 10, 13)], 1, sum)))

nonstat <- stat.test[which(apply(stat.test[, c(9, 10, 13)], 1, sum) != 0), 1]
length(nonstat) 
# 193 individuals had at least one time series of their PA activation that was non stationary. 
# 451 individuals had all their time series of PA activation stationary. 

pdf(file = "Figures/nonstationaryplots.pdf", height = 6, width = 9)
par(mfrow = c(3, 1), mar = c(0.3, 5, 0, 1), oma = c(2, 0, 3, 2))
xaxt <- c(rep("n", 5), "s")
varnames <- c(NA, "energ", "enthu", NA, NA, "cheer")
for (i in 1:length(nonstat)) {
  temp <- HGIN[HGIN$id == nonstat[i], -17]
  temp <- na.omit(temp)
  linec <- ifelse(stat.test[stat.test$id == nonstat[i], 8:13] == TRUE, 1, 2)
  for (j in c(10, 11, 14)) {
    plot(temp$days, temp[, j], type = "l", las = 1, xaxt = xaxt[j - 8], ylab = varnames[j - 8],
         col = linec[j - 8])
    abline(h = mean(temp[, j]), lty = 2, col = gray(0.5))
    abline(lm(temp[, j] ~ temp$days), col = gray(0.25), lwd = 2)
  }
  mtext(paste0("Time series person p", stat.test[stat.test$id == nonstat[i], 1]), 
        outer = TRUE, line = 1)
}
dev.off()

id_keep    <- stat.test[which(apply(stat.test[, c(9, 10, 13)], 1, sum) == 0), 1] # Get IDs to keep.
HGIN_stat <- HGIN[which(HGIN$id %in% id_keep),]       # Create new data with only stationary time series.
nsub(HGIN_stat$id)
rm(id_keep)

detach("package:tseries")
# 4. Export data to Mplus ----

# As we are using the TINTERVAL command in Mplus, data needs to be without missing values
# in the daily diary items. Here, we take those rows out. We also drop some variables,
# which are not needed in the Mplus syntax.

hgin <- HGIN[!is.na(HGIN$relax), -c(2, 3, 5, 8) ]
# Repeat but with stationary sample
#hgin <- HGIN_stat[!is.na(HGIN_stat$relax), -c(2, 3, 5, 8) ]

# Here, we just export the data to Mplus and create an empty input file.
prepareMplusData(hgin[, 1:13], filename = "Mplus/hgin.dat", 
                 writeData = "always",
                 inpfile = FALSE)

runModels(paste0(getwd(), "/Mplus"))


# 5. Import Mplus results of selected analysis----

# Before importing the output from Mplus, we modified some of the output files. 
# Firstly, in the output files that saved the MCMC samples, we deleted the 100 last
# iterations of the first chain. These iterations are additional iterations that are
# used to compute the factor scores. Once, they are deleted, all the chains have the 
# same number of iterations. This was also needed to correctly read the MCMC samples
# into R with the function readModels() of MplusAutomation.
# Secondly, in the files with the factors scores, we replaced the first columns for 
# missing values to hide the data. When saving factor scores in Mplus, it saves in 
# the raw data in the first columns. Given that we are not allowed to share the data,
# we modified these files. 

I      <- 3 # number of indicators
D      <- 1 # number of dummies (Fixed situations)
model1 <- readModels("Mplus/ResultsFullSample/hgin_paa_1_alt.out")

# Packages for mcmc plots
library(ggplot2)
library(bayesplot)
#library(coda)  # Needs to be installed but not necessarily loaded.
#library(mcmcr) # Needs to be installed but not necessarily loaded.
color_scheme_set("darkgray")

# Put valid samples in an array 
fit_samples <- as.array(model1$bparameters$valid_draw)
fit_samples <- aperm(fit_samples, perm = c(1, 3, 2))
fit_samples <- fit_samples[, , -(1:2)] # Take out chain and iteration number.

pdf(file = "Figures/rhatModel2.pdf", height = 4)
print(mcmc_rhat(apply(fit_samples, 3, function (x) {y <- coda::as.mcmc(x)
colnames(y) <- paste0("p", colnames(y))
out <- mcmcr::rhat(y)})))
dev.off()
print(mcmc_trace(fit_samples, pars = dimnames(fit_samples)$var[28]))
print(mcmc_acf(fit_samples, pars = dimnames(fit_samples)$var[28]))

# Extract estimated parameters (Means of posterior distributions)
# The following code works to extract the unstandardized estimates of a ME-TSO model that 
# does not include any time-invariant covariate. In other words, this code works to extract
# the unstandardized estimates of Model 1 and Model 2.
parameters <- model1$parameters$unstandardized[, 3]

within.parameters <- list(lambda.state = parameters[1:I],
                          epsilon.var  = parameters[(I + 2 * D + 1):(2 * I + 2 * D)],
                          zeta.var     = parameters[2 * I + 3 * D + 1])

NC <- ((I * (I + 1)) + ((I * D) * (I * D - 1))) / 2

between.parameters <- list(beta1 = matrix(parameters[(3 * I + 3 * D + 2):((3 + D) * I + 3 * D + 1)], I, D),
                           beta0 = matrix(parameters[(NC + (5 + D) * I + 3 * D + 3):(NC + (5 + 2 * D) * I + 3 * D + 2)], I, D),
                           xi.means = parameters[(NC + (3 + D) * I + 3 * D + 2):(NC + (4 + D) * I + 3 * D + 1)],
                           ar.mean  = parameters[(NC + (4 + D) * I + 3 * D + 2)],
                           xi.var   = parameters[(NC + (5 + 2 * D) * I + 3 * D + 3):(NC + (6 + 2 * D) * I + 3 * D + 2)],
                           ar.var   = parameters[NC + (6 + 2 * D) * I + 3 * D + 3],
                           omega.var = parameters[(NC + (7 + 2 * D) * I + 3 * D + 4):(NC + (7 + 3 * D) * I + 3 * D + 3)])

# Extract autoregressive effect per individual
fscores <- model1$savedata
ind.ar <- get.timeinvar(fscores$AR.Mean, fscores$ID)

source("R/metso.var.coeff.R")
coeff <- metso.var.coeff(within.parameters  = within.parameters, 
                         between.parameters = between.parameters, 
                         ind.ar = ind.ar, 
                         id     = names(ind.ar))

# Create latex tables for the variance coefficients

coeff.fix <- data.frame(matrix(unlist(coeff$fixed.situations), 3, 4))
names(coeff.fix) <- c("Consistency of Traits",
                      "Specificity of Traits",
                      "Person-Situation Interaction Coefficient",
                      "Unique Situation Effect")
row.names(coeff.fix) <- c("Energetic",
                          "Enthusiastic",
                          "Cheerful")

print(xtable(coeff.fix, type = "latex", caption = "Variance Coefficients Across Fixed Situations",
             label = "tab:coeff.fix", align = c("l", "p{3cm}", "p{3cm}", "p{3cm}", "p{3cm}")),
      include.colnames=T, sanitize.rownames.function = identity,
      include.rownames = TRUE, NA.string = "-", caption.placement = "top", sanitize.text.function = function(x){x},
      file = "Tables/coeff.fix.txt")


coeff.xi0 <- lapply(coeff$random.situations$Xi_j0, 
                    function(x) apply(x, 2, function(y) 
                      paste0(round(mean(y), 2), " (", round(sd(y), 2), ")")))

coeff.xi0 <- matrix(unlist(coeff.xi0), ncol = 3, byrow = TRUE)

coeff.xi1 <- lapply(coeff$random.situations$Xi_j1, 
                    function(x) apply(x, 2, function(y) 
                      paste0(round(mean(y), 2), " (", round(sd(y), 2), ")")))

coeff.xi1 <- matrix(unlist(coeff.xi1), ncol = 3, byrow = TRUE)

coeff.rand <- data.frame(rbind(coeff.xi0, coeff.xi1))
coeff.rand <- data.frame(rep(c("Reliability",
                               "Consistency",
                               "Occasion-Specificity",
                               "Predictability by Trait",
                               "Unpredictability by Trait"), 2), coeff.rand)
names(coeff.rand) <- c("Variance Coefficient",
                       "Energetic",
                       "Enthusiastic",
                       "Cheerful")

print(xtable(coeff.rand, type = "latex", caption = "Variance Coefficients Across Random Situations",
             label = "tab:coeff.rand", align = c("l", "l", "c", "c", "c")),
      include.colnames=T, sanitize.rownames.function = identity,
      include.rownames = F, NA.string = "-", caption.placement = "top", sanitize.text.function = function(x){x},
      file = "Tables/coeff.rand.txt")

# Create tables of key parameters comparing Model1 and Model1b or Model2 and Model2b.
model1  <- readModels("Mplus/ResultsFullSample/hgin_paa_1_alt.out")
model1b <- readModels("Mplus/ResultsFullSample/hgin_paa_1b_alt.out")

par.table <- matrix(NA, 14, 2)
colnames(par.table) <- c("\\mathcal{M}_{1} Est. [95\\% C.I.]",
                         "\\mathcal{M}_{1b} Est. [95\\% C.I.]")
row.names(par.table) <- c("Eg-Ev Interaction Effect $\\beta_{111}$",
                          "En-Ev Interaction Effect $\\beta_{121}$",
                          "Ch-Ev Interaction Effect $\\beta_{131}$",
                          "Opt-Eg-Ev Interaction Effect $\\beta_{211}$",
                          "Opt-En-Ev Interaction Effect $\\beta_{221}$",
                          "Opt-Ch-Ev Interaction Effect $\\beta_{231}$",
                          "AR Effect Mean $E(\\varphi)$",
                          "Eg-Ev Effect Residual Variance $Var(\\omega_{11})$",
                          "En-Ev Effect Residual Variance $Var(\\omega_{21})$",
                          "Ch-Ev Effect Residual Variance $Var(\\omega_{31})$",
                          "AR Effect Variance $Var(\\varphi)$",
                          "Number of Free Parameters",
                          "DIC",
                          "pD")

par.table[c(1:3, 7:11), 1] <- paste0(round(model1$parameters$unstandardized[,3], 2), " [",
                                     round(model1$parameters$unstandardized[,6], 2), ",",
                                     round(model1$parameters$unstandardized[,7], 2), "]")[c(14:16, 29, 43:45, 39)]

par.table[1:11, 2] <- paste0(round(model1b$parameters$unstandardized[,3], 2), " [",
                             round(model1b$parameters$unstandardized[,6], 2), ",",
                             round(model1b$parameters$unstandardized[,7], 2), "]")[c(14:19, 37, 52:54, 48)]
par.table[12:14, 1] <- as.character(model1$summaries[11:13])
par.table[12:14, 2] <- as.character(model1b$summaries[11:13])


print(xtable(par.table, type = "latex", caption = "Unstandardized Estimates of the Key Parameters of the Models using the Situation Variable Event",
             label = "tab:event", align = c("p{2.25in}", "c", "c")),
      include.colnames=T, sanitize.rownames.function = identity,
      include.rownames = T, NA.string = "", caption.placement = "top", sanitize.text.function = function(x){x},
      file = "Tables/ResultEvent.txt")

# End ----













