# Script Info -------------------------------------------------------------
# Name:      plots v1.R (ExShale Plotting Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Load Results ------------------------------------------------------------

load(file.path(path$data, paste("exshale Results ", uopt$ver, ".rda", sep = "")))


# Find econ price limit ---------------------------------------------------

# Price cutoff for economic viability
eia.op <- read.csv(file.path(path$raw, "EIA2015AEO highoilOP.csv"), skip = 4)
names(eia.op) <- c("year", "op")

# Prices are in 2013 dollars (CPI = 232.957), adjust to 2014 USD
eia.op$op <- eia.op$op * (uopt$cpi * 229.594 / 232.957)

price.cut <- mean(eia.op$op[eia.op$year >= 2015])

# # Repeat for reference price forecast
# eia.op <- read.csv(file.path(path$raw, "EIA2015AEO refoilOP.csv"), skip = 4)
# names(eia.op) <- c("year", "op")
# 
# # Prices are in 2013 dollars (CPI = 232.957), adjust to 2014 USD
# eia.op$op <- eia.op$op * (uopt$cpi * 229.594 / 232.957)
# 
# price.cut.ref <- mean(eia.op$op[eia.op$year >= 2015])


# Hexbin multiplot function -----------------------------------------------

# Black and white theme with no y axis label
theme_bw_noy <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(colour = "black"),
          axis.title.y = element_blank(),
          legend.key = element_rect(colour = "grey80"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey50"),
          panel.grid.major = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor = element_line(colour = "grey98", size = 0.5),
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}

# Multiplot hexplot function
multi.xyhex <- function(r, logflag, n) {
  
  fFA <- ggplot(r, aes(x = FA, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$FA)), y = seq(min(r$oilSP), max(r$oilSP), length = n))) + 
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("G (gal/ton)") +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  fOPD <- ggplot(r, aes(x = OPD/1e3, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$OPD)/1e3), y = seq(min(r$oilSP), max(r$oilSP), length = n))) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("OPD (thousand BPD)") +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  fMRc <- ggplot(r, aes(x = MRc, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$MRc)), y = seq(min(r$oilSP), max(r$oilSP), length = n))) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab(expression(f[cap])) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  fMRo <- ggplot(r, aes(x = MRo, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$MRo)), y = seq(min(r$oilSP), max(r$oilSP), length = n))) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab(expression(f[op])) +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  froyalr <- ggplot(r, aes(x = royalr, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$royalr)), y = seq(min(r$oilSP), max(r$oilSP), length = n))) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("r") +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  fIRR <- ggplot(r, aes(x = IRR, y = oilSP)) +
    stat_bin2d(breaks = list(x = sort(unique(r$IRR)), y = seq(min(r$oilSP), max(r$oilSP), length = n))) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("IRR") +
    ylab("OSP ($ / bbl)") +
    guides(fill = FALSE)
  
  multiplot(fFA, fOPD, fMRc, fMRo, froyalr, fIRR, cols = 2)
}


# Plot OSP hexbin ---------------------------------------------------------

# Plot full dataset
pdf(file.path(path$plot, paste("xyhex full ", uopt$ver, ".pdf", sep = "")), width = 7, height = 7*4/3)
# tiff(file.path(path$plot, paste("xyhex full ", uopt$ver, ".tif", sep = "")), width = 480*300/72, height = 480*300/72*4/3, res = 300)
multi.xyhex(r = results, logflag = TRUE, n = 9)
dev.off()

# Plot reduced results
pdf(file.path(path$plot, paste("xyhex reduced ", uopt$ver, ".pdf", sep = "")), width = 7, height = 7*4/3)
multi.xyhex(r = results[results$oilSP <= price.cut,], logflag = FALSE, n = 13)
dev.off()


# Boxplots for economically viable set ------------------------------------

r <- results[results$oilSP <= price.cut,]

# Reshape
bdr <- rbind(data.frame(type = as.factor("CTPI"),   cost = (-r$pb.cap)),
             data.frame(type = as.factor("CV"),     cost = (-(r$pb.mine+r$pb.reto))),
             data.frame(type = as.factor("CF"),     cost = (-r$pb.loai)),
             data.frame(type = as.factor("R"),      cost = (-r$pb.royl)),
             data.frame(type = as.factor("TFTS"),   cost = (-r$pb.tfts)),
             data.frame(type = as.factor("STP"),    cost = (-(r$pb.rstp-r$pb.royl-r$pb.tfts))),
             data.frame(type = as.factor("Profit"), cost = r$pb.prof))

# Drop any negatives
bdr <- bdr[bdr$cost >= 0,]

pdf(file.path(path$plot, paste("cost per bbl ", uopt$ver, ".pdf", sep = "")))

boxplot(cost~type, bdr,
        range = 0,
        names = c(expression(C[TPI]),
                  expression(C[V]),
                  expression(C[F]),
                  expression(R),
                  expression(T[F] + T[S]),
                  expression(ST),
                  "Profit"),
        xlab = "Cost Category",
        ylab = "Cost ($/bbl)")

dev.off()

# Repeat for capital
cdr <- rbind(data.frame(type = as.factor("mine"),   frac = r$fc.mine),
             data.frame(type = as.factor("reto"),   frac = r$fc.reto),
             data.frame(type = as.factor("SS"),     frac = (r$fc.site+r$fc.serv)),
             data.frame(type = as.factor("util"),   frac = r$fc.util),
             data.frame(type = as.factor("cont"),   frac = r$fc.cont),
             data.frame(type = as.factor("land"),   frac = r$fc.land),
             data.frame(type = as.factor("permit"), frac = r$fc.permit),
             data.frame(type = as.factor("RIP"),    frac = r$fc.RIP),
             data.frame(type = as.factor("start"),  frac = r$fc.start),
             data.frame(type = as.factor("WC"),     frac = r$fc.WC))

cdrc <- rbind(data.frame(type = as.factor("mine"),   frac = r$fc.mine*r$TCI),
              data.frame(type = as.factor("reto"),   frac = r$fc.reto*r$TCI),
              data.frame(type = as.factor("SS"),     frac = (r$fc.site+r$fc.serv)*r$TCI),
              data.frame(type = as.factor("util"),   frac = r$fc.util*r$TCI),
              data.frame(type = as.factor("cont"),   frac = r$fc.cont*r$TCI),
              data.frame(type = as.factor("land"),   frac = r$fc.land*r$TCI),
              data.frame(type = as.factor("permit"), frac = r$fc.permit*r$TCI),
              data.frame(type = as.factor("RIP"),    frac = r$fc.RIP*r$TCI),
              data.frame(type = as.factor("start"),  frac = r$fc.start*r$TCI),
              data.frame(type = as.factor("WC"),     frac = r$fc.WC*r$TCI))

pdf(file.path(path$plot, paste("capital cost boxplot ", uopt$ver, ".pdf", sep = "")))

boxplot(frac~type, cdrc,
        range = 0,
        log = "y",
        ylim = c(1e6, 5e9),
        yaxt = "n",
        names = c("Mine",
                  "Retort",
                  expression(C[SS]),
                  expression(C[alloc]),
                  expression(C[cont]),
                  expression(C[L]),
                  expression(C[P]),
                  expression(C[RIP]),
                  expression(C[S]),
                  expression(C[WC])),
        xlab = "Capital Cost Category",
        ylab = expression(paste("Capital Cost ($)")))
axis(side = 2, at = c(1e6, 1e7, 1e8, 1e9, 1e10),
     labels = c(expression(10^6),
                expression(10^7),
                expression(10^8),
                expression(10^9),
                expression(10^10)),
     las = 2)

dev.off()


# Tables ------------------------------------------------------------------

# # Correlation coefficient
# test <- corr.test(results[,1:8])
# 
# # Make table
# cpr <- data.frame(rmean = test$r[,8], pmean = test$p[,8])
# 
# # Drop last row and round r value
# cpr <- cpr[-8,]
# cpr$rmean <- round(cpr$rmean,2)


# Regression analysis -----------------------------------------------------

test <- lm(oilSP ~ FA+OPD+MRc+MRo+royalr+IRR-1, results)

mtv <- with(results, c(median(FA),
                       median(OPD),
                       median(MRc),
                       median(MRo),
                       median(royalr),
                       median(IRR)))

pdf(file.path(path$plot, paste("relative OSP ", uopt$ver, ".pdf", sep = "")))
barplot(mtv*coefficients(test)/max(mtv*coefficients(test)),
        ylim = c(-1,1),
        ylab = "Relative OSP Impact",
        xlab = "Input Parameter",
        names.arg = c("G", "OPD", expression(f[cap]), expression(f[op]), "r", "IRR"))
dev.off()


# Log-normal fit ----------------------------------------------------------

test <- fitdist(results$oilSP, "lnorm")

pdf(file.path(path$plot, "lnorm denscomp v3.pdf"))
denscomp(ft = test,
         ylim = c(0, 0.0125),
         demp = T,
         dempcol = "blue",
         xlab = "OSP ($ / bbl)",
         ylab = "Probability Density",
         addlegend = F,
         main = NA)
legend("right", c("Log-Normal Fit", "Empirical PDF"), col = c("red", "blue"), lty = 1)
dev.off()
