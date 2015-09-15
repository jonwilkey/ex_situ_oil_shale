load(file.path(path$data, paste("exshale Results ", uopt$ver, ".rda", sep = "")))

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
multi.xyhex <- function(r, logflag, bs) {
  
  fFA <- ggplot(r, aes(x = FA, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("Fischer Assay (GPT)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  fOPD <- ggplot(r, aes(x = OPD/1e3, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("Scale (1e3 BPD)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  fnyear <- ggplot(r, aes(x = nyear, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw() +
    xlab("Duration (yr)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  fMRco <- ggplot(r, aes(x = MRco, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Mine+Retort Cost (%)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  froyalr <- ggplot(r, aes(x = royalr, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Royalty Rate (%)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  ffmaint <- ggplot(r, aes(x = fmaint, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("Maintenance (%)") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  fIRR <- ggplot(r, aes(x = IRR, y = oilSP)) +
    stat_binhex(bins = bs) +
    scale_fill_gradientn(colours = c("lightgrey","black")) +
    theme_bw_noy() +
    xlab("IRR") +
    ylab("OSP ($/bbl)") +
    guides(fill = FALSE)
  
  multiplot(fFA, fOPD, fnyear, fMRco, froyalr, ffmaint, fIRR, cols = 3)
}

# Plot full dataset
pdf(file.path(path$plot, "xy full multi v2.pdf"))
multi.xyhex(r = results, logflag = TRUE, bs = 20)
dev.off()

# Plot reduced results
pdf(file.path(path$plot, "xyhex reduced v2.pdf"))
multi.xyhex(r = results[results$oilSP <= 175,], logflag = FALSE, bs = 20)
dev.off()

# Boxplots for economically viable set

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

pdf(file.path(path$plot, "costs per bbl.pdf"))

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

r <- results[results$oilSP <= 175,]

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

pdf(file.path(path$plot, "capital cost boxplot.pdf"))

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


# Correlation coefficient
test <- corr.test(results[,1:8])

# Make table
cpr <- data.frame(rmean = test$r[,8], pmean = test$p[,8])

# Drop last row and round r value
cpr <- cpr[-8,]
cpr$rmean <- round(cpr$rmean,2)



# Regression analysis -----------------------------------------------------

test <- lm(oilSP ~ FA+OPD+nyear+MRco+royalr+fmaint+IRR-1, results)

mtv <- with(results, c(median(FA),
                       median(OPD),
                       median(nyear),
                       median(MRco),
                       median(royalr),
                       median(fmaint),
                       median(IRR)))

pdf(file.path(path$plot, "relative OSP barplot.pdf"))
barplot(mtv*coefficients(test)/max(mtv*coefficients(test)),
        ylim = c(-1,1),
        ylab = "Relative OSP Impact",
        xlab = "Input Variable",
        names.arg = c("FA", "OPD", "nyear", "MRco", "royalr", "fmaint", "IRR"))
dev.off()
