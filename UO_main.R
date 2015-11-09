# Script Info -------------------------------------------------------------
# Name:      UO_main.R (Unconventional Oil and Simulation Main Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates runs a set of economic analyses for ex situ oil shale.
# Structure is as follows:
# 
# 1. Sets working directory, libraries, and options
# 2. BLAH

# 1.1 Paths ---------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - uncomment and/or replace with the path directory for your local
# copy of the Git repository and Dropbox files.
pwd.drop <- "C:/Users/jonwi/"                #  Desktop
pwd.git  <- "C:/Users/jonwi/Documents/R/"
# pwd.drop <- "C:/Users/Jon Wilkey/"
# pwd.git  <- "C:/Users/Jon Wilkey/Documents/R/" # Laptop

# Define paths.
# "raw"  is raw data (*.dbf files from DOGM, *.csv files, etc.).
# "data" is prepared data files (typically *.rda).
# "look" is lookup tables.
# "plot" is the directory for saving plot *.pdf files.
# "work" is the working directory where main.R and IO_options.R are located.
# "fun"  is the directory for all *.R functions.
path$raw   <- paste(pwd.drop, "Dropbox/ExShale/Raw Data", sep = "")
path$data  <- paste(pwd.drop, "Dropbox/ExShale/Prepared Data", sep = "")
path$plot  <- paste(pwd.drop, "Dropbox/ExShale/Plots", sep = "")
path$work  <- paste(pwd.git,  "exshale/", sep = "")
path$fun   <- paste(pwd.git,  "exshale/Functions", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("asYear.R",
                              "fwpc.R",
                              "fres.R",
                              "fcap.R",
                              "ffoc.R",
                              "stax.R",
                              "fNPV.R",
                              "fCFterms.R",
                              "multiplot.R",
                              "logGrid.R",
                              "clipboard.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(zoo)
library(sqldf)
library(lhs)
library(ggplot2)
library(AlgDesign)
library(beepr)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("UO_options.R")


# Loop --------------------------------------------------------------------

# Predefine results space
oilSP <-     rep(0, times = nrow(uopt$parR))
Toil <-      oilSP
CPFB <-      oilSP
TCI <-       oilSP
fc.mine <-   oilSP
fc.reto <-   oilSP
fc.site <-   oilSP
fc.serv <-   oilSP
fc.util <-   oilSP
fc.cont <-   oilSP
fc.land <-   oilSP
fc.permit <- oilSP
fc.RIP <-    oilSP
fc.start <-  oilSP
fc.WC <-     oilSP
pb.cap <-    oilSP
pb.loai <-   oilSP
pb.rstp <-   oilSP
pb.royl <-   oilSP
pb.tfts <-   oilSP
pb.mine <-   oilSP
pb.reto <-   oilSP
pb.prof <-   oilSP

# Print and save start time for MC simulation
writeLines(c("",
             "Running Simulation",
             paste("Start time:",Sys.time())))
runstart <- Sys.time()

# Progress Bar (since this next for-loop takes a while)
pb <- txtProgressBar(min = 0, max = nrow(uopt$parR), width = 75, style = 3)

# For each set of input parameter picks j
for (j in 1:nrow(uopt$parR)) {
  
  # Mine and Retort ---------------------------------------------------------
  
  # Find amount of rock (i.e shale) mined and retorted in ton/day
  rock.retort <- uopt$parR$OPD[j]/(uopt$parR$FA[j]*uopt$bmr$eff.retort/42)
  rock.mined <-  rock.retort/uopt$bmr$eff.grind
  
  # Calculate scaling factors
  RM <- rock.mined/uopt$bmr$smine      # Rock mined
  RR <- rock.retort/uopt$bmr$sretort   # Rock retorted
  O  <- uopt$parR$OPD[j]/uopt$bmr$poil # Oil produced
  
  # Capital costs
  cMine <-   uopt$bmr$cmine*RM^0.6*(231.6/100)*uopt$cpi     # Mine
  cRetort <- (uopt$bmr$cretort*RR^0.6+
                uopt$bmr$cclean*O^0.6)*(577.4/297)*uopt$cpi # Retort
  cMR <-     (cMine+cRetort)*uopt$parR$MRc[j]              # Total adjusted capital cost
  
  # Water
  wcool <-  uopt$bmr$wcool*RM
  wboil <-  uopt$bmr$wboil*RR
  wgen <-   uopt$bmr$wgenR*RR+uopt$bmr$wgenO*O
  wshale <- uopt$bmr$wshale*RM
  steam <-  uopt$bmr$steam*RR
  wmake <-  wshale+wcool+wboil+(wboil+(steam/2.20462/998*264.172052))*uopt$wrloss-wgen
  
  # Electricity
  elec <- uopt$bmr$elec*RR
  
  # Operating cost (utilities)
  opMine <-  uopt$bmr$opmine*RM*(231.6/100)*uopt$cpi*uopt$parR$MRo[j]                 # Mining
  opElec <-  uopt$ep*elec*uopt$parR$MRo[j]                                            # Electricity
  opSteam <- uopt$steamp*steam*uopt$parR$MRo[j]                                       # Steam
  opWater <- (wmake*uopt$wmakep+wcool*uopt$wcoolp+wboil*uopt$wboilp)*uopt$parR$MRo[j] # Water
  
  # Mining Labor
  mineWorkers <- ceiling(1.5791*rock.mined^0.5391)
  mineLabor <-   mineWorkers*2080*30
  
  
  # Utilities ---------------------------------------------------------------
  
  # Water pipeline
  wpipe <- fwpc(wmake = wmake)
  
  # Water reservoir
  cRes <- fres(wmake = wmake)
  
  # Final Utilities Capital cost
  cUtility = (50*steam*1000/24/365*uopt$cpi+    # Steam plant ($50/lb/hr)
              with(uopt, hubL*(eline+eswitch))+ # Electrical connection to grid
              58*wcool*1000/365/24/60*uopt$cpi+ # Cooling water ($58/gpm)
              wpipe$cpipe+                      # Water pipeline
              cRes)                             # Water reservoir
  
  
  # DCF Analysis ------------------------------------------------------------
  
  # Create DCF data.frame
  DCF <- data.frame(p = c(rep(0,4),                             # Construction period, no production
                          0.45,                                 # Startup year 1
                          0.45+(uopt$p/365-0.45)/2,             # Startup year 2
                          rep(uopt$p/365, uopt$nyear))) # Full scale production
  
  # Discount factor
  DCF$df <- 1/((1+uopt$parR$IRR[j])^(1:nrow(DCF)))
  
  # Calcualte oil production
  oil <- DCF$p*uopt$parR$OPD[j]*365
  
  # Get capital cost schedule
  ccs <- fcap(cMR, cUtility, oil)
  
  # Capital investment vectors
  DCF$CTDC <-  -ccs$TDC*c(rep(0.25, 4), rep(0, uopt$nyear+2))                        # Total Depreciable Capital
  DCF$WC <-    ccs$WC*c(rep(0, 4), -1, rep(0, uopt$nyear), 1)                        # Working Capital
  DCF$land <-  -ccs$Land*c(0, 1, rep(0, 4+uopt$nyear))                               # Land
  DCF$perm <-  -ccs$Permit*c(1, rep(0, 5+uopt$nyear))                                # Permitting
  DCF$RIP <-   -ccs$RIP*c(rep(0, 4), 1, rep(0, uopt$nyear+1))                        # Royalties for intellectual property
  DCF$start <- -ccs$Start*c(rep(0, 4), 1, rep(0, uopt$nyear+1))                      # Startup
  DCF$D <-     ccs$TDC*c(rep(0, 4), uopt$fD, rep(0, (uopt$nyear-length(uopt$fD)+2))) # Depreciation
  
  # Variable Costs
  DCF$Cv <- (-DCF$p*(opElec+wpipe$elec*uopt$ep) # Electricity
             -oil*uopt$rsp                      # Research spending
             -DCF$p*opMine                      # Mining
             -DCF$p*opSteam                     # Steam
             -DCF$p*opWater)                    # Water
  
  # Calculate fine variable cost fraction
  fracMine <- opMine/(opMine+opSteam+opElec+wpipe$elec*uopt$ep+opWater)
  
  # Fixed Costs
  DCF$Cf <- c(rep(0, 4),
              rep(-ffoc(Nopers =      uopt$Nopers,
                        mineWorkers = mineWorkers,
                        mineLabor =   mineLabor,
                        fmaint =      uopt$fmaint,
                        CTDC =        ccs$TDC,
                        CTPI =        ccs$TPI,
                        salary =      uopt$salary), uopt$nyear+2))  # ... added in here
  
  # Solve for oil price -----------------------------------------------------
  
  # Oil Supply Price
  oilSP[j] <- uniroot(NPV, lower = 0, upper = 1e7)$root
  
  
  # Calculate $/bbl cash flows ----------------------------------------------
  
  # Capital cost fractions
  fc.mine[j] <-   cMine/ccs$TCI         # Mine
  fc.reto[j] <-   cRetort/ccs$TCI       # Retort
  fc.site[j] <-   with(ccs, Site/TCI)   # Site
  fc.serv[j] <-   with(ccs, Serv/TCI)   # Service facilities
  fc.util[j] <-   with(ccs, capU/TCI)   # Utilities
  fc.cont[j] <-   with(ccs, Cont/TCI)   # Contingency
  fc.land[j] <-   with(ccs, Land/TCI)   # Land
  fc.permit[j] <- with(ccs, Permit/TCI) # Permitting
  fc.RIP[j] <-    with(ccs, RIP/TCI)    # Royalties for intellectual property
  fc.start[j] <-  with(ccs, Start/TCI)  # Startup
  fc.WC[j] <-     with(ccs, WC/TCI)     # Working capital
  
  # Run fCFterms function to get terms in cash flow equation that depend on oil
  CF <- fCFterms(oilSP[j])
  
  # Per barrel
  pb.cap[j] <-  with(DCF, sum(CTDC+land+RIP+perm+start))/sum(oil)                            # Capital
  pb.loai[j] <- (sum(DCF$Cf)+sum(0.01*ccs$TPI)+sum(CF$admin.comp))/sum(oil)                  # Labor, maint., overhead, admin salary + comp, insurance
  pb.rstp[j] <- (sum(with(CF, ro+sto+TS+TF))-sum(0.01*ccs$TPI))/sum(oil)                     # Royalties, serverance, income tax, and property taxes
  pb.royl[j] <- sum(CF$ro)/sum(oil)                                                          # Royalties only
  pb.tfts[j] <- sum(with(CF, TS+TF))/sum(oil)                                                # Taxes only
  pb.mine[j] <- sum(DCF$Cv)*fracMine/sum(oil)                                                # Electrical heating cost
  pb.reto[j] <- sum(DCF$Cv)*(1-fracMine)/sum(oil)                                            # Product separation and storage cost
  pb.prof[j] <- sum(CF$osale)/sum(oil)+pb.cap[j]+pb.loai[j]+pb.rstp[j]+pb.mine[j]+pb.reto[j] # Net profit
  
  
  # Save results ------------------------------------------------------------
  
  Toil[j] <-  sum(oil)
  TCI[j] <-   ccs$TCI
  CPFB[j] <-  ccs$TCI/(uopt$parR$OPD[j])
  
  
  # Save results ------------------------------------------------------------
  
  # Update progress bar
  Sys.sleep(1e-3)
  setTxtProgressBar(pb, j)
}

# Close progress bar
close(pb)

# Print stop time for MC simulation section
writeLines(c("",
             paste("Finished simulation at:", Sys.time()),
             paste("Elapsed time:", format(difftime(Sys.time(), runstart)))))

# Print finished message and play sound - feel free to replace with your
# preferred sound, see help for function by typing "?beep" in console
beep(3)
writeLines(c("",
             "Model run complete"))

# ... and really save results
results <- data.frame(uopt$parR,
                      oilSP,
                      Toil,
                      TCI,
                      CPFB,
                      fc.mine,
                      fc.reto,
                      fc.site,
                      fc.serv,
                      fc.util,
                      fc.cont,
                      fc.land,
                      fc.permit,
                      fc.RIP,
                      fc.start,
                      fc.WC,
                      pb.cap,
                      pb.loai,
                      pb.rstp,
                      pb.royl,
                      pb.tfts,
                      pb.mine,
                      pb.reto,
                      pb.prof)

save(results, file = file.path(path$data, "ExShale results qunif v3.rda"))
