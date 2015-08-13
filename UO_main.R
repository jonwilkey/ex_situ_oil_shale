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
pwd.drop <- "C:/Users/jonwi/"
pwd.git  <- "C:/Users/jonwi/Documents/R/"

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
                              "clipboard.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(zoo)
library(sqldf)
library(lhs)
library(beepr)
library(ggplot2)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("UO_options.R")


# 2.0 Read in simulation data ---------------------------------------------



# Loop --------------------------------------------------------------------

# Predefine results space



# Mine and Retort ---------------------------------------------------------

# Find amount of rock (i.e shale) mined and retorted in ton/day
rock.retort <- uopt$parR$OPD/(uopt$parR$FA*uopt$bmr$eff.retort/42)
rock.mined <-  rock.retort/uopt$bmr$eff.grind

# Calculate scaling factors
RM <- rock.mined/uopt$bmr$smine    # Rock mined
RR <- rock.retort/uopt$bmr$sretort # Rock retorted
O  <- uopt$parR$OPD/uopt$bmr$poil

# Capital costs
cMine <-   uopt$bmr$cmine*  RM^0.6*(231.6/100)#(uopt$cpi/uopt$cpiB) # Mine
cRetort <- (uopt$bmr$cretort*RR^0.6*(577.4/297)+#(uopt$cpi/uopt$cpiB) # Retort
            uopt$bmr$cclean*O^0.6*(577.4/297))

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
opMine <-  uopt$bmr$opmine*RM*(231.6/100)#(uopt$cpi/uopt$cpiB) # Mining
opElec <-  uopt$ep*elec                            # Electricity
opSteam <- uopt$steamp*steam                       # Steam
opWater <- wmake*uopt$wmakep+wcool*uopt$wcoolp+wboil*uopt$wboilp

# Mining labor count
mineWorkers <- ceiling(1.5791*rock.mined^0.5391)
mineLabor <-   mineWorkers*2080*30


# Utilities ---------------------------------------------------------------

# Water pipeline
wpipe <- fwpc(wmake = wmake)

# Water reservoir
cRes <- fres(wmake = wmake)

# Final Utilities Capital cost
cUtility = (50*steam*1000/24/365+             # Steam plant ($50/lb/hr)
            with(uopt, hubL*(eline+eswitch))+ # Electrical connection to grid
            58*wcool*1000/365/24/60+          # Cooling water ($58/gpm)
            wpipe$cpipe+                      # Water pipeline
              (elec+wpipe$elec)/365/24*203+
            cRes)                             # Water reservoir


# DCF Analysis ------------------------------------------------------------

# Create DCF data.frame
DCF <- data.frame(p = c(rep(0,4),                          # Construction period, no production
                        0.45,                              # Startup year 1
                        0.45+(uopt$p/365-0.45)/2,          # Startup year 2
                        rep(uopt$p/365, uopt$parR$nyear))) # Full scale production

# Discount factor
DCF$df <- 1/((1+uopt$parR$IRR)^(1:nrow(DCF)))

# Calcualte oil production
oil <- DCF$p*50e3*365#DCF$p*uopt$parR$OPD*365

# Get capital cost schedule
ccs <- fcap(cMine, cRetort, cUtility, oil)

# Capital investment vectors
DCF$CTDC <-  -ccs$TDC*c(rep(0.25, 4), rep(0, uopt$parR$nyear+2))                        # Total Depreciable Capital
DCF$WC <-    ccs$WC*c(rep(0, 4), -1, rep(0, uopt$parR$nyear), 1)                        # Working Capital
DCF$land <-  -ccs$Land*c(0, 1, rep(0, 4+uopt$parR$nyear))                               # Land
DCF$perm <-  -ccs$Permit*c(1, rep(0, 5+uopt$parR$nyear))                                # Permitting
DCF$RIP <-   -ccs$RIP*c(rep(0, 4), 1, rep(0, uopt$parR$nyear+1))                        # Royalties for intellectual property
DCF$start <- -ccs$Start*c(rep(0, 4), 1, rep(0, uopt$parR$nyear+1))                      # Startup
DCF$D <-     ccs$TDC*c(rep(0, 4), uopt$fD, rep(0, (uopt$parR$nyear-length(uopt$fD)+2))) # Depreciation

# Variable Costs
DCF$Cv <- (-DCF$p*(opElec+wpipe$elec*uopt$ep) # Electricity
           -oil*uopt$rsp                      # Research spending
           -DCF$p*opMine                     # Mining
           -DCF$p*opSteam                    # Steam
           -DCF$p*opWater)                   # Water

# Fixed Costs
DCF$Cf <- c(rep(0, 4),
            rep(-ffoc(Nopers = uopt$Nopers,
                 mineWorkers = mineWorkers,
                 mineLabor =   mineLabor,
                 CTDC =        ccs$TDC,
                 CTPI =        ccs$TPI), uopt$parR$nyear+2))  # ... added in here

# Solve for oil price -----------------------------------------------------

# Oil Supply Price
oilSP <- uniroot(NPV, lower = 0, upper = 1e7)$root


# Save results ------------------------------------------------------------



# # Sound off when loop is complete
# beep(3, message("Script Finished"))
# 
# # ... and really save results
# results <- data.frame()
# save(results, file = file.path(path$data, "exshale Results v1.rda"))
