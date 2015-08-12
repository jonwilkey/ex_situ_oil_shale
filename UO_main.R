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
cMine <-   uopt$bmr$cmine*  RM^0.6*(uopt$cpi/uopt$cpiB) # Mine
cRetort <- uopt$bmr$cretort*RR^0.6*(uopt$cpi/uopt$cpiB) # Retort


# Water
wcool <- uopt$bmr$wcool*RM
wboil <- uopt$bmr$wboil*RR
steam <- uopt$bmr$steam*RR
wmake <- wboil*

# Operating cost (utilities)
opElec <- uopt$bmr$elec*RR*uopt$ep # Electricity
opSteam <- uopt$steamp*steam       # Steam


  # Oil Production ----------------------------------------------------------

  # Calculate maximum potential oil production (xg = 0) on daily basis (note -
  # units are in kg)
  moil <- c(0, diff(fcoil(1:max(dcoil$time))))

  # Calculate actual oil (in bbl) = moil*(1-xg)/rho.oil*(6.2898 bbl/m^3)
  oil <- moil*(1-parR$xg[j])/uopt$rho.oil*6.2898


  # Production, separation, and storage -------------------------------------

  # Capital cost formula:
  #
  # capPSS = base(func. of length)*(new max oil/base max oil)^0.6*nwell
  #
  capPSS <- uopt$fcapPSS(wellL$total)*(max(oil)/parR$nwell[j]/uopt$capPSSbase)^0.6*parR$nwell[j]

  # Operating cost formula:
  #
  # opPSS = (base cost/day/well as f(length))*(oil prod as f(time)/base oil prod)
  #
  # Since oil is total oil production from all wells, don't have to multiply by
  # number of wells in simulation.
  opPSS <- (uopt$fopPSS(wellL$total)/365)*(oil/uopt$opPSSbase)


  # Utility Lines -----------------------------------------------------------

  # Capital cost of all utilities
  capU <- with(uopt, hubL*(eline+eswitch))


  # Capital Costing ---------------------------------------------------------

  ccs <- fcap(capheat, capPSS, capU, oil, capwell)


  # Fixed Costs -------------------------------------------------------------

  opF <- ffoc(Nopers = uopt$Nopers, CTDC = ccs$TDC, CTPI = ccs$TPI)


  # DCF Analysis ------------------------------------------------------------

  # Make model data.frame
  model <- data.frame(CTDC = c(rep(x = -ccs$TDC/(tdesign+tconstr), times = tdesign+tconstr),
                               rep(x = 0,                          times = length(oil))))

  # Design Capital
  model$CD <- c(rep(x = -(ccs$Land+ccs$Permit)/tdesign, times = tdesign),
                rep(x = 0,                              times = tconstr+length(oil)))

  # Well Drilling and Completion Capital
  model$CWD <- c(rep(x = 0, times = tdesign),
                 -capwell,
                 rep(0,     times = nrow(model)-(tdesign+length(capwell))))

  # Startup Capital
  model$CSt <- c(rep(x = 0, times = tdesign+tconstr),
                 -(ccs$RIP+ccs$Start),
                 rep(x = 0, times = length(oil)-1))

  # Working Capital
  model$CWC <- c(rep(x = 0, times = tdesign+tconstr),
                 -ccs$WC,
                 rep(x = 0, times = length(oil)-2),
                 ccs$WC)

  # Gas production
  model$gasp <- c(rep(x = 0, times = tdesign+tconstr),
                  moil*parR$xg[j]*uopt$convert.otg)

  # Gas sales
  model$gsale <- model$gasp*parR$gp[j]

  # Gas royalties
  model$rg <- -uopt$royalr*model$gsale

  # Gas severance taxes
  stg <- -stax(prod = model$gasp, ep = parR$gp[j], uopt$royalr, uopt$st.low, uopt$st.high, uopt$st.con, uopt$st.cut.o)

  # PSS operating costs
  model$opPSS <- c(rep(x = 0, times = tdesign+tconstr),
                   -opPSS)

  # Electricity/heating costs
  model$opheat <- c(rep(x = 0, times = tdesign+tconstr),
                    -opheat)

  # Fixed costs (labor, maintenance, property taxes, insurance)
  model$fixed <- c(rep(x = 0,        times = tdesign+tconstr),
                   rep(x = -opF/365, times = length(oil)))

  # Oil production
  model$oilp <- c(rep(x = 0, times = tdesign+tconstr),
                  oil)

  # Depreciation
  model$D <- c(rep(x = 0, times = tdesign+tconstr),
               -ccs$TDC/365*
                 uopt$fD[1:length(oil)]/
                 ((1+uopt$inf)^
                    floor(((tdesign+tconstr+1):(tdesign+tconstr+length(oil)))/365)))

  # Discount factor
  model$df <- 1/((1+parR$IRR[j])^floor((1:(tdesign+tconstr+length(oil)))/365))


  # Solve for oil price -----------------------------------------------------

  # Oil Supply Price
  oilSP[j] <- uniroot(NPV, lower = 0, upper = 1e7)$root


  # Save results ------------------------------------------------------------

  # Total capital cost
  Toil[j] <- sum(oil)
  TCI[j] <-  ccs$TCI
  CPFB[j] <- ccs$TCI/(Toil[j]/length(oil))
  sTE[j] <-  sum(E)
  prodL[j] <- wellL$prod
}

# Sound off when loop is complete
beep(3, message("Script Finished"))

# ... and really save results
results <- data.frame(parR, oilSP, Toil, TCI, CPFB, sTE, prodL)
save(results, file = file.path(path$data, "UO_main Results v5.rda"))
