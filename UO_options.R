# Script Info -------------------------------------------------------------
# Name:      UO_options.R (Unconventional Oil and Simulation Options Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates a list object named "uopt" that contains the options for 
# all the inputs/outputs that control the execution of the UO_main.R script.
# Review each input/output below and change as desired from their base values.


# 1.0 Global Options ------------------------------------------------------

# Define "uopt" list object - this must exist in order to set any other options
uopt <- NULL


# Version History ---------------------------------------------------------

# Version #
uopt$ver <- "v4"

# v4
# - Normal full factorial space

# v3
# - Uniform full factorial space

# v2
# - What went into NETL presentation in September


# Parameter Space Generation ----------------------------------------------

# Switch - use uniform space?
uspace <- F

if (uspace == T) {
  
  # Factorial design
  parLHS <- (gen.factorial(levels = 11, nVars = 6, center = F)-1)/10
  
  # # Latin Hypercube Design
  # parLHS <- randomLHS(n = 1e4, k = 7)
  
  # Get parameter values from parLHS
  
  # For uniform distribution
  uopt$parR <- data.frame(FA =     qunif(parLHS[,1], min = 10,   max = 70),    # Fischer-assay (gal oil / ton shale)
                          OPD =    qunif(parLHS[,2], min = 10e3, max = 100e3), # Oil production rate (BPD)
                          MRc =    qunif(parLHS[,3], min = 0.5,  max = 1.5),   # Mine & Retort cap expense fraction
                          MRo =    qunif(parLHS[,4], min = 0.5,  max = 1.5),   # Mine & Retort op expense fraction
                          royalr = qunif(parLHS[,5], min = 0.05, max = 0.20),  # Royalty rate
                          IRR =    qunif(parLHS[,6], min = 0.10, max = 0.40))  # Internal rate of return
} else {
  
  # Generate factorial design
  parLHS <- (gen.factorial(levels = 11, nVars = 6, center = F))
  
  # Turn into a vector
  parLHS <- c(parLHS[, 1], parLHS[, 2], parLHS[, 3], parLHS[, 4], parLHS[, 5], parLHS[, 6])
  
  # Define quantile sequene
  probs <- seq(0.05, 0.95, 0.09)
  
  # For each quantile
  for (i in 1:length(probs)) {
    
    # Replace level value in parLHS
    parLHS[which(parLHS == i)] <- probs[i]
  }
  
  # Convert back into matrix
  parLHS <- matrix(parLHS, ncol = 6)
  
  # Find actual values for normal distibution with specified mean and SD
  uopt$parR <- data.frame(FA =     qnorm(parLHS[,1], mean = 25,    sd = 10),     # Fischer-assay (gal oil / ton shale)
                          OPD =    qnorm(parLHS[,2], mean = 50e3,  sd = 25e3),   # Oil production rate (BPD)
                          MRc =    qnorm(parLHS[,3], mean = 1.0,   sd = 0.25),   # Mine & Retort cap expense fraction
                          MRo =    qnorm(parLHS[,4], mean = 1.0,   sd = 0.25),   # Mine & Retort op expense fraction
                          royalr = qnorm(parLHS[,5], mean = 0.125, sd = 0.0375), # Royalty rate
                          IRR =    qnorm(parLHS[,6], mean = 0.15,  sd = 0.05))   # Internal rate of return
}

# Remove LHS
remove(parLHS, uspace)


# Index Values ------------------------------------------------------------

# CPI adjustment to go from 2012 to 2014 dollars for all terms taken from Market
# Assessment
uopt$cpi <-  236.736/229.594


# Base Mine & Retort Data -------------------------------------------------

uopt$bmr <- data.frame(eff.retort = 0.92,          # Fraction of oil recovered from retorted shale
                       eff.grind =  0.90,          # Fraction of shale recovered from grinder
                       smine =      165700,        # Base shale mined (TPD)
                       sretort =    149130,        # Base shale retorted (TPD)
                       cmine =      465e6,         # Capital cost for mine (1981 USD)
                       cretort =    302e6,         # Capital cost for retort (1981 USD)
                       elec =       277500*24*365, # Electricity usage (kWh/yr)
                       steam =      666*24*365,    # Steam usage (k lb/yr)
                       wcool =      608162,        # Cooling water usage - for mining (kgal/yr)
                       wshale =     895116,        # Water usage - spent shale disposal (kgal/yr)
                       wboil =      2909611,       # Boiler feed water usage - for cooling tower (kgal/yr)
                       wgenR =      1419259,       # Water generated that scales with retorting (kgal/yr)
                       wgenO =      416859,        # Water generated that scales with oil production (kgal/yr)
                       opmine =     77.22e6,       # Mine operating cost (1981 USD/yr)
                       cclean =     90e6,          # Capital cost for retort gas cleaning and upgrading
                       poil =       99170)         # Crude oil production (BPD)


# Labor -------------------------------------------------------------------

# Number of operators per shift
uopt$Nopers <- 54

# Maintanance Labor % of CTDC
uopt$fmaint <- 0.05

# Salary
uopt$salary <- 82510 # Source: http://www.bls.gov/oes/current/oes_ut.htm#17-0000 (ChemE)


# Utilities ---------------------------------------------------------------

# Utah 2014 annual average industrial electricity price ($/kWh)
# Data from EIA Average retail price of electricity to ultimate customers:
# http://www.eia.gov/electricity/data.cfm#sales
uopt$ep <-      0.0607

# Electrity infrastructure, assuming 230 kV single circuit line (400 MW capacity)
# (https://www.wecc.biz/Reliability/2014_TEPPC_Transmission_CapCost_Report_B+V.pdf).
uopt$eline <-   (959700+   # Base Line cost ($/mi)
                   (15.14* # Right of way width (acres/mile)
                      85)) # BLM land capital cost ($/acre)
uopt$eswitch <- 0.1        # Substation cost as fraction of line cost

# Distance to nearest utility hub (mi)
uopt$hubL <-    6.51

# Water
uopt$wrloss <-  0.03                # Percent water recycling losses
uopt$wmakep <-  50/325.853*uopt$cpi # Makeup water price ($/kgal/yr)
uopt$wcoolp <-  0.075*uopt$cpi      # Cooling water price ($/kgal)
uopt$wboilp <-  1.80*uopt$cpi       # Boiler feed water price ($/kgal)
uopt$pipeL <-   5                   # Water pipeline length (mi)
uopt$elevD <-   711                 # Water pipeline elevation change (ft)
uopt$resS <-    90                  # Reservoir size, in days of makeup water

# Other utility charges
uopt$steamp <-  6.60*uopt$cpi       # Steam price ($/k lb)


# Finance and Econ Terms --------------------------------------------------

# Process Utilization (i.e. number of days per year process is in operation)
uopt$p <- 330

# Number of years of full-scale operation
uopt$nyear <- 20

# ACRS 10-yr Depreciation Schedule
uopt$fD <- c(0.1000,
             0.1800,
             0.1440,
             0.1152,
             0.0922,
             0.0737,
             0.0655,
             0.0655,
             0.0656,
             0.0655,
             0.0328)

# Research spending ($/bbl)
uopt$rsp <- 0.741865*uopt$cpi

# Severance taxes
uopt$st.low <-   0.03
uopt$st.high <-  0.05
uopt$st.con <-   0.002
uopt$st.cut.o <- 13

# Income tax rates
uopt$rTS <- 0.05
uopt$rTF <- 0.35

# Labor
uopt$radmin.comp <- 0.0125
