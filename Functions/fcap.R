# Function Info -----------------------------------------------------------
# Name:       fcap.R (Capital costing function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# cMine - capital cost of mine

# cRetort - capital cost of retort

# cUtility - capital cost of utilities

# oil - oil production history


# Outputs -----------------------------------------------------------------

# ccs - capital cost schedule


# Description -------------------------------------------------------------

# blah


# Function ----------------------------------------------------------------
fcap <- function(cMine, cRetort, cUtility, oil) {

  # Calculate following terms according to Seider et al. (2009)
  ccs <- data.frame(TBM = cMine+cRetort)                   # Total bare module investment
  ccs$Site <-   0.1*ccs$TBM                                # Site preparation
  ccs$Serv <-   0.1*ccs$TBM                                # Service facilities
  ccs$capU <-   cUtility                                   # Utility plants/connections
  ccs$DPI <-    with(ccs, TBM+Site+Serv+capU)              # Direct permanent investment
  ccs$Cont <-   0.18*ccs$DPI                               # Contigency and contractor fees
  ccs$TDC <-    with(ccs, DPI+Cont)                        # Total depreciable capital
  ccs$Land <-   0.02*ccs$TDC                               # Land
  ccs$Permit <- 0.1*sum(oil)                               # Permitting
  ccs$RIP <-    0.02*ccs$TDC                               # Royalties for intellectual property
  ccs$Start <-  0.1*ccs$TDC                                # Startup
  ccs$TPI <-    with(ccs, TDC+Land+Permit+RIP+Start)       # Total permanent investment
  ccs$WC <-     0.05*ccs$TPI                               # Working Capital
  ccs$TCI <-    with(ccs, TPI+WC)                          # Total capital investment

  # Return result
  return(ccs)
}
