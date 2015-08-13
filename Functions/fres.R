# Function Info -----------------------------------------------------------
# Name:       fres.R (Water reservoir costing function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wmake - Makeup water flow rate in kgal/yr


# Outputs -----------------------------------------------------------------

# Capital cost of water reservoir


# Description -------------------------------------------------------------

# This function does what it says, read the Market Assessment for details.


# Function ----------------------------------------------------------------
fres <- function(wmake) {
  
  # Reservoir size (in yd^3)
  size <- wmake*uopt$resS/365/325.851429*(1613+1/3)
  
  # Geometry
  resB <- (243*size/13)^(1/3)         # Base (yd)
  resA <- resB^2/9*(1+8*sqrt(10)/3)*9 # Surface area (ft^2)
  resD <- resB/6*3                    # Depth (ft)
  
  # Itemized costs
  excavate <- 1.07*size
  truck <-    0.15*excavate
  haul <-     2.15*size
  backfill <- 1.36*resA/9
  compact <-  1.57*resA/9
  clay <-     6.50*resA/9
  sheet <-    1.81*resA
  
  # Total
  total <- (excavate+truck+haul+backfill+compact+clay+sheet)*185.7/128.7 # Replace indices
  
  # Return result
  return(total)
}
