# Function Info -----------------------------------------------------------
# Name:       fwpc.R (Water pipeline costing function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wmake - Makeup water flow rate in kgal/yr


# Outputs -----------------------------------------------------------------

# Data.frame with capital cost and electricity requirements for water the
# pipeline


# Description -------------------------------------------------------------

# This function does what it says, read the Market Assessment for details.


# Function ----------------------------------------------------------------
fwpc <- function(wmake) {
  
  # Constants for use in this function
  pipeL <-    uopt$pipeL                      # Pipeline length (mi)
  elevD <-    uopt$elevD                      # Elevation change (ft)
  ENR.con <-  9412.25                         # ENR construction cost index (Dec. 2012)
  ENR.mat <-  2888.62                         # ENR materials cost index (Dec. 2012)
  plim <-     400                             # Maximum pipe operating pressure (psi)
  FLB <-      0.35                            # Friction losses to fittings/bends
  AFC <-      0.20                            # Annual fixed charge fraction
  pse <-      1.50                            # Pipe scaling exponent
  fins <-     1.40                            # Fittings/installation fraction
  peff <-     0.72                            # Pump efficiency
  pcost <-    0.74*(ENR.mat/1701.17)*uopt$cpi # Pipe cost ($/ft)
  conx <-     1.1                             # Congestion multiplier
  appx <-     1.053                           # Appurtenance multiplier
  binstall <- 3.30*uopt$cpi                   # Base installation cost ($/inch/ft)
  srip <-     0.90*uopt$cpi                   # soil ripability adjustment ($/inch/ft)
  
  # Convert water flow rate to ft^3/s from kgal/yr
  wf <- wmake*1000*0.133680556/365/24/3600
  
  # Diameter calculation
  D.lam <-  ((0.096*wf^2*1.002*uopt$ep*(1+FLB)*24*365)/(pse*(1+fins)*pcost*peff*AFC))^(1/(4+pse))
  D.turb <- ((1.32*wf^2.84*62.3^0.84*1.002^0.16*uopt$ep*(1+FLB)*24*365)/(pse*(1+fins)*pcost*peff*AFC))^(1/(4.84+pse))
  
  # Reynolds number for laminar case
  Re <- 4*998*wf/35.3146667/(pi*1.002*0.001*D.lam/39.3700787)
  
  # Check - is flow laminar? (i.e. Re < 2e3)
  econD <- ifelse(test = Re < 2e3,
                  yes =  round(D.lam),
                  no =   round(D.turb))
  
  # Recalculate Reynolds number with given diameter
  Re <- 4*998*wf/35.3146667/(pi*1.002*0.001*econD/39.3700787)
  
  # Capital cost of pipeline
  cpip <- pipeL*5280*conx*appx*(pcost*econD^pse+ENR.con/6300*(binstall+srip)*econD)
  
  # Calculate Fanning friction factor
  fff <- ifelse(test = Re < 2e3,
                yes = 16/Re,
                no = 0.001375*(1+(20000*0.0018/econD+1000000/Re)^(1/3)))
  
  # Calculate pumping head
  head.e <- 9.81*(elevD*0.3048)+4*fff*pipeL*5280*0.3048/(econD*0.0254)*0.5*(4*wf/35.3146667/(pi*(econD*0.0254)^2))^2
  head.h <- head.e/9.81*3.2808399
  head.p <- 998*head.e/6894.75729
  
  # Electricity demand
  elec <- head.e*998*wf/35.3146667*3600*24*365/1000/3600/peff
  
  # Number of pumping stations required
  npump <- ceiling(head.p/plim)
  
  # Capital cost of pumping stations
  cpum <- npump*46000*(wf*7.48051948*60/100)^0.75*(head.h/npump/300)^0.66*ENR.con/6300*uopt$cpi
  
  # Return results
  return(data.frame(cpipe = cpip+cpum, elec = elec))
}
