# Function Info -----------------------------------------------------------
# Name:       ffoc.R (Fixed operating costs function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# Nopers - number of operators per shift

# mineWorkers - total number of mine workers (all shifts) - just used to
# calculate adminstrative expense (labor cost of mining modeled separately)

# fmaint - fraction of CTDC spent on maintenance annually

# CTDC - total depreciable capital cost

# CTPI - total permanent investment

# salary - Annual salary of TA and CL staff


# Outputs -----------------------------------------------------------------

# All fixed costs (minus management incentive compensation)


# Description -------------------------------------------------------------

# Calcualtes fixed costs based on number of operators per shift and capital
# costing terms, returning total fixed expense result.


# Function ----------------------------------------------------------------
ffoc <- function(Nopers, mineWorkers, mineLabor, fmaint, CTDC, CTPI, salary) {

  # Labor for operations
  LW <-   Nopers*5*2080*30           # Labor wages
  LS <-   0.15*LW                    # Labor salaries
  opSS <- 0.06*LW                    # Operating supplies and services
  TA <-   salary*5                   # Technical assistance
  CL <-   salary*5                   # Control laboratory
  L <-    LW+LS+opSS+TA+CL+mineLabor # Total labor

  # Maintenance
  M <-   fmaint*CTDC # Maintenance
  MW <-  1/2.3*M     # Maint. wages
  MS <-  0.25/2.3*M  # Maint. salaries
  MMS <- 1/2.3*M     # Maint. materials
  MO <-  0.05/2.3*M  # Maint. overhead

  # Operating Overhead
  GPO <-      0.071*(LS+LW+MW+MS) # General plant overhead
  MDS <-      0.024*(LS+LW+MW+MS) # Mechanical dept. services
  ERD <-      0.059*(LS+LW+MW+MS) # Employee relations dept.
  BS <-       0.074*(LS+LW+MW+MS) # Business services
  overhead <- GPO+MDS+ERD+BS

  # General Expenses - just administrative expense, incentive compensation is
  # separate
  admin <- 200e3*ceiling((Nopers*5+mineWorkers)/20)

  # Property Taxes
  pt <- 0.01*CTPI

  # Insurance
  ins <- 0.004*CTPI

  # Final fixed cost (per year)
  result <- L+M+overhead+admin+pt+ins

  # Return result
  return(result)
}
