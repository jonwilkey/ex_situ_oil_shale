# Function Info -----------------------------------------------------------
# Name:       fNPV.R (Net present value function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# op - oil price in $/bbl


# Outputs -----------------------------------------------------------------

# Net present value (NPV)


# Description -------------------------------------------------------------

# Calcualtes NPV as function of oil price


# Function ----------------------------------------------------------------
NPV <- function(op) {

  # Oil Sales
  osale <- oil*op

  # Royalties
  ro <- -uopt$parR$royalr[j]*osale

  # Severance taxes
  sto <- -stax(prod = oil, ep = op, uopt$parR$royalr[j], uopt$st.low, uopt$st.high, uopt$st.con, uopt$st.cut.o)

  # Depletion
  d <- -(ccs$Land/sum(oil))*oil

  # Income taxes
  TI <- osale+ro+sto+d+with(DCF, -D+Cv+Cf) # Taxable Income
  TI <- ifelse(TI < 0, 0, TI)              # Only keep positive values of TI
  TS <- -uopt$rTS*TI                       # State income taxes
  TF <- -uopt$rTF*(TI+TS)                  # Federal income taxes

  # Administrative compensation
  NP <- osale+ro+sto+TS+TF+with(DCF, Cv+Cf+CTDC+WC+land+perm+RIP+start)
  admin.comp <- -uopt$radmin.comp*ifelse(NP > 0, NP, 0)

  # Final cash flow
  CF <- osale+ro+sto+TS+TF+admin.comp+with(DCF, Cv+Cf+CTDC+WC+land+perm+RIP+start)

  # Final NPV
  NPV <- sum(DCF$df*CF)

  return(NPV)
}
