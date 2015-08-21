# Function Info -----------------------------------------------------------
# Name:       fCFterms.R (Cash flow terms calculation function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# op - oil price in $/bbl


# Outputs -----------------------------------------------------------------

# Data.frame with terms necessary for calculating cash flow that are based on
# oil price (primarily).


# Description -------------------------------------------------------------

# Calcualtes cash flow terms which are dependent on oil prices (for determining 
# $/bbl costs)


# Function ----------------------------------------------------------------
fCFterms <- function(op) {

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

  return(data.frame(osale,ro,sto,TS,TF,admin.comp))
}
