# Function Info -----------------------------------------------------------
# Name:       logGrid.R (Log grid line plotter)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# min - minimum grid line to plot, expressed as power of ten. For example, if
# minimum grid line was at 100, min input should be set to "2"

# max - maximum grid line to plot, expressed as power of ten

# dir - direction to plot grid lines. "v" for vertical, "h" for horizontal

# lty - line type, default 1

# col - line color, default lightgrey


# Outputs -----------------------------------------------------------------

# Draws grid lines


# Description -------------------------------------------------------------

# This function draws grid lines for log-scale plots using the functions abline
# and seq.


# Function ----------------------------------------------------------------
logGrid <- function(min, max, dir, lty = 1, col = "lightgrey") {

  # For each order of magnitude to span
  for (i in min:(max-1)) {

    # If drawing grid lines vertically
    if (dir == "v") {

      # Draw lines
      abline(v = seq(10^i, 10^(i+1), 10^i), lty = lty, col = col)

      # Else lines are begin drawn horizontally
    } else {

      # Draw lines
      abline(h = seq(10^i, 10^(i+1), 10^i), lty = lty, col = col)
    }
  }
}
