# Install ggplot2
install.packages("ggplot2")

# Install devtools for downloading the newest version of some packages
install.packages("devtools")

# Install ggradar for the radar-plots
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)

# Install tibble (as a dependency for dplyr)
install.packages("tibble")

# Install dplyr
devtools::install_github("tidyverse/dplyr")