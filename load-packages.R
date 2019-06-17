# Load and Install required packages for use.
# Require pacman and pload a list of package names.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    broom
  , curl
  , DT
  , dplyr
  , ggimage
  , ggplot2
  , ggpubr
  , ggthemes
  , googledrive
  , jsonlite
  , lubridate
  , purrr
  , qdapTools
  , readr
  , RCurl
  , ROCR
  , rsvg
  , rvest
  , stringr
  , tibble
  , tictoc
  , tidyr
  , zoo)

