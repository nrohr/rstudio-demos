# This script is used to run the application defined in app.R in the background
options(shiny.autoreload = TRUE)
rmarkdown::run("flexdb-port-vol.Rmd")

# Then run rstudioapi::viewer("url.in.output")