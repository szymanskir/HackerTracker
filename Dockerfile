FROM rocker/tidyverse:3.6.1
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'remotes::install_cran("processx")'
COPY HackerTracker_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz")'
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');HackerTracker::run_app()"
