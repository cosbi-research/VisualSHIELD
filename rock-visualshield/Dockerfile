FROM datashield/rock-base

ENV DSBASE_VERSION 6.2.0

ENV ROCK_LIB /var/lib/rock/R/library

RUN Rscript -e "install.packages(c('tensorflow','geigen','survival','VIM','randomForest'), , repos = c('https://cloud.r-project.org'), dependencies=TRUE, upgrade=FALSE, lib='$ROCK_LIB')"
RUN Rscript -e "remotes::install_github('sib-swiss/dsSwissKnife', repos = c('https://cloud.r-project.org', 'https://cran.datashield.org'), dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"
RUN Rscript -e "remotes::install_github('sib-swiss/dsSwissKnifeClient', repos = c('https://cloud.r-project.org', 'https://cran.datashield.org'), dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"
RUN Rscript -e "remotes::install_github('vanduttran/dsSSCP', repos = c('https://cloud.r-project.org', 'https://cran.datashield.org'), dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"
RUN Rscript -e "remotes::install_github('vanduttran/dsCOV@main', repos = c('https://cloud.r-project.org', 'https://cran.datashield.org'), dependencies = TRUE, upgrade = FALSE, lib = '$ROCK_LIB')"
