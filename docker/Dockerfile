# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.1.0 as base

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \ 
    unixodbc-dev \
    libcurl4-openssl-dev \ 
    libssl-dev \
    libmagick++-dev

## update system libraries
RUN apt-get update && apt-get upgrade -y && apt-get clean

## renv.lock file
COPY renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

RUN Rscript -e 'renv::install("ggimage")'
RUN Rscript -e 'renv::install("RPostgres")'

# need to multi stage build here
RUN useradd --create-home appuser
WORKDIR /home/appuser
USER appuser

# copy necessary files
# COPY data ./data
COPY content ./content
COPY app.R ./app.R
COPY global.R ./global.R
COPY .Renviron ./.Renviron
COPY logos ./logos
COPY www ./www
COPY renv/activate.R renv/activate.R
COPY js/navbar.js ./js/navbar.js

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('app.R', host = '0.0.0.0', port = 3838)"]