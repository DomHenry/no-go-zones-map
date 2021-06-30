# Base image https://hub.docker.com/u/rocker/
FROM rocker/geospatial:4.0.2

# system libraries of general use
## install debian packages
RUN export DEBIAN_FRONTEND=noninteractiv; apt-get update -qq && apt-get -y --no-install-recommends install \
    gdal-bin \
	libgdal-dev \
	libgeos-dev \
	libgeos++-dev \
	make

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## Shiny install
RUN /rocker_scripts/install_shiny_server.sh

# copy necessary files
## renv.lock file
COPY /renv.lock ./renv.lock
## other files in app folder ("./" selects all files in host root directory)
COPY ./ ./app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]