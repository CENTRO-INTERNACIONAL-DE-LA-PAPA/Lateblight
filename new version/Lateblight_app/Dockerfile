FROM ubuntu:latest

ENV TZ=UTC

RUN apk add --no-cache tzdata
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

FROM rocker/shiny:latest

RUN apt-get update -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
    apt-transport-https \
    build-essential \
    curl \
    gcc \
    gdebi \
    gfortran \
    libatlas-base-dev \
    libbz2-dev \
    libcairo2 \
    libcurl4-openssl-dev \
    libicu-dev \
    liblzma-dev \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libpcre3-dev \
    libsodium-dev \
    libtcl8.6 \
    libtiff5-dev \
    libtk8.6 \
    libx11-6 \
    libxt6 \
    locales \
    zlib1g-dev \
    wget && apt-get clean

RUN apt-get update -qq && apt-get install -y \
    autoconf \
    automake \
    pkg-config \
    git \
    && apt-get clean


RUN apt-get update -qq && apt-get install -y \
    make \
    libcurl4-openssl-dev \
    libssl-dev \
    pandoc \
    libxml2-dev \
    libxml2 \
    gdal-bin\
    proj-bin\
    libproj-dev\
    libgdal-dev \
    libgeos-dev \
    default-jdk \
    && apt-get clean


# r packages
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "install.packages(c('shiny','bs4Dash','toastui','shinycustomloader','DT','gdata','DBI','RMySQL','maps','ggmap','shinyTree','dismo','rJava','leaflet','rsconnect','shiny.i18n','leaflet.extras','data.table','slickR','openair','shinyalert','bslib','htmltools','tippy','fresh','bsicons','rjson'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('r-lib/svglite')"
RUN R -e "remotes::install_github('cran/rgdal')"
RUN R -e "remotes::install_github('cran/tidyverse')"

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

CMD ["R","-e","shiny::runApp('/home/app', port=3838, host='0.0.0.0')"]
