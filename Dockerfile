FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmariadb-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','shinyWidgets','DBI','RMySQL','RMariaDB','jsonlite','digest'), repos='https://cloud.r-project.org')"

# Copy CA certificate (from repo root)
COPY ca.pem /srv/shiny-server/ca.pem
RUN chmod 644 /srv/shiny-server/ca.pem

# Copy app to shiny-server root so it runs at "/"
COPY . /srv/shiny-server/
RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
