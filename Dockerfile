FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmariadb-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','shinyWidgets','DBI','RMySQL','RMariaDB','jsonlite','digest'), repos='https://cloud.r-project.org')"

# Copy your app into /srv/shiny-server/app
COPY . /srv/shiny-server
RUN chmod -R 755 /srv/shiny-server

# Put Aiven CA cert in a standard location (this works ONLY if ca.pem exists in your repo)
RUN cp /srv/shiny-server/app/ca.pem /etc/ssl/certs/aiven-ca.pem && chmod 644 /etc/ssl/certs/aiven-ca.pem

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
