FROM rocker/shiny:latest

# System libs (MariaDB/MySQL + SSL)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmariadb-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (IMPORTANT: add RMariaDB)
RUN R -e "install.packages(c('shiny','shinyWidgets','DBI','RMariaDB','jsonlite','digest'), repos='https://cloud.r-project.org')"

# Copy your app into /app (served at /app/)
COPY . /srv/shiny-server/
RUN chmod -R 755 /srv/shiny-server


RUN cp /srv/shiny-server/app/ca.pem /etc/ssl/certs/aiven-ca.pem \
    && chmod 644 /etc/ssl/certs/aiven-ca.pem

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
