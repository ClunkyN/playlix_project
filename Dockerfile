FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmariadb-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install RMariaDB (Aiven SSL) + your packages
RUN R -e "install.packages(c('shiny','shinyWidgets','DBI','RMariaDB','jsonlite','digest'), repos='https://cloud.r-project.org')"

# Copy your app into /app (served at /app/)
COPY . /srv/shiny-server/
RUN chmod -R 755 /srv/shiny-server

# Put Aiven CA cert into container (you must add ca.pem in repo root)
COPY ca.pem /etc/ssl/certs/aiven-ca.pem
ENV DB_SSL_CA=/etc/ssl/certs/aiven-ca.pem

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
