# Use the rocker/shiny image as the base image
FROM rocker/shiny:4.1.0

# Install system libraries required for some R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'readr', 'readxl', 'ggplot2', 'plotly', 'caret', 'randomForest', 'cluster', 'Rtsne'), repos='https://cloud.r-project.org/')"

# Copy the app to the image
COPY app.R /srv/shiny-server/

# Make port 3838 available to the world outside this container
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]