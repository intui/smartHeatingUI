# Basis-Package
FROM rocker/shiny

# R-Packages installieren
RUN R -e "install.packages(c('ggplot2','openxlsx','openxlsx','jsonlite','lubridate','tidyr','stringr','shinydashboard'))"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/


# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]



# build package
# docker build -t ufhuser .

# simple start
# docker run -d -p 80:3838 ufhuser -v /srv/shinyapps/:/srv/shiny-server/

# start Docker on port 80 with logging
# docker run --user shiny -d -p 80:3838 \
#    -v /srv/shinyapps/:/srv/shiny-server/ \
#    -v /srv/shinylog/:/var/log/shiny-server/ \
#    rocker/shiny
