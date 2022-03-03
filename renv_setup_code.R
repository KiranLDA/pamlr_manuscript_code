#install the renv package
install.packages("renv")

# navigate to the pamlr_manuscript_code folder from zenodo
setwd("path/to/rstudio/project/named/pamlr_manuscript_code")
# activate the environment
renv::activate()
# automatically find the lock file and get packages and versions information
renv::restore()
# install packages based on version in lockfile
install.packages(c('changepoint', 'data.table', 'depmixS4', 'dplyr', 'dygraphs', 'EMbC', 'GeoLight',
                   'htmltools', 'raster', 'rgl', 'RColorBrewer', 'viridis', 'xts', 'zoo'))
# install pamlr from the tar.gz file
install.packages("path/to/PAMLr-v.2.2.tar.gz", source=TRUE)
