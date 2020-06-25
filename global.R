library(dplyr)

if (!exists("accidents")) {
  accidents <- read.csv(file = 'data/futureAccidentDATA.csv', 
                        header = TRUE,
                        sep = ',')
}

if (!exists("hospitals")) {
  hospitals <- accidents[sample.int(nrow(accidents), 100),]
}
