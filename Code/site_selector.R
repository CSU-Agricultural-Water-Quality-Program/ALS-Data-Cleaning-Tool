


package.list <- c('ggplot2',
                  'dplyr'
)
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)


source('./Code/file-merger.R')


data_path <- paste(dirname(getwd()), "/Data", sep = "")
print(data_path)


dat <- returnAllFiles(d=directory, export=FALSE)
