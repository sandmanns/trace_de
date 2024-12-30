loadOrInstall <- function (packageName, type = "CRAN") {
    isPackageInstalled <- packageName %in% rownames(installed.packages())
    if(!isPackageInstalled){
          install.packages(packageName)
    }
    library(packageName, character.only = TRUE)
}

cranPackages <- c(
    "shiny",
    "shinythemes",
    "shinyjs",
    "openxlsx",
    "timetools",
    "stringr",
    "stringi",
    "igraph",
    "networkD3",
    "DT",
    "htmlwidgets",
    "RColorBrewer",
    "dplyr",
    "shinyWidgets"
)


for (package in cranPackages) {
    loadOrInstall(package)
}
