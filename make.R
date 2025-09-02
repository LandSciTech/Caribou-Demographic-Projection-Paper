#' Caribou-Demographic-Projection-Paper: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Sarah Endicott \email{sarah.endicott@canada.ca}
#'
#' @date 2023/02/16

# I copied these from the rocker/r-bspm Dockerfile because it sets them in the
# RProfile but it wasn't working
bspm::enable()
options(pkgType="binary", install.packages.check.source = "no")

cargs <- commandArgs(trailingOnly = TRUE)

pat <- cargs[1]

Sys.setenv(GITHUB_PAT = pat)

utils::install.packages("remotes", dependencies = TRUE)
utils::install.packages("terra", dependencies = TRUE, type = "binary")
# utils::install.packages("curl", dependencies = TRUE, type = "binary")
# utils::install.packages("ragg", dependencies = TRUE, type = "binary")
## Install Dependencies (listed in DESCRIPTION) ----
# print("install deps")
# try(remotes::install_deps("/Caribou-Demographic-Projection-Paper", upgrade = "never"))
# try(remotes::install_deps("/Caribou-Demographic-Projection-Paper", type = "source",
#                           upgrade = "never"))

message("install caribouMetrics from GitHub")

# do deps separately so they can be installed from binaries
# only do download once
bbt_download <- remotes::remote_download(remotes::github_remote("LandSciTech/bboutoolsMultiPop",
                                                                auth_token = pat))

try(remotes::install_deps(bbt_download, upgrade = "never"))
# this should do ones that were not available from binary
try(remotes::install_deps(bbt_download, type = "source", upgrade = "never"))

try(remotes::install_local(bbt_download,
                           type = "source", dependencies = FALSE, upgrade = "never"))


cm_download <- remotes::remote_download(remotes::github_remote("LandSciTech/caribouMetrics",ref="BbouIntegration"))

try(remotes::install_deps(cm_download, upgrade = "never"))
# this should do ones that were not available from binary
try(remotes::install_deps(cm_download, type = "source", upgrade = "never"))

try(remotes::install_local(cm_download,
                             type = "source", dependencies = FALSE, upgrade = "never"))



## Load Project Addins (R Functions and Packages) ----

# devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))

# code to make commands for all batches to use in bash script
# cat(paste0('nohup Rscript "Caribou-Demographic-Projection-Paper/analysis/scripts/sensitivityMinimal.R" ',
#        1:24,
#        ' "cloud"', sep = "\n"))

