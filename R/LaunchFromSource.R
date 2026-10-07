libpath <- Sys.getenv("R_LIBS_USER")
message(libpath)
# create local user library path (not present by default)
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", lib = libpath, repos = "https://cloud.r-project.org")
  Sys.sleep(15)
  library(pacman)
}

"%notin%" <- Negate("%in%")
setVer <- function(){
  tday <- lubridate::now()
  yr <- lubridate::year(tday)
  mn <- lubridate::month(tday)
  dy <- stringr::str_pad(lubridate::day(tday), width = 2, pad = "0")
  hr <- lubridate::hour(tday)
  min <- stringr::str_pad(lubridate::minute(tday), width = 2, pad = "0")
  desc::desc_set_version(paste0(yr,".",mn, ".",dy , "." , hr, min), file = "DESCRIPTION", normalize = TRUE)
}

clearEnv <- function(){
  "%notin%" <- Negate("%in%")
  Elist <- ls(envir = .GlobalEnv)
  Elist <- Elist[Elist %notin% c("gs","reload", "clearEnv", "setVer")]
  rm(list = Elist, envir = .GlobalEnv)
  rm(Elist)
  pacman::p_unload(negate = TRUE)
}

gs <- function() {
  clearEnv()
  setVer()
  devtools::document()
  devtools::load_all()
  iRECUIpkg::run_iRECUI()
}


reload <- function () {
  try(unloadNamespace("iRECUIpkg"))
  try(remove.packages("iRECUIpkg"))
  setVer()
  devtools::document()
  devtools::install()
  #remotes::install_github("https://github.com/DFOSolo/iRECUIpkg")
}
