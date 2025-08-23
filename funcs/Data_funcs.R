## *****************************************************************************
## # Data create
## Create data sets, if they do not already exist. Helps make sure that the
## data structure is as desired.
## *****************************************************************************
data_crt <- function(data, filepath) {
  if (!file.exists(filepath)) {
    saveRDS(object = data, file = filepath)
  }
}


## *****************************************************************************
## # Data import
## *****************************************************************************
data_imp <- function(filepath) {
    readRDS(filepath)
}


## *****************************************************************************
## # Data export
## *****************************************************************************
data_exp <- function(data, filepath) {
  saveRDS(object = data, file = filepath)
}


## *****************************************************************************
## # ELO Rating
## *****************************************************************************
ELO_func <- function(elo1, score1, elo2, score2, k = 20) {
  elo::elo.calc(
    wins.A = elo::score(score.A = score1, score.B = score2),
    elo.A = elo1,
    elo.B = elo2,
    k = k
  )
}
