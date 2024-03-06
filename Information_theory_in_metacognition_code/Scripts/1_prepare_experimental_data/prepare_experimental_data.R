
prepare_experimental_data <- function()
{
  files <- dir("experimental_data/", full.names = TRUE)
  dat <- data.frame()
  for (file in files)
  {
    one_dat <- read.table(file, header = TRUE)
    dat <- rbind(dat, one_dat)
  }
  dat <- extract_session_gabor(dat)
  dat <- remove_trials_gabor(dat)

  head(dat)
  dat$stimulus[dat$stimulus == "vertikal"] = "vertical"
  dat$decision[dat$decision == "vertikal"] = "vertical"

  dat <- dat[, c("participant_id", "session", "experimentBlockNumber", "trialNumber", "frames", "stimulus", "decision", "responseCorrect", "confidence")]
  head(dat)

  # dat <- dat[dat$participant_id == unique(dat$participant_id)[6], ]

  write.csv(dat, "exp_2022_grating.csv", row.names = FALSE)

}