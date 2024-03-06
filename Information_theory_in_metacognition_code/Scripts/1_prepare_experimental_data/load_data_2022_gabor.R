################################################################################
#' 
#' Load in participant data
#' 
#'                                                 Dates in 2022 when data was gathered 
#' participant 1 = DL06FA16, group=LeftHorizontal, 13.01. / 20.01. / 24.01.
#' participant 2 = ER05NG19, group=LeftVertical    20.02. / 21.02. / 27.02.
#' participant 3 = ER07LF30, group=LeftVertical    19.01. / 24.01. / 26.01.
#' participant 4 = IG05NS23, group=LeftHorizontal  16.02. / 17.02. / 27.02.
#' participant 5 = NA06LF22, group=LeftVertical    13.01. / 20.01. / 26.01.
#' participant 6 = NN03UT30, group=LeftHorizontal  13.01. / 17.01. / 24.01.
#' 
#' Dr. Sascha Meyen, saschameyen@gmail.com
#' 2023-08-01 10:33
#' 
################################################################################

load_gabor_patch_data = function()
{
  dat = read_all_data_gabor_files()
  dat = extract_session_gabor(dat)
  dat = remove_trials_gabor(dat)
  dat = add_math_variables_gabor(dat)

  head(dat)
  dat
}

read_all_data_gabor_files = function()
{
  path = "Data/exp_2022_gabor_patch/"
  filenames = paste0(path, dir(path))

  dat = data.frame()
  for (filename in filenames)
  {
    part_dat = read.table(filename, header = TRUE)
    dat = rbind(dat, part_dat)
  }
  dat
}

extract_session_gabor = function(dat)
{
  participant_id = sapply(dat$participantID, 
                          function(participant_id) 
                          {
                            strsplit(participant_id, "_")[[1]][1]
                          }
                          )
  session = sapply(dat$participantID, 
                   function(participant_id) 
                   {
                     strsplit(participant_id, "_")[[1]][2]
                   }
                   )

  dat$participantID  = NULL
  dat = cbind(data.frame(participant_id = participant_id,  
                         session        = session),
              dat)

  dat
}

remove_trials_gabor = function(dat)
{
  s = dat$block == "PracticeBlock"
  dat = dat[!s,]

  s = dat$block == "FastPracticeBlock"
  dat = dat[!s,]

  s = dat$validOrInvalidTrial == "valid"
  dat = dat[s,]

  dat
}

add_math_variables_gabor = function(part_dat)
{
  part_dat$confidence = ifelse(part_dat$confidence == 1, .999, part_dat$confidence)
  part_dat$Y          = ifelse(part_dat$stimulus == "vertikal", -1, 1)
  part_dat$Yhat       = ifelse(part_dat$decision == "vertikal", -1, 1)
  part_dat$correct    = 1 * ( part_dat$Y == part_dat$Yhat )
  part_dat$logodds    = logit(part_dat$confidence)
  part_dat$evidence   = part_dat$Y * part_dat$Yhat * part_dat$logodds

  part_dat
}