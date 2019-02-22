#
# scoreMGSE for a run and
# statMGSE  for all runs in a visit 
#

# GLOBALS
source('eyescoreFunctions.R')

bea_res <- function(...){
   br <- '/Volumes/L/bea_res/'
   if (!file.exists(br) ) br <- '~/rcn/bea_res'
   if (!file.exists(br) ) br <- '/mnt/B/bea_res'
   if (!file.exists(br) ) stop("cannot find bea_res!")
   return(file.path(br,...))
}

# make a file name , .txt is only diff over paste0
mkfilen <- function(pf,sfx,ext=".txt") {
  sprintf('%s%s%s',pf,sfx,ext)
}

# get file names for each stage/step
filesMGS <- function(lunaid,vdate,pfix=bea_res("Data/Tasks/MGS/Basic/")) {
  fp <- sprintf("%s/%d/%d/Raw/EyeData/txt/%d.%d.mgs.",pfix,lunaid,vdate,lunaid,vdate)

  f <- sapply(c("raw","preproc","runData","scored"),
         function(s){ mkfilen(fp,s)} )
  
  f[['log']] <- mkfilen(gsub('mgs','MGS',fp),'EPxdat','.log')
  f[['willout']] <- sprintf("./willout/%d.%d.MGS.txt",lunaid,vdate)
  f[['willout_fix']] <- sprintf("./willout/%d.%d.FIX.txt",lunaid,vdate)
  # files, eg.
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125.20061021.MGS.raw.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125.20061021.MGS.preproc.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125.20061021.MGS.runData.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125.20061021.MGS.scored.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125.20061021.MGS.stats.txt")
  return(f)
}
xposEP2ASL <- function(EPxpos,xmaxPx=640,xmaxASL=261) {
  # scale from xmax=640(px) to xmax=261(asl)
  xposTargetList <- round(EPxpos * xmaxASL / xmaxPx)
}

# MGSEncode settings
settingsList$MGS <- function(){
  xposScreenList <- c( 40,160,460,600)
  # possible position of targets
  xposTargetList <- xposEP2ASL( xposScreenList  )
  trialTypes <- 'mgs'
  # return in list
  makeList(c("xposTargetList","trialTypes","xposScreenList"))
}

# dani functions depend on some globals
# set them here
setMGSGlobals <- function(){
  task       <<- "MGS"
  settings   <<- settingsList$MGS()
  #taskData   <<- taskList$MGSEncode(path="/Volumes/Phillips/COG")
}

