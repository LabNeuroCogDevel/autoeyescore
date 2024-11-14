#!/usr/bin/env Rscript

# 20241114WF - init
#   
source('dollarreward.R')



#' asc file name to 12345_20231231_run-1
id_from_fname <- function(ascfile) 
   gsub('.*(\\d{5}_\\d{8}).*run.(\\d+).*','\\1_run-\\2',ascfile)

# construct consistant output name (and dir) 
scored_file_from_asc <- function(ascfile){
   sesroot <- gsub('(MR/\\d{5}_\\d{8}/).*','\\1',dirname(ascfile))
   score_path <- file.path(sesroot,"Scored")
   ld8_run <- id_from_fname(ascfile)
   score_name <- paste0(ld8_run, "_score.txt")
   score_file <- file.path(score_path, score_name)
}

#' read in or score and save 
#' @param ascfile raw EyeLink input file
#' @return dataframe with 10 cols including 'Count' score
score_from_or_save <- function(ascfile){
   # /Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/12100_20241105/
   score_file <- scored_file_from_asc(ascfile)
   if(file.exists(score_file)) return(read.table(score_file,sep="\t",header=T))
   dir.create(dirname(score_file), recursive=TRUE, showWarnings=FALSE)
   scored <- tryCatch(score_file_antiDR(ascfile),error=function(e){print(e);return(NULL)})
   if(is.null(scored)) return(scored)
   scored$file <- id_from_fname(ascfile)
   write.table(scored, score_file, sep="\t", quote=F, row.names=F)
   cat("# writting ",score_file,"\n")
   return(scored)
}

score_all_DR_MR <- function(){
  #all_edf <- Sys.glob('/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/1*_2*/*.edf')
  all_edf <- system(intern=T, "find /Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/ -iname '*edf'")
  # dont run if mounts are messed up
  stopifnot(length(all_edf)>2L)

  all_asc <- lapply(all_edf, make_asc)

  # debugging, ensure all are saved same place
  #ideal_names <- lapply(all_asc, scored_file_from_asc) 

  all_scored_list <- lapply(all_asc, score_from_or_save)
}

if (sys.nframe() == 0){
   args <- commandArgs(trailingOnly = FALSE)
   score_all_DR_MR()|>
      dplyr::bind_rows() |>
      write.table(file="/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/all_scored.csv",
                  sep="\t", quote=F, row.names=F)
}

