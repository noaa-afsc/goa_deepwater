# Year, Expected Catch, OFL, ABC, Mean SSB, Mean Relative Spawning Biomass 
#(ratio of female spawning biomass to equilibrium unfished female spawning biomass (B100%)


five_year_hp_table<-function(hp_dir) {
  library(readr)
  library(spmR)
  library(tidyr)
  library(dplyr)
  df <- readr::read_csv(file.path(hp_dir, "spm_detail.csv"))
  input <- spmR::dat2list(file.path(hp_dir, "spm.dat"))
  scalar<-input$scalars
    short_df<-readr::read_csv(file.path(hp_dir, "spm_summary.csv"))
  
  ref_pts <-short_df |>
   dplyr::filter(is.na(Alt)) |>
    select(-c(spp_file,Alt,Year)) |>
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  five_yr<-df |>
          dplyr::filter(Year>end_year,Year<=end_year+5,Alt==3) |>
          dplyr::select(c(Year,Alt,Sim,Catch,OFL,ABC,SSB,B100)) |>
          dplyr::group_by(Year) |>
          dplyr::summarise(mean_catch = mean(Catch*scalar),mean_OFL = mean(OFL*scalar), mean_ABC = mean(ABC*scalar),
                           mean_ssb = mean(SSB*scalar),mean_rel_ssb = mean(SSB/B100))
  write.csv(five_yr,file.path(hp_dir,"five_year_table.csv"))          
 return(five_yr)
}