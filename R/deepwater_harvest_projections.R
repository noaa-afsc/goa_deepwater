# deepwater harvest projections
library(dplyr)
library(spmR)
library(tidyr)
library(ggplot2)
library(stringr)
#library(readxl)
source("C:/GitProjects/goa_deepwater/R/make_exec_table_spm.R", echo=TRUE)

# #read in weekly data:
# SELECT
# council.comprehensive_blend_ca.week_end_date,
# council.comprehensive_blend_ca.retained_or_discarded,
# council.comprehensive_blend_ca.weight_posted,
# council.comprehensive_blend_ca.year,
# council.comprehensive_blend_ca.fmp_area,
# council.comprehensive_blend_ca.agency_species_code,
# council.comprehensive_blend_ca.species_group_code,
# council.comprehensive_blend_ca.fmp_subarea,
# council.comprehensive_blend_ca.reporting_area_code,
# council.comprehensive_blend_ca.species_name,
# council.comprehensive_blend_ca.species_group_name
# FROM
# council.comprehensive_blend_ca
# WHERE
# council.comprehensive_blend_ca.fmp_area = 'GOA'
# AND  council.comprehensive_blend_ca.agency_species_code = 124


the_dir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2025\\deepwater\\data"
run_dir<-"C:\\Users\\carey.mcgilliard\\Work\\FlatfishAssessments\\2025\\deepwater\\harvest_projections"
endyr<-2025


#sql data
the_data<-read.csv(file.path(the_dir,"dover_only.csv"),header = TRUE) %>%
  rename_with(tolower) %>%
  mutate(weight_posted = ifelse(is.na(weight_posted), 0, weight_posted)) %>%
  mutate(day = substring(week_end_date,1,2),month = substring(week_end_date,4,6))



#find the latest data (this needs to be generalized to work every year)
late<-the_data %>% filter(year == endyr,month=="SEP") %>%
  mutate(end_date= max(as.numeric(day)))

end_day<-unique(late$end_date)
end_month<-unique(late$month)

endyr_dat<-the_data %>% filter(month == "SEP" | month == "OCT" | month == "NOV" | month == "DEC") %>%
  select(c(year,agency_species_code,weight_posted)) %>%
  group_by(year,agency_species_code) %>%
  summarize(endyr_weight = sum(weight_posted))


past_dat<-the_data %>% select(c(year,agency_species_code,weight_posted)) %>%
  group_by(year,agency_species_code) %>%
  summarize(tot_weight = sum(weight_posted)) %>%
  left_join(endyr_dat) %>%
  mutate(prop_endyr = endyr_weight/tot_weight) %>%
  filter(!is.na(prop_endyr),year<endyr)

mean_prop<-past_dat %>% group_by(agency_species_code) %>%
                        summarize(mean_prop = mean(prop_endyr))


est_catch<-the_data %>% group_by(year,agency_species_code) %>%
         summarize(tot_weight = sum(weight_posted)) %>%
         filter(year == endyr) %>%
         left_join(mean_prop) %>%
         mutate(est_weight = tot_weight/(1-mean_prop))

write.csv(past_dat,file.path(the_dir,"past_catch.csv"))
write.csv(est_catch,file.path(the_dir,"current_estimated_catch.csv"))

the_codes<-the_data %>% distinct(agency_species_code,species_name)
all_dat <-the_data %>% group_by(year,agency_species_code) %>%
           summarize(tot_weight=sum(weight_posted)) %>%
           pivot_wider(names_from = agency_species_code,values_from = tot_weight)
write.csv(all_dat,file.path(the_dir,"yearly_catch_by_species.csv"))
write.csv(the_codes,file.path(the_dir,"species_names_and_codes.csv"))
#Find average complete catches for the last 5 years
avg_catches<-past_dat %>% filter(year>=endyr-5) %>% ungroup() %>%
  group_by(agency_species_code) %>%
  summarize(avg_catches = mean(tot_weight))

write.csv(avg_catches,file = file.path(the_dir,"five_yr_avg_catch.csv"))

#enter estimated catches to projection file
exec<-make_tier3a_exec_table(run_dir,endyr=2025,the_scalar=1000)

#get the stuff for total biomass

bdf <- runSPM(run_dir,run=FALSE)
df <- readr::read_csv(file.path(run_dir,"spm_summary.csv"))
inp <- spmR::dat2list(file.path(run_dir,"spm.dat"))
fc <- data.frame(Year=inp$fixed_catch[,1], mean=inp$fixed_catch[,2], type='C')# |>
names(fc) <- c("Year", "mean", "type")
fc <- fc |> expand_grid(Alt=c(1,3,5,7)) |> mutate(Alt=as.factor(Alt), ub=mean,lb=mean)
proj_plot<-plotSPM(df) + geom_point(data=fc,aes(x=Year,y=mean)) + theme_classic()
ggsave(proj_plot,file = file.path(run_dir,"spm_plot.png"),device = "png",height= 6, width = 8)

tot_biom<-bdf %>% select(c(Year,Alt,Sim,SSB,F,Tot_biom,OFL,ABC)) %>% filter(Alt==2) %>%
  group_by(Year,Alt) %>%
  summarize(meanTot_biom = mean(Tot_biom*1000))
write.csv(tot_biom,file.path(run_dir,"tot_biom_df.csv"))
names(tot_biom)<-c("Year","Alt","tot_biom")
write.csv(tot_biom,file.path(run_dir,"tot_biom.csv"))



