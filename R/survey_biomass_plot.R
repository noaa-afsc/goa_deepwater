#' grab and plot survey biomass information
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @export
#' 
.get_bts_df <- function(conn = akfin)
{
  # get the bts data
  
  
  #calculate lognormal quantiles:
  index.t <- index.t %>% mutate(lower = qlnorm(0.025,meanlog = log(obs),sdlog = se),
                                upper = qlnorm(1-0.025,meanlog = log(obs),sdlog = se))
}


#' Plot bottom trawl survey biomass
#'
plot_srv_index<-function(new_dat, term_yr=2026,plot_dir='foo')
{
  new_dat2= new_dat %>%
    mutate(lwrb = qlnorm(.025, meanlog=log(obs), sdlog=se_log),
           uprb = qlnorm(1-.025, meanlog=log(obs), sdlog=se_log))
  #           lwrn = qlnorm(.025, meanlog=log(population), sdlog=se_log_num),
  #           uprn = qlnorm(1-.025, meanlog=log(population), sdlog=se_log_num))
  

  
  g=ggplot(new_dat2, aes(x = year, y = obs,
                         ymin = lwrb,
                         ymax = uprb)) +
 #                        fill = as.factor(survey), group =  survey,
#                         col =  survey)) +
    geom_ribbon(col = 'white', alpha = 0.2) +
    geom_point() +
    geom_line() +
    scale_color_viridis_d(name="Survey",option='plasma')+
    scale_fill_viridis_d(name="Survey",option='plasma')+
    labs(x = "Year", y = "GOA Bottom Trawl Survey Biomass (mt)")+theme(legend.position='none') + expand_limits(y=0)
  
  ggsave(plot=g, filename=here::here(plot_dir,"survBiom.png"), units = "in",width = 8, height = 6,bg='white')
  
  # comp_dat=bind_rows(new_dat2,old_dat2) 
  # 
  # #COMPARISON OF OLD AND NEW
  # g2=ggplot(comp_dat, aes(x = year, y = obs,
  #                         ymin = lwrb,
  #                         ymax = uprb,
  #                         fill = as.factor(source), group =  as.factor(source),
  #                         col =  as.factor(source))) +
  #   geom_ribbon(col = 'white', alpha = 0.2) +
  #   geom_point() +
  #   geom_line() +
  #   scale_color_manual(name="Survey",values=c("goldenrod1","navyblue"))+
  #   scale_fill_manual(name="Survey",values=c("goldenrod1","navyblue"))+
  #   #    scale_color_viridis_d(name="Survey",option='plasma')+
  #   #   scale_fill_viridis_d(name="Survey",option='plasma')+
  #   labs(x = "Year", y = "index")+
  #   facet_grid(survey~.,scales='free_y') + expand_limits(y=0) 
  # 
  # ggsave(plot=g2, filename=here::here(plot_dir, "survBiom_comparison.png"), units = "in",width = 7, height = 7,bg='white')
  # p<-list()
  # p$g<-g
 # p$g2<-g2
  g
  return(g)
  
}