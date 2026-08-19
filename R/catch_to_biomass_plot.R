library(r4ss)
library(spmR)
library(dplyr)
library(viridis)     
library(hrbrthemes)  
library(scales)
catch_to_biomass_plot<-function(hp_dir,model_dir,plot_dir,current_year) {

  #read in model using r4ss
  output<-r4ss::SS_output(dir = model_dir) 
  
  model_biomass<-output$timeseries |>
            dplyr::select(c(Yr,Bio_smry)) |>
            dplyr::filter(Yr>=output$startyr,Yr<=output$endyr)
  
  model_df<-output$catch |>
              dplyr::filter(Fleet == 1, Yr>=output$startyr, Yr<output$endyr) |>
              dplyr::select(c(Yr,Obs)) |>
              dplyr::left_join(model_biomass) |>
              dplyr::rename(year= Yr,tot_bio = Bio_smry,catch = Obs)
    
  
  #read in projection modeling results and input
  df <- readr::read_csv(file.path(hp_dir, "spm_detail.csv"))
  input <- spmR::dat2list(file.path(hp_dir, "spm.dat"))
  
  hp_df<-df |>
        dplyr::filter(Alt==2,Year>=output$endyr) |>
        dplyr::select(c(Year,Tot_biom,Catch)) |>
        dplyr::rename(year = Year) |>
        dplyr::group_by(year) |>
        dplyr::summarise(tot_bio = mean(Tot_biom)*input$scalars,catch = mean(Catch)*input$scalars)
        
  #cobble together the data from the model and projection period and make the ratio
  all_df <-model_df |>
           dplyr::bind_rows(hp_df) |>
           mutate(ratio = catch/tot_bio) |>
           filter(year<=current_year+2)
  
  #plot the catch to biomass ratio
      
  # Load libraries
  library(ggplot2)
  library(viridis)     
  library(hrbrthemes)  
  library(scales)      
  

  
  # Calculate the upper limit dynamically (0.1 above the maximum ratio value)
  y_max_limit <- max(all_df$ratio) + 0.01
  
  # 2. Build the precise document plot
  p<-ggplot(all_df, aes(x = year, y = ratio)) +
 #   # Add the vertical Y-axis line at 1978
    geom_vline(xintercept = 1978, color = "#333333", linewidth = 0.5) +
    geom_hline(yintercept = 0.0, color = "#333333", linewidth = 0.5) +
    
    geom_line(color = "#34495e", linewidth = 0.6) + 
    geom_point(aes(color = ratio), size = 1.8) +   
    scale_color_viridis_c(option = "plasma", name = "Ratio:") +
    
    # X-axis: Explicitly start at 1978, end at 2028, breaking every 10 years
    scale_x_continuous(
      limits = c(1978, 2028),
      breaks = c(1978, seq(1988, 2018, by = 10), 2028)
    ) + 
    
    # Y-axis: Starts at 0 and dynamically caps exactly 0.1 above the max value
    scale_y_continuous(
      limits = c(0, y_max_limit), 
      breaks = seq(0, y_max_limit, by = 0.02)
    ) +
    
    theme_ipsum(
      base_family = "Arial",   
      base_size = 10,          
      axis_title_size = 10,    
      plot_title_size = 12     
    ) + 
    labs(
   #   title = "Figure 1: Annual Ratio Changes (1978 - 2028)",
  #    subtitle = "Simulated time series with customized mathematical axis constraints.",
      x = "Year",
      y = "Ratio"
    ) +
    theme(
    #  plot.title.position = "plot",                 
    #  plot.title = element_text(face = "bold"),
    #  plot.subtitle = element_text(size = 9, color = "#555555"),
      
      # Remove background horizontal grid lines
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Perfect centering and spacing for the X-axis label
      axis.title.x = element_text(hjust = 0.5, margin = margin(t = 8)), 
      axis.title.y = element_text(hjust = 0.5, margin = margin(r = 6)),
      
      # Legend settings
      legend.position = "right",                    
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),          
      legend.key.width = unit(0.2, "cm")
    )
  ggsave(filename = file.path(plot_dir,"catch_to_biomass_plot.png"),type = "png")
return(p)  
}