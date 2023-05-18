# Rendering
library(quarto)

#loop for html output - for this format I could render correctly
for(season_no_ in seq(1,8)) {
  
  quarto_render("Assignment.qmd", execute_params = list(
    season_no = season_no_,
    printcode = FALSE
  ), output_format = "html",
  output_file = paste0("Assignment_season_", season_no_, ".html"))
  
}


#loop for pdf output 
#for(season_no_ in seq(1,8)) {
  
#  quarto_render("Assignment.qmd", execute_params = list(
#    season_no = season_no_,
#    printcode = FALSE
#  ), output_format = "pdf",
#  output_file = paste0("Assignment_season_", season_no_, ".pdf"))
  
#}

