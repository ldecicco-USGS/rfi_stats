library(googlesheets)
library(dplyr)
library(tidyr)


gs_auth()

my_sheets <- gs_ls()

scorecards <- my_sheets$sheet_title[grep(pattern = "Scorecard",x = my_sheets$sheet_title)]
scorecards <- scorecards[scorecards != "Scorecard_Original"]
massive_score <- data.frame()

for(vendor in scorecards){
  card_title <- gs_title(vendor)

  for(i in gs_ws_ls(card_title)[1:12]){
  
    individual_score <- gs_read(card_title, ws = i, col_names = TRUE, skip = 1)
    individual_score$person <- i
    individual_score$vender <- vendor
    if(i == "Melanie" & vendor == "Aquatic Informatics Scorecard"){
      co_types <- lapply(individual_score, class)
    } else {
      co_types_new <- lapply(individual_score, class)
      change_col_type <- unlist(lapply(names(co_types), function(x) co_types[[x]] == co_types_new[[x]]))
      if(sum(!change_col_type) > 0){
        for(k in which(!change_col_type)){
          class(individual_score[[k]]) <- co_types[[k]]
        }        
      }
    }
    
    massive_score <- bind_rows(massive_score, individual_score)
    Sys.sleep(time = 6)
  }
}

write.csv(massive_score, file = "massive_score_table.csv", row.names = FALSE)

must_haves <- massive_score %>%
  filter(`Final Requirement Level` == "Must have") %>%
  group_by(vender, Category) %>%
  summarise(count = n())

sumStat <- table(select(massive_score, vender, `Final Requirement Level`, Category, `Feature Satisfaction`)) %>%
  data.frame() %>%
  filter(Freq > 0) %>%
  arrange(Category, Feature.Satisfaction)

satisfaction <- select(massive_score, vender, `Final Requirement Level`, Category, `Satisfaction Value`, `Feature Present Value`, `Feature Present`) %>%
  mutate(Feature = ifelse(`Feature Present Value` == "FALSE", NA, as.numeric(`Feature Present Value`))) %>%
  mutate(SatValue = ifelse(`Satisfaction Value` == "FALSE", NA, as.numeric(`Satisfaction Value`))) %>%
  mutate(multiplier = Feature * SatValue) %>%
  group_by(vender, Category, `Final Requirement Level`) %>%
  summarize(agStat = sum(multiplier, na.rm = TRUE)/sum(!is.na(Feature)),
            count = sum(!is.na(Feature))) %>%
  arrange(`Final Requirement Level`, desc(agStat) )

satisfaction_wide <- satisfaction %>%
  select(-count) %>%
  spread(key = vender, value = agStat) %>%
  filter(`Final Requirement Level` != "Pass") %>%
  arrange(desc(`Final Requirement Level`), desc(`Aquatic Informatics Scorecard`))

write.csv(sumStat, file="sumStat.csv", row.names = FALSE)



library(DT)
library(RColorBrewer)
library(htmlwidgets)
tableSumm <- datatable(satisfaction_wide, options = list(pageLength = nrow(satisfaction_wide)))
ignoreIndex <- 1:2
tableSumm <- formatRound(tableSumm, names(satisfaction_wide)[-ignoreIndex], 2)

colors <- brewer.pal(ncol(satisfaction_wide)-2,"Set3")
names(colors) <- names(satisfaction_wide)[3:ncol(satisfaction_wide)]
for(i in names(satisfaction_wide)[3:ncol(satisfaction_wide)]){
  tableSumm <- formatStyle(tableSumm, as.character(i),
                          background = styleColorBar(range(matrix(satisfaction_wide[3:ncol(satisfaction_wide)]), na.rm = TRUE), 
                                                     colors[as.character(i)]),
                          backgroundSize = '100% 90%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center' )
}
tableSumm
saveWidget(tableSumm, file = "satisfaction.html")
