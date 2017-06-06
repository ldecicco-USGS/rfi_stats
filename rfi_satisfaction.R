library(googlesheets)
library(dplyr)

gs_auth()

my_sheets <- gs_ls()

scorecards <- grep(pattern = "Scorecard",x = my_sheets$sheet_title)


scoreAqu<- gs_title(my_sheets$sheet_title[scorecards[1]])

massive_score <- data.frame()

for(i in gs_ws_ls(scoreAqu)[1:12]){

  individual_score <- gs_read(scoreAqu, ws = i, col_names = TRUE, skip = 1)
  individual_score$person <- i
  if(i == "Melanie"){
    co_types <- lapply(individual_score, class)
  } else {
    co_types_new <- lapply(individual_score, class)
    change_col_type <- unlist(lapply(names(co_types), function(x) co_types[[x]] == co_types_new[[x]]))
    for(k in which(!change_col_type)){
      class(individual_score[[k]]) <- co_types[[k]]
    }
  }
  
  massive_score <- bind_rows(massive_score, individual_score)
  Sys.sleep(time = 6)
}

must_haves <- massive_score %>%
  filter(`Final Requirement Level` == "Must have") %>%
  group_by(Category) %>%
  summarise(count = n())

sumStat <- table(select(massive_score, `Final Requirement Level`, Category, `Feature Satisfaction`)) %>%
  data.frame() %>%
  arrange(Category, Feature.Satisfaction)

sum_ws <- gs_ws_new(scoreAqu, ws_title = "sumStat", input = sumStat)

