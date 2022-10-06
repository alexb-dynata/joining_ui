
setwd("C:/Users/alex.bass/projects/Joining_UI")

#load excel
df <- openxlsx::read.xlsx("dm.xlsx")

df


split(df, zdf$question_id)[[1]]

df <- openxlsx::read.xlsx("dm.xlsx") |>
    dplyr::mutate(inx = factor(question_id, levels = unique(question_id))) |>
    dplyr::arrange(factor(variable_id,
                          levels = unique(naturalsort::naturalsort(variable_id)))) |>
    dplyr::arrange(inx) |>
    dplyr::select(-inx)

df_list <- split(df, df$question_id)

df_list[[1]] |> 
  dplyr::select(variable_id, answer_code)

df_list[[1]] |> 
  dplyr::select(question_label1, question_type1, answer_label1)

df_list[[1]] |> 
  dplyr::select(question_label2, question_type2, answer_label2)



df <- openxlsx::read.xlsx("dm.xlsx") |>
  dplyr::mutate(inx = factor(question_id, levels = unique(question_id))) |>
  dplyr::arrange(factor(variable_id,
                        levels = unique(naturalsort::naturalsort(variable_id)))) |>
  dplyr::arrange(inx) |>
  dplyr::select(-inx)

df_spl <- split(df, df$question_id)


df_spl[c(1,2,3)]
