##### function to download clean data ####

download_clean_data <- function(start_year = 2011, end_year = 2020) {
  
  if (end_year < start_year) {
    warning('End year must be greater than or equal to start year')
  }
  
  if (start_year < 2011 | start_year > 2020 | end_year < 2011 | end_year > 2020){
    warning('Invalid year(s). Data available from 2011 to 2020')
  }
  
  links <- 
    c(GY_2020 = 
        'https://download.gosa.ga.gov/2020/Graduation_Rate_2020_Dec112020.csv', 
      GY_2019 = 
        'https://download.gosa.ga.gov/2019/Graduation_Rate_2019_Dec2nd_2019.csv', 
      GY_2018 = 
        'https://download.gosa.ga.gov/2018/Graduation_Rate_2018_JAN_24th_2019.csv',
      GY_2017 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2017_DEC_1st_2017.csv', 
      GY_2016 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2016_DEC_1st_2016.csv',
      GY_2015 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2015_DEC_1st_2016.csv', 
      GY_2014 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2014_DEC_1st_2016.csv',
      GY_2013 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2013_DEC_1st_2016.csv',
      GY_2012 = 
        'https://gosa.georgia.gov/sites/gosa.georgia.gov/files/related_files/site_page/Graduation_Rate_2012_4yr.csv',
      GY_2011 = 
        'https://download.gosa.ga.gov/2011/Graduation_Rate_2011_MAR_23_2020.csv'
    )
  
  download_links <- links[paste0('GY_', start_year:end_year)]
  
  map_dfr(download_links, ~rio::import(.x, setclass = "tibble")) %>% 
    janitor::clean_names() %>% 
    mutate(
      student_group = 
        factor(gsub('Grad Rate -', '', label_lvl_1_desc)), 
      num_graduates = program_total, 
      perc_graduate = program_percent,
      prop_graduate = program_percent/100,
      num_students = num_graduates/prop_graduate,
      granularity = detail_lvl_desc,
      grad_year = as.numeric(
        str_replace(long_school_year, '1[:digit:]-', '')
      )
    ) %>% 
    filter(
      ## only want schools (not districts or whole state)
      granularity == 'School' &  
        ## eliminate schools which couldn't have graduation 
        ## (i.e., non high schools)
        grepl(pattern = '12' , x = grades_served_desc) == T
    ) %>% 
    select(
      instn_name, 
      grad_year,
      student_group, 
      num_students, 
      num_graduates, 
      perc_graduate, 
      grades_served_desc,
    )
}



##### function to select group of interest #####

select_groups <- 
  function(
    cleaned_data_frame, 
    groups_of_interest = unique(cleaned_data_frame$student_group)
    ){
  
  stu_group_options <- 
    gsub( 
      pattern = 'Grad Rate -', 
      replacement = '',
      x = unique(cleaned_data_frame$student_group)
    )
  
  check_group <- groups_of_interest %in% stu_group_options
  
  if (any(check_group == F)){
    
    NA_group <- groups_of_interest[which (check_group == F)]
    
    length_NA <- length(NA_group)
    
    print_NA_group <- 
      printList(
        toPrint = NA_group, 
        finalSepWord = "and", 
        midSep = ","
      )
    
    warning(
      length_NA , 
      ' specified group (or groups) not available: ',
      print_NA_group,
      '. There are ',
      length(stu_group_options), 
      ' groups available for subsetting: ',
      knitr::combine_words(stu_group_options),
      '. Please select one or more of these groups.'
      
    )}
  
  cleaned_data_frame %>% 
    filter(student_group %in% groups)
  
}

##### funciton to make plot titles #####

make_plot_titles <- 
  function(df){
    df %>% 
      mutate(
        student_group_label =  
          trimws(
            gsub(
              pattern = 'Students', 
              replacement = '', 
              student_group)
          ),
        student_group_label = str_to_sentence(student_group_label),
        student_group_label = 
          gsub(
            pattern = ' ', 
            replacement = '\n', 
            student_group_label
          ), 
        title = 
          glue::glue('Percent Graduation of Student Groups for\n{instn_name}\nfrom {min(grad_year)} to {max(grad_year)}')
      ) 
  }


##### function to make plots #####

grad_year_plots <- 
  function(
    cleaned_data_frame, 
    groups_of_interest = unique(cleaned_data_frame$student_group)
    )
{
  cleaned_data_frame %>% 
    select_groups(groups_of_interest = groups_of_interest) %>% 
    drop_na(student_group, perc_graduate) %>%
    group_by(instn_name, student_group) %>% 
    mutate(
      average_grad = mean(perc_graduate),
      se_grad = sd(perc_graduate)/sqrt(n()),
      lower_ci_90 =
        average_grad - 1.65*se_grad,
      upper_ci_90 =
        average_grad + 1.65*se_grad,
      lower_ci_95 =
        average_grad - 1.96*se_grad,
      upper_ci_95 =
        average_grad + 1.96*se_grad,
      lower_ci_99 =
        average_grad - 2.58*se_grad,
      upper_ci_99 =
        average_grad + 2.58*se_grad,
    ) %>%
    ungroup() %>% 
    make_plot_titles() %>% 
    nest_by(instn_name, title) %>% 
    transmute(
      plot = 
        list(
          ggplot(
            data = data,
            aes(
              y = 
                fct_reorder(
                  student_group_label, 
                  average_grad
                ),
              x = average_grad, 
              color = student_group)
          ) +
            geom_vline(
              aes(
                xintercept = mean(average_grad)
              )
            ) +
            geom_errorbar(
              aes(
                xmin = lower_ci_90, 
                xmax = upper_ci_90
              ),
              width = 0.2, 
              alpha = 0.1
            ) +
            geom_errorbar(
              aes(
                xmin = lower_ci_95, 
                xmax = upper_ci_95
              ),
              width = 0.4, 
              alpha = 0.2
            ) +
            geom_errorbar(
              aes(
                xmin = lower_ci_99, 
                xmax = upper_ci_99
              ),
              width = 0.6, 
              alpha = 0.6
            ) +
            geom_point() +
            theme_minimal() + 
            theme(
              axis.text.y = 
                element_text(
                  vjust = 0, 
                  size = 7
                ), 
              legend.position = 'none',
              plot.title.position = 'plot'
            ) +
            colorblindr::scale_fill_OkabeIto() + 
            colorblindr::scale_color_OkabeIto() +
            labs(
              y =  NULL,
              x = 'Percent Graduation', 
              caption = 'Horizontal line = average percent graduation of all groups
source = https://gosa.georgia.gov/dashboards-data-report-card/downloadable-data
Error bars represent 90%, 95%, & 99% CIs', 
              title = title)
        )) %>% 
    ungroup() %>% 
    select(-title)
}