##make sure you first run the functions.R document before attempt to run this file##

theme_set(theme_gray(base_family='Malgun Gothic'))
par(family='Malgun Gothic')

################################ Basic EDA ################################

#load dataset
grad_history <- prepare_grad_history("D:/code/data_project/ergm/grad_history.xlsx")
grad_info <- prepare_grad_info("D:/code/data_project/ergm/grad_info.xlsx")
grad_merge <- merge_grad_datasets(grad_history, grad_info)

#Show first few rows
grad_history %>% head()
grad_info %>% head()
grad_merge %>% head()

#Longitudinal EDA
grad_year <-grad_history %>% 
  count(year) %>%
  mutate(prop=prop.table(n)) #total n by year
grad_year %>% ggplot(aes(year, n)) + geom_line() + geom_text(aes(label=n), vjust = -1.0) #total n by year, line graph

#Longitudinal EDA by student department
grad_year_student_department <- grad_history %>% 
  count(year, student_department) %>%
  group_by(year) %>%
  mutate(prop = prop.table(n)) #n by student department from 2011 to 2021

show_student_department(grad_year_student_department, 2010)
show_student_department(grad_year_student_department, 2011)
show_student_department(grad_year_student_department, 2012)
show_student_department(grad_year_student_department, 2013)
show_student_department(grad_year_student_department, 2014)
show_student_department(grad_year_student_department, 2015)
show_student_department(grad_year_student_department, 2016)
show_student_department(grad_year_student_department, 2017)
show_student_department(grad_year_student_department, 2018)
show_student_department(grad_year_student_department, 2019)
show_student_department(grad_year_student_department, 2020)
show_student_department(grad_year_student_department, 2021) #barplot for n by student department

grad_year_student_department %>%
  ggplot(aes(x = year, y = n, color = student_department)) +
  geom_line() +
  facet_wrap(~ student_department) #empirical growth plots by student department

#Longitudinal EDA by class department
grad_year_class_department <- grad_history %>% 
  count(year, class_department) %>%
  group_by(year) %>%
  mutate(prop = prop.table(n)) #n by class department from 2011 to 2021

show_class_department(grad_year_class_department, 2010)
show_class_department(grad_year_class_department, 2011)
show_class_department(grad_year_class_department, 2012)
show_class_department(grad_year_class_department, 2013)
show_class_department(grad_year_class_department, 2014)
show_class_department(grad_year_class_department, 2015)
show_class_department(grad_year_class_department, 2016)
show_class_department(grad_year_class_department, 2017)
show_class_department(grad_year_class_department, 2018)
show_class_department(grad_year_class_department, 2019)
show_class_department(grad_year_class_department, 2020)
show_class_department(grad_year_class_department, 2021) #barplot for n by class department

grad_year_class_department %>%
  ggplot(aes(x = year, y = n, color = class_department)) +
  geom_line() +
  facet_wrap(~ class_department) #empirical growth plots by student department


#Treemap
grad_by_department <- grad_history %>% 
                        count(student_department, student_major) %>%
                        group_by(student_department) %>%
                        mutate(prop = prop.table(n))

grad_by_department %>% treemap(index=c("student_department", "student_major"), 
                                vSize = "n",  type="value",vColor = "n", bg.labels="yellow",
                                fontfamily.labels="AppleGothic",
                                align.labels=list(c("right", "center"), c("left", "top")),
                                fontsize.labels=12,
                                lowerbound.cex.labels= 0.2)

grad_by_division <- grad_history %>% 
                      count(student_department, student_major) %>%
                      group_by(student_department) %>%
                      mutate(prop = prop.table(n))

grad_by_division %>% treemap(index=c("student_department", "student_major"), 
                             vSize = "n",  type="value",vColor = "n", bg.labels="yellow",
                             fontfamily.labels="AppleGothic",
                             align.labels=list(c("right", "center"), c("left", "top")),
                             fontsize.labels=12,
                             lowerbound.cex.labels= 0.2)

grad_year_student_department %>% treemap(index=c("year", "student_department"), 
                                          vSize = "n",  type="value",vColor = "n", bg.labels="yellow",
                                          fontfamily.labels="AppleGothic",
                                          align.labels=list(c("right", "center"), c("left", "top")),
                                          fontsize.labels=12,
                                          lowerbound.cex.labels= 0.1)

grad_year_class_department %>% treemap(index=c("year", "class_department"), 
                                         vSize = "n",  type="value",vColor = "n", bg.labels="yellow",
                                         fontfamily.labels="AppleGothic",
                                         align.labels=list(c("right", "center"), c("left", "top")),
                                         fontsize.labels=12,
                                         lowerbound.cex.labels= 0.1)





