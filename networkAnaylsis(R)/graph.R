##make sure you first run the functions.R document before attempt to run this file##

theme_set(theme_gray(base_family='Malgun Gothic'))
par(family='Malgun Gothic')

################################ Basic EDA ################################

#load dataset
grad_history = prepare_grad_history("D:/code/data_project/ergm/grad_history.xlsx")
grad_info = prepare_grad_info("D:/code/data_project/ergm/grad_info.xlsx")
grad_merge = merge_grad_datasets(grad_history, grad_info)


#Show first few rows
grad_history %>% head()
grad_info %>% head()
grad_merge %>% head()

################################ Exploratory network analysis ################################
#prepare edge lists and the corresponding networks (student_division)
#for bipartite to one-mode conversion please consult the link below
#https://solomonmg.github.io/post/working-with-bipartite-affiliation-network-data-in-r/

grad_history_2010 = history_year(grad_history, 2010)
grad_history_2011 = history_year(grad_history, 2011)
grad_history_2012 = history_year(grad_history, 2012)
grad_history_2013 = history_year(grad_history, 2013)
grad_history_2014 = history_year(grad_history, 2014)
grad_history_2015 = history_year(grad_history, 2015)
grad_history_2016 = history_year(grad_history, 2016)
grad_history_2017 = history_year(grad_history, 2017)
grad_history_2018 = history_year(grad_history, 2018)
grad_history_2019 = history_year(grad_history, 2019)
grad_history_2020 = history_year(grad_history, 2020)
grad_history_2021 = history_year(grad_history, 2021)

edge_list_2010 = edge_list(grad_history_2010)
edge_list_2011 = edge_list_class(grad_history_2011)
edge_list_2012 = edge_list(grad_history_2012)
edge_list_2013 = edge_list(grad_history_2013)
edge_list_2014 = edge_list(grad_history_2014)
edge_list_2015 = edge_list(grad_history_2015)
edge_list_2016 = edge_list_class(grad_history_2016)
edge_list_2017 = edge_list(grad_history_2017)
edge_list_2018 = edge_list(grad_history_2018)
edge_list_2019 = edge_list(grad_history_2019)
edge_list_2020 = edge_list(grad_history_2020)
edge_list_2021 = edge_list_class(grad_history_2021)
edge_list_all = edge_list(grad_history)

division_matrix_adj_2010 = create_onemode_byDivison(edge_list_2010)
division_matrix_adj_2011 = create_onemode_byDivison(edge_list_2011)
division_matrix_adj_2012 = create_onemode_byDivison(edge_list_2012)
division_matrix_adj_2013 = create_onemode_byDivison(edge_list_2013)
division_matrix_adj_2014 = create_onemode_byDivison(edge_list_2014)
division_matrix_adj_2015 = create_onemode_byDivison(edge_list_2015)
division_matrix_adj_2016 = create_onemode_byDivison(edge_list_2016)
division_matrix_adj_2017 = create_onemode_byDivison(edge_list_2017)
division_matrix_adj_2018 = create_onemode_byDivison(edge_list_2018)
division_matrix_adj_2019 = create_onemode_byDivison(edge_list_2019)
division_matrix_adj_2020 = create_onemode_byDivison(edge_list_2020)
division_matrix_adj_2021 = create_onemode_byDivison(edge_list_2021)
division_matrix_adj_all = create_onemode_byDivison(edge_list_all)

#sample network 
#please consult https://rpubs.com/pjmurphy/542335

net_2010 = create_net(division_matrix_adj_2010)
net_2011 = create_net(division_matrix_adj_2011)
net_2012 = create_net(division_matrix_adj_2012)
net_2013 = create_net(division_matrix_adj_2013)
net_2014 = create_net(division_matrix_adj_2014)
net_2015 = create_net(division_matrix_adj_2015)
net_2016 = create_net(division_matrix_adj_2016)
net_2017 = create_net(division_matrix_adj_2017)
net_2018 = create_net(division_matrix_adj_2018)
net_2019 = create_net(division_matrix_adj_2019)
net_2020 = create_net(division_matrix_adj_2020)
net_2021 = create_net(division_matrix_adj_2021)
net_all = create_net(division_matrix_adj_all)

plot_net(net_2010)
plot_net(net_2011)
plot_net(net_2012)
plot_net(net_2013)
plot_net(net_2014)
plot_net(net_2015)
plot_net(net_2016)
plot_net(net_2017)
plot_net(net_2018)
plot_net(net_2019)
plot_net(net_2020)
plot_net(net_2021)
plot_net(net_all)

#calculate centrality indices
#create division lists by year

division_list_2010 = division_list_year(net_2010)
division_list_2011 = division_list_year(net_2011)
division_list_2012 = division_list_year(net_2012)
division_list_2013 = division_list_year(net_2013)
division_list_2014 = division_list_year(net_2014)
division_list_2015 = division_list_year(net_2015)
division_list_2016 = division_list_year(net_2016)
division_list_2017 = division_list_year(net_2017)
division_list_2018 = division_list_year(net_2018)
division_list_2019 = division_list_year(net_2019)
division_list_2020 = division_list_year(net_2020)
division_list_2021 = division_list_year(net_2021)
division_list_all = division_list_year(net_all)

#degree, betweenness, closeness, eigen

division_centrality_2010 = division_centrality_year(net_2010, division_list_2010)
division_centrality_2011 = division_centrality_year(net_2011, division_list_2011)
division_centrality_2012 = division_centrality_year(net_2012, division_list_2012)
division_centrality_2013 = division_centrality_year(net_2013, division_list_2013)
division_centrality_2014 = division_centrality_year(net_2014, division_list_2014)
division_centrality_2015 = division_centrality_year(net_2015, division_list_2015)
division_centrality_2016 = division_centrality_year(net_2016, division_list_2016)
division_centrality_2017 = division_centrality_year(net_2017, division_list_2017)
division_centrality_2018 = division_centrality_year(net_2018, division_list_2018)
division_centrality_2019 = division_centrality_year(net_2019, division_list_2019)
division_centrality_2020 = division_centrality_year(net_2020, division_list_2020) #과목 특성쪽으로 보는 것도 좋을 거 같음
division_centrality_2021 = division_centrality_year(net_2021, division_list_2021)
division_centrality_all = division_centrality_year(net_all, division_list_all)

write.xlsx(division_centrality_2010, "D:/code/data_project/ergm/classdivision_centrality_2010.xlsx")
write.xlsx(division_centrality_2011, "D:/code/data_project/ergm/classdivision_centrality_2011.xlsx")
write.xlsx(division_centrality_2012, "D:/code/data_project/ergm/classdivision_centrality_2012.xlsx")
write.xlsx(division_centrality_2013, "D:/code/data_project/ergm/classdivision_centrality_2013.xlsx")
write.xlsx(division_centrality_2014, "D:/code/data_project/ergm/classdivision_centrality_2014.xlsx")
write.xlsx(division_centrality_2015, "D:/code/data_project/ergm/classdivision_centrality_2015.xlsx")
write.xlsx(division_centrality_2016, "D:/code/data_project/ergm/classdivision_centrality_2016.xlsx")
write.xlsx(division_centrality_2017, "D:/code/data_project/ergm/classdivision_centrality_2017.xlsx")
write.xlsx(division_centrality_2018, "D:/code/data_project/ergm/classdivision_centrality_2018.xlsx")
write.xlsx(division_centrality_2019, "D:/code/data_project/ergm/classdivision_centrality_2019.xlsx")
write.xlsx(division_centrality_2020, "D:/code/data_project/ergm/classdivision_centrality_2020.xlsx")
write.xlsx(division_centrality_2021, "D:/code/data_project/ergm/classdivision_centrality_2021.xlsx")
write.xlsx(division_centrality_all, "D:/code/data_project/ergm/classdivision_centrality_all.xlsx")


#k-cores for finding subgroups
kc_2010 = kcores(net_2010, mode = "graph", cmode = "freeman") 
  kc_2010
kc_2011 = kcores(net_2011, mode = "graph", cmode = "freeman") 
  kc_2011
kc_2012 = kcores(net_2012, mode = "graph", cmode = "freeman") 
  kc_2012
kc_2013 = kcores(net_2013, mode = "graph", cmode = "freeman") 
  kc_2013
kc_2014 = kcores(net_2014, mode = "graph", cmode = "freeman") 
  kc_2014
kc_2015 = kcores(net_2015, mode = "graph", cmode = "freeman") 
  kc_2015
kc_2016 = kcores(net_2016, mode = "graph", cmode = "freeman") 
  kc_2016
kc_2017 = kcores(net_2017, mode = "graph", cmode = "freeman") 
  kc_2017
kc_2018 = kcores(net_2018, mode = "graph", cmode = "freeman") 
  kc_2018
kc_2019 = kcores(net_2019, mode = "graph", cmode = "freeman") 
  kc_2019
kc_2020 = kcores(net_2020, mode = "graph", cmode = "freeman") 
  kc_2020
kc_2021 = kcores(net_2021, mode = "graph", cmode = "freeman") 
  kc_2021
kc_all = kcores(net_all, mode = "graph", cmode = "freeman") 
  kc_all
  
plot_net_kc(net_2010, kc_2010)
plot_net_kc(net_2011, kc_2011)
plot_net_kc(net_2012, kc_2012)
plot_net_kc(net_2013, kc_2013)
plot_net_kc(net_2014, kc_2014)
plot_net_kc(net_2015, kc_2015)
plot_net_kc(net_2016, kc_2016)
plot_net_kc(net_2017, kc_2017)
plot_net_kc(net_2018, kc_2018)
plot_net_kc(net_2019, kc_2019)
plot_net_kc(net_2020, kc_2020)
plot_net_kc(net_2021, kc_2021)
plot_net_kc(net_all, kc_all)

write.csv(kc_2010, "D:/code/data_project/ergm/kc_2010.csv")
write.csv(kc_2011, "D:/code/data_project/ergm/kc_2011.csv")
write.csv(kc_2012, "D:/code/data_project/ergm/kc_2012.csv")
write.csv(kc_2013, "D:/code/data_project/ergm/kc_2013.csv")
write.csv(kc_2014, "D:/code/data_project/ergm/kc_2014.csv")
write.csv(kc_2015, "D:/code/data_project/ergm/kc_2015.csv")
write.csv(kc_2016, "D:/code/data_project/ergm/kc_2016.csv")
write.csv(kc_2017, "D:/code/data_project/ergm/kc_2017.csv")
write.csv(kc_2018, "D:/code/data_project/ergm/kc_2018.csv")
write.csv(kc_2019, "D:/code/data_project/ergm/kc_2019.csv")
write.csv(kc_2020, "D:/code/data_project/ergm/kc_2020.csv")
write.csv(kc_2021, "D:/code/data_project/ergm/kc_2021.csv")
write.csv(kc_all, "D:/code/data_project/ergm/kc_all.csv")

#brokerage
b_2010=brokerage(net_2010,kc_2010)
summary(b_2010)

b_2015=brokerage(net_2015,kc_2015)
summary(b_2015)

b_2020=brokerage(net_2020,kc_2020)
summary(b_2020)

#dynamic
#please consult https://statnet.org/Workshops/ndtv_workshop.html
square_2010 = set_square_matrix(division_list_2010, division_matrix_adj_2010)
square_2011 = set_square_matrix(division_list_2011, division_matrix_adj_2011)
square_2012 = set_square_matrix(division_list_2012, division_matrix_adj_2012)
square_2013 = set_square_matrix(division_list_2013, division_matrix_adj_2013)
square_2014 = set_square_matrix(division_list_2014, division_matrix_adj_2014)
square_2015 = set_square_matrix(division_list_2015, division_matrix_adj_2015)
square_2016 = set_square_matrix(division_list_2016, division_matrix_adj_2016)
square_2017 = set_square_matrix(division_list_2017, division_matrix_adj_2017)
square_2018 = set_square_matrix(division_list_2018, division_matrix_adj_2018)
square_2019 = set_square_matrix(division_list_2019, division_matrix_adj_2019)
square_2020 = set_square_matrix(division_list_2020, division_matrix_adj_2020)
square_2021 = set_square_matrix(division_list_2021, division_matrix_adj_2021)

gradList=list(square_2010, square_2011, square_2012, square_2013, square_2014, 
             square_2015, square_2016, square_2017, square_2018, square_2019, 
             square_2020, square_2021)
gradList=lapply(gradList, as.network.matrix, matrix.type='adjacency', directed=FALSE)
grad_dynamic=networkDynamic(network.list=gradList)
names = rownames(square_2021)

network.vertex.names(grad_dynamic)=names

#add metadata
grad_dynamic%n%'net.obs.period'=list(
  observations=list(c(0,11)),
  mode="discrete", 
  time.increment=1,
  time.unit="year")

#animation
render.animation(grad_dynamic)
compute.animation(grad_dynamic,
                  animation.mode='MDSJ',
                  default.dist=2,
                  verbose=FALSE)
render.d3movie(grad_dynamic,
               render.par=list(tween.frames=20),
               verbose=FALSE,
               jitter=TRUE,
               vertex.cex=0.8,
               edge.lwd = 0.3,
               edge.col = 'gray',
               label.cex=0.6,
               label.col='black',
               displaylabels=TRUE,
               output.mode = 'htmlWidget')

#save data
write.csv(division_matrix_adj_2010, "D:/code/data_project/ergm/adj_2010.csv")
write.csv(division_matrix_adj_2011, "D:/code/data_project/ergm/adj_2011.csv")
write.csv(division_matrix_adj_2012, "D:/code/data_project/ergm/adj_2012.csv")
write.csv(division_matrix_adj_2013, "D:/code/data_project/ergm/adj_2013.csv")
write.csv(division_matrix_adj_2014, "D:/code/data_project/ergm/adj_2014.csv")
write.csv(division_matrix_adj_2015, "D:/code/data_project/ergm/adj_2015.csv")
write.csv(division_matrix_adj_2016, "D:/code/data_project/ergm/adj_2016.csv")
write.csv(division_matrix_adj_2017, "D:/code/data_project/ergm/adj_2017.csv")
write.csv(division_matrix_adj_2018, "D:/code/data_project/ergm/adj_2018.csv")
write.csv(division_matrix_adj_2019, "D:/code/data_project/ergm/adj_2019.csv")
write.csv(division_matrix_adj_2020, "D:/code/data_project/ergm/adj_2020.csv")
write.csv(division_matrix_adj_2021, "D:/code/data_project/ergm/adj_2021.csv")
write.csv(division_matrix_adj_all, "D:/code/data_project/ergm/adj_all.csv")

write.csv(square_2010, "D:/code/data_project/ergm/square_2010.csv")
write.csv(square_2011, "D:/code/data_project/ergm/square_2011.csv")
write.csv(square_2012, "D:/code/data_project/ergm/square_2012.csv")
write.csv(square_2013, "D:/code/data_project/ergm/square_2013.csv")
write.csv(square_2014, "D:/code/data_project/ergm/square_2014.csv")
write.csv(square_2015, "D:/code/data_project/ergm/square_2015.csv")
write.csv(square_2016, "D:/code/data_project/ergm/square_2016.csv")
write.csv(square_2017, "D:/code/data_project/ergm/square_2017.csv")
write.csv(square_2018, "D:/code/data_project/ergm/square_2018.csv")
write.csv(square_2019, "D:/code/data_project/ergm/square_2019.csv")
write.csv(square_2020, "D:/code/data_project/ergm/square_2020.csv")
write.csv(square_2021, "D:/code/data_project/ergm/square_2021.csv")