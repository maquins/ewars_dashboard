observeEvent(c(input$population_spat,
               input$number_of_cases_spat,
               input$alarm_indicators_spat,
               input$dat_spat,
               input$spat_Input_Type),{
                if(input$spat_Input_Type=="sub_district"){
               cat(".....running Areal Data \n")
                 #req(input$dat)
                 #req(input$shape_file)                            
                 #data for covariates
                 dat_fl<-var_names_spat()$dat
                 boundary_file<-var_names_spat()$SHP
                 shp_data<-boundary_file@data
                 print(names(shp_data))
                # if(!(stringr::str_detect(paste(names(dat_fl),collapse =" "),'x') & stringr::str_detect(paste(names(dat_fl),collapse =" "),'y') )){
                   x_y_coords_S<-sum(str_detect(names(dat_fl),'x'))+sum(str_detect(names(dat_fl),'y'))
                   if(!x_y_coords_S==2){
                     
                 #print(names(dat_fl))
                 #print(dat_fl$district)
                 
                 if('district' %in% names(shp_data)){
                   dis_shp<-sort(unique(shp_data$district))
                 }else{
                   dis_shp<-rep(NA,nrow(shp_data))
                 }
                 
                 if('district' %in% names(dat_fl)){
                   dis_dat<-sort(unique(dat_fl$district))
                 }else{
                   dis_dat<-rep(NA,nrow(dat_fl))
                 }
                 
                 #print(unique(dis_dat),sep='\n')
                 #print(unique(dis_shp),sep='\n')
                 
                 if(length(unique(dis_dat))==1&is.na(unique(dis_dat)[1])|length(unique(dis_shp))==1&is.na(unique(dis_shp)[1])){
                   error1<-T
                 }else{
                   error1<-F
                 }
                 
                 if (length(unique(dis_dat))>1&length(unique(dis_shp))>1){
                   second_Error<-T
                   n_in<-length(which(dis_shp%in% dis_dat))
                   pct_N<-round((n_in/length(unique(dis_dat)))*100,2)
                   pctn<-paste(round((n_in/length(unique(dis_dat)))*100,2),'%')
                 }else{
                   second_Error<-F
                   pct_N<-NA
                   n_in<-NA
                   pctn<-""
                 }
                 print(paste('pctn::',pct_N,'\n'))
                 print(paste('second error::',second_Error,'\n'))
                 
                 if(second_Error&pct_N<50){
                   error2<-T
                 }else{
                   error2<-F
                 }
                 
                 if(error1){
                   output$Error<-renderUI(tags$h3("Error::district id  should be both in  shapefile and surveillance data !!",
                                                  style="color:red;font-weight:bold;background-color:black"))
                 }
                 else if(error2){
                   output$Error<-renderUI(tags$h3("Error:: less than  50% of disticts in surveillance are in shapefile !!",
                                                  style="color:red;font-weight:bold;background-color:black"))
                 }else{
                   output$Error<-renderUI(" ")
                   
                   paste("boundary file")
                   var_names_spat()$SHP
                   boundary_file<-var_names_spat()$SHP
                   dat_fl<-var_names_spat()$dat
                   
                   #untar("INLA_20.03.17.tar.gz")
                   
                   #pkgload::load_all(paste0(getwd(),"/INLA"))
                   #inla.dynload.workaround()
  #output$Error<-renderUI(" ")
  
  paste("boundary file")
  var_names_spat()$SHP
  boundary_file<-var_names_spat()$SHP
  boundary_file$district<-as.numeric(boundary_file$district)
  
  dat_fl<-var_names_spat()$dat
  
  #untar("INLA_20.03.17.tar.gz")
  
  #pkgload::load_all(paste0(getwd(),"/INLA"))
  #inla.dynload.workaround()
  
  boundary_file_mod<-boundary_file[which(boundary_file$district %in% dat_fl$district),]
  
  nbh_a<-poly2nb(boundary_file)
  
  nb2INLA("spatial_Mod.graph", nbh_a)
  
  H <- inla.read.graph(filename="spatial_Mod.graph") 
  
  spat_adj<-paste0(getwd(),'/','spatial_Mod.graph')
  
  
  
  population<-input$population_spat
  alarm_indicators<-input$alarm_indicators_spat
  number_of_cases<-input$number_of_cases_spat
  base_vars<-c("district","year","week")
  
  dat_A<-var_names_spat()$dat %>% 
    dplyr::arrange(district,year,week) %>% 
    dplyr::filter(district %in% boundary_file$district &!week==53)
  
  ## balance the observations to compute expected cases correctly
  
  beg.year<-min(dat_A$year)
  end.year<-max(dat_A$year)
  
  dat_dist<-expand.grid(week=1:52,
                        year=sort(unique(dat_A$year)),
                        district=sort(unique(dat_A$district))) %>% 
    dplyr::select(district,year,week)
  
  data_augmented<-merge(dat_dist,dat_A,by=c("district","year","week"),all.x=T,sort=T) %>% 
    dplyr::mutate(district_w=paste0(district,'_',week)) %>% 
    dplyr::arrange(district,year,week)
  
  n_strata<-nrow(data_augmented)/length(unique(data_augmented$district_w))
  
  exp_vars<-c(population,number_of_cases)
  
  for_exp<-data_augmented[,c(exp_vars,"year","week")]  %>% 
    #dplyr::filter(year==yr) %>% 
    dplyr::select(-year,-week)
  
  ca.ses<-ifelse(is.na(for_exp[,2]),0,for_exp[,2])
  pop.exp<-ifelse(is.na(for_exp[,1]),0,for_exp[,1])
  
  data_augmented$Expected_cases<-expected(population=pop.exp,
                                          cases=ca.ses,
                                          n.strata =n_strata) 
  
  data_augmented_Expected<-data_augmented[,c("district","year","week","Expected_cases")]
    
  ## merge to surveillance
  dat_A<-merge(dat_A,data_augmented_Expected,by=c("district","year","week"),sort=T,all.x=T) %>% 
    dplyr::arrange(district,year,week) 
  ##run the INLA model here
  
  dat_A$district1<-dat_A$district
  
  dat_A$idtime<-dat_A$year-(beg.year-1)
  
  dat_IDDS<-data.frame(district=boundary_file$district) %>% 
    dplyr::mutate(id=1:n())
  
  dat_A1<-merge(dat_A,dat_IDDS,by="district",sort=F)
  
  dat_A1$id1<-dat_A1$id
  
  list_mod_check<<-list(dat_A=dat_A,
                        boundary_file_mod=boundary_file,
                        graph=H,
                        spat_adj=spat_adj)
  
  formu<-paste0(number_of_cases,"~f(id,model='bym',graph =H)+
    f(id1,idtime,model='iid')+",paste0(alarm_indicators,collapse ="+"),"+f(week,model='ar1')")
  
  res<-inla(as.formula(formu),family ="poisson",data=dat_A1,E=Expected_cases,
            control.predictor =list(compute=T),
            control.compute =list(dic=T,openmp.strategy="medium"),
            control.inla = list(strategy = 'gaussian',
                                int.strategy = "eb",
                                stupid.search=FALSE,
                                step.factor=0.5,dz=1),
            num.threads=4,
            blas.num.threads=4,
            verbose =F)
  
  all_vars<-c(base_vars,number_of_cases,'Expected_cases',population,alarm_indicators)
  
  dat_Sel<-dat_A1[,all_vars]
  
  SIR_dat<-dat_A1[,base_vars] %>% 
    mutate(SIR=res$summary.fitted.values$mean,
           year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2)))%>% 
    dplyr::select(district,year_week,SIR)
  
  #pkgload::unload("INLA")
  
  
  melted_dat<-melt(dat_Sel,base_vars) %>% 
    mutate(year_week=paste0(year,'_',str_pad(week,side ="left",pad =0,width =2))) %>% 
    dplyr::select(district,year_week,variable,value)
  
  #change to wide
  
  melted_dat_wide<-melted_dat %>% 
    dplyr::group_by(district,variable) %>% 
    tidyr::spread(year_week,value)
  
  wide_for_dygraph<-melted_dat %>% 
    dplyr::group_by(variable,year_week) %>% 
    tidyr::spread(district,value)
  
  SIR_wide<-SIR_dat %>% 
    dplyr::group_by(district) %>% 
    tidyr::spread(year_week,SIR)
  
  ##merge to polygons for plotting
  SIR_Poly<-merge(boundary_file,SIR_wide,by="district",sort=F,all.x=T)
  
  ##function to get data for plotting
  
  covar_to_Plot<-c(number_of_cases,'Expected_cases',population,alarm_indicators)
  names_cov_Plot<-c("Cases","Expected_cases","Population",alarm_indicators)
  
  p<-1
  get_Spatial_poly_dat<-function(p){
    dat_n<-melted_dat_wide %>% filter(variable==covar_to_Plot[p])
    merge_Poly<-merge(boundary_file,dat_n,by="district",sort=F,all.x=T)
    aa<-list(merge_Poly)
    names(aa)<-names_cov_Plot[p]
    aa
  }
  
  
  
  dates_s<-seq.Date(as.Date(paste0(beg.year,'-01-01')),
                    as.Date(paste0(end.year,'-12-31')),
                    by='day')
  
  
  
  data_Weeks<-data.frame(date=dates_s,
                         year_week=format.Date(dates_s,"%Y_%W"),
                         year=year(dates_s),
                         stringsAsFactors =F,
                         week=week(dates_s)) %>% 
    mutate(Week=str_split_fixed(year_week,pattern ='_',n=2)[,2]) %>% 
    dplyr::filter(as.numeric(Week)%in% 1:52)
  
  weeks.in.data<-dat_A %>% 
    dplyr::mutate(year_week=paste0(year,'-',str_pad(week,side ="left",pad =0,width =2))) 
  
  year_week_S<-data_Weeks %>% dplyr::group_by(year,Week) %>% 
    dplyr::summarise(.group="drop",date_Beg=min(date)) %>% 
    dplyr::mutate(year_week=format.Date(date_Beg,"%Y-%W"))%>% 
    dplyr::filter(year_week %in% weeks.in.data$year_week)
  
  get_xts_dat<-function(p){
    dat_n<-wide_for_dygraph %>% filter(variable==covar_to_Plot[p])
    dat_n<-dat_n[,-2]
    dat_n1<-dat_n[,-1]
    dat_n2<-xts(dat_n1,order.by =as.Date(as.character(year_week_S$date_Beg)),
                frequency=52)
    plo<-dygraph(dat_n2,xlab ="Year week",ylab=covar_to_Plot[p]) %>%
      #dyMultiColumn()
      dySeries() %>% 
      dyRangeSelector() %>% 
      dyLegend(show = "follow") %>% 
      dyHighlight(highlightCircleSize =2, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = T)
    aa<-list(plo)
    names(aa)<-names_cov_Plot[p]
    aa
  }
  
  all_Plot_Poly<-foreach(a=1:length(covar_to_Plot),.combine =c)%do% get_Spatial_poly_dat(a)
  all_xts_Plots<-foreach(a=1:length(covar_to_Plot),.combine =c)%do% get_xts_dat(a)
  
  ##tmap plots
  
  dat_For_Ui<-data.frame(var=covar_to_Plot,num=1:length(covar_to_Plot))
  
  #length(covar_to_Plot)/2
  
  max_groups<-ceiling(length(covar_to_Plot)/2)
  
  dat_For_Ui$group=expand.grid(a=1:2,b=1:max_groups)[,2][1:length(covar_to_Plot)]
  
  dat_For_Ui$plot_Output<-paste0("plot1_",dat_For_Ui$num)
  dat_For_Ui$plot_Output1<-paste0("plot2_",dat_For_Ui$num)
  
  
  get_Fluid<-function(t){
    dat_For_Ui1<-dat_For_Ui %>% filter(group==t)
    
    dd<-paste("fluidRow(",
              paste0('column(6,leafletOutput("',dat_For_Ui1$plot_Output[1],'")),'),
              paste0('column(6,offset =0,leafletOutput("',dat_For_Ui1$plot_Output[2],'")))')
    )
    #DT::dataTableOutput("Uploaded_data")
    dd
  }
  
  get_Fluid1<-function(t){
    dat_For_Ui1<-dat_For_Ui %>% filter(group==t)
    
    dd<-paste(
      paste0('column(12,dygraphOutput("',dat_For_Ui1$plot_Output1[1],'")),'),
      paste0('column(12,offset =0,dygraphOutput("',dat_For_Ui1$plot_Output1[2],'"))')
    )
    #DT::dataTableOutput("Uploaded_data")
    dd
  }
  
  all_out<-foreach(a=1:max_groups,.combine =c)%do% get_Fluid(a)
  all_out1<-foreach(a=1:max_groups,.combine =c)%do% get_Fluid1(a)
  
  
  par_text<-parse(text=
                    paste('tabPanel("Spatial_Plots",',
                          paste(all_out,collapse =',')
                          , ')')
  )
  
  par_text1<-parse(text=
                     paste('tabPanel("Time_series",',"fluidRow(",
                           paste(all_out1,collapse =',')
                           , '))')
  )
  
  
  ##update the plot UI 
  
  output$Spat_Plot<-renderUI({
    tagList(eval(par_text))
  })
  
  output$Time_series_Plots<-renderUI({
    tagList(eval(par_text1))
  })
  
  #print(names_cov_Plot)
  
  var_p<-names_cov_Plot
  observeEvent(c(input$Year_plot_spat,
                 input$Week_plot_spat),
               {
                 print(var_p)
                 yr_week<-paste0(input$Year_plot_spat,'_',str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week1<-paste0(input$Year_plot_spat,':',str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 
                 yr_week_input<-paste0(input$Year_plot_spat,":",str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week_input1<-paste0(input$Year_plot_spat,"_",str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 

                 print(paste("from input ::",yr_week_input))
                 names_Plots_s<-paste(names(SIR_Poly),collapse =" ")
                 
                 first_pos_SIR<-which(stringr::str_detect(names(SIR_Poly),'[:number:]+_[:number:]+'))[1]
                 
                 first_YR_week<-stringr::str_extract(names(SIR_Poly)[first_pos_SIR],'[:number:]+_[:number:]+')
                 first_YR_week1<-str_replace(first_YR_week,'_',":")
                 
                 #print(names_Plots_s)
                 out_for_Test<<-list(SIR_Poly=SIR_Poly,
                                    all_Plot_Poly=all_Plot_Poly)
                 if(stringr::str_detect(names_Plots_s,yr_week_input1)==FALSE){
                   yr_week1<-first_YR_week1
                   yr_week<-first_YR_week
                   
                 }else{
                   yr_week1<-yr_week_input
                   yr_week<-yr_week_input1
                 }
                 #print(paste("from input ::",yr_week1))
                 #print(paste("from input ::",yr_week))
                 
                 output$plot3_spat<-renderLeaflet({
                
                   week_slice<-SIR_Poly[,c("district",yr_week)]
                   
                   lng1<-as.numeric(week_slice@bbox[,1][1])
                   lat1<-as.numeric(week_slice@bbox[,1][2])
                   lng2<-as.numeric(week_slice@bbox[,2][1])
                   lat2<-as.numeric(week_slice@bbox[,2][2])
                   
                   labels <- sprintf(
                     "<strong>%s</strong><br/>%g",
                     week_slice$district, eval(parse(text=paste0("week_slice$`",yr_week,"`")))
                   ) %>% lapply(htmltools::HTML)
                   
                   
                   legend_title<-sprintf(
                     "<strong>%s</strong><br/>%s",
                     "SIR",yr_week1 
                   ) %>% lapply(htmltools::HTML)
                   
                   pal <- colorNumeric("YlOrRd", 
                                       domain =eval(parse(text=paste0("week_slice$`",yr_week,"`"))),
                                       reverse=F)
                   
                   leaflet(week_slice[,yr_week]) %>% 
                     addTiles() %>% 
                     addProviderTiles(providers$OpenStreetMap) %>% 
                     fitBounds(lng1,lat1,lng2,lat2) %>% 
                     #addPolylines() %>% 
                     addPolygons(fillColor = eval(parse(text=paste0("~pal(`",yr_week,"`)"))),
                                 color = "black",weight =0.8,
                                 dashArray = " ",
                                 fillOpacity = 0.9,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = "green",
                                   dashArray = "2",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE),
                                 label = labels,
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")) %>% 
                     addLegend(pal = pal, values = eval(parse(text=paste0("~`",yr_week,"`"))), 
                               opacity = 0.7, title = legend_title,
                               position = "bottomright") 
                   
                 })
                 
                 plot_Func<-function(p){
                   
                   plot_Now<-all_Plot_Poly[[var_p[p]]]
                   week.idx<-which(names(plot_Now)==yr_week)
                   week_slice<-plot_Now[,c("district",yr_week)]
                   
                   lng1<-as.numeric(week_slice@bbox[,1][1])
                   lat1<-as.numeric(week_slice@bbox[,1][2])
                   lng2<-as.numeric(week_slice@bbox[,2][1])
                   lat2<-as.numeric(week_slice@bbox[,2][2])
                   
                   labels <- sprintf(
                     "<strong>%s</strong><br/>%g",
                     week_slice$district, eval(parse(text=paste0("week_slice$`",yr_week,"`")))
                   ) %>% lapply(htmltools::HTML)
                   
                   
                   legend_title<-sprintf(
                     "<strong>%s</strong><br/>%s",
                     var_p[p],yr_week1 
                   ) %>% lapply(htmltools::HTML)
                   
                   pal <- colorNumeric("YlOrRd", 
                                       domain =eval(parse(text=paste0("week_slice$`",yr_week,"`"))),
                                       reverse=F)                   
                   plo1<-leaflet(week_slice[,yr_week]) %>% 
                     addTiles() %>% 
                     addProviderTiles(providers$OpenStreetMap) %>% 
                     fitBounds(lng1,lat1,lng2,lat2) %>% 
                     #addPolylines() %>% 
                     addPolygons(fillColor = eval(parse(text=paste0("~pal(`",yr_week,"`)"))),
                                 color = "black",weight =0.8,
                                 dashArray = " ",
                                 fillOpacity = 0.9,
                                 highlight = highlightOptions(
                                   weight = 5,
                                   color = "green",
                                   dashArray = "2",
                                   fillOpacity = 0.7,
                                   bringToFront = TRUE),
                                 label = labels,
                                 labelOptions = labelOptions(
                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                   textsize = "15px",
                                   direction = "auto")) %>% 
                     addLegend(pal = pal, values = eval(parse(text=paste0("~`",yr_week,"`"))), 
                               opacity = 0.7, title = legend_title,
                               position = "bottomright") 
                   list(plo1)
                   
                 }
                 
                 plot_List<-foreach(a=1:length(var_p),.combine =c)%do% plot_Func(a)
                 print(var_p)
                 #prrrrr<<-plot_List
                 ## render Plots in a loop
                 i<-1
                 for(i in 1:length(covar_to_Plot)){
                   text_rend<-paste0('output$plot1_',i,'<-renderLeaflet({
                     plot_List[[',i,']]})'
                   )
                   text_rend1<-paste0('output$plot2_',i,'<-renderDygraph({
                     all_xts_Plots[[',i,']]})'
                   )
                   eval(parse(text=text_rend))
                   eval(parse(text=text_rend1))
                 }
                 
               })
  
  #covar_risk$all_Plot_Poly<-all_Plot_Poly
  #covar_risk$var_p<-names_cov_Plot 
  ##run the INLA model
                 }
                 }else{
                   safeError("data does not have district")
                 }
                 ##////>end_of_spat
                }else{
                  NULL
                }
               })