# contact: sewemaquins@gmail.com

observeEvent(c(input$dat_spat,
               input$spat_Input_Type),{
                 if(input$spat_Input_Type=="point (LatLon)"){
                 cat(".....running Spatial point Codes \n")
                 #req(input$dat)
                 #req(input$shape_file)                            
                 #data for covariates
                 dat_fl<-var_names_spat()$dat
                 boundary_file<-var_names_spat()$SHP
                 one_shape<-clgeo_Clean(gUnaryUnion(boundary_file))
                 shp_data<-boundary_file@data
                 #print(names(shp_data))
                 
    
                 #print(unique(dis_dat),sep='\n')
                 #print(unique(dis_shp),sep='\n')
                 
                 if(!'x' %in% names(dat_fl)|!'y' %in% names(dat_fl)){
                   error1<-T
                 }else{
                   error1<-F
                 }
                 ## second error checks if points within district polygons
                 x_y_coords_S<-sum(str_detect(names(dat_fl),'x'))+sum(str_detect(names(dat_fl),'y'))
                 
                 if(x_y_coords_S==2){
                 
                 coord_points<-dat_fl %>% 
                   dplyr::select(x,y) %>% 
                   unique()
                 coordinates(coord_points)<-~x+y
                 
                 proj4string(coord_points)<-proj4string(one_shape)
                 }else{
                   safeError("dataset has no x and y coordinates")
                 }
                 
                 
                 #test_contain<-rgeos::gContainsProperly(coord_points,one_shape,
                                                #byid=T,
                                                #checkValidity =F)
                 #in_poly<-which(test_contain==T)
                 #if (length(in_poly)<2){
                 #error2<-T
                   
                 #}else{
                 #error2<-F
                   
                 #}
                 error2<-F
                 
                 if(error1){
                   output$Error<-renderUI(tags$h3("Error::points data doe snot contain x or y coordinates !!",
                                                  style="color:red;font-weight:bold;background-color:black"))
                 }
                 else if(error2){
                   output$Error<-renderUI(tags$h3("Error:: x and y coordinates not within district boundary !!",
                                                  style="color:red;font-weight:bold;background-color:black"))
                 }else{
  output$Error<-renderUI(" ")
  
  paste("boundary file")
  var_names_spat()$SHP
  boundary_file<-var_names_spat()$SHP
  dat_fl<-var_names_spat()$dat
  
  untar("INLA_20.03.17.tar.gz")
  
  pkgload::load_all(paste0(getwd(),"/INLA"))
  inla.dynload.workaround()
  
  #boundary_file_mod<-boundary_file[which(boundary_file$district %in% dat_fl$district),]
  
  
  
  bound_fl <- inla.sp2segment(one_shape)
  mesh_fl<-inla.mesh.2d(boundary =bound_fl,
                        max.edge =c(0.3,1.2),
                        offset =c(0.3,1),
                        cutoff =0.1)
  
  
  #population<-input$population_spat
  #alarm_indicators<-input$alarm_indicators_spat
  #number_of_cases<-input$number_of_cases_spat
  #base_vars<-c("district","year","week")
  
  spde_funcs<-parse(text=readLines("spde tutorial functions.R"))

  eval(spde_funcs)
  
  dat_A<-var_names_spat()$dat 
  
  beg.year<-min(dat_A$year)
  end.year<-max(dat_A$year)
  
  year_WEEKs<-dat_A %>% 
    dplyr::select(year,week) %>% 
    unique()
  
  ##compute the expected cases
  dmesh <- inla.mesh.dual(mesh_fl)
  plot(dmesh)
  
  one_shape<-gUnaryUnion(boundary_file)
  one_shape1<-cleangeo::clgeo_Clean(one_shape)
  
  dmesh1<-cleangeo::clgeo_Clean(dmesh)
  
  
  get.a<-function(p){
    
    if (gIntersects(dmesh1[p,],one_shape1))
      return(gArea(gIntersection(dmesh1[p,],one_shape1)))
    else return(0)
  }
  
  
  w<-foreach(a=1:length(dmesh),.combine =c,
             .packages =c("rgeos","cleangeo")) %dopar% get.a(a)
  
  
  
  imat <- Diagonal(mesh_fl$n,rep(1,mesh_fl$n))
  
  
  Week_n<-1
 
  
  
  get_point_week_pred<-function(NN_week){
    
    yr<-year_WEEKs[NN_week,1]
    Week_n<-year_WEEKs[NN_week,2]
    
    points_now<-dat_A %>% 
      dplyr::filter(year==yr & week==Week_n)
    
  
    A.est<-inla.spde.make.A(mesh=mesh_fl,
                            loc=as.matrix(points_now[,c("x","y")]))
    
    y.pp <- rep(0:1, c(mesh_fl$n, nrow(points_now)))
    
    e.pp <- c(w, rep(0, nrow(points_now)))
    
    lmat<- inla.spde.make.A(mesh=mesh_fl,
                            loc=as.matrix(points_now[,c("x","y")]))
    
    A.pp <- rbind(imat,lmat)
    
    names(points_now)
    #covs_dat<-points_now %>% 
    #dplyr::mutate(a0=1) %>% 
    #dplyr::select(a0,rainsum,mintemperature,meantemperature,maxtemperature)
    
    
    #extract data for mesh vertices
    
    
    cru_vars<-"pre|tmn|tmp|tmx"
    
    create_mesh_covs_data<-function(n){
      
      
      #plot(boundary_file)
      points_mesh<-data.frame(x=mesh_fl$loc[,1],y=mesh_fl$loc[,2]) %>% 
        dplyr::mutate(id_num=1:dplyr::n())
      coord_mesh<-points_mesh
      
      coordinates(coord_mesh)<-~x+y
      
      
      
      clim_ext<-raster::extract(brick_climate,coord_mesh,fun="mean",na.rm=TRUE)
      
      data_ext<-data.frame(points_mesh,clim_ext) 
      
      data_ext_long<-reshape2::melt(data_ext,c('id_num','x','y')) %>% 
        dplyr::mutate(week=as.numeric(stringr::str_extract(variable,'[:number:]+')),
                      var=stringr::str_extract(variable,cru_vars))
      
      names(data_ext_long)
      data_ext_long.a<-data_ext_long %>% 
        dplyr::select(id_num,x,y,var,week,value)
      
      data_link<-data_ext_long.a %>% 
        dplyr::group_by(id_num,x,y,week) %>% 
        tidyr::spread(var,value) %>% 
        dplyr::filter(week==n)
      
      
    }
    
    #mesh_Cov_dat<-create_mesh_covs_data(Week_n)
    
    #mesh_Cov_dat_Final<-mesh_Cov_dat %>% 
    #dplyr::rename(rainsum=pre,meantemperature=tmp,
    #mintemperature=tmn,maxtemperature=tmx)
    
    
    #Cov_dat_mesh<-mesh_Cov_dat_Final %>% 
    #dplyr::ungroup() %>% 
    #data.frame()%>% 
    #dplyr::mutate(a0=1) %>% 
    #dplyr::select(a0,rainsum,meantemperature,mintemperature,maxtemperature)
    
    
    
    
    #cov_data_all<-rbind(Cov_dat_mesh,covs_dat)
    
    #stk.pp <- inla.stack(data=list(y=y.pp, expect=e.pp),
    #A=list(A.pp,1), tag='est',
    #effects=list(s.index,cov_data_all))
    
    spde_mod<-inla.spde2.pcmatern(
      mesh=mesh_fl, alpha=2, ### mesh and smoothness parameter
      prior.range=c(0.5, 0.01), ### P(practic.range<0.05)=0.01
      prior.sigma=c(1, 0.01))
    
    
    
    s.index<-inla.spde.make.index(name='s',
                                  n.spde=spde_mod$n.spde)
    
    
    dat_cons<-data.frame(a0=rep(1,length(y.pp)))
    stk.pp <- inla.stack(data=list(y=y.pp, expect=e.pp),
                         A=list(A.pp,1), tag='est',
                         effects=list(s.index,dat_cons))
    
    #formu <- y ~ 0+a0+f(s,model=spde_mod)+rainsum+meantemperature
    
    formu <- y ~ 0+a0+f(s,model=spde_mod)
    
    
    spde_mod<-inla.spde2.pcmatern(
      mesh=mesh_fl, alpha=2, ### mesh and smoothness parameter
      prior.range=c(0.5, 0.01), ### P(practic.range<0.05)=0.01
      prior.sigma=c(1, 0.01))
    
    res_inla<- inla(formu, family='poisson',
                    data=inla.stack.data(stk.pp),E=expect,
                    control.predictor=list(A=inla.stack.A(stk.pp),compute=T),
                    control.compute =list(waic=T,dic=T),
                    control.inla = list(strategy = 'gaussian',
                                        int.strategy = "eb",
                                        stupid.search=FALSE,
                                        step.factor=0.5,dz=1),
                    verbose = F)
    
    summary(res_inla)
    
    ##plot on a granular scale
    
    
    
    for_res<-raster(extent(boundary_file),res=0.00083*15)
    
    proj4string(for_res)<-boundary_file@proj4string
    
    
    #plot(mask_lc2)
    
    
    prog_mat<-inla.mesh.projector(mesh_fl,
                                  loc =coordinates(for_res),
                                  xlim =as.numeric(one_shape@bbox[1,]),
                                  ylim =as.numeric(one_shape@bbox[2,]),
                                  dims  =c(0.00083*15,0.00083*15))
    
    
    
    g.mean_r1 <- inla.mesh.project(prog_mat,exp(res_inla$summary.random$s$mean))
    g.lower_r <- inla.mesh.project(prog_mat,exp(res_inla$summary.random$s$`0.025quant`))
    g.upper_r <- inla.mesh.project(prog_mat,exp(res_inla$summary.random$s$`0.975quant`))
    
    dat_all<-cbind(g.lower_r,g.mean_r1,g.upper_r)
    
    RR_pred<-brick(for_res,nl=3)
    
    Week_n_pad<-stringr::str_pad(Week_n,side ='left',width =2,pad=0)
    names(RR_pred)<-c(paste0("RR_Lower_",yr,Week_n_pad),paste0("RR_",yr,Week_n_pad),paste0("RR_Upper_",yr,Week_n_pad))
    
    
    values(RR_pred)[,]<-dat_all
    
    
    
    
    RR_pred<-mask(RR_pred,one_shape)
    
    coord_points<-points_now %>% 
      dplyr::select(x,y)
    
    coordinates(coord_points)<-~x+y
    
    proj4string(coord_points)<-proj4string(one_shape)
    
    plot0<-tm_shape(coord_points)+
      tm_dots(title="Point Pattern",size=0.06,shape =21,col='red4')+
      tm_shape(boundary_file)+
      #tm_polygons()+
      tm_fill(alpha=0.1) +
      tm_borders("grey40") + 
      tm_text("district", size = 0.3)+
      tm_layout(legend.bg.color = "white",
                title=paste0(yr,'-',Week_n_pad),
                title.position=c("right","top"))
    
    
    
    plot1<-tm_shape(RR_pred[[1]])+
      tm_raster(title="RR_lower")+
      tm_shape(boundary_file)+
      #tm_polygons()+
      tm_fill(alpha=0.1) +
      tm_borders("grey40") + 
      tm_text("district", size = 0.3)+
      tm_layout(legend.bg.color = "white",
                title=paste0(yr,'-',Week_n_pad),
                title.position=c("right","top"))
    
    
    plot2<-tm_shape(RR_pred[[2]])+
      tm_raster(title="RR_mean")+
      tm_shape(boundary_file)+
      #tm_polygons()+
      tm_fill(alpha=0.1) +
      tm_borders("grey40") + 
      tm_text("district", size = 0.3)+
      tm_layout(legend.bg.color = "white",
                title=paste0(yr,'-',Week_n_pad),
                title.position=c("right","top"))
    
    
    plot3<-tm_shape(RR_pred[[3]])+
      tm_raster(title="RR_upper")+
      tm_shape(boundary_file)+
      #tm_polygons()+
      tm_fill(alpha=0.1) +
      tm_borders("grey40") + 
      tm_text("district", size = 0.3)+
      tm_layout(legend.bg.color = "white",
                title=paste0(yr,'-',Week_n_pad),
                title.position=c("right","top"))
    
    all_plots<-list(plot0,plot1,plot2,plot3)
    
    names(all_plots)<-c(paste0("Point_pattern_",yr,Week_n_pad),paste0("RR_Lower_",yr,Week_n_pad),paste0("RR_",yr,Week_n_pad),paste0("RR_Upper_",yr,Week_n_pad))
    all_plots
    
  }
  
  ## number of weeks
  
  
  #all_smooth<-foreach(a=1:nrow(year_WEEKs),.final =brick)%do% get_point_week_pred(a)
  all_point_Plots<-foreach(a=1:nrow(year_WEEKs),.combine=c)%do% get_point_week_pred(a)
  
  ##merge to polygons for plotting
  
  
  ##tmap plots
  
  covar_to_Plot<-c("RR_lower","RR_mean","RR_Upper")
  
  dat_For_Ui<-data.frame(var=covar_to_Plot,num=1:length(covar_to_Plot))
  
  length(covar_to_Plot)/2
  
  max_groups<-1
  
  dat_For_Ui$cat_g<-expand.grid(a=1:3,b=1)[,2][1:length(covar_to_Plot)]
  
  dat_For_Ui$plot_Output<-paste0("plot1_",dat_For_Ui$num)
  dat_For_Ui$plot_Output1<-paste0("plot2_",dat_For_Ui$num)
  
  tt<-1
  get_Fluid<-function(t){
    dat_For_Ui1<-dat_For_Ui %>% dplyr::filter(cat_g==t)
    
    dd<-paste("fluidRow(",
              paste0('column(12,tmapOutput("',dat_For_Ui1$plot_Output1[1],'")),'),
              paste0('column(12,offset =0,tmapOutput("',dat_For_Ui1$plot_Output1[2],'")),'),
              paste0('column(12,offset =0,tmapOutput("',dat_For_Ui1$plot_Output1[3],'")))')
    )
    #DT::dataTableOutput("Uploaded_data")
    dd
  }
  
  
  
  all_out<-foreach(a=1:max_groups,.combine =c)%do% get_Fluid(a)
  #all_out1<-foreach(a=1:max_groups,.combine =c)%do% get_Fluid1(a)
  
  
  par_text<-parse(text=
                    paste('tabPanel("plot3_spat",',
                          paste(all_out,collapse =',')
                          , ')')
  )
  
  #par_text1<-parse(text=
                     #paste('tabPanel("Time_series",',"fluidRow(",
                           #paste(all_out1,collapse =',')
                           #, '))')
  #)
  
  
  ##update the plot UI 
  
  output$plot3_spat<-renderUI({
    tagList(eval(par_text))
  })
  
  #output$Time_series_Plots<-renderUI({
    #tagList(eval(par_text1))
  #})
  
  #print(names_cov_Plot)
  
  #var_p<-names_cov_Plot
  observeEvent(c(input$Year_plot_spat,
                 input$Week_plot_spat),
               {
                 #print(var_p)
                 yr_week<-paste0(input$Year_plot_spat,'_',str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week1<-paste0(input$Year_plot_spat,':',str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 yr_week_input<-paste0(input$Year_plot_spat,str_pad(input$Week_plot_spat,side ="left",pad =0,width =2))
                 print(paste("from input ::",yr_week_input))
                 names_Plots_s<-paste(names(all_point_Plots),collapse =" ")
                 
                 #print(names_Plots_s)
                 
                 if(stringr::str_detect(names_Plots_s,yr_week_input)==FALSE){
                   yr_week2<-stringr::str_extract(names(all_point_Plots)[1],"[:number:]+")
                 }else{
                   yr_week2<-yr_week_input
                 }
                 #yr_week2<-"202001"
                 print(paste("yr_week2::",yr_week2))
                 point_N<-grep(paste0("Point_pattern_",yr_week2),names(all_point_Plots))
                 risk_map_N<-c(grep(paste0("RR_Lower_",yr_week2),names(all_point_Plots)),
                               grep(paste0("RR_",yr_week2),names(all_point_Plots)),
                               grep(paste0("RR_Upper_",yr_week2),names(all_point_Plots)))
                 print(point_N)
                 print(all_point_Plots[[point_N]])
                 output$Spat_Plot<-renderTmap({
                   all_point_Plots[[point_N]]
                 })
                 
                 plot_List<-all_point_Plots[risk_map_N]
                 
                 #prrrrr<<-plot_List
                 ## render Plots in a loop
                 i<-1
                 for(i in 1:3){
                   text_rend<-paste0('output$plot2_',i,'<-renderTmap({
                     plot_List[[',i,']]})'
                   )
                   
                   eval(parse(text=text_rend))
                 }
                 
               })
  
  #covar_risk$all_Plot_Poly<-all_Plot_Poly
  #covar_risk$var_p<-names_cov_Plot 
  ##run the INLA model
                 }      
  ##////>end_of_spat
                 }else{
                   NULL
                 }
  })