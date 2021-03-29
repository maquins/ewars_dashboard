# contact: sewemaquins@gmail.com
print("attempting to run....")
#profvis({
  #Sys.sleep(1)


out_x<-eventReactive({input$number_of_cases
                       input$population
                       input$stop_runinYear
                       input$stop_runinWeek
                       input$run_per_district
                       input$alarm_indicators
                       input$outbreak_week_length
                       input$alarm_indicators
                       input$alarm_window
                       input$alarm_threshold
                       input$season_length
                       input$z_outbreak
                       input$outbreak_window
                       input$prediction_distance
                       input$outbreak_threshold
                       input$spline
                       input$spline_alarm_indicators
                       input$dat
                       }
                     ,
                      {
                        
                      #invalidateLater(1000)
                         #setwd("/Ewars/Ewars dashboard with dashboardplus/ewars with conditional panels")
                         
                         #original_data_file_name<-input$original_data_file_name
                         #original_data_sheet_name<-input$original_data_sheet_name
                         
                         #psswd<-input$psswd
                        #validate(need(USER$login, message = FALSE))
                         automate<-input$automate
                         stop_runin<-paste(input$stop_runinYear,input$stop_runinWeek,sep='')
                         #generating_surveillance_workbook<-input$generating_surveillance_workbook
                         run_per_district<-sort(unique(as.numeric(str_split(input$run_per_district,',',simplify =T))))
                         population.a<-input$population
                         population_var<-input$population
                         number_of_cases<-input$number_of_cases
                         graph_per_district<-input$graph_per_district
                         alarm_indicators<-input$alarm_indicators
                         outbreak_week_length<-input$outbreak_week_length
                         alarm_indicators<-input$alarm_indicators
                         spline_vars<-input$spline_alarm_indicators
                         alarm_window<-input$alarm_window
                         alarm_threshold<-input$alarm_threshold
                         season_length<-as.numeric(input$season_length)
                         z_outbreak<-input$z_outbreak
                         outbreak_window<-input$outbreak_window
                         prediction_distance<-input$prediction_distance
                         outbreak_threshold<-input$outbreak_threshold
                         spline<-input$spline 
                           check_list<<-list(inputdata=input$dat,alarm_threshold=input$alarm_threshold,
                                             districts=run_per_district)
                         
                    
                       
                         
                         print(paste("season_length:",season_length))
                         print(class(season_length))
                        
                         
                         inFile <- input$dat
                         #print(inFile$datapath)
                         data<-var_names()$dat
                        print("reached here man") 
                       
                         data<-data %>% filter(!week %in% c(1,53)) 

                         #cases_per1000<-paste0(number_of_cases,'/',population_var,sep='')
                         #add condition in case var in character
                         paste('as.numeric(as.character(',number_of_cases,'))',sep='')
                         cases_per1000<-paste0(paste('as.numeric(as.character(',number_of_cases,'))',sep=''),
                                               '/',population_var,sep='')
                         
                         data<-data %>% mutate(outbreak=eval(parse(text=cases_per1000))*1000,
                                               year_week=year*100+week)
                         
                         
                         if(length(run_per_district)>0){
                           data<-data %>% filter(district %in% run_per_district)
                         }else{
                           stop("Choose atleast one district")
                         }
                         
                         al_var<- str_split(alarm_indicators,pattern=",",simplify =T)
                         sp_var<- str_split(spline_vars,pattern=",",simplify =T)
                         
                         n_alarm_indicators<-length(al_var)
                         
                         print(paste("no of vars:",n_alarm_indicators))
                         print(al_var)
                         
                         
                         
                         if(outbreak_threshold> 1|outbreak_threshold<= 0) { 
                           stop( " You specified an invalid value for the threshold variable. The threshold variable should be in (0,1]. ")
                           
                         }
                         
                         
                         if(!spline %in% c(TRUE,FALSE)){
                           stop("You specified an invalid value for the spline. The spline option should takes value either 0(No) or 1(Yes).")
                         }
                         
                         
                         ############# Runin: Here starting the analysis
                         #To capture the length of runin period and evaluation period
                         
                         data<-data %>% mutate(year_week=year*100+week)
                         
                         
                         
                         #if(generating_surveillance_workbook==FALSE){
                           data<-data %>% mutate(runin=as.numeric((stop_runin>=year_week)))
                         #}
                         
                         #if(generating_surveillance_workbook==TRUE){
                           #data<-data %>% mutate(runin=1)
                         #}
                           
                             al_var<- str_split(alarm_indicators,pattern=",",simplify =T)
                             n_alarm_indicators<-length(al_var)
                             
                             ##create alarm indicators
                             
                             alarm_ind<-paste(paste(paste('alarm',1:n_alarm_indicators,sep=''),'=',alarm_indicators),collapse =',')
                             create_alarm_inds<-paste('data %>%mutate(',alarm_ind,')',sep='')
                           
                         
                         data<-eval(parse(text=create_alarm_inds))
                         data<-data %>% mutate(district_no=as.numeric(as.factor(district)))
                         
                         #egen district_no=group(district)
                         
                         
                         data<-data %>% arrange(district_no,year_week)
                         
                         #gen obs_no=_n
                         #data<-data %>% mutate(obs_no=1:nrow(data))
                         
                         data<-data %>% dplyr::group_by(district_no)%>% dplyr::mutate(obs_no=1:n())
                         data<-ungroup(data)
                         
                         ## add runnin based on surveillance workbook
                         
                         #data<-data %>% mutate(runin=case_when(generating_surveillance_workbook==F~as.numeric((stop_runin>=year_week)),
                                                               #TRUE~1))
                         
                         data<-data %>% mutate(outbreak_tmp=case_when(runin==1~outbreak,
                                                                      TRUE~as.double(NA)))
                         
                         data<- data %>% dplyr::group_by(district_no) %>% 
                           mutate(outbreak_moving_tmp=case_when(!is.na(outbreak_tmp)~
                                                                  rollapply(outbreak_tmp,
                                                                            FUN=mean,
                                                                            width=list(-3:3),
                                                                            align = "center",
                                                                            fill = NA,na.rm = T,
                                                                            partial=T),
                                                                
                                                                TRUE~outbreak_tmp))
                         data<-ungroup(data)
                         
                         
                         data<-data %>% dplyr::group_by(district_no,week) %>% mutate(outbreak_moving=mean(outbreak_moving_tmp,na.rm=T),
                                                                                     outbreak_moving_sd=sd(outbreak_moving_tmp,na.rm=T))
                         
                         
                         
                         data<-data %>% arrange(district_no,obs_no)
                         
                         #Estimating parameters
                         
                         
                         data<-data %>% mutate(outbreak_moving_limit=outbreak_moving+(z_outbreak*outbreak_moving_sd))
                         
                         
                         data<-data %>% mutate(outbreakweek=case_when(
                           ((outbreak >=outbreak_moving_limit & !is.na(outbreak))|
                              (outbreak >=outbreak_moving_limit & !outbreak_moving_limit==0 & !outbreak==0))~1,
                           TRUE~0
                         ))
                         
                         out_wrks<-outbreak_week_length-1
                         
                         lag_outb<-paste('data %>% group_by(district_no) %>% mutate(sum_outbreak=',paste('dplyr::lag(outbreakweek,',out_wrks:0,')',collapse ='+'),')')
                         
                         
                         data<-eval(parse(text=lag_outb))
                         
                         cascade_func<-function(x){
                           to_rep<-which(is.na(x))
                           xtt<-foreach(a=to_rep) %do% x[(a-1):1][complete.cases(x[(a-1):1])]
                           cdd<-sapply(xtt,FUN=function(x) x[1])
                           cdd1<-which(cdd==1)
                           to_rep1<-to_rep[cdd1]
                           x[to_rep1]<-1
                           x
                           
                         }
                         
                         
                         
                         data<-data %>% mutate(sum_outbreak=case_when(is.na(sum_outbreak)~0,
                                                                      TRUE~sum_outbreak),
                                               none_outbreakweek=outbreak_week_length-sum_outbreak,
                                               outbreakperiod=case_when(sum_outbreak==outbreak_week_length~1,
                                                                        none_outbreakweek==outbreak_week_length~0,
                                                                        TRUE~as.double(NA))) 
                         
                         ## casced by district
                         
                         data<-data %>% mutate(outbreakperiod=cascade_func(outbreakperiod))%>%
                           mutate(outbreakperiod=case_when(is.na(outbreakperiod) ~0,
                                                           TRUE~outbreakperiod))
                         
                         for_mean1<-paste('mean_alarm',1:n_alarm_indicators,sep='')
                         
                         for_mean2<-paste("rollapply(",alarm_indicators,',FUN=mean,width=list(-',alarm_window-1,':0),
                                          align = "center",
                                          fill = NA,na.rm = T,
                                          partial=T)',sep='')
                         
                         for_mean3<-paste('data %>% mutate(',paste(for_mean1,'=',for_mean2,collapse =','),')',sep='')
                         parse(text=for_mean3)
                         
                         data<-eval(parse(text=for_mean3))
                         
                         pre_dist<-(0:(outbreak_window-1))+prediction_distance
                         
                         for_mean_outbreak<-paste("rollapply(",'outbreakperiod',',
                                                  FUN=mean,width=list(',min(pre_dist),':',max(pre_dist),'),',
                                                  'align = "center",
                                                  fill = NA,na.rm = T,
                                                  partial=T)',sep='')
                         
                         for_mean_outbreak1<-paste('data %>% mutate(',paste('outbreak_mean','=',for_mean_outbreak,collapse =','),')',sep='')
                         
                         
                         data<-eval(parse(text=for_mean_outbreak1))
                         
                         ## variable for season
                         
                         #season_length<-12
                         
                         #data$season<-as.numeric(cut(data$week,c(seq(1,48,season_length),53),include.lowest  =F))
                         
                         data<-data %>% mutate(outbreak_mean_cutoff=case_when(outbreak_mean>=outbreak_threshold~1,
                                                                              TRUE~0))
                         
                         
                         loop<- n_alarm_indicators+1
                         
                         ## add spline choice 
                         
                         get_sp<-function(p){
                           
                           if (input$spline==T & al_var[p] %in% sp_var){
                             paste('s(',"mean_alarm",p,',k=4,fx=T)',sep='')
                           }else{
                             paste("mean_alarm",p,sep='')
                           }
                           
                         }
                         
                         #independ_var_name<-paste("mean_alarm",1:n_alarm_indicators,sep="")
                         
                         independ_var_name<-foreach(a=1:n_alarm_indicators,.combine=c)%do% get_sp(a)
                         
                         #* To generate list of all coefficients names(e.g. c1 c2 c3 and so on)
                         cols_name<-paste("coef",1:loop,sep="")
                         ## split week by season
                         paste("season length :",season_length)
                         if(!season_length==1){
                         data<-data %>% mutate(season=cut(week,season_length,include.lowest =T))
                         }else{
                           data<-data %>% mutate(season=cut(week,breaks=c(1,52),include.lowest =T))
                           
                         }
                         ## run by district
                         get_predD<-function(d){
                           
                          data_d<-data %>% filter(district==d) %>% mutate(Year_F=as.factor(year))
                         dat_tra<-data_d %>% filter(runin==1) %>% 
                           dplyr::select(district,week,season,year,Year_F,contains("mean_alarm"),runin,outbreak_mean_cutoff)
                         
                         max_Yr_train<-max(dat_tra$year)
                         
                         data_pred<-data_d %>% filter(runin==0) %>% mutate(Year_F=max_Yr_train)
                         
                         
                         formu1<-paste('outbreak_mean_cutoff~s(week,bs ="cc",k=4,fx=T)+s(Year_F,bs="re")+',
                                      paste(independ_var_name,collapse ="+"),sep='')
                         if(!season_length==1){
                         formu<-paste('outbreak_mean_cutoff~s(season,bs ="re")+',
                                      paste(independ_var_name,collapse ="+"),sep='')
                         }else{
                           formu<-paste('outbreak_mean_cutoff~',
                                        paste(independ_var_name,collapse ="+"),sep='')
                         }
                         mod_gam<-gam(as.formula(formu),family="binomial",data=dat_tra,knots=list(week=c(0,52)))
                         mod_chk<<-mod_gam
                         
                         pred_d<-predict(mod_gam,data_pred,'response')
                         
                         
                         
                         dat_test<-data_d %>% filter(runin==0) %>% 
                           mutate(prob_outbreak=as.numeric(pred_d))
                         
                         ## add dates to the data for plot labelling
                         
                         min_year<-min(data_d$year)
                         max_year<-max(data_d$year)
                         
                         beg_d<-as.Date(paste(min_year,'-01-01',sep=''))
                         end_d<-as.Date(paste(max_year,'-12-31',sep=''))
                         
                         dates_test<-data.frame(date=seq.Date(beg_d,end_d,'day')) %>% 
                           mutate(year=year(date),
                                  month=month(date),
                                  week=week(date),
                                  day=day(date)) %>% filter(week %in% 2:52)
                         
                         dates_x<-dates_test %>% group_by(year,week) %>% mutate(day_choose=min(day)==day) %>% 
                           filter(day_choose==T) %>% group_by(year,month) %>% mutate(week_choose=ceiling(median(week))==week)
                         
                         data_d<-merge(data_d,dates_x,by=c('year','week'),all.x =T,sort=F)
                         
                         pred_all<-c(rep(NA,nrow(dat_tra)),as.numeric(pred_d))
                         
                         ##compute full dataset model without removing test
                         mod_gam_Full<-gam(as.formula(formu),family="binomial",data=data_d)
                         
                         
                         data_d<-data_d %>% mutate(prob_outbreak=pred_all)
                         ## parameters dataset
                         pw<-c(paste(input$alarm_indicators,collapse =','),
                               round(input$z_outbreak,3),
                               input$prediction_distance,
                               input$outbreak_window
                               ,input$alarm_window,
                               round(input$outbreak_threshold,3),
                               round(input$alarm_threshold,3)
                               ,input$outbreak_week_length,
                               input$season_length,
                               paste(input$stop_runinYear,'52',sep=''),
                               input$spline,
                               paste(input$spline_alarm_indicators,collapse =',')
                               )
                         par_names<-c("alarm indicators",
                                      "z outbreak",
                                      "prediction distance",
                                      "outbreak window",
                                      "alarm window",
                                      "outbreak threshold",
                                      "alarm threshold",
                                      "outbreak week length",
                                      "seasons",
                                      "Stop runin",
                                      "Spline",
                                      "Spline vars")
                         
                         cc_v<-data.frame(parameter=par_names,
                                          value=pw)
                         
                         out_l<-list(data_d,mod_gam_Full,cc_v)
                         names(out_l)<-c(paste('data_prob',d,sep=''),
                                         paste("gam_model_",d,sep=''),
                                         paste("param_values_",d,sep='')
                                        )
                         out_l
                      }
                       out_all<-foreach(a=run_per_district,.combine =c)  %do% get_predD(a)
                       
                       ##recombine dataset with probabilities
                       dat_p<-grep('data',names(out_all))
                       dat_p1<-grep('gam|param',names(out_all))
                       dat_p1.1<-grep('param',names(out_all))
                       
                       
                       data<-do.call(rbind,out_all[dat_p])
                       
                       models_ls<-out_all[dat_p1]
                      param_ls<-out_all[dat_p1.1]
                         ## replace NA for runin==1
                         
                         
                         summary(data$prob_outbreak)
                         
                         prob_round<-round(alarm_threshold,6)
                         alarm_var_name<-round((prob_round*1000),1)
                         
                         alarm_eq<-paste('data %>% mutate(alarm_',alarm_var_name,'=case_when(round(prob_outbreak,6)>=prob_round & !is.na(prob_outbreak)~1,
                                         round(prob_outbreak,6)<prob_round & !is.na(prob_outbreak)~0,
                                         TRUE~as.double(NA)','))',sep='')
                         
                         
                         
                         data<-eval(parse(text=alarm_eq))
                         
                         #descr::freq(data$alarm_120)
                         
                         
                         
                         data$alarm_com<-eval(parse(text=paste('data$alarm_',alarm_var_name,sep ='')))
                         data$pop_val<-eval(parse(text=paste('data$',population_var,sep ='')))
                         
                         data<-data %>% mutate(outbreak_ply=case_when(outbreakperiod==1~outbreak,
                                                                      TRUE~as.double(NA)))
                         
                         al_var<-paste("data %>% mutate(alarm_plot=case_when(alarm_",
                                       alarm_var_name,'==1~prob_outbreak,TRUE~as.double(NA)))',sep='')
                         
                         data<-eval(parse(text=al_var))
                         ##runin plot
                         #p<-15
                         endmic<-paste("Endemic Channel (z outbreak=",z_outbreak,')',sep="")
                         get_plots<-function(p){
                           
                           dat_plot1<-data %>% filter(runin==1 & district==p)%>% 
                             group_by(year) %>% mutate(week_choose=ceiling(min(week))==week)
                           ##ungroup the data
                           dat_plot1<-data.frame(ungroup(dat_plot1))
                           
                           tick_points<-which(c(dat_plot1$week_choose)==T)
                           tick_text<-format.Date(dat_plot1$date[tick_points],"%b%y")
                           
                           plot.review<<-list(dat_plot1=dat_plot1,tick_points=tick_points,tick_text=tick_text,endmic=endmic)
                           
                           (plot1<-plot_ly(dat_plot1, x = ~obs_no,width=900,height=570) %>% 
                               add_ribbons(ymin=0,ymax=~outbreak_moving_limit,name=endmic,
                                           opacity=0.7) %>% 
                               add_lines(y=~outbreak,color=I("tomato2"),name="Cases per 1000 pop") %>% 
                               add_markers(y=~outbreak_ply,color=I("red"),name="Outbreak period",
                                           marker=list(symbol="circle",size=8)) %>% 
                               
                               layout(#title=list(text=paste('Runin Period District:',p)),
                                 title=paste('Runin Period District:',p),
                                 orientation="center",
                                 #titlefont=list(color="red"),
                                 yaxis2 = list(overlaying='y',side = "right",
                                               title="Probability of outbreak period",
                                               scaleanchor = "y",mirror=T),
                                 xaxis = list(title="Epidemiological week",dtick="M2",tickvals=tick_points,
                                              ticktext=tick_text,
                                              zeroline=F),
                                 yaxis=list(title="Cases per 1000 pop",visible=T,
                                            scaleanchor = "y2",
                                            showline=F),
                                 legend=list(orientation="h",
                                             xanchor="center"),
                                 margin=list(b=50,r=300)
                               ) )
                           
                           
                           ## plot evaluation period plot
                           
                           
                           #p<-3
                           dat_plot2<-data %>% filter(runin==0 & district==p)%>% 
                             group_by(month) %>% mutate(week_choose=ceiling(median(week))==week)
                           dat_plot2<-data.frame(ungroup(dat_plot2))
                          
                           
                           
                           tick_points<-which(c(dat_plot2$week_choose)==T)
                           tick_text<-format.Date(dat_plot2$date[tick_points],"%b%y")
                           
                           
                           dat_plot2<-data.frame(ungroup(dat_plot2)) %>% dplyr::mutate(plot_n=1:n())
                           lim_prob<-max(dat_plot2$prob_outbreak,na.rm=T)
                           
                           plot2<-plot_ly(dat_plot2, x = ~plot_n,width=800,height=570) %>% 
                             add_ribbons(ymin=0,ymax=~outbreak_moving_limit,name=endmic,
                                         opacity=0.7) %>% 
                             add_lines(y=~outbreak,color=I("tomato2"),name="Cases per 1000 pop") %>% 
                             add_markers(y=~outbreak_ply,color=I("red"),name="Outbreak period",
                                         marker=list(symbol="circle",size=8)) %>% 
                             add_lines(y=~prob_outbreak,color=I("darkgreen"),yaxis = "y2",name="Probability of outbreak period") %>% 
                             add_markers(y=~alarm_plot,color=I("blue"),yaxis = "y2",marker=list(symbol="circle",
                                                                                                size=8),
                                         name="Alarm signal") %>% 
                             add_lines(y=alarm_threshold,line=list(color="black"),yaxis = "y2",
                                       name="Alarm threshold") %>%   
                             layout(title=paste('Evaluation Period District:',p),
                                    yaxis2 = list(overlaying='y',side = "right",title="Probability of outbreak period",
                                                  fixedrange=T,mirror=T,
                                                  rangemode="tozero"),
                                    xaxis = list(title="Epidemiological week",dtick="M2",tickvals=tick_points,
                                                 ticktext=tick_text,fixedrange=T
                                    ),
                                    yaxis=list(title="Cases per 1000 pop",fixedrange=T,rangemode="tozero"),
                                    legend=list(orientation="h",
                                                xanchor="center"),
                                    margin=list(b=50,r=200)
                             ) 
                           
                           
                           ## plot runnin+Evaluation period plot
                           
                           dat_plot3<-data %>% filter(district==p)%>% 
                             group_by(year) %>% mutate(week_choose=ceiling(min(week))==week)
                           dat_plot3<-data.frame(ungroup(dat_plot3))
                           
                           
                           tick_points<-which(c(dat_plot3$week_choose)==T)
                           tick_text<-format.Date(dat_plot3$date[tick_points],"%b%y")
                           
                           
                           plot3<-plot_ly(dat_plot3, x = ~obs_no,width=900,height=600) %>% 
                             add_ribbons(ymin=0,ymax=~outbreak_moving_limit,name=endmic,
                                         opacity=0.7) %>% 
                             #add_lines(y=~outbreak_moving_limit,name=endmic,
                             #opacity=0.7) %>% 
                             add_lines(y=~outbreak,color=I("tomato2"),name="Cases per 1000 pop") %>% 
                             add_markers(y=~outbreak_ply,color=I("red"),name="Outbreak period",
                                         marker=list(symbol="circle",size=8)) %>% 
                             add_lines(y=~prob_outbreak,color=I("darkgreen"),yaxis = "y2",name="Probability of outbreak period") %>% 
                             add_markers(y=~alarm_plot,color=I("blue"),yaxis = "y2",marker=list(symbol="circle",
                                                                                                size=8),
                                         name="Alarm signal") %>% 
                             add_lines(y=alarm_threshold,line=list(color="black"),
                                       name="Alarm threshold",yaxis = "y2") %>%   
                             layout(title=paste('Runin Evaluation Period District:',p),
                                    yaxis2 = list(overlaying="y",side = "right",title="Probability of outbreak period",
                                                  showline=F,fixedrange=T,rangemode="tozero",
                                                  rangemode="nonnegative",zeroline=F,
                                                  mirror=T),
                                    xaxis = list(title="Epidemiological week",dtick="M2",tickvals=tick_points,
                                                 ticktext=tick_text,zeroline=F,fixedrange=T),
                                    yaxis=list(title="Cases per 1000 pop",zeroline=F,fixedrange=T,rangemode="tozero"),
                                    legend=list(orientation="h",
                                                xanchor="center"),
                                    margin=list(b=50,r=300)
                             ) 
                           
                           re_p<-list(plot1,plot2,plot3,dat_plot2)
                           names(re_p)<-c(paste('runin_',p,sep=''),
                                          paste('eval_',p,sep=''),
                                          paste('runin_eval_',p,sep=''),
                                          paste('data_eval_',p,sep=''))
                           re_p
                         }
                         
                         all_plots<-foreach(a=run_per_district,.combine=c)%do% get_plots(a)
                         
                         ##compute stat total table
                         
                         dat_stat<-data %>% filter(runin==0) %>% mutate(correct_alarm=as.numeric(outbreak_mean_cutoff==1 & alarm_com==1),
                                                                        no_alarm_no_outbreak=as.numeric(alarm_com==0 & outbreak_mean_cutoff==0),
                                                                        missed_outbreak=as.numeric(alarm_com==0 & outbreak_mean_cutoff==1),
                                                                        false_alarm=as.numeric(correct_alarm==0 & alarm_com==1),
                                                                        excess_cases=((outbreak-outbreak_moving_limit)*pop_val)/1000,
                                                                        excess_cases=case_when(excess_cases<0~0,
                                                                                               TRUE~excess_cases),
                                                                        weeks=1,
                                                                        total_cases=outbreak*pop_val / 1000,
                                                                        alarm_threshold=alarm_threshold
                         )
                         
                         
                         dat_stat<-dat_stat %>% mutate(cases_below_threshold=
                                                         case_when(excess_cases==0~(outbreak*pop_val /1000),
                                                                   excess_cases!=0 & !is.na(excess_cases)~(outbreak_moving_limit*pop_val /1000),
                                                                   TRUE~as.double(NA)
                                                         ))
                         
                         
                         StatTotal<-dat_stat %>% dplyr::group_by(district) %>% summarise(n_weeks=sum(weeks),
                                                                                         n_outbreak_weeks= sum(outbreakweek),
                                                                                         n_outbreak_periods=sum(outbreakperiod,na.rm =T),
                                                                                         n_alarms=sum(alarm_com),
                                                                                         n_correct_alarms=sum(correct_alarm),
                                                                                         n_false_alarms=sum(false_alarm),
                                                                                         n_missed_outbreaks=sum(missed_outbreak),
                                                                                         n_no_alarm_no_outbreak=sum(no_alarm_no_outbreak),
                                                                                         all_cases =sum(total_cases),
                                                                                         n_outbreak_mean_cutoff=sum(outbreak_mean_cutoff),
                                                                                         n_cases_below_threshold=sum(cases_below_threshold,na.rm =T)) %>% 
                           dplyr::select(district,n_weeks,n_outbreak_weeks,n_outbreak_periods,n_outbreak_mean_cutoff,n_alarms,n_correct_alarms,n_false_alarms,n_missed_outbreaks,n_no_alarm_no_outbreak,all_cases,n_cases_below_threshold)
                         
                         mat_nam<-c("district" ,"weeks" ,"outbreak_weeks" ,"outbreak_periods" ,"defined_outbreaks", "alarms",  
                                    "correct_alarms","false_alarms" ,"missed_outbreaks", "no_alarm_no_outbreak", "all_cases" ,"cases_below_threshold")
                         names(StatTotal)<-mat_nam
                         
                         StatTotal<-data.frame(StatTotal %>% mutate(sensitivity=correct_alarms/(defined_outbreaks),
                                                                    PPV=correct_alarms/(correct_alarms+false_alarms)))
                         ## add parameter values
                         
                         

                    
                         ret<-c(all_plots,models_ls,
                                list(tab=StatTotal,
                                     xls_file=NULL,
                                     auto_tab=NULL))
                         print("reached here 2") 
                         ret
                         
                      },
                     ignoreNULL =T
)



