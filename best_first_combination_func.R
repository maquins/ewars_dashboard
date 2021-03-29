# contact: sewemaquins@gmail.com
weeks_func<-function(p,dis_N){
  week_sel<-week_params[p,]
  outbreak_week_length<-week_sel$outbreak_week_length
  alarm_window<-week_sel$alarm_window
  alarm_threshold<-0.01
  z_outbreak<-1.005
  outbreak_window<-week_sel$outbreak_window
  prediction_distance<-week_sel$prediction_distance
  outbreak_threshold<-0.6
  season_length<-1
  #stop_runin<-"201252"
  data<-data_orig %>% filter(district==dis_N)
  max_yr<-max(data$year,na.rm =T)
  stop_runin<-paste(max_yr-1,"52",sep='')
  
  data<-data %>% filter(!week %in% c(1,53)) 
  
  
  ##compute cases per 1000
  al_var<- str_split(alarm_indicators,pattern=",",simplify =T)
  n_alarm_indicators=length(al_var)
  
  cases_per1000<-paste0(number_of_cases,'/',population_var,sep='')
  alarm_ind<-paste(paste(paste('alarm',1:n_alarm_indicators,sep=''),'=',alarm_indicators),collapse =',')
  create_alarm_inds<-paste('data %>%mutate(',alarm_ind,')',sep='')
  
  
  
  data<-data %>% mutate(outbreak=eval(parse(text=cases_per1000))*1000,
                        year_week=year*100+week) 
  data<-eval(parse(text=create_alarm_inds))
  data<-data %>% mutate(district_no=as.numeric(as.factor(district)))
  #sort district_no year_week
  
  data<-data %>% arrange(district_no,year_week)
  data<-data %>% dplyr::group_by(district_no)%>% mutate(obs_no=1:dplyr::n())
  
  #gen outbreak_tmp=outbreak if runin==1
  
  ## add runnin based on surveillance workbook
  
  #data<-data %>% mutate(runin=case_when(generating_surveillance_workbook%in%c(F,T)~as.numeric((stop_runin>=year_week)),
  #TRUE~1))
  
  data<-data %>% mutate(runin=as.numeric((stop_runin>=year_week)))
  
  
  #summary(data)
  
  data<-data %>% mutate(outbreak_tmp=case_when(runin==1~outbreak,
                                               TRUE~as.double(NA)))
  ## compute lags and leads and compare with rollapply
  
  lag1<-paste(paste(paste('lag',3:0,sep=''),'=',paste('dplyr::lag(outbreak_tmp,',3:0,')',sep='')),collapse =',')
  lead1<-paste(paste(paste('lead',1:3,sep=''),'=',paste('dplyr::lead(outbreak_tmp,',1:3,')',sep='')),collapse =',')
  lag_lead<-paste(lag1,lead1,sep =',')
  
  create_smooth<-paste('data %>% dplyr::group_by(district_no) %>% mutate(',lag_lead,')',sep='')
  
  
  data<-data %>% arrange(year_week)
  get_tmp<-paste('data %>% mutate(outbreak_moving_tmp=(',paste(c(paste('lag',0:3,sep=''),paste('lead',1:3,sep='')),collapse ='+'),'))',sep='')
  
  #data<-eval(parse(text=get_tmp))
  
  data_lags<-data %>% dplyr::select(matches('lead|lag'))
  
  data_lags<-data_lags[,-1]
  
  data_lags$summ<-apply(data_lags,1,FUN=function(x) sum(x,na.rm =T))/7
  
  data_lags$summ_na<-apply(data_lags,1,FUN=function(x) sum(is.na(x)))
  
  sum(1:5, NA, na.rm = TRUE)
  vccc<-ungroup(data[52:56,])
  ?sum
  ## compare with rollapply
  
  data<- data %>% dplyr::group_by(district_no) %>% 
    mutate(outbreak_moving_tmp=case_when(!is.na(outbreak_tmp)~
                                           rollapply(outbreak_tmp,
                                                     FUN=mean,
                                                     width=list(-3:3),
                                                     align = "center",
                                                     fill = NA,na.rm = T,
                                                     partial=T),
                                         
                                         TRUE~outbreak_tmp))
  
  data<-data.frame(ungroup(data) %>% arrange(district_no,year_week))
  
  (pxx<-data[1,])
  class(data)
  data<-data %>% dplyr::group_by(district_no,week) %>% mutate(outbreak_moving=mean(outbreak_moving_tmp,na.rm=T),
                                                              outbreak_moving_sd=sd(outbreak_moving_tmp,na.rm=T))
  
  
  
  data<-data %>% arrange(district_no,obs_no)
  
  class(data)
  
  data<-data.frame(ungroup(data))
  
  check_2<-data %>% select(district,runin,year_week,outbreak,outbreak_tmp,outbreak_moving_tmp)
  
  #Estimating parameters
  
  
  data<-data %>% mutate(outbreak_moving_limit=outbreak_moving+(z_outbreak*outbreak_moving_sd))
  
  ## replace missing with +infinity
  
  #data$outbreak<-ifelse(is.na(data$outbreak),Inf,data$outbreak)
  
  data<-data %>% mutate(outbreakweek=case_when(
    ((outbreak >=outbreak_moving_limit & !is.na(outbreak))|
       (outbreak >=outbreak_moving_limit & !outbreak_moving_limit==0 & !outbreak==0))~1,
    TRUE~0
  ))
  
  check_2<-data %>% select(district,runin,year_week,outbreak,outbreak_tmp,outbreak_moving_tmp,outbreakweek)
  descr::freq(check_2$outbreakweek)
  
  data<-data %>% arrange(district_no,obs_no)
  
  ##create outbreak period, compute number of outbreaks vs no outbreaks
  out_wrks<-max(c(outbreak_week_length-1,0))
  
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
  descr::freq(data$outbreakperiod)
  
  #descr::freq(cascade_func(data$outbreakperiod))
  
  
  ##cascade replacements & replace missing with 0
  if(length(which(is.na(data$outbreakperiod)))>0){
    data<-data %>% mutate(outbreakperiod=cascade_func(outbreakperiod))
  }
  data<-data %>%mutate(outbreakperiod=case_when(is.na(outbreakperiod) ~0,
                                                TRUE~outbreakperiod))
  
  ttt1<<-data
  
  descr::freq(data$outbreakweek)
  descr::freq(data$sum_outbreak)
  descr::freq(data$outbreakperiod)
  
  ##create outbreak period
  
  #wrk_15<-data %>% filter(district==15)
  
  summary(data$outbreak_moving_limit)
  
  dat_outmov<-data %>% select(outbreak_moving)
  
  ##create mean alarm with rollaplly
  
  ## generate code to compute mean alarm
  
  
  for_mean1<-paste('mean_alarm',1:n_alarm_indicators,sep='')
  
  for_mean2<-paste("rollapply(",alarm_indicators,',FUN=mean,width=list(-',alarm_window-1,':0),
                   align = "center",
                   fill = NA,na.rm = T,
                   partial=T)',sep='')
  
  for_mean3<-paste('data %>% mutate(',paste(for_mean1,'=',for_mean2,collapse =','),')',sep='')
  parse(text=for_mean3)
  
  data<-eval(parse(text=for_mean3))
  ##verify the output 
  #ungroup(data[290:308,]) %>% dplyr::select(district,year_week,rainsum,mean_alarm1)
  
  mean(c(62.2,2.2,18))
  
  ##compute mean of outbreak season
  names(data)
  class(data)
  
  #data %>% filter(rainsum==3)[1:10,]
  
  ##compute mean of outbreak season
  
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
  #str(data$season)
  
  #descr::freq(cut(data$week,c(seq(1,48,season_length),53),include.lowest  =F))
  
  ##create outbreak mean cutoff
  
  data<-data %>% mutate(outbreak_mean_cutoff=case_when(outbreak_mean>=outbreak_threshold~1,
                                                       TRUE~0))
  
  descr::freq(data$outbreak_mean_cutoff)
  
  ##create regression coefficients,regression is done by season and district
  
  loop<- n_alarm_indicators+1
  independ_var_name<-paste("mean_alarm",1:n_alarm_indicators,sep="")
  
  #* To generate list of all coefficients names(e.g. c1 c2 c3 and so on)
  cols_name<-paste("coef",1:loop,sep="")
  
  paste("season length :",season_length)
  if(!season_length==1){
    data<-data %>% mutate(season=cut(week,season_length,include.lowest =T))
  }else{
    data<-data %>% mutate(season=cut(week,breaks=c(1,52),include.lowest =T))
    
  }
  
  dat_tra<-data %>% filter(runin==1)  %>% mutate(Year_F=as.factor(year)) %>% 
    dplyr::select(district,week,season,year,Year_F,contains("mean_alarm"),runin,outbreak_mean_cutoff)
  max_Yr_train<-max(dat_tra$year)
  
  data_pred<-data %>% filter(runin==0) %>% mutate(Year_F=max_Yr_train)
  
  formu1<-paste('outbreak_mean_cutoff~s(week,bs ="cc",k=4,fx=T)+s(Year_F,bs="re")+',
                paste(independ_var_name,collapse ="+"),sep='')
  
  if(!season_length==1){
    formu<-paste('outbreak_mean_cutoff~s(season,bs ="re")+',
                 paste(independ_var_name,collapse ="+"),sep='')
  }else{
    formu<-paste('outbreak_mean_cutoff~',
                 paste(independ_var_name,collapse ="+"),sep='')
  }
  
  #data$outbreak_mean_cutoff[]
  
  
  
  mod_gam<-gam(as.formula(formu),family="binomial",data=dat_tra)
  
  pred_d<-predict(mod_gam,data_pred,'response')
  
  pro_obs<-cbind(data$outbreak_mean_cutoff[which(data$runin==0)],
                 as.numeric(pred_d)) 
  
  pro_obs1<-pro_obs[complete.cases(pro_obs),]
  
  if(sum(pro_obs1[,1])>0){
    
    ro.d<-plot(rocit(as.numeric(pred_d),data$outbreak_mean_cutoff[which(data$runin==0)]))
    
    
    alarm_threshold<-  round(as.numeric(ro.d$`optimal Youden Index point`[4]),6) 
    for_roc_gam<-data %>% filter(runin==0) %>% dplyr::select(district,outbreak_mean_cutoff) %>% 
      mutate(prob_outbreak=round(as.numeric(pred_d),6),
             pred_out=as.numeric(prob_outbreak>=alarm_threshold))
    for_roc_gam1<<-for_roc_gam
    #use confusion matrix
    
    (bx<-confusionMatrix(factor(for_roc_gam$pred_out,levels =c(1,0)),
                         factor(for_roc_gam$outbreak_mean_cutoff,levels =c(1,0))
    ) )
    confusion_out<<-bx
    print(bx$overall[2])
    #Pro<<-Pro+1
    #n_d<-length(run_per_district)
    #Pro_pcn<-paste(round((Pro/(N_RUN*n_d))*100,1),'%',sep='')
    #incProgress(1/(N_RUN*n_d),message =paste('auto calib.. running (',Pro_pcn,') ',sep='\n'))
    bx$overall[2]
    week_sel$alarm_threshold<-alarm_threshold
    week_sel$kappa=bx$overall[2]
  }else{
    week_sel$alarm_threshold<-NA
    week_sel$kappa<-NA
  }
  data.frame(district=dis_N,par_pos=p,week_sel)
}
