

rotation_head<-function(description,end_year,count=1, version=c('v4','v5')){
a1<- paste0('[rotation]
details_URL=
description=',description,'\n','end_year=',end_year,'
years=1
sowing:count=',count,'\n')
if (version=='v4'){
a2<-paste0('[version]
major=4
release=19
minor=17')}
if (version=='v5'){a2<-paste0('[version]','\n','code=25992656','\n','program=2019108')}

  paste0(a1,a2)
}



sowing<-function(crop,mgt, number_event,path="C:/path/Database/",date_event=as.Date(),planting_mode='date',avg_temp=0){
  path<-chartr("/", "\\", path)
data_event<-format(date_event,'%Y/%m/%d')
cat(paste0('[sowing:',number_event,']
ID=',(format(Sys.time(), '%Y%j.%H%M%S')),' (default)
res_set_ID=0
contingency=none
operation_type=
operation_name=
operation_filename=/
enable_for_model=secondary_only
event_synchronization=actual_date','\n',
'event_date=',(format(date_event, '%Y%j')),' (',(date_event),' (actual date))','\n',
'checked=true
enabled=true
description=crop:',crop,'.crp | management:',mgt,'.mgt
name=
start_hour=0 Start hour
duration=6 Duratation
OP_ID=0
carbon_footprint=0.00000000
terminate_crop=true
NRCS_field_op=
LAND_USE_ID=0
MGT_ID=0
crop=',path,'\\Crop\\',crop,'.crp
management=',path,'\\Management\\',mgt,'.mgt',
ifelse(planting_mode=='date','\n',paste0('\n','plant_by=0
mode=min_temp_req
delay_after_precip=0 Days
expected_days_to_emerge=',format(date_event, '%j'),' Days','\n',
'avg_temp=',avg_temp,' °C
min_PAW=0.00 0-1
max_PAW=0.00 0-1
appreciable_precip=0.0 mm')),'\n',
'seeding_rate=300.00000000 seeds/m²
emergence_rate=100.00000000 Emergence rate
sowing_depth=5.00000000 Sowing depth','\n',
ifelse(planting_mode=='date','\n',paste0('date=',format(date_event, '%Y%j'),'\n'))))
}

write_file_rotation<-function(filename, directory=getwd(),rotation_df,
                              column=list(crop=1,mgt=2,number_event=3,path=4,order=4,sowing_event=5,avg_temp=NA,sowing_mode='date'),
                              description="NA",version='v4',end_year) {


  crop<-rotation_df[column$crop]
  crop<-as.character(crop[,1])
  mgt<-rotation_df[column$mgt]
  mgt<-as.character(mgt[,1])
  number_event<-rotation_df[column$number_event]
  number_event<-as.numeric(number_event[,1])
  path<-rotation_df[column$path]
  path<-as.character(path[,1])
  sowing_event<-rotation_df[column$sowing_event]
  sowing_event<-as.Date(sowing_event[,1])
  sowing_mode=as.character(rotation_df[,column$sowing_mode])
  if (!is.na(column$avg_temp)) {avg_temp<-rotation_df[column$avg_temp]
  avg_temp<-as.character(avg_temp[,1])}

  number_events<-max(number_event)
  file_name<-paste0(directory,'\\',filename,'.rot')
  create_scenario_rotation(directory)
  sink(file=file_name)
  cat(rotation_head(description,end_year ,count = number_events, version))
  cat('\n')
  for( i in number_event) {
    cat(sowing(crop[i],mgt[i],i,path[i],sowing_event[i],sowing_mode[i],avg_temp[i]))
    #cat('\n')
  }
  sink()
}
create_scenario_rotation<-function(directory){
  if (!dir.exists(directory)){
      dir.create(directory,showWarnings = FALSE,recursive=TRUE)
  }
}

write_file_scenario<-function(rot_filename=NULL, file_name_CS=NULL,weather_file=NULL,
                               CO2=NULL,soil=NULL,
                               simulation_date=NULL, write_to_file=FALSE,
                              additional_arguments=NULL,append=FALSE,overwrite=TRUE) {
  #argument<-
  rot = weather = CO2_argument = simulation = soil_argument = add_argument= NULL
  if(!is.null(rot_filename)) {rot<-paste0('[cropping_system]
cropping_mode=rotation
rotation=',rot_filename,'.rot','\n')}
  if(!is.null(weather_file)) {
    weather<-paste0('[parameter_filenames]
weather_database=',weather_file,'\n')}
  if (!is.null(CO2)){ if(CO2){CO2_argument <- paste0('[CO2]
enable=true
[parameter_filenames]
recalibration=~CONTEXT:\\Database\\Weather\\future_period_2018-2099\\RCP8.5\\atmospheric_CO2.rcl','\n')} else {
  CO2_argument<-paste0('[CO2]
enable=false
initial_CO2_conc=350
annual_CO2_change=0','\n')
}}
  if(!is.null(simulation_date)){simulation<-paste0('[simulation]
start_date=',format(simulation_date[1], '%Y%j'),'\n',
                                                  'stop_date=',format(simulation_date[2], '%Y%j'),'\n')}
  if(!is.null(soil)) {
    soil_argument<-paste0('[parameter_filenames]
soil=',soil,'\n')}
  if(!is.null(additional_arguments)) {
    add_argument<-paste0(additional_arguments,'\n')}

  argument <- paste0(rot,weather,CO2_argument,simulation,soil_argument,add_argument)

  if (write_to_file){

    #file_name_CS<-paste0(directory,'/','.CropSyst_scenario')
    #directory<-chartr("/", "\\", directory)
    file_exists<-file.exists(file_name_CS)
    if (overwrite|(!file_exists)) {
      sink(file=file_name_CS,append = append)
      cat(argument)
      sink()

      }} else {return(argument)}

}


#x <- cat(paste("G:",nchar("\\"),"potato",nchar("\\"),sep=""))
# #x <- "fa\xE7ile"
# Encoding(x) <- "UTF-8"
# x<-"G:/potato/"
# chartr("/", "\\",path)


#
# chr <- function(n) { rawToChar(as.raw(n)) }
# chr(92)
# cat("C:\\potato")

create_rotation_new<-function(crop_mgt_path_order_dayOfYear,
                              column=list(crop="crop",
                                          mgt="management",
                                          path="database_path",
                                          order='order',
                                          sowing_day="sowing_day",
                                          avg_temp="sowing_temp",
                                          sowing_mode="sowing_mode"),
                              year_start,year_end, hemisphere_north=TRUE,order_df=TRUE){
  if (order_df==TRUE){
    crop_mgt_path_order_dayOfYear<-crop_mgt_path_order_dayOfYear[order(crop_mgt_path_order_dayOfYear[column$order]),]}

  crop<-crop_mgt_path_order_dayOfYear[column$crop]
  crop<-as.character(crop[,1])
  mgt<-crop_mgt_path_order_dayOfYear[column$mgt]
  mgt<-as.character(mgt[,1])
  sowing_day<-crop_mgt_path_order_dayOfYear[column$sowing_day]
  sowing_day<-as.character(sowing_day[,1])
  path<-crop_mgt_path_order_dayOfYear[column$path]
  path<-as.character(path[,1])
  sowing_mode<-as.character(crop_mgt_path_order_dayOfYear[,column$sowing_mode])
  if( !sowing_mode=='date') {
    avg_temp<-crop_mgt_path_order_dayOfYear[column$avg_temp]
    avg_temp<-as.numeric(round(avg_temp[,1],2))
    seasonofsowing<-getSeason(as.Date(paste(year_start,sowing_day),format='%Y %j'),
                              hemisphere_north)
    yearofsowing<-ifelse(seasonofsowing=="Winter"|seasonofsowing=="Spring",0,
                         -1)
  } else {yearofsowing=0}

  count_years<-0
  number_crops<-nrow(crop_mgt_path_order_dayOfYear)
  rotation<-data.frame()
  event_number<-0
  for (i in year_start:year_end) {
    event_number<-event_number+1
    count_years<-ifelse(count_years<number_crops,count_years+1,1)
    year_event<-ifelse(i+yearofsowing[count_years]<year_start,year_start,i+yearofsowing[count_years])
    rotation_line<-data.frame(crop=crop[count_years],mgt=mgt[count_years],event_number=event_number,
                              path=path[count_years],
                              sowing_event=as.Date(paste(year_event,sowing_day[count_years]),format='%Y %j'),
                              sowing_mode=sowing_mode[count_years])
    if( !sowing_mode=='date') {rotation_line$avg_temp=avg_temp[count_years]}
    rotation<-rbind(rotation,rotation_line)
  }
  return(rotation)
}



create_rotation<-function(crop_mgt_path_order_dayOfYear,column=list(crop=1,
                          mgt=2,path=3,order=4,sowing_day=5),
                          year_start,year_end, hemisphere_north=TRUE){

  crop_mgt_path_order_dayOfYear<-crop_mgt_path_order_dayOfYear[order(crop_mgt_path_order_dayOfYear[column$order]),]

  crop<-crop_mgt_path_order_dayOfYear[column$crop]
  crop<-as.character(crop[,1])
  mgt<-crop_mgt_path_order_dayOfYear[column$mgt]
  mgt<-as.character(mgt[,1])
  sowing_day<-crop_mgt_path_order_dayOfYear[column$sowing_day]
  sowing_day<-as.character(sowing_day[,1])
  path<-crop_mgt_path_order_dayOfYear[column$path]
  path<-as.character(path[,1])

  seasonofsowing<-getSeason(as.Date(paste(year_start,sowing_day),format='%Y %j'),
                          hemisphere_north)
  yearofsowing<-ifelse(seasonofsowing=="Winter"|seasonofsowing=="Spring",0,
                       -1)

  count_years<-0
  number_crops<-nrow(crop_mgt_path_order_dayOfYear)
  rotation<-data.frame()
  event_number<-0
  for (i in year_start:year_end) {
    event_number<-event_number+1
    count_years<-ifelse(count_years<number_crops,count_years+1,1)
    year_event<-ifelse(i+yearofsowing[count_years]<year_start,year_start,i+yearofsowing[count_years])
    rotation_line<-data.frame(crop=crop[count_years],mgt=mgt[count_years],event_number=event_number,
                              path=path[count_years],sowing_event=as.Date(paste(year_event,sowing_day[count_years]),
                                              format='%Y %j'))
    rotation<-rbind(rotation,rotation_line)
  }
  return(rotation)
  }

  getSeason <- function(DATES,hemisphere_north=TRUE) {
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    if (hemisphere_north){
    WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))

    } else {
      SS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Summer Solstice
      FE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Fall Equinox
      WS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Winter Solstice
      SE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Spring Equinox
      ifelse (d >= WS & d < SE, "Winter",
              ifelse (d >= SE & d < SS, "Spring",
                      ifelse (d >= SS | d < FE, "Summer", "Fall")))
    }

  }

  find_unit_path<-function (path,anchor_folder="Dropbox") {
    path_of_this_computer<-getwd()
    if (grep(anchor_folder,path_of_this_computer)){
      positions<-regexpr(anchor_folder,path_of_this_computer)
      start_length<-positions[1]+as.numeric((attributes(positions)[1]))
      unit<-substr(path_of_this_computer,1,start_length)
      new_path<-paste0(unit,substr(path,1+start_length,nchar(path)))

    }
    return(new_path)
  }
  split_rot<-function(rot,label=c(),id=c(1:n),split="_"){
    library(plyr)
    check<-data.frame(label=label,id=id)
    check$label<-as.character(check$label)
    rot_c<-data.frame(label=unlist(strsplit(rot,split)))
    n_row<-nrow(rot_c)
    rot_c$original_order=c(1:n_row)
    out=join(check,rot_c)
    out_1=na.omit(out[,c(1,3)])
    return(out_1)
    #(check$label[check$rot])

  }
  rwind<-  function(string){
    chartr("/","\\",string)
    #gsub('"', "", gsub("////", "/", string))
  }

