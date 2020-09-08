# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#Math and processing functions####
#the functions here are availble to processing data and calculated function tht are not availble in common libraries in R.
# Copyright (C) <2019>  <Fidel Maureira>
#
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]}


getmode_zero <- function(v) {
  vi<-as.integer(v)
  uniqv <- unique(vi)
  x<-uniqv[which.max(tabulate(match(vi, uniqv)))]
  v0<-v[v>0]
  uniqv0<-unique(v0)
  ifelse(x>0,uniqv0[which.max(tabulate(match(v0,uniqv0 )))],x)
  ifelse(x>0,stats::median(v0),x)
}
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

upper_envelope_fun<-function(x,n_seg=32){
  n_row<-length(x)
  segm<-as.integer(n_row/n_seg)
  seq_alongdata<-seq(from=0, to=n_row,by=segm)
  sq<-seq_along(seq_alongdata)
  upper_envelope<-c()

  for(i in sq){
    if (i==max(sq)){break}#j<-n_row} else{j<-seq_alongdata[i+1]}
    j<-seq_alongdata[i+1]
    select<-x[seq_alongdata[i]:j]
    pos_max_yield<-which.max(select)
    upper_envelope<-c(upper_envelope,select[pos_max_yield])

  }
  upper_envelope<-unique(upper_envelope)
  return(list(x_upper=upper_envelope,index=which(x %in% upper_envelope)))
}
unitization_with_zero_minimum<-function(x,na.rm=TRUE){
  rg_x<-range(x,na.rm=na.rm)
  #x[is.na(x)]<-mean(x,na.rm=na.rm)
  (x-min(x,na.rm=na.rm))/(rg_x[2]-rg_x[1])
}

percentil_above<-function(x,critical_value,na.rm=TRUE) {
  y<-quantile(x,prob=critical_value,na.rm=na.rm)
  x[x>y]<-y
  x/y
}

#Conversion of units from ####
units_conversion<-function (value,unit_from, convertion_2finalunit=NULL){
  #function convert to kg, ha, $ and percentage whatever is as numerator or denominator. Also can convert to other units if is provide a convertion factor.
  #If does not find the unit of convertion return a unknown string
  #from USDA-ERS, 1992 conversion units
  #100 pounds = 1 cwt
  #20 cwt = 1 ton
  #1 kg = 2.204 pounds
  #1 kg = 0.001102311 short tons
  # 1 ha 2.471 acres
  #define type of unit
  units<-gsub(pattern = "\\s",
              replacement = "",
              x = as.character(unit_from))
  units<-unlist(strsplit(units,'/'))
  value<-as.numeric(value)
  if (!is.null(convertion_2finalunit)) { #conversion is knew
    values<-value*convertion_2finalunit
  } else { #conversion is unkown
    if (length(units)==1) { #single conversion
      values<-determine_kind_of_unit(units,value)
    } else { #unit is a ratio conversion
      values<-c(determine_kind_of_unit(units[1],value),
                determine_kind_of_unit(units[2],1)) #denomitor assumed the numerator is divide by a unit of denominator
      msg<-unlist(lapply(values,  function (x) {if (is.na(x)) {NA}}))
      if(!is.null(msg)) {values=msg} else {
        if (values[2]==0) {values<- values[1]} else {values<- values[1]/values[2]}
      }
    }
  }
  return(values)
}

determine_kind_of_unit<- function (units,value) {
  weight_units<-c('grams','g','kg','tons','ton','cwt', 'hundredweight','lb','lbs','pound','pounds','mpounds')
  area_units<-c('ha', 'hectare','ac','acre','acres', 'sq m', 'square m', 'sq foot', 'sq ft')
  units<-tolower(as.character(units))
  if (units %in% weight_units) { #weight
    'weight_units'
    weight_conversion(units,value)
  } else if (units %in% area_units) { #area
    'area'
    area_conversion(units,value)
  } else if(grepl('$',units,fixed = TRUE)) { #monetary unit
    'monetary_unit'
    value
  } else if(grepl('%',units,fixed = TRUE)|units %in% c('perc','percentage')) { #perc
    "percentage"
    value
  }else {

    'unknown'
  }

}

weight_conversion<-function(units, value) {
  weight_units<-c('grams','g','gr','kg','tons','ton','cwt', 'hundredweight','lb','lbs','pound','pounds', 'mpounds') #assumed tons is short tons. SII tons is metric tons
  weight_conversion2kg<-c(1000,1000,1000,1,0.001102311,0.001102311,0.02204623,0.02204623,2.204623,2.204623,2.204623,2.204623,2.204623/10^6)

  (1/weight_conversion2kg[grepl(units,weight_units,fixed=TRUE)][1])*value
}

area_conversion<-function(units, value) {
  area_units<-c('ha', 'hectare','ac','acre','acres', 'sq m', 'square m', 'sq foot', 'sq ft')
  area_conversion2ha<-c(1,1,2.471044,2.471044,2.471044,10000,10000,107638.7,107638.7)

  (1/area_conversion2ha[grepl(units,area_units,fixed=TRUE)][1])*value
}



######



