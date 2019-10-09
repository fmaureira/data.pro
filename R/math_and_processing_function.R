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
