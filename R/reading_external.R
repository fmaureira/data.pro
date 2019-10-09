#reading external files
#this function are destinated to read external files. en general would be sort by topics of interest
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


####Read htm Cropsyst outputs####
workerfun <- function(i) {
  tryCatch({
    read_htm_out(i)
  },
  error=function(e) {
    print(e)
    stop(e)
  })
}


openHTML <- function(x=NA) { if (!is.na(x)) {paste0('file://', file.path(getwd(), x))}}

read_htm_out<-function(connection){
  requireNamespace('XML')
  requireNamespace('RCurl')
  requireNamespace('rlist')
  sink('.historialof_files.txt',append=TRUE)
  cat(paste(format(Sys.time(), "%b %d %r"),connection,'\n'))
  sink()
  full_path<-strsplit(dirname(connection),'Output')[[1]]
  tmp<-basename(full_path)
  theurl <- RCurl::getURL(openHTML(connection),.opts = list(ssl.verifypeer = FALSE) )
  tables <- XML::readHTMLTable(connection)
  #tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  #df2<-data.frame(HarvestYear=c(1981:2018))
  if (n.rows>1) {
    df<-tables[[1]]
    df<-df[-1,]
    names(df)<-c(names(df)[1:16],'Cropname')
    df[1:16]<-lapply(df[1:16],as.character)
    df[1:16]<-lapply(df[1:16],as.numeric)
    df$Cropname<-as.character(df$Cropname)
    df$Yield.Mg.ha<-df$`Yield Grain, tuber, leaf, fruit or root`/1000
    df$Planting<- as.numeric(df$Planting)

    df$date.m.d<-as.Date(paste0('2019-',df$Planting),format='%Y-%j')

    df$full_path<-full_path
    df$Folder.6<-tmp} else {

      sink("./logofailed_files.txt",append=TRUE)
      cat(paste(connection,'\n'))
      sink()
      df<-data.frame()
    }#df2}
  # df2$Cropname<-df$Cropname[!is.na(df$Cropname)][1]
  # df<-merge(df2,df,all.x=TRUE)
  # df$Yield.Mg.ha[is.na(df$Yield.Mg.ha)]<-0
  # df$Planting[is.na(df$Planting)]<-df$Planting[!is.na(df$Planting)][1]
  df}
