#' Edit multiple files side by side
#' @importFrom shiny runApp
#' @export
editFiles=function(){
     shiny::runApp(system.file('multipleFiles2',package='editData'))
}

#' Extract extension from a file name
#' @param filename A character string naming a file
file2ext=function(filename){
     namelist=unlist(strsplit(filename,".",fixed=TRUE))
     result=namelist[length(namelist)]
     return(tolower(result))
}

#'read csv file
#' @param file A character string naming a file
#' @param ... Further arguments to be passed to read.csv
#' @export
myimport_csv=function(file,...){

        data1<-tryCatch(read.csv(file,...),error = function(e) "error")
        if(class(data1)=="character") data1<-tryCatch(read.csv(file,fileEncoding="euc-kr",...),error = function(e) "error")
        result<-tryCatch(max(sapply(data1,nchar),na.rm=TRUE),error = function(e) "error")
        if(!is.numeric(result)) {
                data1=read.csv(file,fileEncoding="euc-kr",...)
        }
        data1
}

#' Read in a data.frame from a file
#' @param file A character string naming a file
#' @param ... Further arguments to be passed to rio::import
#' @importFrom utils read.csv
#' @importFrom rio import
#' @export
myimport=function(file,...){
     ext=file2ext(file)
     if(ext=="csv"){
          result<-myimport_csv(file)
     } else{
          result=rio::import(file,...)
     }
     result
}
