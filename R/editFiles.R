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


#' Read in a data.frame from a file
#' @param file A character string naming a file
#' @param ... Further arguments to be passed to rio::import
#' @importFrom utils read.csv
#' @importFrom rio import
#' @export
myimport=function(file,...){
     ext=file2ext(file)
     if(ext=="csv"){
          result<-tryCatch(read.csv(file,stringsAsFactors = FALSE),error=function(c) "error")
          if(class(result)!="data.frame"){
               result<-tryCatch(read.csv(file,stringsAsFactors = FALSE,fileEncoding = "euc-kr"),error=function(c) "error")
          }
     } else{
          result=rio::import(file,...)
     }
     result
}
