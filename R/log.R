#' Write log files.
#'
#' Internal functions to be called from within another functions (see details).
#'
#' These functions are used together to write logfiles containing information related
#' to the outputs produced by the function that calls these internal functions.
#' The functions should be used as follows:
#'
#' \code{.openlog} opens a log file
#'
#' \code{.addlog} optionally adds additional messages to the log.
#'
#' \code{.closelog} closes the log file opened by \code{.openlog}.
#'
#'
#' Note that if arguments \code{filename} and \code{dir.output} are missing,
#' they must be defined in the environment of the function that calls the internal functions (see examples).
#'
#' The name of the logfile produced is defined by the \code{filename} plus the extension .txt.
#' Therefore, the logfile has the same name of the product it refers to
#' (e.g. output_of_a_function.txt).
#' The log file is saved in directory "logfiles", found inside the folder of \code{filename}.
#'
#' The content of the logfile is devided in three sections:
#' \emph{System info}, \emph{Function info}, and \emph{Additional info}.
#' \emph{System info} includes \emph{User}, \emph{Nodename}, \emph{Operating system},
#' \emph{System time}, and \emph{logfile version}.
#' \emph{Function info} includes \emph{logfile function} (i.e. the function that produced the log),
#' \emph{Data produced}, and \emph{Data location}.
#' Both \emph{System info} and \emph{Function info} are automatically produced by \code{.openlog}.
#' \emph{Additional info} includes all the messages added with function
#' \code{.addlog}, plus the time elapsed (in seconds), which is automatically added by \code{.closelog}.
#'
#' \code{.addlog} may be used as many times as needed;
#' each message added will correspond to a line in the logfile.
#'
#' @keywords internal
#'
#' @param filename (optional) character. Full name of the file to be produced by the function that calls the internal function. If missing, command \code{get("filename", envir = parent.frame())} is used.
#' @param fun (optional) character. Name of the function that opens the log file. If missing, command \code{sys.call(-1)[[1]]} is used, but it won't work when the log is opened inside a foreach loop.
#' @param dir.output (optional) character vector. Where the outputs of the function that calls the internal functions will be saved.
#' @param cancel logical. Cancels the logfile (nothing is produced).
#' @param ...  character. Strings to be concatenated and form a message.
#' @param sep a character string to separate the strings passed via \code{...} (see \code{\link{paste}})
#'
#'
#' @examples
#' x<-file.path(tempdir(),"S2A_L2A_20171002_T29SNC.tif")
#' y<-dirname(x)
#' fun<-function(a,b){
#'   logfile:::.openlog(a)
#'   logfile:::.addlog("Add", "a message", "to the logfile")
#'   logfile:::.closelog(b)
#' }
#' fun(x,y) # check logfile in folder y
#'
# equivalent to
#' fun<-function(filename,dir.output){
#'   logfile:::.openlog()         # no argument is provided as defined in fun
#'   logfile:::.addlog("Add", "a message", "to the logfile")
#'   logfile:::.closelog()        # no arguments are provided as defined in fun
#' }
#' fun(x,y)
#'
#' @name internal.logfiles
NULL

#' @rdname internal.logfiles
.openlog<-function(filename, fun){
  if(missing(filename)) filename<-get("filename", envir = parent.frame()) # get the product name related to the logfile
  # .check_classes("character", filename)
  # if(!missing(fun)) .check_classes("character", fun)
  assign("t_start", Sys.time(), envir = parent.frame())                   # make start time visible to .closelog()
  logname<-file.path(tempdir(), paste0(basename(filename),".txt"))        # define path to a temporary logfile
  assign("logfile", file(logname, open="a"),envir = parent.frame())       # open connection to the temporary logfile

  # add system information to logfile
  system<-Sys.info()
  session<-utils::sessionInfo()
  cat("###########################################################################", file=logname, sep="\n")
  cat("System info\n",                                                               file=logname, sep="\n", append=TRUE)
  cat(paste("User:",              system[7],                          sep="\t\t\t"), file=logname, sep="\n", append=TRUE)
  cat(paste("Nodename:",          system[4],                          sep="\t\t"),   file=logname, sep="\n", append=TRUE)
  cat(paste("Operating system:",  session$running,                    sep="\t"),     file=logname, sep="\n", append=TRUE)
  cat(paste("System time:",       Sys.time(),                         sep="\t\t"),   file=logname, sep="\n", append=TRUE)
  cat(paste("logfile version:",      utils::packageVersion("logfile"),"\n", sep="\t\t"),   file=logname, sep="\n", append=TRUE)

  cat("###########################################################################", file=logname, sep="\n", append=TRUE)
  cat("Function info\n",                                                             file=logname, sep="\n", append=TRUE)
  if(missing(fun)){
    cat(paste("logfile function:", sys.call(-1)[[1]],     sep="\t\t"),                  file=logname, sep="\n", append=TRUE)
  }else{
    cat(paste("logfile function:", fun,                   sep="\t\t"),                  file=logname, sep="\n", append=TRUE)
  }
  cat(paste("Data produced:", basename(filename),      sep="\t\t"),                  file=logname, sep="\n", append=TRUE)
  cat(paste("Data location:", dirname(filename), "\n", sep="\t\t"),                  file=logname, sep="\n", append=TRUE)

  cat("###########################################################################", file=logname, sep="\n", append=TRUE)
  cat("Additional info\n",                                                           file=logname, sep="\n", append=TRUE)

}


#' @rdname internal.logfiles
.addlog<-function(..., sep = " "){
  # .check_classes("character", sep)
  logfile<-get("logfile", envir = parent.frame())          # get the logfile opened by .openlog
  cat(paste(...,sep=sep), file=logfile, sep="\n")          # write message in the logfile
}


#' @rdname internal.logfiles
.closelog<-function(dir.output, cancel=FALSE){
  if(missing(dir.output)) dir.output<-get("dir.output",envir=parent.frame())  # find the output directory of the product related to the logfile
  # .check_classes("character", dir.output)
  # .check_classes("logical", cancel)
  logfile<-get("logfile", envir = parent.frame())                             # get the logfile opened by .openlog
  logname<-summary(logfile)$description                                       # temporary location of the logfile

  # .addlog("\n###########################################################################")
  t_start<-get("t_start", envir = parent.frame())                             # get starting time recorded in .openlog
  t_end<-Sys.time()                                                           # read ending time
  difftime<-difftime(t_end, t_start, units="secs")                            # time difference
  .addlog("\nSeconds elapsed:", round(as.numeric(difftime),2),sep = "\t")     # write elapsed time
  close(logfile)                                                              # close connection to the logfile

  if(!cancel){
    dir.log<-file.path(dir.output)                                            # define final directory for the logfile
    dir.create(dir.log, showWarnings = FALSE, recursive = TRUE)               # create logfile directory
    file.copy(logname, file.path(dir.log, basename(logname)), overwrite=TRUE) # copy the temporary logfile to the final directory
  }
  file.remove(logname)                                                        # remove temporary logfile
}
