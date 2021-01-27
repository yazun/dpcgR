# initializer of a DB connection to PG
#' Creates connection to DPCG database
#' To be reused in the subsequent class and markdown SQL chunks.
#' @param hostname DB hostname or localhost if ssh tunnel is used
#' @param dbname Usually surveys for DPCG
#' @param port 55431 for Postgres-XZ
#' @param user DB user, i.e. nienarto_local
#' @param pwd optional password. Not needed if .pgpass is up to date (and it should be!)
#'
#' @return DB connection object to be reused in SQL chunks and other calls
#' @importFrom RPostgres dbConnect
#' @export
#'
#' @examples \dontrun{connect(user="nienarto_local", port=55431)}
connect<- function(hostname="localhost",dbname="surveys",port="55431",user="johnny_local",pwd) {
  # load PG driver lib
  # init DB driver
  #drv <- dbDriver("PostgreSQL")
  # connect to the server
  conn <- dbConnect(RPostgres::Postgres(),dbname=dbname,host=hostname,port=port,user=user)
  return(conn)
}
