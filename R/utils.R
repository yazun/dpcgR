



# NPixels 	Mean Spacing (deg) 	Area (sterad)
.cHpx =  c(
  "0	1	12	58.6323	1.0471976E+00",
  "1	2	48	29.3162	2.6179939E-01",
  "2	4	192	14.6581	6.5449847E-02",
  "3	8	768	7.3290	1.6362462E-02",
  "4	16	3072	3.6645	4.0906154E-03",
  "5	32	12288	1.8323	1.0226539E-03",
  "6	64	49152	0.9161	2.5566346E-04",
  "7	128	196608	0.4581	6.3915866E-05",
  "8	256	786432	0.2290	1.5978967E-05",
  "9	512	3145728	0.1145	3.9947416E-06",
  "10	1024	12582912	0.0573	9.9868541E-07")


#' Healpix data
#' function to convert hpx into sq degs for legend.
#' steradian * (180/pi)^2 = area in deg^2
#' https://lambda.gsfc.nasa.gov/toolbox/tb_pixelcoords.cfm
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @export
dfHPX = data.frame(x = .cHpx) %>% tidyr::separate(x,c("Level","NPixels","Mean", "Spacing_deg", "Area_sterad"),  sep = "[ \t]+") %>% mutate(Area_sterad = as.numeric(Area_sterad), sq_deg = as.numeric(Area_sterad) * (180/pi)^2 )


#' Project coordinates from Galactic to Aitoff-Hemmer.
#'
#' Useful for skymap plots.
#' @param l in radians
#' @param b in radians
#'
#' @return dataframe with transposed coords (x,y)
#' @export
#'
#' @examples aitoffFn(0,0)
aitoffFn <- function (l, b) {
  radeg = 180/pi
  sa = l
  x180 = which(sa > 180)
  sa[x180] = sa[x180] - 360
  alpha2 = sa/(2 * radeg)
  delta = b/radeg
  r2 = sqrt(2)
  f = 2 * r2/pi
  cdec = cos(delta)
  denom = sqrt(1 + cdec * cos(alpha2))
  x = cdec * sin(alpha2) * 2 * r2/denom
  y = sin(delta) * r2/denom
  x = x * radeg/f
  y = y * radeg/f
  return(data.frame(x = x, y = y))
}


#' Converting from ecliptic to galactic coordinates in J1950
#'
#' @param ra alpha in radians
#' @param dec delata in radians
#'
#' @return data frame of (gl,gb)
#' @export
#'
#' @importFrom astrolibR bprecess
#'
#' @examples togalactic(pi/4,pi/4)
togalactic <- function (ra, dec) {
  radeg = 180/pi
  # 1950
  rapol = 12 + 49/60
  decpol = 27.4
  dlon = 123

  ras = ra * radeg
  decs = dec * radeg

  tmp = bprecess(ras, decs)
  ras = tmp$ra_1950
  decs = tmp$dec_1950

  # J2000 FK5
  # rapol = 12 + 51.4/60
  # decpol = 27.13
  # dlon = 123
  #19h50m47s and delta=8d52m6s (FK5, J2000)
  # rapol = 19 + 50.47/60
  # decpol = 8 + 13/60 + 6/3600

  sdp = sin(decpol/radeg)
  cdp = sqrt(1 - sdp * sdp)
  radhrs = radeg/15

  ras = ras/radeg - rapol/radhrs
  sdec = sin(decs/radeg)
  cdec = sqrt(1 - sdec * sdec)
  sgb = sdec * sdp + cdec * cdp * cos(ras)
  gb = radeg * asin(sgb)
  cgb = sqrt(1 - sgb * sgb)
  sine = cdec * sin(ras)/cgb
  cose = (sdec - sdp * sgb)/(cdp * cgb)
  gl = dlon - radeg * atan2(sine, cose)
  ltzero = (gl < 0)
  gl[ltzero] = gl[ltzero] + 360
  return(data.frame(gl = gl, gb = gb))
}



#' Export results to DB and other means with a given name
#'
#' @param conn DB connection
#' @param schema DB schema to create table in
#' @param dbTableNameExport name of the table
#' @param inData dataframe with results.
#' @param variType name of the type
#' @param cumulativeTable name of the cumulative table, where we append sourceid and variType.
#' @param prefix prefix of the name of the snapshot table
#'
#' @return TRUE if ok.
#' @export
#' @importFrom RPostgres dbWriteTable Id
#' @importFrom DBI dbExecute
#'
#' @examples \dontrun{
#' exportResults(dbTableNameExport, sosSet)
#' }
exportResults<-function(conn = conn, schema , dbTableNameExport, inData, variType, cumulativeTable = "dr3_common_export", prefix = "dr3_validated_" ) {

  # export data to a single table with full selection
  fullTableName = paste(prefix,tolower(dbTableNameExport),sep="")
  tableId = Id(schema = schema, table = fullTableName)
  message(sprintf("Overwriting table %s.%s",schema,  fullTableName))
  # delete it as a common user to be sure
  DBI::dbExecute(conn,sprintf("select clean_common_export('%s.%s')",schema,  fullTableName))
  DBI::dbExecute(conn,'set role cu7gva');
  RPostgres::dbWriteTable(conn, tableId, inData, overwrite = T)
  #but also ingest the digest to a single table for the global view

  sqlInsert = sprintf("select populate_common_export('%s.%s','%s','%s.%s')", schema, cumulativeTable, variType, schema , fullTableName)

  return(dbExecute(conn,sqlInsert))
}



#' Export results to prefix||variType table and to cumulativeTable classification table and other means with a given name.
#' Contrary to SOS version, It completely replaces cumulativeTable with the selection given in the data frame.
#'
#' @param conn DB connection
#' @param schema DB schema to create table in
#' @param dbTableNameExport name of the table
#' @param inData dataframe with results: must include sourceid, varitype and score fields.
#' @param scoreName name of the score field in the data frame provided
#' @param cumulativeTable name of the cumulative table, where we append sourceid and variType.
#' @param prefix prefix of the name of the snapshot table
#' @return TRUE if ok.
#' @export
#' @importFrom RPostgres dbWriteTable Id
#' @importFrom DBI dbExecute
#'
#' @examples \dontrun{
#' exportClassificationResults(dbTableNameExport, classSet, scoreName = "scoreColumn")
#' }
exportClassificationResults<-function(conn = conn, schema , dbTableNameExport, inData, scoreName = "score", cumulativeTable = "dr3_classification_export", prefix = "dr3_validated_" ) {

  # export data to a single table with full selection
  fullTableName = paste(prefix,tolower(dbTableNameExport),sep="")
  tableId = Id(schema = schema, table = fullTableName)
  inData = inData %>% mutate(sourceid = as.integer64(sourceid));
  RPostgres::dbWriteTable(conn, tableId, inData, overwrite = T)
  #but also ingest the digest to a single table for the global view


  # sqlDelete = sprintf("delete from %s.%s s using %s.%s t where  (s.sourceid,s.varitype) = (t.sourceid,t.varitype)",schema, cumulativeTable, schema, fullTableName)
  sqlDelete = sprintf("truncate table %s.%s ",schema, cumulativeTable)
  DBI::dbExecute(conn,sqlDelete)
  sqlInsert = sprintf("insert into %s.%s select distinct on (sourceid) sourceid,varitype,%s from %s.%s", schema, cumulativeTable, scoreName, schema , fullTableName)
  return(dbExecute(conn,sqlInsert))
}

#' Function to convert from a database-recovered string to an array of numbers
#'
#' @param theString The string containing the array of strings from DB array
#'
#' @return a numeric array with the components
#' @export
#'
#' @examples dbstrToNumArray("{1,2,3}")
dbstrToNumArray <- function(theString) {
  numbers<-as.numeric(unlist(strsplit(substr(theString,2,nchar(theString)-1), split=",")))
}


#' Function to convert from a database-recovered string to an array of strings
#'
#' @param theString The string containing the array of strings from DB array
#'
#' @return a string array with the components
#' @export
#'
#' @examples dbstrToStrArray("{a,b,c}")
dbstrToStrArray <- function(theString) {
  strings <- as.character(unlist(strsplit(substr(theString, 2, nchar(theString)-1), split=",")))
  return(strings)
}

getPhase <- function(referenceTime, times, period) {
  return(((times - referenceTime) %% period) / period);
}


#' Folding timeseries function
#'
#' @param period given period
#' @param times vector of obstimes
#' @param values vector of values
#' @param errors  vector of errors
#' @param referenceTime reference time for folding
#' @param range range of the folded timeseries
#'
#' @return dataframe with folded indexes, phases, magnitudes and errors
#' @export
#'
#' @examples \dontrun{
#' foldTimeseries(period, times, values, errors, referenceTime, range=1.5)
#' }
foldTimeseries <- function(period, times, values, errors, referenceTime, range=1.5) {

  referenceTime=as.numeric(referenceTime) # otherwise the getPhase does not work
  phases <- getPhase(referenceTime, times, period);

  indexes <- order(phases);
  sortPhases <- phases[indexes];
  sortMag <- values[indexes];
  sortErrors <- errors[indexes];

  if (range > 1.0) {
    tsLength <- length(phases);
    finalLength <- range * tsLength;
    for (index in c((tsLength+1):finalLength)) {
      sortPhases[index] <- 1 + sortPhases[index - tsLength];
      sortMag[index] <- sortMag[index - tsLength];
      sortErrors[index] <- sortErrors[index - tsLength];
      indexes[index] <- indexes[index - tsLength]
    }
  }

  data.frame(
    indexes = indexes,
    phases = sortPhases,
    magnitudes = sortMag,
    errors = sortErrors
  );
}


#' Folding timeseries function for sourceid and tag
#'
#' @param period given period
#' @param times vector of obstimes
#' @param values vector of values
#' @param errors  vector of errors
#' @param referenceTime reference time for folding
#' @param range range of the folded timeseries
#' @param insourceid sourceid
#' @param intag tag
#'
#' @return dataframe with folded indexes, phases, magnitudes and errors
#' @export
#'
#' @examples \dontrun{
#' foldTimeseries(sourcied, tag, period, times, values, errors, referenceTime, range=1.5)
#' }
foldTimeseriesFull <- function(insourceid, intag, period, times, values, errors, referenceTime, range=1.5) {

  referenceTime=as.numeric(referenceTime) # otherwise the getPhase does not work
  phases <- getPhase(referenceTime, times, period);

  indexes <- order(phases);
  sortPhases <- phases[indexes];
  sortMag <- values[indexes];
  sortErrors <- errors[indexes];

  if (range > 1.0) {
    tsLength <- length(phases);
    finalLength <- range * tsLength;
    for (index in c((tsLength+1):finalLength)) {
      sortPhases[index] <- 1 + sortPhases[index - tsLength];
      sortMag[index] <- sortMag[index - tsLength];
      sortErrors[index] <- sortErrors[index - tsLength];
      indexes[index] <- indexes[index - tsLength]
    }
  }

  dret = data.frame(
    indexes = indexes,
    phases = sortPhases,
    magnitudes = sortMag,
    errors = sortErrors
  );
  dret$sourceid = as.character(insourceid[1])
  dret$tag = as.character(intag[1])
  dret
}


bprecess = function(
    ra,
    dec,
    mu_radec,
    parallax = numeric(length(ra)),
    rad_vel = numeric(length(ra)),
    epoch = 2000) {

  n = length( ra )

  if(length(rad_vel)!=n )
    stop(paste('rad_vel keyword vector must contain ',
               n,' values'))

  if(!missing(mu_radec) && (length(mu_radec)!=2*n ))
    stop('mu_radec keyword (proper motion) be dimensioned (2,' +
           n, ')')

  radeg = 180/pi
  sec_to_radian = 1/radeg/3600
  m =  cbind( c(+0.9999256795, -0.0111814828, -0.0048590040,
                -0.000551,  -0.238560,     +0.435730)     ,
              c(+0.0111814828, +0.9999374849, -0.0000271557,
                +0.238509,     -0.002667,      -0.008541)     ,
              c(+0.0048590039, -0.0000271771, +0.9999881946 ,
                -0.435614,      +0.012254,      +0.002117)      ,
              c(-0.00000242389840, +0.00000002710544, +0.00000001177742,
                +0.99990432,    -0.01118145,    -0.00485852)    ,
              c(-0.00000002710544, -0.00000242392702, +0.00000000006585,
                +0.01118145,     +0.99991613,    -0.00002716)    ,
              c(-0.00000001177742, +0.00000000006585,-0.00000242404995,
                +0.00485852,   -0.00002717,    +0.99996684))
  a_dot = 1e-3*c(1.244, -1.579, -0.660 )           #in arc seconds per century
  ra_rad = ra/radeg
  dec_rad = dec/radeg
  cosra =  cos( ra_rad )
  sinra = sin( ra_rad )
  cosdec = cos( dec_rad )
  sindec = sin( dec_rad )
  dec_1950 = dec*0.
  ra_1950 = ra*0.
  for(i  in 1:n) {
    a = 1e-6*c( -1.62557, -0.31919, -0.13843)        #in radians
    r0 = c( cosra[i]*cosdec[i], sinra[i]*cosdec[i], sindec[i] )
    if(!missing(mu_radec) ){
      mu_a = mu_radec[ (2*n-1) ]
      mu_d = mu_radec[ 2*n ]
      r0_dot = c( -mu_a*sinra[i]*cosdec[i] -
                    mu_d*cosra[i]*sindec[i] ,  #velocity vector
                  mu_a*cosra[i]*cosdec[i] -
                    mu_d*sinra[i]*sindec[i] ,
                  mu_d*cosdec[i] ) +
        21.095 * rad_vel[i] * parallax[i] * r0
    }
    else {
      r0_dot = c(0.0, 0.0, 0.0)
    }

    r_0 = c(r0, r0_dot)

    r_1 =  r_0 %*% t(m)
    # include the effects of the e-terms of aberration to form r and r_dot.
    r1 = r_1[1:3]
    r1_dot = r_1[4:6]
    if(!missing(mu_radec) ){
      r1 = r1 + sec_to_radian * r1_dot * (epoch - 1950.0)/100.
      a = a + sec_to_radian * a_dot * (epoch - 1950.0)/100.
    }
    x1 = r_1[1]
    y1 = r_1[2]
    z1 = r_1[3]
    rmag = sqrt( x1^2 + y1^2 + z1^2 )
    s1 = r1/rmag
    s1_dot = r1_dot/rmag
    s = s1

    for(j  in 1:3) {
      r = s1 + a - (sum(s * a))*s
      s = r/rmag
    }
    x = r[1]
    y = r[2]
    z = r[3]
    r2 = x^2 + y^2 + z^2
    rmag = sqrt( r2 )

    if(!missing(mu_radec) ){
      r_dot = s1_dot + a_dot - ( sum( s * a_dot))*s
      x_dot = r_dot[1]  ; y_dot= r_dot[2]  ;  z_dot = r_dot[3]
      mu_radec[(2*n-1)] = ( x*y_dot - y*x_dot) / ( x^2 + y^2)
      mu_radec[2*n] = ( z_dot* (x^2 + y^2) - z*(x*x_dot + y*y_dot) ) /
        ( r2*sqrt( x^2 + y^2) )
    }
    dec_1950[i] = asin( z / rmag)
    ra_1950[i] = atan2( y, x)
    if(parallax[i]>0 ){
      rad_vel[i] = ( x*x_dot + y*y_dot + z*z_dot )/ (21.095*parallax[i]*rmag)
      parallax[i] = parallax[i] / rmag
    }
  }
  neg = (ra_1950<0)
  ra_1950[neg] = ra_1950[neg] + 2.*pi
  ra_1950 = ra_1950*radeg
  dec_1950 = dec_1950*radeg

  return(list(ra_1950 = ra_1950, dec_1950=dec_1950))
}
