



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

  ras = ra
  decs = dec

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
#' @param inData dataframe with results.
#' @param dbTableNameExport name of the table
#'
#' @return TRUE if ok.
#' @export
#' @importFrom RPostgres dbWriteTable
#'
#' @examples \dontrun{
#' exportResults(dbTableNameExport, sosSet)
#' }
exportResults<-function(conn = conn, inData, dbTableNameExport ) {
    RPostgres::dbWriteTable(conn, tolower(dbTableNameExport), inData, overwrite = T)
    return(TRUE);
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
