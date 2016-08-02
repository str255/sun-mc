/* julian.mc Copyright 2016 Nicholas C. Strauss (strauss@positive-internet.com)

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>

   Low precision ephemeris for the Sun.
   Source: Explanatory Supplement to the Astronomical Almanac
   P. Kenneth Seidelmann, United States Naval Observatory.
   Nautical Almanac Office, Great Britain. Nautical Almanac Office
*/

julian(day, month, year):=
  block([a,b,c,d,jd],
	numer:true,
	keepfloat:true,
	if (year < 1) then year : year + 1,
	if (month < 3) then (
			     year : year -1,
			     month : month + 12
        ),
        a : floor(year/100),
	if (year = 1582 and month = 10 and 
	day >= 5 and day < 15) then return(invalid_date),
	if (year < 1582 
	    or (year = 1582 and month < 10 )
	    or (year = 1582 and month = 10 and day < 5))
	  then b : 0
	  else b : 2 - a + floor(a/4),
       /* if (month >= 10 or day >= 15) then  b : 2 - a + floor(a/4), */
       if (year < 0) then c : truncate((365.25 * year) - 0.75) - 694025
       else c : floor(365.25 * year) - 694025,
       d : floor(30.6001 * (month + 1)),
       b + c + d + day - 0.5 + 2415020)$   /* 2415020 converts to julian 1/1/4713BC */
/* modified julian (mjd) used by Astronomers for many observational data */
modified_julian(day, month, year):= julian(day,month,year) - 2400000.5$
/* julian 1900 used in Peter Duffett-Smith's book "Astronomy with your Personal Computer" */
julian1900(day,month,year):=julian(day,month,year)-2415020$
/* julian 1904 was used on the Classic Macintosh */
julian1904(day,month,year):=julian1900(day,month,year)-4*365+0.125$

/* sqlite3:julianday() uses the iso8601 standard and the proleptic Gregorian calendar.
   It extends the Gregorian calendar backward from 1582.
   Julian days start at noon in Greenwich on November 24, 4714 B.C. */
iso8601(day, month, year):=
  block([a,b,c,d,jd],
	numer:true,
	keepfloat:true,
	if (month < 3) then (
			     year : year -1,
			     month : month + 12
        ),
        a : floor(year/100),
	b : 2 - a + floor(a/4),
       if (year < 0) then c : truncate((365.25 * year) - 0.75) - 694025
       else c : floor(365.25 * year) - 694025,
       d : floor(30.6001 * (month + 1)),
       b + c + d + day - 0.5 + 2415020)$   

dayOfWeek_array:[Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]$
month_array:[January,February,March,April,May,June,July,August,September,October,November,December]$
month_string_array:["January","February","March","April","May","June","July","August","September","October","November","December"]$

calendar(julianDay):=
  block([a,b,c,d,fd,i,g,day,month,year,dayOfWeek],
	numer:true,
	keepfloat:true,
	if (julianDay = invalid_date) then return(invalid_date),
	julianDay: julianDay  - 2415020,  /* convert to julian epoch 1900 */
	d : julianDay + 0.5,
	i : floor(d),
	fd : d - i,
	if (fabs(fd-1.0)<1e-10) then 
					( 
					 fd : 0.0,
					 i : i+1
					  ),
	if (i > -115860) then (
			       a : floor((i/36524.25) + 9.9835726e-1) + 14,
			       i : i + 1 + a - floor(a/4.0)
			       ),
	b : floor((i/365.25) + 8.02601e-1),
	c : i - floor((365.25*b)+7.50001e-1) + 416,
	g : floor(c/30.6001),
	day : floor(c - floor(30.6001 * g) + fd),
	if (g>13.5) then  month : g - 13
	else  month : g - 1,
	if (month > 2.5) then year : b + 1899
	else year : b + 1900,
	if (year < 0) then year : year - 1,
	julianDay : julianDay + 2415020,
	b : truncate((julianDay+1.5)/7)*7,
	dayOfWeek : truncate(julianDay + 1.5  - b) + 1,
	[dayOfWeek_array[dayOfWeek], dayOfWeek, fd*24, day, month_array[month], month, year])$

  
calendar8601(julianDay):=
  block([a,b,c,d,fd,i,g],
	numer:true,
	keepfloat:true,
	julianDay: julianDay  - 2415020,  /* convert to julian epoch 1900 */
	d : julianDay + 0.5,
	i : floor(d),
	fd : d - i,
	if (fabs(fd-1.0)<1e-10) then 
					( 
					 fd : 0.0,
					 i : i+1
					  ),
	a : floor((i/36524.25) + 9.9835726e-1) + 14,
	i : i + 1 + a - floor(a/4.0),
	b : floor((i/365.25) + 8.02601e-1),
	c : i - floor((365.25*b)+7.50001e-1) + 416,
	g : floor(c/30.6001),
	day : floor(c - floor(30.6001 * g) + fd),
	if (g>13.5) then  month : g - 13
	else  month : g - 1,
	if (month > 2.5) then year : b + 1899
	else year : b + 1900,
	julianDay : julianDay + 2415020,
	b : truncate((julianDay+1.5)/7)*7,
	dayOfWeek : truncate(julianDay + 1.5  - b) + 1,
	[dayOfWeek_array[dayOfWeek], fd*24, day, month_array[month], year])$	

emacs_absolute_day(day, month, year):=iso8601(day, month, year) - iso8601(31,12,0)$

centuries_j2000(jd) := (jd - 2451545.0)/ 36525.0$ 
sind(x):=sin(%pi/180*x)$
cosd(x):=cos(%pi/180*x)$

