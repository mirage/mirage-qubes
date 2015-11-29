(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *               2014 Daniel C. Bünzli
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Temporary copy of gmtime from new mirage-clock-xen while waiting for that
   to be released. *)

type tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_wday : int;
  tm_yday : int;
  tm_isdst : bool;
}

(* Julian day to gregorian conversions are as per calendar FAQ:
   http://www.tondering.dk/claus/cal/julperiod.php#formula

   They work for positive julian days, i.e. all date after 4800 BC.
   BC years are represented by negative number, 10 BC is -9 and BC 1
   is 0. *)

let jd_to_greg jd =
  let a = jd + 32044 in
  let b = (4 * a + 3) / 146097 in
  let c = a - ((146097 * b) / 4) in
  let d = (4 * c + 3) / 1461 in
  let e = c - ((1461 * d) / 4) in
  let m = (5 * e + 2) / 153 in
  let day = e - ((153 * m + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = 100 * b + d - 4800 + (m / 10) in
  (year, month, day)

let jd_of_greg year month day =
  let a = (14 - month) / 12 in
  let y = year + 4800 - a in
  let m = month + 12 * a - 3 in
  day + ((153 * m) + 2)/ 5 + 365 * y +
  (y / 4) - (y / 100) + (y / 400) - 32045

let jd_to_year_day jd greg_year (* of [jd] *) =
  jd - (jd_of_greg (greg_year - 1) 12 31) (* last year's last date in jd *)

let jd_to_week_day jd =
  (* The 0 julian day was a Monday, in Unix sunday is 0 *)
  (jd + 1) mod 7

(* Decomposing a unix time stamp.

   POSIX time counts seconds since the epoch 1970-01-01 00:00:00 UTC
   without counting leap seconds (when a leap second occurs a posix
   second can be two SI seconds or zero SI second). Hence 86_400 posix
   seconds always represent an UTC day and the translation below is
   completly accurate. Note that by definition a unix timestamp cannot
   represent a leap second.

   The algorithm proceeds by dividing the time stamp by 86_400 this
   gives us the number of julian days since the epoch, with that we
   can find the julian day and convert it to a gregorian calendar date
   using [jd_to_greg]. The remainder of initial division is the number
   of remaining number of seconds in that day, it defines the time.

   N.B for negative unix time stamp (stricly speaking undefined on
   gmtime) the proleptic gregorian calendar is used. *)

let gmtime u =
  let unix_epoch_julian_day = 2440588 in
  let t = floor u in
  let day_num, day_clock =
    if t >= 0. then
      let num = truncate (t /. 86_400.) in
      let clock = truncate (mod_float t 86_400.) in
      num, clock
    else
      let t = t +. 1. in
      let num = truncate (t /. 86_400.) - 1 in
      let clock = (86_400 + (truncate (mod_float t 86_400.))) - 1 in
      num, clock
  in
  let hh = day_clock / 3600 in
  let mm = (day_clock mod 3600) / 60 in
  let ss = day_clock mod 60 in
  let jd = unix_epoch_julian_day + day_num in
  let year, month, month_day = jd_to_greg jd in
  let week_day = jd_to_week_day jd in
  let year_day = jd_to_year_day jd year in
  { tm_year = year - 1900; tm_mon = month - 1; tm_mday = month_day;
    tm_hour = hh; tm_min = mm; tm_sec = ss;
    tm_wday = week_day; tm_yday = year_day - 1;
    tm_isdst = false; }

let to_string tm =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d.%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
