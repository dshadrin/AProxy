----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal; use Pal;
with Interfaces.C;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

------------------------------------------------------------------------------------------------------------------------
package TimeStamp is

   ---------------------------------------------------------------------------------------------------------------------
   type timespec is
      record
         tv_sec  : Interfaces.C.long;
         tv_nsec : Interfaces.C.long;
      end record;

   function "<" (lhd : in Interfaces.C.long; rhd : in Interfaces.C.long) return bool;
   function "<" (lhd : in timespec; rhd : in timespec) return bool;


   ---------------------------------------------------------------------------------------------------------------------
   type tm is
      record
         tm_year  : Interfaces.C.int;
         tm_mon   : Interfaces.C.int;
         tm_day   : Interfaces.C.int;
         tm_hour  : Interfaces.C.int;
         tm_min   : Interfaces.C.int;
         tm_sec   : Interfaces.C.int;
         tm_isdst : Interfaces.C.int;
      end record;

   ---------------------------------------------------------------------------------------------------------------------
   procedure TimestampAdjust (tv : in out timespec; deltaMicrosec : Integer);
   procedure ConvertTimestamp (tv : in timespec; tmStruct : out tm; us : out Long_Integer; deltaMicrosec : Integer := 0);
   function GetTimestamp return timespec;
   function GetTimestampStr(tmStruct : in tm; us : in Long_Integer) return String;
   function GetTimestampStr(tv : in timespec) return String;
   function GetTimestampStr return String;

end TimeStamp;
