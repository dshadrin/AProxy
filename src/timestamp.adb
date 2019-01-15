----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Calendar.Conversions;
with Ada.Real_Time;
with Formatted_Output; use Formatted_Output;
with Formatted_Output.Integer_Output;

--------------------------------------------------------------------------------
package body TimeStamp is

   package Formatter_LongInteger is new Formatted_Output.Integer_Output (Long_Integer);
   package Formatter_Integer is new Formatted_Output.Integer_Output (Integer);
   package Formatter_Cint is new Formatted_Output.Integer_Output (Interfaces.C.int);
   ONE_SECOND_IN_MICROSECONDS : constant Long_Long_Integer := 1000000;

   procedure SetTimeCorrectValue (val : in Long_Integer) is
   begin
      TIME_CORRECT_VALUE := val;
   end SetTimeCorrectValue;

   -----------------------------------------------------------------------------
   function "<" (lhd : in timespec; rhd : in timespec) return bool is
      status : bool := lhd.tv_sec < rhd.tv_sec;
   begin
      if not status and then lhd.tv_sec = rhd.tv_sec
      then
         status := lhd.tv_nsec < rhd.tv_nsec;
      end if;
      return status;
   end;

   -----------------------------------------------------------------------------
   function GetTimestamp return timespec is
      tmPoint : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      nsec    : Ada.Real_Time.Time_Span;
      sec     : Ada.Real_Time.Seconds_Count;
      tv      : timespec := (0, 0);
   begin
      Ada.Real_Time.Split (tmPoint, sec, nsec);
      Ada.Calendar.Conversions.To_Struct_Timespec (Ada.Real_Time.To_Duration (nsec), tv.tv_sec, tv.tv_nsec);
      tv.tv_sec := Interfaces.C.long (sec);
      tv.tv_nsec := Interfaces.C.long (Integer (tv.tv_nsec) / 1000);
      tv.tv_sec := Interfaces.C.long (Long_Integer (tv.tv_sec) + TIME_CORRECT_VALUE);
      return tv;
   end GetTimestamp;

   -----------------------------------------------------------------------------
   procedure TimestampAdjust (tv : in out timespec; deltaMicrosec : Integer) is
      mcs  : Long_Long_Integer;
      sec  : Long_Long_Integer;
      part : Long_Long_Integer;
   begin
      if deltaMicrosec /= 0 then
         mcs := ((Long_Long_Integer (tv.tv_sec) rem ONE_SECOND_IN_MICROSECONDS) *
                   ONE_SECOND_IN_MICROSECONDS) + Long_Long_Integer (tv.tv_nsec) +
           Long_Long_Integer (deltaMicrosec);

         sec := ((Long_Long_Integer (tv.tv_sec) / ONE_SECOND_IN_MICROSECONDS) *
                   ONE_SECOND_IN_MICROSECONDS);

         tv.tv_sec := Interfaces.C.long (sec + mcs / ONE_SECOND_IN_MICROSECONDS);

         part := mcs rem ONE_SECOND_IN_MICROSECONDS;
         if part < 0 then
            part := part + ONE_SECOND_IN_MICROSECONDS;
            tv.tv_sec := Interfaces.C.long (Long_Long_Integer (tv.tv_sec) - 1);
         end if;

         tv.tv_nsec := Interfaces.C.long (part);
      end if;
   end TimestampAdjust;

   -----------------------------------------------------------------------------
   procedure ConvertTimestamp (tv            : in timespec;
                               tmStruct      : out tm;
                               us            : out Long_Integer;
                               deltaMicrosec : Integer := 0) is
      tv_temp : timespec := tv;
      dur     : Duration;
      nsec    : Ada.Real_Time.Time_Span;
      sec     : Ada.Real_Time.Seconds_Count;
      tmPoint : Ada.Real_Time.Time;
   begin
      TimestampAdjust (tv_temp, deltaMicrosec);
      dur := Ada.Calendar.Conversions.To_Duration (0, Interfaces.C.long (Long_Integer (tv_temp.tv_nsec) * 1000));
      nsec := Ada.Real_Time.To_Time_Span (dur);
      sec := Ada.Real_Time.Seconds_Count (tv_temp.tv_sec);
      tmPoint := Ada.Real_Time.Time_Of (sec, nsec);

      Ada.Calendar.Conversions.To_Struct_Tm (Ada.Calendar.Conversions.To_Ada_Time
                                             (Interfaces.C.long (tv_temp.tv_sec)),
                                             tmStruct.tm_year, tmStruct.tm_mon,
                                             tmStruct.tm_day, tmStruct.tm_hour,
                                             tmStruct.tm_min, tmStruct.tm_sec);
      tmStruct.tm_isdst := 0;
      us := Long_Integer (tv_temp.tv_nsec);
   end ConvertTimestamp;

   -----------------------------------------------------------------------------
   function GetTimestampStr (tmStruct : in tm; us : in Long_Integer) return String is
   begin
      return FormatDateTime("%H:%M:%S.%06d", tmStruct, us);
   end GetTimestampStr;

   -----------------------------------------------------------------------------
   function GetTimestampStr (tv : in timespec) return String is
      tmStruct : tm;
      us       : Long_Integer;
   begin
      ConvertTimestamp (tv, tmStruct, us);
      return GetTimestampStr (tmStruct, us);
   end GetTimestampStr;

   -----------------------------------------------------------------------------
   function GetTimestampStr return String is
   begin
      return GetTimestampStr (GetTimestamp);
   end GetTimestampStr;

   -----------------------------------------------------------------------------
   function FormatDateTime (fmt : in String; tmStruct : in tm; us : in Long_Integer) return String is
      use Formatter_LongInteger;
      aMonth : constant array (Interfaces.C.int(0)..11) of String(1..3) := ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");
      ufmt   : Unbounded_String := To_Unbounded_String(fmt);
      repl   : Unbounded_String;
      fmtCh      : Character;
      posPersent : Natural := Index (ufmt, "%");
      isUs : Boolean := false;
   begin
      while posPersent > 0 and then Length (ufmt) > posPersent  loop
         fmtCh := Element (ufmt, posPersent + 1);
         case fmtCh is
            when 'Y' =>
               repl := To_Unbounded_String (Integer'Image (Integer (tmStruct.tm_year) + 1900));
            when 'y' =>
               repl := To_Unbounded_String (Integer'Image (Integer (tmStruct.tm_year) rem 100));
            when 'm' =>
               repl := To_Unbounded_String (aMonth(tmStruct.tm_mon));
            when 'd' =>
               repl := To_Unbounded_String (Interfaces.C.int'Image(tmStruct.tm_day));
            when 'H' =>
               repl := To_Unbounded_String (Interfaces.C.int'Image(tmStruct.tm_hour));
            when 'M' =>
               repl := To_Unbounded_String (Interfaces.C.int'Image(tmStruct.tm_min));
            when 'S' =>
               repl := To_Unbounded_String (Interfaces.C.int'Image(tmStruct.tm_sec));
            when others =>
               repl := To_Unbounded_String ( To_String (+To_String (ufmt) & us));
               isUs := true;
         end case;
         if isUs then
            ufmt := repl;
            isUs := false;
         else
            if Element (repl, 1) = ' ' then
               if Length (repl) = 2 then
                  Overwrite (repl, 1, "0");
               else
                  Trim(repl, Ada.Strings.Left);
               end if;
            end if;
            Replace_Slice (ufmt, posPersent, posPersent + 1, To_String (repl));
         end if;
         posPersent := Index(ufmt, "%");
      end loop;
      return To_String (ufmt);
   end FormatDateTime;

end TimeStamp;
