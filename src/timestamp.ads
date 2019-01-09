with pal; use pal;
with Ada.Real_Time;

package TimeStamp is

   type timespec is
      record
         tv_sec  : Ada.Real_Time.Seconds_Count;
         tv_nsec : Ada.Real_Time.Time_Span;
      end record;

   function GetTimestamp return timespec;

end TimeStamp;
