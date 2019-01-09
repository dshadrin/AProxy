with Ada.Real_Time;

package body TimeStamp is

   function GetTimestamp return timespec is
      t  : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      tv : timespec;
   begin
      Ada.Real_Time.Split(t, tv.tv_sec, tv.tv_nsec);
      return tv;
   end GetTimestamp;

end TimeStamp;
