with Pal;
with Logging;
with Logging_Message;
with TimeStamp; use TimeStamp;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Logging_Message is

   function "<" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool is
   begin
      return lhd.tm_stamp < rhd.tm_stamp;
   end;
   
   procedure Log (sev : ESeverity; tag : String; str : String) is
      msg : LogMessage := SP.Make_Shared(new SLogPackage);
   begin
      SP.Get_Object(msg).all := (To_Unbounded_String(str), tag, GetTimestamp, eMessage, sev, 0);
      Logging.SendLogMessage(msg);
   end Log;
   
   procedure LOG_INFO (tag : String; str : String) is
   begin
      Log(Info, tag, str);
   end LOG_INFO;
   

end Logging_Message;
