with Formatted_Output; use Formatted_Output;
with Logging_Message; use Logging_Message;
with Ada.Text_IO; use Ada.Text_IO;
with TimeStamp; use TimeStamp;
with Formatted_Output.Enumeration_Output;
with Ada.Strings.Unbounded;

package body Logging is

   package Formatter_Severity is new Formatted_Output.Enumeration_Output(ESeverity);

   function FormatMessage(ptr : SLogPackagePtr) return String is
      use Formatter_Severity;
   begin
      return To_String(+"[%s][%s][%-4u] - %s" & GetTimestampStr(ptr.tm_stamp) & ptr.tag & ptr.severity & Ada.Strings.Unbounded.To_String(ptr.message));
   end FormatMessage;
   
   procedure SendLogMessage (msg : LogMessage) is
   begin
      Put_Line(FormatMessage(SP.Get_Object(msg)));
   end SendLogMessage;
   
end Logging;
