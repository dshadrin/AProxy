----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Formatted_Output; use Formatted_Output;
with Logging_Message; use Logging_Message;
with Ada.Text_IO; use Ada.Text_IO;
with TimeStamp; use TimeStamp;
with Ada.Strings.Unbounded;

------------------------------------------------------------------------------------------------------------------------
package body Logging is

   ---------------------------------------------------------------------------------------------------------------------
   function SeverityStr(sev : ESeverity) return String is
      type SeverityValueArray is array (ESeverity) of String(1..4);
      sevVal : constant SeverityValueArray := ("TRAC", "DBG ", "INFO", "TEST", "WARN", "ERR ", "CRIT");
   begin
      return sevVal(sev);
   end SeverityStr;

   ---------------------------------------------------------------------------------------------------------------------
   function FormatMessage(ptr : SLogPackagePtr) return String is
   begin
      return To_String(+"[%s][%s][%s] - %s" & GetTimestampStr(ptr.tm_stamp) & ptr.tag & SeverityStr(ptr.severity) & Ada.Strings.Unbounded.To_String(ptr.message));
   end FormatMessage;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure SendLogMessage (msg : LogMessage) is
   begin
      Put_Line(FormatMessage(SP.Get_Object(msg)));
   end SendLogMessage;
   
end Logging;
