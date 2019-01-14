----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Logging_Message; use Logging_Message;
with TimeStamp;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;
with Formatted_Output;

package body Sinks is

   -----------------------------------------------------------------------------
   use all type Ada.Strings.Unbounded.Unbounded_String;
   type SinkTypeValueArray is array (ESinkType) of Ada.Strings.Unbounded.Unbounded_String;
   sinkTypesStr : constant SinkTypeValueArray := (To_Unbounded_String ("CONSOLE"), To_Unbounded_String ("FILE"));
   
   type SeverityConfigureString is array (ESeverity) of Ada.Strings.Unbounded.Unbounded_String;
   sinkSeverityStr : constant SeverityConfigureString := (To_Unbounded_String ("TRACE"),
                                                           To_Unbounded_String ("DEBUG"),
                                                           To_Unbounded_String ("INFO"),
                                                           To_Unbounded_String ("TEST"),
                                                           To_Unbounded_String ("WARN"),
                                                           To_Unbounded_String ("ERROR"),
                                                          To_Unbounded_String ("CRIT"));
   
   type SeverityValueArray is array (ESeverity) of String (1 .. 4);
   severityLogOutput : constant SeverityValueArray := ("TRAC", "DBG ", "INFO", "TEST", "WARN", "ERR ", "CRIT");
   
   -----------------------------------------------------------------------------
   function SinkTypeStr (sev : ESinkType) return Ada.Strings.Unbounded.Unbounded_String with inline is
   begin
      return sinkTypesStr (sev);
   end SinkTypeStr;
   
   -----------------------------------------------------------------------------
   function SeverityFromStr (str : String) return ESeverity is
   begin
      for i in ESeverity'Range loop
         if To_Unbounded_String(str) = sinkSeverityStr(i) then
            return i;
         end if;
      end loop;
      raise Program_Error;
   end SeverityFromStr;

   -----------------------------------------------------------------------------
   function SeverityStr (sev : ESeverity) return String with inline is
   begin
      return severityLogOutput (sev);
   end SeverityStr;
   
   -----------------------------------------------------------------------------
   function FormatMessage (ptr : SLogPackagePtr) return String is
      use Formatted_Output;
   begin
      return To_String (+"[%s][%s][%s] - %s"
                        & TimeStamp.GetTimestampStr (ptr.tm_stamp)
                        & ptr.tag
                        & SeverityStr (ptr.severity)
                        & Ada.Strings.Unbounded.To_String (ptr.message));
   end FormatMessage;
   
   -----------------------------------------------------------------------------
   procedure MakeSink (self : out Sink; name : in String; cfg : in ConfigTree.NodePtr) is
      use Ada.Strings.Unbounded;
      sinkName : Ada.Strings.Unbounded.Unbounded_String := Trim ( To_Unbounded_String (Ada.Characters.Handling.To_Upper (name)), Ada.Strings.Both);
      severity : ESeverity := SeverityFromStr (cfg.GetValue ("severity"));
      channel  : LogChannel := LogChannel'Value (cfg.GetValue ("channel"));
   begin
      if sinkName = SinkTypeStr (CONSOLE_SINK) then
         self := (CONSOLE_SINK, severity, channel);
      elsif sinkName = SinkTypeStr (FILE_SINK) then
         declare
            template : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
            prefix   : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
            suffix   : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String ("");
            demand   : Pal.bool := false;
            nd       : ConfigTree.NodePtr := cfg.GetFirst;
         begin
            while not ConfigTree.IsNull (nd) loop
               if nd.GetName = "template" then
                  template := To_Unbounded_String (nd.GetValue);
               elsif nd.GetName = "prefix" then
                  prefix :=  To_Unbounded_String (nd.GetValue);
               elsif nd.GetName = "suffix" then
                  suffix := To_Unbounded_String (nd.GetValue);
               elsif nd.GetName = "open_by_demand" and then Ada.Characters.Handling.To_Upper (nd.GetValue) = "TRUE" then
                  demand := true;
               end if;
               nd := nd.GetNext;
            end loop;
            self := (FILE_SINK, severity, channel, template, prefix, suffix, demand);
         end;
      else
         Put_Line("Unknown sink :" & To_String(sinkName));
      end if;
   end MakeSink;
   
   -----------------------------------------------------------------------------
   function Channel (self : access Sink) return Logging_Message.LogChannel is
   begin
      return self.channel;
   end Channel;
   
   -----------------------------------------------------------------------------
   procedure WriteLogs (self : access Sink; logs : in LogMessages) is
   begin
      for i in logs.Get.First_Index..logs.Get.Last_Index loop
         Write (self, logs.Get.Element(i));
      end loop;
   end WriteLogs;
      
   -----------------------------------------------------------------------------
   procedure Write (self : access Sink; log : in Logging_Message.LogMessage) is
   begin
      if self.SinkType = CONSOLE_SINK then
         Put_Line (FormatMessage (log.Get));
      end if;
   end Write;
   
   procedure Close (self : access Sink) is
   begin
      null;
   end Close;
   
   
end Sinks;
