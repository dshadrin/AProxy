----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Logging_Message; use Logging_Message;
with TimeStamp;
with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Formatted_Output; use Formatted_Output;
with Formatted_Output.Enumeration_Output;

package body Sinks is

   package ASU renames Ada.Strings.Unbounded;

   -----------------------------------------------------------------------------
   use all type ASU.Unbounded_String;
   type SinkTypeValueArray is array (ESinkType) of ASU.Unbounded_String;
   sinkTypesStr : constant SinkTypeValueArray := (To_Unbounded_String ("CONSOLE"), To_Unbounded_String ("FILE"));
   
   type SeverityConfigureString is array (ESeverity) of ASU.Unbounded_String;
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
         if To_Unbounded_String (str) = sinkSeverityStr (i) then
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
      package Formatter_SinkType is new Formatted_Output.Enumeration_Output (Sinks.ESinkType);
      use Ada.Strings.Unbounded;
      use Formatter_SinkType;
      use Logging_Message.Formatter_Channel;

      sinkName : Unbounded_String := Trim ( To_Unbounded_String (Ada.Characters.Handling.To_Upper (name)), Ada.Strings.Both);
      severity : ESeverity := SeverityFromStr (cfg.GetValue ("severity"));
      channel  : LogChannel := LogChannel'Value (cfg.GetValue ("channel"));
   begin
      self.severity := severity;
      self.channel := channel;
      if sinkName = SinkTypeStr (CONSOLE_SINK) then
         self.SinkType := CONSOLE_SINK;
      elsif sinkName = SinkTypeStr (FILE_SINK) then
         declare
            template : Unbounded_String := Null_Unbounded_String;
            prefix   : Unbounded_String := Null_Unbounded_String;
            suffix   : Unbounded_String := Null_Unbounded_String;
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
            self.SinkType := FILE_SINK;
            self.template := template;
            self.prefix := prefix;
            self.suffix := suffix;
            self.filename := Null_Unbounded_String;
            self.open_by_demand := demand;
            if demand = false then
               self.filename := MakeFilename(self);
            end if;
         end;
      else
         Put_Line ("Unknown sink :" & To_String (sinkName));
         raise Program_Error;
      end if;
      self.handler := new SinkOutputer;
      self.handler.Start (self.SinkType, severity, channel);
      --LOG_INFO ("SINK", To_String (+"Created sink with channel %d : %s" & channel & self.SinkType));
   end MakeSink;
   
   -----------------------------------------------------------------------------
   function Channel (self : access Sink) return Logging_Message.LogChannel is
   begin
      return self.channel;
   end Channel;
   
   -----------------------------------------------------------------------------
   procedure WriteLogs (self : access Sink; logs : in LogMessages) is
   begin
      self.handler.Write (logs, self.filename);
   end WriteLogs;
   
   -----------------------------------------------------------------------------
   procedure Close (self : access Sink) is
      procedure Free is new Ada.Unchecked_Deallocation (SinkOutputer, SinkOutputerPtr);
   begin
      self.handler.Stop;
      Free (self.handler);
   end Close;
   
   -----------------------------------------------------------------------------
   task body SinkOutputer is
      isWorked : Pal.bool := true;
      sinkTag  : ESinkType;
      severity : Logging_Message.ESeverity;
      channel  : Logging_Message.LogChannel;
      var      : Pal.uint32_t;
   begin
      accept Start (tag : ESinkType;
                    sev  : Logging_Message.ESeverity;
                    ch   : Logging_Message.LogChannel) do
         sinkTag := tag;
         severity := sev;
         channel := ch;
      end Start;
      
      while isWorked loop
         select
            accept Write (logs : in LogMessages;
                          file : in ASU.Unbounded_String := ASU.Null_Unbounded_String) do
               for i in logs.Get.First_Index .. logs.Get.Last_Index loop
                  declare
                     msg : Logging_Message.LogMessage := logs.Get.Element (i);
                  begin
                     if msg.Get.lchannel = channel and then
                       msg.Get.severity >= severity and then
                       msg.Get.command = Logging_Message.eMessage then
                        if sinkTag = CONSOLE_SINK then
                           Put_Line (FormatMessage (msg.Get));
                        elsif sinkTag = FILE_SINK then
                           -- TODO: implement
                           null;
                        end if;
                     end if;
                  end;
               end loop;
               var := Pal.Atomic_Sub_Fetch_32 (activeSinksCounter'Access, 1);
            end Write;
         or
            accept Stop do
               isWorked := false;
            end Stop;
         end select;
      end loop;
   end SinkOutputer;
   
   -----------------------------------------------------------------------------
   function MakeFilename (self : in out Sink) return Ada.Strings.Unbounded.Unbounded_String is
      outStr : ASU.Unbounded_String := ASU.Null_Unbounded_String;
      posDot : Natural := 0;
   begin
      if self.SinkType = FILE_SINK then
         outStr := self.prefix;
         if ASU.Length (outStr) > 0 then
            ASU.Append (outStr, "_");
         end if;
         ASU.Append (outStr, self.template);
         if ASU.Length (self.suffix) > 0 then
            posDot := ASU.Index (outStr, ".", 1);
            if posDot > 0 then
               ASU.Insert (outStr, Positive (posDot), ASU.To_String (ASU.To_Unbounded_String ("_") & self.suffix));
            else
               ASU.Append (outStr, "_");
               ASU.Append (outStr, self.suffix);
            end if;
         end if;
      end if;
      
      return outStr;
   end MakeFilename;
   
   
end Sinks;
