----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Bounded;

package body Sinks is

   package SinkTypeNames is new Ada.Strings.Bounded.Generic_Bounded_Length (7);
   use SinkTypeNames;

   ---------------------------------------------------------------------------------------------------------------------
   function SinkTypeStr (sev : ESinkType) return SinkTypeNames.Bounded_String is
      type SinkTypeValueArray is array (ESinkType) of SinkTypeNames.Bounded_String;
      val : constant SinkTypeValueArray := (To_Bounded_String ("CONSOLE"), To_Bounded_String ("FILE"));
   begin
      return val (sev);
   end SinkTypeStr;

   ---------------------------------------------------------------------------------------------------------------------
   function MakeSink (name : in String; cfg : in ConfigTree.NodePtr) return access Sink'Class is
      sinkName : SinkTypeNames.Bounded_String := To_Bounded_String (Ada.Characters.Handling.To_Upper (name));
      sinkInst : access Sink'Class := null;
   begin
      sinkName := Trim (sinkName, Ada.Strings.Both);
      
      if sinkName = SinkTypeStr (CONSOLE_SINK) then
         sinkInst := new ConsoleSink;
      elsif sinkName = SinkTypeStr (FILE_SINK) then
         sinkInst := new FileSink;
      else
         null;
      end if;
      return sinkInst;
   end MakeSink;
   
   ---------------------------------------------------------------------------------------------------------------------
   function Channel (self : in out Sink) return Logging_Message.LogChannel is
   begin
      return self.channel;
   end Channel;
      
   procedure WriteLog (self : in out Sink'Class; log : in Logging_Message.LogMessage) is
   begin
      self.Write (log);
   end WriteLog;
   
   procedure SetSinkProperty (self : in out Sink'Class; name : in String; value : in String) is
   begin
      self.SetProperty (name, value);
   end SetSinkProperty;

   ---------------------------------------------------------------------------------------------------------------------
   overriding procedure Write (self : in out ConsoleSink; log : in Logging_Message.LogMessage) is
   begin
      null;
   end Write;
   
   overriding procedure SetProperty (self : in out ConsoleSink; name : in String; value : in String) is
   begin
      null;
      -- Sink.SetProperty (name, value);
   end SetProperty;
   
   ---------------------------------------------------------------------------------------------------------------------
   overriding procedure Write (self : in out FileSink; log : in Logging_Message.LogMessage) is
   begin
      null;
   end Write;

   overriding procedure SetProperty (self : in out FileSink; name : in String; value : in String) is
   begin
      null;
      -- Sink.SetProperty (name, value);
   end SetProperty;

   
end Sinks;
