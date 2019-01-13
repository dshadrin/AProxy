----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

package body Sinks is

   ---------------------------------------------------------------------------------------------------------------------
   function SinkTypeStr (sev : ESinkType) return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;
      type SinkTypeValueArray is array (ESinkType) of Ada.Strings.Unbounded.Unbounded_String;
      val : constant SinkTypeValueArray := (To_Unbounded_String ("CONSOLE"), To_Unbounded_String ("FILE"));
   begin
      return val (sev);
   end SinkTypeStr;

   ---------------------------------------------------------------------------------------------------------------------
   function MakeSink (name : in String; cfg : in ConfigTree.NodePtr) return access Sink'Class is
      use Ada.Strings.Unbounded;
      sinkName : Ada.Strings.Unbounded.Unbounded_String := To_Unbounded_String (Ada.Characters.Handling.To_Upper (name));
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
   
   procedure CloseSink (self : in out Sink'Class) is
   begin
      self.Close;
   end CloseSink;
   
   procedure SetCommonProperty (self : in out Sink; name : in String; value : in String) is
   begin
      null;
   end SetCommonProperty;

   ---------------------------------------------------------------------------------------------------------------------
   overriding procedure Write (self : in out ConsoleSink; log : in Logging_Message.LogMessage) is
   begin
      null;
   end Write;
   
   overriding procedure SetProperty (self : in out ConsoleSink; name : in String; value : in String) is
   begin
      null;
   end SetProperty;
   
   overriding procedure Close (self : in out ConsoleSink) is
   begin
      null;
   end Close;

   ---------------------------------------------------------------------------------------------------------------------
   overriding procedure Write (self : in out FileSink; log : in Logging_Message.LogMessage) is
   begin
      null;
   end Write;

   overriding procedure SetProperty (self : in out FileSink; name : in String; value : in String) is
   begin
      null;
   end SetProperty;

   overriding procedure Close (self : in out FileSink) is
   begin
      null;
   end Close;

   
end Sinks;
