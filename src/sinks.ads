----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal;
with Logging_Message;
with ConfigTree;

package Sinks is

   type ESinkType is ( CONSOLE_SINK, FILE_SINK );
   for ESinkType'Size use Pal.uint8_t'Size;

   type Sink is abstract tagged private;
   
   procedure Write (self : in out Sink; log : in Logging_Message.LogMessage) is abstract;
   procedure SetProperty (self : in out Sink; name : in String; value : in String) is abstract;
   procedure Close (self : in out Sink) is abstract;
   
   procedure SetCommonProperty (self : in out Sink; name : in String; value : in String);
   function Channel (self : in out Sink) return Logging_Message.LogChannel;
   
   procedure WriteLog (self : in out Sink'Class; log : in Logging_Message.LogMessage);
   procedure SetSinkProperty (self : in out Sink'Class; name : in String; value : in String);
   procedure CloseSink (self : in out Sink'Class);
   
   function MakeSink (name : in String; cfg : in ConfigTree.NodePtr) return access Sink'Class;
   
private
   type Sink is abstract tagged
      record
         severity : Logging_Message.ESeverity;
         channel  : Logging_Message.LogChannel;
      end record;
   
   type ConsoleSink is new Sink with
      record
         null;
      end record;
   
   overriding procedure Write (self : in out ConsoleSink; log : in Logging_Message.LogMessage);
   overriding procedure SetProperty (self : in out ConsoleSink; name : in String; value : in String);
   overriding procedure Close (self : in out ConsoleSink);
   
   type FileSink is new Sink with
      record
         null;
      end record;
   
   overriding procedure Write (self : in out FileSink; log : in Logging_Message.LogMessage);
   overriding procedure SetProperty (self : in out FileSink; name : in String; value : in String);
   overriding procedure Close (self : in out FileSink);

end Sinks;
