----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal;
with Logging_Message;
with ConfigTree;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Sinks is

   use all type Logging_Message.LogMessage;
   package LogsVector is new Ada.Containers.Vectors (Pal.uint32_t, Logging_Message.LogMessage);
   type LVectorPtr is access all LogsVector.Vector;
   package LogsSP is new Pal.Smart_Ptr (TypeName => LogsVector.Vector, SharedObject => LVectorPtr);
   subtype LogMessages is LogsSP.Shared_Ptr;
   
   type ESinkType is ( CONSOLE_SINK, FILE_SINK );
   for ESinkType'Size use Pal.uint8_t'Size;

   type Sink(SinkType : ESinkType := CONSOLE_SINK) is private;

   function Channel (self : access Sink) return Logging_Message.LogChannel
     with inline, pre => self /= null;
   
   procedure WriteLogs(self : access Sink; logs : in LogMessages)
     with pre => self /= null;
   
   procedure Close (self : access Sink)
     with pre => self /= null;
   
   procedure MakeSink (self : out Sink; name : in String; cfg : in ConfigTree.NodePtr)
     with pre => not ConfigTree.IsNull(cfg) and then cfg.GetName = "sink";
   
private
   type Sink(SinkType : ESinkType := CONSOLE_SINK) is
      record
         severity : Logging_Message.ESeverity;
         channel  : Logging_Message.LogChannel;
         case SinkType is
            when CONSOLE_SINK =>
               null;
            when FILE_SINK =>
               template       : Ada.Strings.Unbounded.Unbounded_String;
               prefix         : Ada.Strings.Unbounded.Unbounded_String;
               suffix         : Ada.Strings.Unbounded.Unbounded_String;
               open_by_demand : Pal.bool;
         end case;
      end record;
   
   procedure Write (self : access Sink; log : in Logging_Message.LogMessage);

end Sinks;
