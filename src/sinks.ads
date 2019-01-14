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

   type Sink is limited private;

   function Channel (self : access Sink) return Logging_Message.LogChannel
     with inline, pre => self /= null;
   
   procedure WriteLogs (self : access Sink; logs : in LogMessages)
     with pre => self /= null;
   
   procedure Close (self : access Sink)
     with pre => self /= null;
   
   procedure MakeSink (self : out Sink; name : in String; cfg : in ConfigTree.NodePtr)
     with pre => not ConfigTree.IsNull (cfg) and then cfg.GetName = "sink";
   
   activeSinksCounter : aliased Pal.uint32_t := 0;
   
   -----------------------------------------------------------------------------
   task type SinkOutputer is
      entry Write (logs : in LogMessages;
                   file : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String);
      entry Start (tag  : ESinkType;
                   sev  : Logging_Message.ESeverity;
                   ch   : Logging_Message.LogChannel);
      entry Stop;      
   end SinkOutputer;
   
   type SinkOutputerPtr is access SinkOutputer;
   
private
   
   type Sink is limited
      record
         SinkType       : ESinkType;
         severity       : Logging_Message.ESeverity;
         channel        : Logging_Message.LogChannel;
         template       : Ada.Strings.Unbounded.Unbounded_String;
         prefix         : Ada.Strings.Unbounded.Unbounded_String;
         suffix         : Ada.Strings.Unbounded.Unbounded_String;
         filename       : Ada.Strings.Unbounded.Unbounded_String;
         open_by_demand : Pal.bool;
         handler        : SinkOutputerPtr;
      end record;

end Sinks;
