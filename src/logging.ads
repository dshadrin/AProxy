----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal;
with ConfigTree;
with Sinks;
with Logging_Message; use Logging_Message;
with Ada.Containers.Ordered_Multisets;
with Ada.Containers.Vectors;

--------------------------------------------------------------------------------
package Logging is
--------------------------------------------------------------------------------
   procedure SendLogMessage (msg : LogMessage);

   -----------------------------------------------------------------------------
   package LogsMultiset is new Ada.Containers.Ordered_Multisets (LogMessage);
   type SinksArray is array (1 .. 10) of aliased Sinks.Sink;

   protected type LogRecords is

      entry Push (item : in LogMessage);
      entry Pop (items : in Sinks.LogMessages);
      entry WaitEmpty;

   private
      entry Discard;

      messages : LogsMultiset.Set;
      isWorked : Pal.bool := true;

   end LogRecords;

   type LogRecordsPtr is access all LogRecords;

   -----------------------------------------------------------------------------
   type Logger is tagged limited private;
   type LoggerPtr is access Logger;

   procedure Init (lg : in out Logger; cfg : in ConfigTree.NodePtr);

   function StartLogger (cfg : in ConfigTree.NodePtr) return LoggerPtr;
   procedure StopLogger;

   procedure CreateSinks (sArray     : access SinksArray;
                          sArraySize : in out Pal.uint32_t;
                          cfg        : in ConfigTree.NodePtr);

   -----------------------------------------------------------------------------
   task type LogMultiplexer is

      entry Start(cfg : in ConfigTree.NodePtr);
      entry Stop;

   end LogMultiplexer;

private
   type Logger is tagged limited
      record
         isWorked   : Pal.bool;
         logs       : LogRecords;
         mp         : LogMultiplexer;
      end record;

   -----------------------------------------------------------------------------
   loggerInstance : LoggerPtr;

end Logging;
