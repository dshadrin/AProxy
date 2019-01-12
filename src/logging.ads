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

------------------------------------------------------------------------------------------------------------------------
package Logging is
---------------------------------------------------------------------------------------------------------------------
   procedure SendLogMessage (msg : LogMessage);

   ---------------------------------------------------------------------------------------------------------------------
   package LogsMultiset is new Ada.Containers.Ordered_Multisets (LogMessage);
   package LogsVector is new Ada.Containers.Vectors (Pal.uint32_t, LogMessage);
   type SinksArray is array (1 .. 10) of access Sinks.Sink'Class;

   protected type LogRecords is

      entry Push (item : in LogMessage);
      entry Pop (items : out LogsVector.Vector);
      entry WaitEmpty;

   private
      entry Discard;

      messages : LogsMultiset.Set;
      isWorked : Pal.bool := true;

   end LogRecords;

   type LogRecordsPtr is access all LogRecords;

   ---------------------------------------------------------------------------------------------------------------------
   type Logger is tagged limited private;
   type LoggerPtr is access Logger;

   procedure Init (lg : in out Logger; cfg : in ConfigTree.NodePtr);
   procedure CreateSinks (lg : in out Logger; cfg : in ConfigTree.NodePtr);

   function StartLogger (cfg : in ConfigTree.NodePtr) return LoggerPtr;
   procedure StopLogger;

   ---------------------------------------------------------------------------------------------------------------------
   task type LogMultiplexer is

      entry Start;
      entry Stop;

   end LogMultiplexer;

private
   type Logger is tagged limited
      record
         isWorked   : Pal.bool;
         logs       : LogRecords;
         mp         : LogMultiplexer;
         sArray     : SinksArray;
      end record;

   ---------------------------------------------------------------------------------------------------------------------
   loggerInstance : LoggerPtr;

end Logging;
