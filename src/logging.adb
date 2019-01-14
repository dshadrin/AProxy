----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Formatted_Output; use Formatted_Output;
with Logging_Message; use Logging_Message;
with TimeStamp;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Sinks;

------------------------------------------------------------------------------------------------------------------------
package body Logging is

   LOG_OUTPUT_DELAY_MS     : constant Integer := 2500;
   LOG_OUTPUT_TIMER_PERIOD : constant Duration := 1.5;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure SendLogMessage (msg : LogMessage) is
   begin
     
      if loggerInstance.isWorked then
         if msg.Get.lchannel /= LOG_UNKNOWN_CHANNEL then
            loggerInstance.logs.Push (msg);
         else
            LOG_WARN ("LOG ", "Skip log message with unknown channel");
         end if;
      end if;
      
   end SendLogMessage;
   
   -----------------------------------------------------------------------------
   protected body LogRecords is
      
      --------------------------------------------------------------------------
      entry Push (item : in LogMessage) when true is
      begin
         if isWorked then
            messages.Insert (item);
         else
            requeue Discard;
         end if;
      end Push;
      
      --------------------------------------------------------------------------
      entry Pop (items : in Sinks.LogMessages) when true is
         use type TimeStamp.timespec;
         tm_stamp : TimeStamp.timespec := TimeStamp.GetTimestamp;
         lm       : LogMessage;
      begin
         if isWorked and then not messages.Is_Empty then
            TimeStamp.TimestampAdjust (tm_stamp, -LOG_OUTPUT_DELAY_MS);
         
            lm := messages.First_Element;
            while lm.Get.tm_stamp < tm_stamp loop
               items.Get.Append (lm);
               messages.Delete_First;
               exit when messages.Is_Empty;
               lm := messages.First_Element;
            end loop;
         end if;
      end Pop;
      
      --------------------------------------------------------------------------
      entry WaitEmpty when messages.Is_Empty is
      begin
         isWorked := false;
      end WaitEmpty;
      
      --------------------------------------------------------------------------
      entry Discard when true is
      begin
         null;
      end Discard;
                  
   end LogRecords;
   
   -----------------------------------------------------------------------------
   task body LogMultiplexer is
      sArray     : aliased SinksArray;
      sArraySize : Pal.uint32_t := 0;
      isWorked   : Pal.bool := true;
      
   begin
     
      accept Start (cfg : in ConfigTree.NodePtr) do
         CreateSinks (sArray'Access, sArraySize, cfg.GetChild ("sinks"));
      end Start;
            
      while isWorked loop
         select
            accept Stop do
               isWorked := false;
               for i in sArray'First .. sArraySize loop
                  Sinks.Close (sArray (i)'Access);
               end loop;
            end Stop;
         else
            delay LOG_OUTPUT_TIMER_PERIOD;
            declare
               vec : Sinks.LogMessages := Sinks.LogsSP.Make_Shared(new Sinks.LogsVector.Vector);
            begin
               loggerInstance.logs.Pop (vec);
               if not vec.Get.Is_Empty then
                  for i in sArray'First .. sArraySize loop
                     Sinks.WriteLogs (sArray (i)'Access, vec);
                  end loop;
               end if;
            end;
         end select;
      end loop;

   end LogMultiplexer;
   
   -----------------------------------------------------------------------------
   procedure Init (lg : in out Logger; cfg : in ConfigTree.NodePtr) is
   begin
      lg.mp.Start(cfg);
   end Init;
   
   -----------------------------------------------------------------------------
   procedure CreateSinks (sArray : access SinksArray; sArraySize : in out Pal.uint32_t; cfg : in ConfigTree.NodePtr) is
      use Ada.Strings.Unbounded;
      nd  : ConfigTree.NodePtr := cfg.GetFirst;
      str : Unbounded_String;
   begin
      while not ConfigTree.IsNull (nd) loop
         str := To_Unbounded_String ( nd.GetValue ("name"));
         sArraySize := sArraySize + 1;
         Sinks.MakeSink (sArray(sArraySize), To_String (str), nd);
         nd := nd.GetNext;
      end loop;
      
   end CreateSinks;
      
   -----------------------------------------------------------------------------
   function StartLogger (cfg : in ConfigTree.NodePtr) return LoggerPtr is
   begin
      loggerInstance := new Logger;
      loggerInstance.isWorked := false;
      loggerInstance.Init (cfg);
      loggerInstance.isWorked := true;
      LOG_INFO ("LOG ", "Start logger");
      return loggerInstance;
   end StartLogger;
   
   -----------------------------------------------------------------------------
   procedure StopLogger is
      procedure Free is new Ada.Unchecked_Deallocation (Logger, LoggerPtr);
   begin
      LOG_INFO ("LOG ", "Stop logger");
      loggerInstance.isWorked := false;
      loggerInstance.logs.WaitEmpty;
      loggerInstance.mp.Stop;
      Free (loggerInstance);
   end StopLogger;
   
      
end Logging;
