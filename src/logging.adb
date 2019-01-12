----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Formatted_Output; use Formatted_Output;
with Logging_Message; use Logging_Message;
with Ada.Text_IO; use Ada.Text_IO;
with TimeStamp;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Sinks;

------------------------------------------------------------------------------------------------------------------------
package body Logging is

   LOG_OUTPUT_DELAY_MS     : constant Integer := 2500;
   LOG_OUTPUT_TIMER_PERIOD : constant Duration := 1.5;
   
   ---------------------------------------------------------------------------------------------------------------------
   function SeverityStr (sev : ESeverity) return String is
      type SeverityValueArray is array (ESeverity) of String (1 .. 4);
      sevVal : constant SeverityValueArray := ("TRAC", "DBG ", "INFO", "TEST", "WARN", "ERR ", "CRIT");
   begin
      return sevVal (sev);
   end SeverityStr;

   ---------------------------------------------------------------------------------------------------------------------
   function FormatMessage (ptr : SLogPackagePtr) return String is
   begin
      return To_String (+"[%s][%s][%s] - %s"
                        & TimeStamp.GetTimestampStr (ptr.tm_stamp)
                        & ptr.tag
                        & SeverityStr (ptr.severity)
                        & Ada.Strings.Unbounded.To_String (ptr.message));
   end FormatMessage;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure SendLogMessage (msg : LogMessage) is
   begin
     
      if loggerInstance.isWorked then
         if msg.Get_Object.lchannel /= LOG_UNKNOWN_CHANNEL then
            loggerInstance.logs.Push (msg);
         else
            LOG_WARN ("LOG ", "Skip log message with unknown channel");
         end if;
      end if;
      
   end SendLogMessage;
   
   ---------------------------------------------------------------------------------------------------------------------
   protected body LogRecords is
      
   ------------------------------------------------------------------------------------------------------------------
      entry Push (item : in LogMessage) when true is
      begin
         if isWorked then
            messages.Insert (item);
         else
            requeue Discard;
         end if;
      end Push;
      
      ------------------------------------------------------------------------------------------------------------------
      entry Pop (items : out LogsVector.Vector) when true is
         use type TimeStamp.timespec;
         tm_stamp : TimeStamp.timespec := TimeStamp.GetTimestamp;
         lm       : LogMessage;
      begin
         if isWorked and then not messages.Is_Empty then
            TimeStamp.TimestampAdjust (tm_stamp, -LOG_OUTPUT_DELAY_MS);
         
            lm := messages.First_Element;
            while lm.Get_Object.tm_stamp < tm_stamp loop
               items.Append (lm);
               messages.Delete_First;
               exit when messages.Is_Empty;
               lm := messages.First_Element;
            end loop;
         end if;
      end Pop;
      
      ------------------------------------------------------------------------------------------------------------------
      entry WaitEmpty when messages.Is_Empty is
      begin
         isWorked := false;
      end WaitEmpty;
      
      ------------------------------------------------------------------------------------------------------------------
      entry Discard when true is
      begin
         null;
      end Discard;
                  
   end LogRecords;
   
   ---------------------------------------------------------------------------------------------------------------------
   task body LogMultiplexer is
      isWorked    : Pal.bool := true;
      
   begin
     
      accept Start;
      
      while isWorked loop
         select
            accept Stop do
               isWorked := false;
            end Stop;
         else
            delay LOG_OUTPUT_TIMER_PERIOD;
            declare
               vec : LogsVector.Vector;
            begin
               loggerInstance.logs.Pop (vec);
               if not vec.Is_Empty then
                  -- TODO: send 'vec' to sinks
                  for count in vec.First_Index .. vec.Last_Index loop
                     Put_Line (FormatMessage (SP.Get_Object (vec (count))));
                  end loop;
                  
               end if;
            end;
         end select;
      end loop;

   end LogMultiplexer;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Init (lg : in out Logger; cfg : in ConfigTree.NodePtr) is
      sinksCfg : ConfigTree.NodePtr;
   begin
      lg.sArray := (null, null, null, null, null, null, null, null, null, null);
      sinksCfg := cfg.GetChild ("sinks");
      lg.CreateSinks (sinksCfg);
      lg.mp.Start;
   end Init;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure CreateSinks (lg : in out Logger; cfg : in ConfigTree.NodePtr) is
      nd  : ConfigTree.NodePtr := cfg.GetFirst;
      sk  : access Sinks.Sink'Class := null;
      str : String (1 .. 7) := "";
   begin
      while not ConfigTree.IsNull (nd) loop
         str := nd.GetValue ("name");
         sk := Sinks.MakeSink (str, nd);
         nd := cfg.GetNext;
      end loop;
      
   end CreateSinks;
      
   ---------------------------------------------------------------------------------------------------------------------
   function StartLogger (cfg : in ConfigTree.NodePtr) return LoggerPtr is
   begin
      loggerInstance := new Logger;
      loggerInstance.isWorked := false;
      loggerInstance.Init (cfg);
      loggerInstance.isWorked := true;
      LOG_INFO ("LOG ", "Start logger");
      return loggerInstance;
   end StartLogger;
   
   ---------------------------------------------------------------------------------------------------------------------
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
