----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal;
with Logging;
with Logging_Message;
with TimeStamp; use TimeStamp;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

------------------------------------------------------------------------------------------------------------------------
package body Logging_Message is

   ---------------------------------------------------------------------------------------------------------------------
   function "<" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool is
   begin
      return lhd.tm_stamp < rhd.tm_stamp;
   end;
   
   ---------------------------------------------------------------------------------------------------------------------
   function "=" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool is
   begin
      return lhd.tm_stamp = rhd.tm_stamp;
   end;
      
   ---------------------------------------------------------------------------------------------------------------------
   function "<" (lhd : in LogMessage; rhd : in LogMessage) return bool is
   begin
      return lhd.Get_Object.tm_stamp < rhd.Get_Object.tm_stamp;
   end;
   
   ---------------------------------------------------------------------------------------------------------------------
   function "=" (lhd : in LogMessage; rhd : in LogMessage) return bool is
   begin
      return lhd.Get_Object.tm_stamp = rhd.Get_Object.tm_stamp;
   end;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Log (sev : ESeverity; tag : String; str : String; ch : int8_t) is
      ptr : SLogPackagePtr := new SLogPackage;
      msg : LogMessage;
   begin
      ptr.all := (To_Unbounded_String (str), tag, GetTimestamp, eMessage, sev, ch);
      msg := SP.Make_Shared (ptr);
      Logging.SendLogMessage (msg);
   end Log;

   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_INFO (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eInfo, tag, str, ch);
   end LOG_INFO;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_WARN (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eWarn, tag, str, ch);
   end LOG_WARN;

   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_DEBUG (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eDebug, tag, str, ch);
   end LOG_DEBUG;

   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_ERR (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eError, tag, str, ch);
   end LOG_ERR;

   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_TEST (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eTest, tag, str, ch);
   end LOG_TEST;

   ---------------------------------------------------------------------------------------------------------------------
   procedure LOG_TRACE (tag : String; str : String; ch : int8_t := 0) is
   begin
      Log (eTrace, tag, str, ch);
   end LOG_TRACE;

end Logging_Message;
