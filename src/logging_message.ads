----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Pal; use Pal;
with TimeStamp;
with Ada.Strings.Unbounded;
with Formatted_Output; use Formatted_Output;

------------------------------------------------------------------------------------------------------------------------
package Logging_Message is

   type ESeverity is
     (
      eTrace,
      eDebug,
      eInfo,
      eTest,
      eWarn,
      eError,
      eCrit
     );
   for ESeverity'Size use int8_t'Size;
   
   ---------------------------------------------------------------------------------------------------------------------
   type LogChannel is new Pal.int8_t;
   
   LOG_UNKNOWN_CHANNEL :  constant LogChannel := -1;
   LOG_INTERNAL_CHANNEL : constant LogChannel := 0;
   LOG_UART_FILTERED_CHANNEL : constant LogChannel := 1;
   LOG_UART_RAW_CHANNEL : constant LogChannel := 2;
   LOG_CLIENT_CHANNEL : constant LogChannel := 3;
   
   G_TagSize : constant uint32_t := 4;
   G_MaxMessageSize : constant uint32_t := 4096;
   
   ---------------------------------------------------------------------------------------------------------------------
   type ELoggingMode is ( eLoggingToServer, eNoLogging );
   type ELogCommand is ( eMessage, eChangeFile, eStop );
   
   ---------------------------------------------------------------------------------------------------------------------
   type SLogPackage is
      record
         message  : Ada.Strings.Unbounded.Unbounded_String;
         tag      : String (1 .. G_TagSize);
         tm_stamp : TimeStamp.timespec;
         command  : ELogCommand;
         severity : ESeverity;
         lchannel : LogChannel;
      end record;
   
   type SLogPackagePtr is access all SLogPackage;
   
   function "<" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool;
   function "=" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool;
   
   ---------------------------------------------------------------------------------------------------------------------
   package SP is new Smart_Ptr (TypeName => SLogPackage, SharedObject => SLogPackagePtr);
   subtype LogMessage is SP.Shared_Ptr;
      
   function "<" (lhd : in LogMessage; rhd : in LogMessage) return bool;
   function "=" (lhd : in LogMessage; rhd : in LogMessage) return bool;

   procedure LOG_INFO (tag : String; str : String; ch : LogChannel := 0);
   procedure LOG_WARN (tag : String; str : String; ch : LogChannel := 0);
   procedure LOG_DEBUG (tag : String; str : String; ch : LogChannel := 0);
   procedure LOG_ERR (tag : String; str : String; ch : LogChannel := 0);
   procedure LOG_TEST (tag : String; str : String; ch : LogChannel := 0);
   procedure LOG_TRACE (tag : String; str : String; ch : LogChannel := 0);
   
private
   procedure Log (sev : ESeverity; tag : String; str : String; ch : LogChannel);
   
end Logging_Message;
