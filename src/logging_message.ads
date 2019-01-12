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
   LOG_UNKNOWN_CHANNEL : int8_t := -1;
   LOG_INTERNAL_CHANNEL : int8_t := 0;
   LOG_UART_FILTERED_CHANNEL : int8_t := 1;
   LOG_UART_RAW_CHANNEL : int8_t := 2;
   LOG_CLIENT_CHANNEL : int8_t := 3;
   
   G_TagSize : constant uint32_t := 4;
   G_MaxMessageSize : constant uint32_t := 4096;
   
   ---------------------------------------------------------------------------------------------------------------------
   type ELoggingMode is ( eLoggingToServer, eNoLogging );
   type ELogCommand is ( eMessage, eChangeFile, eStop );
   
   ---------------------------------------------------------------------------------------------------------------------
   type SLogPackage is tagged
      record
         message  : Ada.Strings.Unbounded.Unbounded_String;
         tag      : String (1 .. G_TagSize);
         tm_stamp : TimeStamp.timespec;
         command  : ELogCommand;
         severity : ESeverity;
         lchannel : int8_t;
      end record;
   
   type SLogPackagePtr is access all SLogPackage;
   
   function "<" (lhd : access constant SLogPackage; rhd : access constant SLogPackage) return bool;
   
   ---------------------------------------------------------------------------------------------------------------------
   package SP is new Smart_Ptr (TypeName => SLogPackage, SharedObject => SLogPackagePtr);
   subtype LogMessage is SP.Shared_Ptr;
      
   procedure LOG_INFO (tag : String; str : String; ch : int8_t := 0);
      
private

   procedure Log(sev : ESeverity; tag : String; str : String; ch : int8_t);
   
end Logging_Message;
