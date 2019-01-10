------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output.ads                                                 --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output package specification, contains prototypes of       --
--     miscellaneous conversion functions and support for formatted output  --
--     of standard fixed-length strings                                     --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     27/01/99 - original                                                  --
--     14/03/99 - Format function renamed to "+" the unary operator         --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded,
     Ada.Text_IO;
use  Ada.Text_IO;

package Formatted_Output is

   type Format_Type is limited private;

   function "+" (Fmt_String : String) return Format_Type;
   --  Converts String to Format_Type

   function "&" (Fmt : Format_Type; Value : String) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted
   --  Value string, then returns Fmt. Raises exception Format_Error
   --  when invalid string formatting sequence is found or no formatting
   --  sequence found at all

   procedure Put (Fmt : Format_Type);
   --  Puts formatted string to console using Ada.Text_IO
   --  Defined as Ada.Text_IO.Put (To_String (Fmt));
   --  Example:
   --     Put (+"%s %s %s\n" & "Just" & "a" & "test");

   procedure Put (File : File_Type; Fmt : Format_Type);
   --  Puts formatted string to file using Ada.Text_IO
   --  Defined as Ada.Text_IO.Put (File, To_String (Fmt));

   function To_String (Fmt : Format_Type) return String;
   --  Convert formatted string to fixed-length string
   --  Example:
   --     Send_To_Terminal (To_String (+"Hello %s\n" & "world"));

   Format_Error : exception;
   --  Format_Error exception raised when bad format detected

   Filler : Character := ' ';
   --  Filling character, used when padding strings

private

   type Format_Type is new Ada.Strings.Unbounded.Unbounded_String;
   
   Maximal_Item_Length : constant := 4096;

   function Scan_To_Percent_Sign (Fmt : Format_Type) return Integer;
   --  Scans string to the first occurence of percent sign ignoring the double
   --  percent, returns index of the found sign or zero, if no percent sign is
   --  found

end Formatted_Output;
