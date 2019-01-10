------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output.adb                                                 --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output package body                                        --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     27/01/99 - original                                                  --
--     14/03/99 - Format function renamed to the "+" unary operator         --
--     16/03/99 - added support for justification characters                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Float_Text_IO,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;
use  Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Float_Text_IO,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

package body Formatted_Output is

   function "+" (Fmt_String : String) return Format_Type is
   begin -- "+"
      return To_Unbounded_String (Fmt_String);
   end "+";

   function Format_String
     (Value : String; Initial_Width : Integer; Justification : Alignment)
      return String is
      
      Width : Integer;

   begin -- Format_String
      if Initial_Width < Value'Length then
         Width := Value'Length;
      else
         Width := Initial_Width;
      end if;
      declare
         S : String (1 .. Width);
      begin
         Move (Value, S, Justify => Justification, Pad => Filler);
         return S;
      end;
   end Format_String;

   function "&" (Fmt : Format_Type; Value : String) return Format_Type is

      Command_Start : constant Integer := Scan_To_Percent_Sign (Fmt);
      Width : Integer := 0;
      Digit_Occured, Justification_Changed : Boolean := False;
      Justification : Alignment := Right;

      Fmt_Copy : Unbounded_String;

   begin -- "&"
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);
         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 's' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format_String (Value, Width, Justification));
                  return Format_Type (Fmt_Copy);
               when '-' | '+' | '*' =>
                  if Justification_Changed or else Digit_Occured then
                     raise Format_Error;
                  end if;
                  Justification_Changed := True;
                  case Element (Fmt_Copy, I) is
                     when '-' =>
                        Justification := Left;
                     when '+' =>
                        Justification := Right;
                     when '*' =>
                        Justification := Center;
                     when others =>
                        null;
                  end case;
               when '0' .. '9' =>
                  Digit_Occured := True;
                  Width := Width * 10
                     + Character'Pos (Element (Fmt_Copy, I))
                     - Character'Pos ('0');
               when others =>
                  raise Format_Error;
            end case;
         end loop;
      end if;
      raise Format_Error;
   end "&";

   procedure Put (Fmt : Format_Type) is
   begin -- Put
      Put (To_String (Fmt));
   end Put;

   procedure Put (File : File_Type; Fmt : Format_Type) is
   begin -- Put
      Put (File, To_String (Fmt));
   end Put;

   function To_String (Fmt : Format_Type) return String is

      CR : constant String (1 .. 1) := (1 => ASCII.CR);
      LF : constant String (1 .. 1) := (1 => ASCII.LF);
      BS : constant String (1 .. 1) := (1 => ASCII.BS);

      I : Integer := 1;

      Fmt_Copy : Unbounded_String := Unbounded_String (Fmt);

   begin -- To_String
      while I < Length (Fmt_Copy) loop
         if Element (Fmt_Copy, I) = '\' then
            case Element (Fmt_Copy, I + 1) is
               when 'n' =>
                  Replace_Slice (Fmt_Copy, I, I + 1, LF);
                  I := I + 1;
                  --  Uncomment line above, if your system using two-byte
                  --  representation of the next line character. Example of
                  --  such system is EZ2LOAD.
               when 'r' =>
                  Replace_Slice (Fmt_Copy, I, I + 1, CR);
               when 'b' =>
                  Replace_Slice (Fmt_Copy, I, I + 1, BS);
               when '\' =>
                  Delete (Fmt_Copy, I, I);
               when others =>
                  null;
            end case;
         elsif Element (Fmt_Copy, I) = '%' then
            case Element (Fmt_Copy, I + 1) is
               when '%' =>
                  Delete (Fmt_Copy, I, I);
               when others =>
                  raise Format_Error;
            end case;
         end if;
         I := I + 1;
      end loop;
      return To_String (Fmt_Copy);
   end To_String;

   function Scan_To_Percent_Sign (Fmt : Format_Type) return Integer is
   begin -- Scan_To_Percent_Sign
      for I in 1 .. Length (Fmt) - 1 loop
         if Element (Fmt, I) = '%' and then Element (Fmt, I + 1) /= '%' then
            return I;
         end if;
      end loop;
      return 0;
   end Scan_To_Percent_Sign;

end Formatted_Output;
