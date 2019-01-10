------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output-modular_output.adb                                  --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output.Modular_Output generic package body                 --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     27/01/99 - original                                                  --
--     16/03/99 - added support for justification characters                --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;
use  Ada.Text_IO,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

package body Formatted_Output.Modular_Output is

   package Item_Type_IO is new Modular_IO (Item_Type);
   use Item_Type_IO;
 
   function Format
     (Value : Item_Type; Initial_Width : Integer;
      Leading_Zero : Boolean; Base : Integer;
      Justification : Alignment) return String is

      Img : String (1 .. Maximal_Item_Length);
      Width, Real_Width : Integer;
      Pre_First, Last, Current : Natural;

   begin -- Format
      Put (Img, Value, Base);
      if Base = 10 then
         Last := Maximal_Item_Length;
      else
         Last := Maximal_Item_Length - 1;
      end if;
      Pre_First := Last;
      while Img (Pre_First) /= ' ' and then Img (Pre_First) /= '#' loop
         Pre_First := Pre_First - 1;
      end loop;
      Real_Width := Last - Pre_First;
      if Initial_Width < Real_Width then
         Width := Real_Width;
      else
         Width := Initial_Width;
      end if;
      declare
         S : String (1 .. Width);
      begin
         Move (Img (Pre_First + 1 .. Last), S,
           Justify => Justification, Pad => Filler);
         if Leading_Zero then
            Current := 1;
            while S (Current) = Filler loop
               S (Current) := '0';
               Current := Current + 1;
            end loop;
         end if;
         return S;
      end;
   end Format;

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is

      Command_Start : constant Integer := Scan_To_Percent_Sign (Fmt);
      Leading_Zero : Boolean := False;
      Width : Integer := 0;
      Digit_Occured, Justification_Changed : Boolean := False;
      Justification : Alignment := Right;

      Fmt_Copy : Unbounded_String;

   begin -- "&"
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);
         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'd' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Leading_Zero, 10, Justification));
                  return Format_Type (Fmt_Copy);
               when 'x' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Leading_Zero, 16, Justification));
                  return Format_Type (Fmt_Copy);
               when 'o' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Leading_Zero, 8, Justification));
                  return Format_Type (Fmt_Copy);
               when 'b' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Leading_Zero, 2, Justification));
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
                  if Width = 0 and then Element (Fmt_Copy, I) = '0' then
                     Leading_Zero := True;
                  else
                     Width := Width * 10
                        + Character'Pos (Element (Fmt_Copy, I))
                        - Character'Pos ('0');
                  end if;
               when others =>
                  raise Format_Error;
            end case;
         end loop;
      end if;
      raise Format_Error;
   end "&";

end Formatted_Output.Modular_Output;
