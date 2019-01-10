------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output-float_output.adb                                    --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output.Float_Output generic package body                   --
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

package body Formatted_Output.Float_Output is

   package Item_Type_IO is new Float_IO (Item_Type);
   use Item_Type_IO;

   Maximal_Item_Length : constant := 128;

   function Format
     (Value : Item_Type; Initial_Width, Initial_Width_After : Integer;
      Strip_Trailing_Zeroes, Leading_Zero : Boolean; Width_Exp : Integer;
      Justification : Alignment) return String is

      Img : String (1 .. Maximal_Item_Length);
      Width, Width_After, Real_Width : Integer;
      Pre_First, Last : Natural := Maximal_Item_Length;
      Current : Natural;

   begin -- Format
      if Initial_Width_After = 0 then
         Width_After := Default_Aft;
      else
         Width_After := Initial_Width_After;
      end if;
      Put (Img, Value, Aft => Field (Width_After), Exp => Field (Width_Exp));
      while Img (Pre_First) /= ' ' loop
         Pre_First := Pre_First - 1;
      end loop;
      if Strip_Trailing_Zeroes then
         while Img (Last) = '0' loop
            Last := Last - 1;
         end loop;
         if Img (Last) = '.' then
            Last := Last - 1;
         end if;
      end if;
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
            if Value < 0.0 then
               S (1) := '-';
               Current := 2;
            else
               Current := 1;
            end if;
            while S (Current) = Filler or else S (Current) = '-' loop
               S (Current) := '0';
               Current := Current + 1;
            end loop;
         end if;
         return S;
      end;
   end Format;

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is

      Command_Start : constant Integer := Scan_To_Percent_Sign (Fmt);
      Leading_Zero, After_Point : Boolean := False;
      Width, Width_After : Integer := 0;
      Digit_Occured, Justification_Changed : Boolean := False;
      Justification : Alignment := Right;

      Fmt_Copy : Unbounded_String;

   begin -- "&"
      if Command_Start /= 0 then
         Fmt_Copy := Unbounded_String (Fmt);
         for I in Command_Start + 1 .. Length (Fmt_Copy) loop
            case Element (Fmt_Copy, I) is
               when 'e' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Width_After,
                             False, Leading_Zero, Default_Exp, Justification));
                  return Format_Type (Fmt_Copy);
               when 'f' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Width_After,
                             False, Leading_Zero, 0, Justification));
                  return Format_Type (Fmt_Copy);
               when 'g' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Width_After,
                             True, Leading_Zero, 0, Justification));
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
               when '.' =>
                  Digit_Occured := True;
                  if After_Point then
                     raise Format_Error;
                  else
                     After_Point := True;
                  end if;
               when '0' .. '9' =>
                  Digit_Occured := True;
                  if After_Point then
                     Width_After := Width_After * 10
                        + Character'Pos (Element (Fmt_Copy, I))
                        - Character'Pos ('0');
                  else
                     if Width = 0 and then Element (Fmt_Copy, I) = '0' then
                        Leading_Zero := True;
                     else
                        Width := Width * 10
                           + Character'Pos (Element (Fmt_Copy, I))
                           - Character'Pos ('0');
                     end if;
                  end if;
               when others =>
                  raise Format_Error;
            end case;
         end loop;
      end if;
      raise Format_Error;
   end "&";

end Formatted_Output.Float_Output;
