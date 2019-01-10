------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output-enumeration_output.adb                              --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output.Enumeration_Output generic package body             --
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
     Ada.Characters.Handling,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;
use  Ada.Text_IO,
     Ada.Characters.Handling,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded;

package body Formatted_Output.Enumeration_Output is

   package Item_Type_IO is new Enumeration_IO (Item_Type);
   use Item_Type_IO;

   type Style_Type is (Capitalized, Upper_Case, Lower_Case);

   function Format
     (Value : Item_Type; Initial_Width : Integer; Justification : Alignment;
      Style : Style_Type) return String is

      Img : String (1 .. Maximal_Item_Length);
      Width, Real_Width : Integer;
      Past_Last : Integer := 1;

   begin -- Format
      case Style is
         when Capitalized =>
            Put (Img, Value, Set => Type_Set'(Lower_Case));
            Img (1) := To_Upper (Img (1));
         when Lower_Case =>
            Put (Img, Value, Set => Type_Set'(Lower_Case));
         when Upper_Case =>
            Put (Img, Value, Set => Type_Set'(Upper_Case));
      end case;
      while Img (Past_Last) /= ' ' loop
         Past_Last := Past_Last + 1;
      end loop;
      Real_Width := Past_Last - 1;
      if Initial_Width < Real_Width then
         Width := Real_Width;
      else
         Width := Initial_Width;
      end if;
      declare
         S : String (1 .. Width);
      begin
         Move (Img (Past_Last - Real_Width .. Past_Last - 1), S,
           Justify => Justification, Pad => Filler);
         return S;
      end;
   end Format;

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type is

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
               when 'c' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Capitalized));
                  return Format_Type (Fmt_Copy);
               when 'u' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Upper_Case));
                  return Format_Type (Fmt_Copy);
               when 'l' =>
                  Replace_Slice (Fmt_Copy, Command_Start, I,
                     Format (Value, Width, Justification, Lower_Case));
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

end Formatted_Output.Enumeration_Output;
