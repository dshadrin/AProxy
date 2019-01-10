------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output-float_output.ads                                    --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output.Float_Output generic package specification,         --
--     contains prototypes of functions that supports formatted output of   --
--     floating point types                                                 --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     28/01/99 - original                                                  --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Item_Type is digits <>;

package Formatted_Output.Float_Output is

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted
   --  Value image, then returns Fmt. Raises exception Format_Error
   --  when invalid formatting sequence is found or no formatting
   --  sequence found at all

end Formatted_Output.Float_Output;
