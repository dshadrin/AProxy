------------------------------------------------------------------------------
--                                                                          --
--  File:                                                                   --
--     formatted_output-integer_output.ads                                  --
--                                                                          --
--  Description:                                                            --
--     Formatted_Output.Integer_Output generic package specification,       --
--     contains prototypes of functions that supports formatted output of   --
--     integer types                                                        --
--                                                                          --
--  Author:                                                                 --
--     Eugene Nonko, cm@liceum.secna.ru                                     --
--                                                                          --
--  Revision history:                                                       --
--     27/01/99 - original                                                  --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Item_Type is range <>;

package Formatted_Output.Integer_Output is

   function "&" (Fmt : Format_Type; Value : Item_Type) return Format_Type;
   --  Replaces leftmost formatting sequence in Fmt with formatted
   --  Value image, then returns Fmt. Raises exception Format_Error
   --  when invalid formatting sequence is found or no formatting
   --  sequence found at all

end Formatted_Output.Integer_Output;
