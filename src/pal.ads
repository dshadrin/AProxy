----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Finalization;
with Formatted_Output; use Formatted_Output;
with Formatted_Output.Integer_Output;

------------------------------------------------------------------------------------------------------------------------
package Pal is

   ---------------------------------------------------------------------------------------------------------------------
   subtype int8_t  is Integer range -2 **  7 .. 2 **  7 - 1;
   subtype int16_t is Integer range -2 ** 15 .. 2 ** 15 - 1;
   subtype int32_t is Integer range -2 ** 31 .. 2 ** 31 - 1;
   subtype int64_t is Long_Long_Integer range -2 ** 63 .. 2 ** 63 - 1;

   subtype uint8_t  is Integer range 0 .. 2 **  7 - 1;
   subtype uint16_t is Integer range 0 .. 2 ** 15 - 1;
   subtype uint32_t is Integer range 0 .. 2 ** 31 - 1;
   subtype uint64_t is Long_Long_Integer range 0 .. 2 ** 63 - 1;

   subtype bool is Boolean;

   ---------------------------------------------------------------------------------------------------------------------
   package Formatter_Integer is new Formatted_Output.Integer_Output (Integer);
   use Formatter_Integer;


   ---------------------------------------------------------------------------------------------------------------------
   generic
      type TypeName is tagged private;
      type SharedObject is access all TypeName;

   package Smart_Ptr is

      type Shared_Ptr is tagged private;

      function Get_Object (obj : in Shared_Ptr) return SharedObject;
      function Make_Shared (ptr : in SharedObject) return Shared_Ptr;

   private
      type uint32_Ptr is access uint32_t;

      type Shared_Ptr is new Ada.Finalization.Controlled with
         record
            pt : SharedObject;
            pn : uint32_Ptr;
            pragma Atomic (pn);
            pragma Volatile(pn);
         end record;

      procedure Initialize (obj : in out Shared_Ptr);
      procedure Adjust (obj : in out Shared_Ptr);
      procedure Finalize (obj : in out Shared_Ptr);

   end Smart_Ptr;

end Pal;
