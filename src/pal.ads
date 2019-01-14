----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Finalization;
with Formatted_Output; use Formatted_Output;
with Formatted_Output.Integer_Output;
with Ada.Unchecked_Conversion;
--with System.Atomic_Primitives

--------------------------------------------------------------------------------
package Pal is

   -----------------------------------------------------------------------------
   subtype int8_t  is Integer range -2 **  7 .. 2 **  7 - 1;
   subtype int16_t is Integer range -2 ** 15 .. 2 ** 15 - 1;
   subtype int32_t is Integer range -2 ** 31 .. 2 ** 31 - 1;
   subtype int64_t is Long_Long_Integer range -2 ** 63 .. 2 ** 63 - 1;

   subtype uint8_t  is Integer range 0 .. 2 **  7 - 1;
   subtype uint16_t is Integer range 0 .. 2 ** 15 - 1;
   subtype uint32_t is Integer range 0 .. 2 ** 31 - 1;
   subtype uint64_t is Long_Long_Integer range 0 .. 2 ** 63 - 1;

   subtype bool is Boolean;

   type AtomicMemorder is (ATOMIC_RELAXED, ATOMIC_CONSUME, ATOMIC_ACQUIRE, ATOMIC_RELEASE, ATOMIC_ACQ_REL, ATOMIC_SEQ_CST);
   for AtomicMemorder'Size use Integer'Size;
   for AtomicMemorder use (0, 1, 2, 3, 4, 5);
   function MemorderCode is new Ada.Unchecked_Conversion (AtomicMemorder, Integer);

   -----------------------------------------------------------------------------
   package Formatter_Integer is new Formatted_Output.Integer_Output (Integer);
   use Formatter_Integer;

   -----------------------------------------------------------------------------
   function Sync_Sub_And_Fetch (Reference : not null access uint32_t;
                                Increment : uint32_t) return uint32_t;

   pragma Import (Intrinsic, Sync_Sub_And_Fetch, "__sync_sub_and_fetch_4");

   -----------------------------------------------------------------------------
   function Sync_Add_And_Fetch (Reference : not null access uint32_t;
                                Increment : uint32_t) return uint32_t;

   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");


   -- https://code.i-harness.com/ru/docs/gcc~7/_005f_005fatomic-builtins
   function Atomic_Load_32 (src       : not null access uint32_t;
                            memorder  : int32_t) return uint32_t;

   pragma Import (Intrinsic, Atomic_Load_32, "__atomic_load_4");

   -----------------------------------------------------------------------------
   generic
      type TypeName is private;
      type SharedObject is access all TypeName;

   package Smart_Ptr is

      type Shared_Ptr is tagged private;

      function Get (obj : in Shared_Ptr) return SharedObject with inline;
      function Make_Shared (ptr : in SharedObject) return Shared_Ptr with inline;

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
