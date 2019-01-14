----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Finalization;
with Formatted_Output; use Formatted_Output;
with Formatted_Output.Integer_Output;
with Ada.Unchecked_Conversion;

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

   AtomicRelaxed : constant := 0;
   AtomicConsume : constant := 1;
   AtomicAcquire : constant := 2;
   AtomicRelease : constant := 3;
   AtomicAcqRel  : constant := 4;
   AtomicSeqCst  : constant := 5;
   subtype MemModel is Integer range AtomicRelaxed .. AtomicSeqCst;

   -----------------------------------------------------------------------------
   package Formatter_Integer is new Formatted_Output.Integer_Output (Integer);
   use Formatter_Integer;

   -----------------------------------------------------------------------------
   -- https://code.i-harness.com/ru/docs/gcc~7/_005f_005fatomic-builtins
   function Atomic_Load_32 (ptr      : not null access uint32_t;
                            memorder : MemModel := AtomicSeqCst) return uint32_t;

   pragma Import (Intrinsic, Atomic_Load_32, "__atomic_load_4");

   -----------------------------------------------------------------------------
   procedure Atomic_Store_32 (var      : not null access uint32_t;
                              val      : uint32_t;
                              memorder : MemModel := AtomicSeqCst);

   pragma Import (Intrinsic, Atomic_Store_32, "__atomic_store_4");

   -----------------------------------------------------------------------------
   function Atomic_Add_Fetch_32 (ptr      : not null access uint32_t;
                                 val      : uint32_t;
                                 memorder : MemModel := AtomicSeqCst) return uint32_t;

   pragma Import (Intrinsic, Atomic_Add_Fetch_32, "__atomic_add_fetch_4");

   -----------------------------------------------------------------------------
   function Atomic_Sub_Fetch_32 (ptr      : not null access uint32_t;
                                 val      : uint32_t;
                                 memorder : MemModel := AtomicSeqCst) return uint32_t;

   pragma Import (Intrinsic, Atomic_Sub_Fetch_32, "__atomic_sub_fetch_4");

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
            pragma Volatile (pn);
         end record;

      procedure Initialize (obj : in out Shared_Ptr);
      procedure Adjust (obj : in out Shared_Ptr);
      procedure Finalize (obj : in out Shared_Ptr);

   end Smart_Ptr;

end Pal;
