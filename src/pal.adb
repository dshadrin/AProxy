----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

--------------------------------------------------------------------------------
package body Pal is

   package body Smart_Ptr is

      procedure Free is new Ada.Unchecked_Deallocation (TypeName, SharedObject);
      procedure Free is new Ada.Unchecked_Deallocation (uint32_t, uint32_Ptr);

      --------------------------------------------------------------------------
      procedure Initialize (obj : in out Shared_Ptr) is
      begin
         obj.pn := new uint32_t;
         obj.pn.all := 1;
      end Initialize;

      --------------------------------------------------------------------------
      procedure Adjust (obj : in out Shared_Ptr) is
         var : aliased uint32_t;
      begin
         var := Sync_Add_And_Fetch (obj.pn, 1);
      end Adjust;

      --------------------------------------------------------------------------
      procedure Finalize (obj : in out Shared_Ptr) is
      begin
         if Sync_Sub_And_Fetch (obj.pn, 1) = 0 then
            Free (obj.pt);
            Free (obj.pn);
         end if;
      end Finalize;

      --------------------------------------------------------------------------
      function Get (obj : in Shared_Ptr) return SharedObject is
      begin
         return obj.pt;
      end Get;

      --------------------------------------------------------------------------
      function Make_Shared (ptr : in SharedObject) return Shared_Ptr is
         obj : Shared_Ptr;
      begin
         obj.pt := ptr;
         return obj;
      end Make_Shared;


   end Smart_Ptr;

end Pal;
