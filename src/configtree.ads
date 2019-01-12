----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

------------------------------------------------------------------------------------------------------------------------
package ConfigTree is

   ---------------------------------------------------------------------------------------------------------------------
   type Node is tagged limited private;
   type NodePtr is access Node;
   
   function IsNull (ptr : in NodePtr) return Boolean;
   function GetFirst (Object : in out Node) return NodePtr;
   function GetNext (Object : in out Node) return NodePtr;
   function GetChild (Object : in out Node; path : in String) return NodePtr;
   function GetValue (Object : in out Node; path : in String; default : in String := "") return String;
   function GetName (Object : in out Node; path : in String; default : in String := "") return String;
  
   ---------------------------------------------------------------------------------------------------------------------
   type Tree is tagged limited private;
   
   function GetChild (Object : in out Tree; path : in String) return NodePtr;
   function GetValue (Object : in out Tree; path : in String; default : in String := "") return String;

private
   
   ---------------------------------------------------------------------------------------------------------------------
   type Node is limited new Ada.Finalization.Limited_Controlled with
      record
         parent      : NodePtr;  -- parent node
         next        : NodePtr;  -- reference to next node on the same level
         childFirst  : NodePtr;  -- list of childs (reference to first child)
         childLast   : NodePtr;  -- list of childs (reference to last child)
         name        : Unbounded_String;   -- name of node
         data        : Unbounded_String;   -- data
      end record;
   
   procedure Finalize (Object : in out Node);
   
   ---------------------------------------------------------------------------------------------------------------------
   type Tree is new Ada.Finalization.Limited_Controlled with
      record
         root : NodePtr := new Node;  -- reference to root node
      end record;

   procedure Initialize (Object : in out Tree);
   procedure Finalize (Object : in out Tree);


end ConfigTree;
