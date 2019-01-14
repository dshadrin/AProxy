----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with ConfigTree.SaxParser;

------------------------------------------------------------------------------------------------------------------------
package body ConfigTree is

   procedure Free is new Unchecked_Deallocation (Node, NodePtr);
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Initialize (Object : in out Node) is
   begin
      Object.parent := null;
      Object.next := null;
      Object.previous := null;
      Object.childFirst := null;
      Object.childLast := null;
      Object.name := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Object.data := Ada.Strings.Unbounded.To_Unbounded_String ("");
   end Initialize;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Node) is
   begin
      
      if Object.next /= null then
         Free (Object.next);
      end if;
      if Object.childFirst /= null then
         Free (Object.childFirst);
      end if;
   end Finalize;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetChild (Object : in out Node; path : String) return NodePtr is
      use type Ada.Strings.Unbounded.Unbounded_String;
      str   : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String (path);
      pos   : Natural := Ada.Strings.Unbounded.Index (str, ".", 1);
      first : Ada.Strings.Unbounded.Unbounded_String;
      last  : Ada.Strings.Unbounded.Unbounded_String;
      node  : NodePtr := Object.childFirst;
   begin
      if pos = 0 then
         first := str;
      else
         if pos > 1 then
            first := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Strings.Unbounded.Slice (str, 1, pos - 1));
         end if;
         last := str;
         Ada.Strings.Unbounded.Delete (last, 1, pos);
      end if;
      
      if Ada.Strings.Unbounded.Length (first) > 0 then
         while node /= null loop
            exit when first = node.name;
            node := node.next;
         end loop;
      end if;
      
      if Ada.Strings.Unbounded.Length (last) > 0 then
         node := node.GetChild (Ada.Strings.Unbounded.To_String (last));
      end if;
      
      return node;
   end GetChild;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetFirst (Object : in out Node) return NodePtr is
   begin
      return Object.childFirst;
   end GetFirst;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetNext (Object : in out Node) return NodePtr is
   begin
      return Object.next;
   end GetNext;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetValue (Object : in out Node; path : in String; default : in String := "") return String is
      node : NodePtr := Object.GetChild (path);
   begin
      if not IsNull (node) then
         return Ada.Strings.Unbounded.To_String (node.data);
      end if;
      return default;
   end GetValue;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetValue (Object : in out Node) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.data);
   end GetValue;

   ---------------------------------------------------------------------------------------------------------------------
   function GetName (Object : in out Node) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Object.name);
   end GetName;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Tree) is
   begin
      if Object.root /= null then
         Free (Object.root);
      end if;
      -- Put_Line("Finalize ConfigTree");
   end Finalize;
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Initialize (Object : in out Tree) is
   begin
      Object.root := new Node;
      Object.root.name := Ada.Strings.Unbounded.To_Unbounded_String ("root");
      SaxParser.Parse (Object.root);
   end Initialize;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetChild (Object : in out Tree; path : in String) return NodePtr is
   begin
      return Object.root.GetChild (path);
   end GetChild;
   
   ---------------------------------------------------------------------------------------------------------------------
   function GetValue (Object : in out Tree; path : in String; default : in String := "") return String is
   begin
      return Object.root.GetValue (path, default);
   end GetValue;
   
   ---------------------------------------------------------------------------------------------------------------------
   function IsNull (ptr : in NodePtr) return Boolean is
   begin
      return ptr = null;
   end IsNull;


end ConfigTree;
