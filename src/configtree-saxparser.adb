----------------------------------------
-- Copyright (C) 2019 Dmitriy Shadrin --
-- All rights reserved.               --
----------------------------------------

with Sax.Readers;        use Sax.Readers;
with Input_Sources.File; use Input_Sources.File;
with Unicode.CES;
with Sax.Attributes;
with Sax.Utils;
with Sax.Symbols;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions;

------------------------------------------------------------------------------------------------------------------------
package body ConfigTree.SaxParser is

   ---------------------------------------------------------------------------------------------------------------------
   type Reader is new Sax.Readers.Reader with
      record
         parent  : NodePtr;
         current : NodePtr;
      end record;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   ---------------------------------------------------------------------------------------------------------------------
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   ---------------------------------------------------------------------------------------------------------------------
   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);

   ---------------------------------------------------------------------------------------------------------------------
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      temp : NodePtr := new Node;
   begin
      if Handler.current = null then
         -- add next parent child
         if Handler.parent.childFirst = null then
            Handler.parent.childFirst := temp;
         else
            Handler.parent.childLast.next := temp;
         end if;
         Handler.parent.childLast := temp;
         temp.parent := Handler.parent;
      else
         if Handler.current.childFirst = null then
            Handler.current.childFirst := temp;
         else
            Handler.current.childLast.next := temp;
         end if;
         Handler.current.childLast := temp;
         temp.parent := Handler.current;
         Handler.parent := Handler.current;
      end if;
      Handler.current := temp;
      Handler.current.name := Ada.Strings.Unbounded.To_Unbounded_String (Qname);
      -- Put_Line("Start_Element: " & To_String(To_Unbounded_String(Qname)));
   end Start_Element;

   ---------------------------------------------------------------------------------------------------------------------
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
      x : Integer := 0;
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Handler.current.name = Ada.Strings.Unbounded.To_Unbounded_String (Qname) then
         Handler.current := Handler.parent;
         Handler.parent := Handler.current.parent;
      else
         raise Program_Error;
      end if;
      -- Put_Line("End_Element: " & Ada.Strings.Unbounded.To_String(Ada.Strings.Unbounded.To_Unbounded_String(Qname)));
   end End_Element;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      Handler.current.data := Handler.current.data & Ada.Strings.Unbounded.To_Unbounded_String (Ch);
      -- Put_Line("Characters: " & Ada.Strings.Unbounded.To_String(Handler.current.data));
   end Characters;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Parse (root : in out ConfigTree.NodePtr) is
      saxReader : Reader;
      input     : File_Input;
   begin
      Set_Public_Id (input, "Configure file");
      Set_System_Id (input, "proxy.xml");
      Open ("proxy.xml", input);


      Set_Feature (saxReader, Namespace_Prefixes_Feature, False);
      Set_Feature (saxReader, Namespace_Feature, False);
      Set_Feature (saxReader, Validation_Feature, False);

      saxReader.parent := root;
      Parse (saxReader, input);
      Close (input);
   end Parse;

end ConfigTree.SaxParser;
