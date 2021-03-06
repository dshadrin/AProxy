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

--------------------------------------------------------------------------------
package body ConfigTree.SaxParser is

   -----------------------------------------------------------------------------
   type Reader is new Sax.Readers.Reader with
      record
         parent  : NodePtr;
         current : NodePtr;
      end record;

   -----------------------------------------------------------------------------
   procedure Start_Element (Handler       : in out Reader;
                            Namespace_URI : Unicode.CES.Byte_Sequence := "";
                            Local_Name    : Unicode.CES.Byte_Sequence := "";
                            Qname         : Unicode.CES.Byte_Sequence := "";
                            Atts          : Sax.Attributes.Attributes'Class);

   -----------------------------------------------------------------------------
   procedure End_Element (Handler       : in out Reader;
                          Namespace_URI : Unicode.CES.Byte_Sequence := "";
                          Local_Name    : Unicode.CES.Byte_Sequence := "";
                          Qname         : Unicode.CES.Byte_Sequence := "");

   -----------------------------------------------------------------------------
   procedure Characters (Handler : in out Reader;
                         Ch      : Unicode.CES.Byte_Sequence);

   -----------------------------------------------------------------------------
   procedure Start_Element (Handler       : in out Reader;
                            Namespace_URI : Unicode.CES.Byte_Sequence := "";
                            Local_Name    : Unicode.CES.Byte_Sequence := "";
                            Qname         : Unicode.CES.Byte_Sequence := "";
                            Atts          : Sax.Attributes.Attributes'Class) is
      temp : NodePtr := new Node;
   begin
      if Handler.current.childFirst = null then
         Handler.current.childFirst := temp;
         Handler.current.childLast := temp;
      else
         Handler.current.childLast.next := temp;
         temp.previous := Handler.current.childLast;
         Handler.current.childLast := temp;
      end if;
      temp.parent := Handler.current;
      Handler.parent := Handler.current;

      Handler.current := temp;
      Handler.current.name := Ada.Strings.Unbounded.To_Unbounded_String (Qname);
   end Start_Element;

   -----------------------------------------------------------------------------
   procedure End_Element (Handler       : in out Reader;
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
   end End_Element;

   -----------------------------------------------------------------------------
   procedure Characters (Handler : in out Reader;
                         Ch      : Unicode.CES.Byte_Sequence) is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      Handler.current.data := Handler.current.data & Ada.Strings.Unbounded.To_Unbounded_String (Ch);
   end Characters;

   -----------------------------------------------------------------------------
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

      saxReader.parent := null;
      saxReader.current := root;
      Parse (saxReader, input);
      Close (input);
   end Parse;

end ConfigTree.SaxParser;
