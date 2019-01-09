with Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with ConfigTree.SaxParser;

package body ConfigTree is

   procedure Free is new Unchecked_Deallocation(Node, NodePtr);
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Node) is
   begin
      
      if Object.next /= null then
         Free (Object.next);
      end if;
      if Object.childFirst /= null then
         Free (Object.childFirst);
      end if;
      Put_Line("Finalize Node: " & To_String(Object.name));
   end Finalize;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Tree) is
   begin
      if Object.head /= null then
         Free (Object.head);
      end if;
      Put_Line("Finalize ConfigTree");
   end Finalize;
   
   procedure Initialize (Object : in out Tree) is
   begin
      SaxParser.Parse(Object.head);
   end Initialize;

end ConfigTree;
