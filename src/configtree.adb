with Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with ConfigTree.SaxParser;

package body ConfigTree is

   procedure Free is new Unchecked_Deallocation(Node, NodePtr);
   
   ---------------------------------------------------------------------------------------------------------------------
   procedure DeleteNode(ptr : in out NodePtr) is
      workNode : NodePtr;
      tempNode : NodePtr;
   begin
      if ptr /= null then
         workNode := ptr.childFirst;
         -- remove cilds in loop
         while workNode /= null loop
            tempNode := workNode.next; -- save reference to next child
            DeleteNode(workNode);      -- delete node with it childs
            workNode := tempNode;      -- restore saved reference
         end loop;
         Free(ptr);
      end if;
   end DeleteNode;

   ---------------------------------------------------------------------------------------------------------------------
   procedure Finalize (Object : in out Tree) is
   begin
      DeleteNode(Object.head);
      Put_Line("Finalize ConfigTree");
   end Finalize;
   
   procedure Initialize (Object : in out Tree) is
   begin
      SaxParser.Parse(Object.head);
   end Initialize;

end ConfigTree;
