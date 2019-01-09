with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ConfigTree is

   type Node is limited private;
   type NodePtr is access all Node;

   type Tree is tagged limited private;

private
   
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
   
   type Tree is new Ada.Finalization.Limited_Controlled with
      record
         head : NodePtr;  -- reference to first node
      end record;

   procedure Initialize (Object : in out Tree);
   procedure Finalize (Object : in out Tree);


end ConfigTree;
