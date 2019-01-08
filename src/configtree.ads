with Ada.Finalization;

package ConfigTree is

   type Node(nameSize, dataSize : Positive) is tagged limited private;
   type NodePtr is access all Node; 

   type Tree is tagged limited private;

private
   
   type Node(nameSize, dataSize : Positive) is tagged limited
      record
         parent      : NodePtr;  -- parent node
         next        : NodePtr;  -- reference to next node on the same level
         prev        : NodePtr;  -- reference to previous node on the same level
         childFirst  : NodePtr;  -- list of childs (reference to first child)
         childLast   : NodePtr;  -- list of childs (reference to last child)
         name        : String(1..nameSize);   -- name of node
         data        : String(1..dataSize);   -- data
      end record;
      
   type Tree is new Ada.Finalization.Limited_Controlled with
      record
         head : NodePtr;  -- reference to first node
      end record;

   procedure Initialize (Object : in out Tree);
   procedure Finalize (Object : in out Tree);


end ConfigTree;
