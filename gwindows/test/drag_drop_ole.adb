--  Derived from Tutorial 24 - testing Drag & Drop with OLE
--  Especially: dragging something from the application window.
--  to the file explorer.
--
--  *** NB: This test does not work properly yet. ***
--  For a simple, working Drag & Drop from a GWindows
--  application to Windows Explorer or the Desktop, *without using OLE*,
--  see the Sample: gwindows\samples\mouse\cap_test.adb.

with Drag_Drop_OLE_Window; use Drag_Drop_OLE_Window;

with GWindows.Application;
with GWindows.Common_Controls; use GWindows.Common_Controls;

procedure Drag_Drop_OLE is
   pragma Linker_Options ("-mwindows");
   My_Window   : Drag_Drop_OLE_Window.My_Window_Type;
   root, n1, n11, n2, n21, n22 : Tree_Item_Node;
begin
   Create (My_Window, "Drop to explorer test");
   --  Put some fuzzy contents in the list
   Insert_Item (My_Window.Some_list, "List item Nr 1", 0);
   Insert_Item (My_Window.Some_list, "List item Nr 2", 1);
   --  Put some fuzzy contents in the tree
   Insert_Item (My_Window.Some_tree, "Tree root", 0, root, As_A_Root);
   Insert_Item (My_Window.Some_tree, "Tree node 1", root, n1);
   Insert_Item (My_Window.Some_tree, "Tree node 1-1", n1, n11);
   Insert_Item (My_Window.Some_tree, "Tree node 2", root, n2);
   Insert_Item (My_Window.Some_tree, "Tree node 2-1", n2, n21);
   Insert_Item (My_Window.Some_tree, "Tree node 2-2", n2, n22);
   Expand (My_Window.Some_tree, root);
   Visible (My_Window, True);
   GWindows.Application.Message_Loop;
end Drag_Drop_OLE;
