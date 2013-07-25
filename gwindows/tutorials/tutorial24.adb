with Tutorial24_Window; use Tutorial24_Window;

with GWindows.Application;
with GWindows.Common_Controls; use GWindows.Common_Controls;

procedure Tutorial24 is
   pragma Linker_Options ("-mwindows");
   My_Window   : Tutorial24_Window.My_Window_Type;
   root, n1, n11, n2, n21, n22 : Tree_Item_Node;
begin
   Create (My_Window, "Drag test - tutorial 24");
   --  Put some fuzzy contents
   Insert_Item (My_Window.Some_list, "List item Nr 1", 0);
   Insert_Item (My_Window.Some_list, "List item Nr 2", 1);
   Insert_Item (My_Window.Some_tree, "Tree root", 0, root, As_A_Root);
   Insert_Item (My_Window.Some_tree, "Tree node 1", root, n1);
   Insert_Item (My_Window.Some_tree, "Tree node 1-1", n1, n11);
   Insert_Item (My_Window.Some_tree, "Tree node 2", root, n2);
   Insert_Item (My_Window.Some_tree, "Tree node 2-1", n2, n21);
   Insert_Item (My_Window.Some_tree, "Tree node 2-2", n2, n22);
   Expand (My_Window.Some_tree, root);
   Visible (My_Window, True);
   GWindows.Application.Message_Loop;
end Tutorial24;
