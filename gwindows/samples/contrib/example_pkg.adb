package body Example_Pkg is

procedure Main is
   Main_Window: Ex_Main_Window_type;
   Toolbar: Ex_ToolBar_Control_Type;
   Imagelist_Toolbar, Imagelist_TreeList: Ex_Image_List_Type;
   Split_Lr, Split_Tb: Splitbar_Type;
   Content_Window: Window_Type;
   Win_Left, Win_Right, Win_Top, Win_Bottom: Window_Type;
   Tree: Ex_Tree_View_Control_Type;
   Bitmap: Bitmap_Type;
   Bitmap_play, Bitmap_Stop: Bitmap_Type;
   Bitmap_Dog1, Bitmap_Dog2, Bitmap_Mouse: Bitmap_Type;
   New_Node, Parent_node: Gwindows.Common_Controls.Tree_Item_Node;
   List: Ex_list_View_Control_Type;
   Button_play, Button_stop, Button_flash: extended_Button_Type;

   -- menu
   procedure Do_On_Menu_Select (Window : in out Gwindows.Base.Base_Window_Type'Class;
                                Item   : in     Integer                               ) is
   begin
      Message_Box("Message", "ID: " & To_Gstring_From_String(Integer'Image(Item)));
   end Do_On_Menu_Select;

   -- sort columns
   procedure Do_On_Header_Click (Control : in out Ex_List_View_Control_Type;
                                 Column  : in     Integer                    ) is
   begin
      if Column = 0 then Return; end if;
      Sort_Column(Control, Column);
   end Do_On_Header_Click;

   -- fill listview
   procedure Insert_Listitem(List: in out Ex_List_View_Control_Type;
                             Index: in Integer;
                             Icon: in Integer;
                             Text: in Gwindows.GString;
                             color: in Color_type)is
   begin
      Insert_Item(Control => List, Text => "", Index => Index, Icon => icon);
      Set_Sub_Item(Control => List, Text => text, Index => Index, Sub_index => 1);
      Set_Sub_Item(Control => List, Text => text, Index => Index, Sub_index => 2);
      Set_Item_Color(Control => List, Text_color => color, Bk_Color => White,
                     Index => Index, Sub_Index => 1);
      if Color = Black then
         Set_Item_Color(Control => List, Text_Color => white, Bk_Color => color,
                        Index => Index, Sub_Index => 2);
      else
         Set_Item_Color(Control => List, Text_Color => black, Bk_Color => color,
                        Index => Index, Sub_Index => 2);
      end if;
   end Insert_Listitem;

   -- fill treeview
   procedure Insert_treeitem(tree: in out Ex_tree_View_Control_Type;
                             Parent_node: in Tree_Item_node;
                             new_node: in out Tree_Item_node;
                             Icon: in Integer;
                             Text: in Gwindows.GString;
                             color: in Color_Type;
                             Where : in Tree_View_List_Location_Type)is
   begin
      Insert_Item(Control => Tree, Text => text, Parent_node => parent_node,
                  New_Node => New_Node, where => where);
      Set_Image(Control => Tree, item => new_Node, Image_List_Index => icon);
      Set_Item_Color(Tree, color, White, New_Node);
   end Insert_treeitem;

   -- button-click-handler
   procedure Do_On_Button_click(Window : in out GWindows.Base.Base_Window_Type'Class)is
      use Interfaces.C;
   begin
      if Handle(Extended_Button_Type(Window)) = Handle(Button_play) then
         Start_Flash(Button_flash);
      else
         Stop_Flash(Button_flash);
      end if;
   end Do_On_Button_Click;

begin
   -- Main-window
   Create(Window => Main_Window, Title => "Example Gwindows_Extended", Left => 0, Top => 0,
          Width => 500, Height => 350);
   Center(Main_Window);
   Visible(Main_Window);

   -- Imagelists
   CreateEx(List => Imagelist_toolbar, Name => "#101", Width => 16);
   CreateEx(List => Imagelist_treelist, Width => 16, Height => 16,
            Initial_size => 10);

   -- sorticons for header
   for I in 201..202 loop
      Load_Bitmap(Bitmap, To_Gstring_From_String("#" & I'img), Transparent_button);
      Add(Imagelist_Treelist, Bitmap);
   end loop;
   -- icons for items
   for I in 203..209 loop
      Load_Bitmap(Bitmap, To_Gstring_From_String("#" & I'img), Transparent_window);
      Add(Imagelist_Treelist, Bitmap);
   end loop;

   -- Toolbar
   CreateEx(Control => Toolbar, Parent => Main_Window, Left => 0, Top => 0,
            Width => 200, Height => 25);
   Dock(Toolbar, Gwindows.Base.At_Top);
   Set_Image_List(Toolbar, Imagelist_toolbar);
   on_button_select_handler(toolbar, Do_On_Menu_Select'unrestricted_access);

   -- toolbar-buttons
   Add_Button (Control => Toolbar, Image_Index => 1, Command => 101,
               Button_Style => No_Style, Text => "new", Tooltip => "new document");
   Add_Button (Control => Toolbar, Image_Index => 2, Command => 102,
               Button_Style => No_Style, Text => "open", Tooltip => "open document");
   Add_Button (Control => Toolbar, Image_Index => 3, Command => 103,
               Button_Style => No_Style, Text => "cut", Tooltip => "cut a part");
   Add_Separator(Control => Toolbar, Width => 20);
   Add_Button (Control => Toolbar, Image_Index => 4, Command => 104,
               Button_Style => check_Style, Text => "toggle", Tooltip => "toggle button");
   Add_Button (Control => Toolbar, Image_Index => 1, Command => 105,
               Button_Style => No_Style, Text => "just text",
               Tooltip => "button has no image", None_Image => True);

   -- activate toolbar-tooltips
   Activate_Tooltip(Main_Window, Toolbar);

   -- content-window
   Create_As_Control(Window => Content_Window, Parent => Main_Window, Title => "",
                     Left => 0, Top => 0, Height => 0, Width => 0);
   Dock(Content_Window, Gwindows.Base.Fill);

   -- windows
   Create_As_control(Window => Win_top, Parent => content_window, Left => 0, Top => 0,
                     Height => 100, Width => 0);
   Dock(Win_top, Gwindows.Base.At_top);

   Create_As_control(Window => Win_bottom, Parent => content_window, Left => 0, Top => 0,
                     Height => 100, Width => 0);
   Border(Win_bottom, True);
   Dock(Win_bottom, Gwindows.Base.At_bottom);

   Create_As_control(Window => Win_Left, Parent => Win_top, Left => 0, Top => 0,
                     Height => 0, Width => 100);
   Dock(Win_Left, Gwindows.Base.At_Left);

   Create_As_control(Window => Win_right, Parent => Win_top, Left => 0, Top => 0,
                     Height => 0, Width => 100);
   Dock(Win_right, Gwindows.Base.At_right);

   -- tree
   Createex(Control => Tree, Parent => Win_Left, Left => 0, Top => 0,
            Width => 0, Height => 0);
   Dock(Tree, Gwindows.Base.Fill);
   Set_Image_List(tree, Imagelist_treelist);

   -- inserting, setting image and color for node
   Insert_Treeitem(Tree => Tree, Parent_Node => 0, New_Node => Parent_Node,
                   Where => As_A_Root, Text => "Colors", Color => Black, Icon => 2);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "black", Color => Black, Icon => 3);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "red", Color => red, Icon => 4);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "blue", Color => Blue, Icon => 5);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "yellow", Color => yellow, Icon => 6);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "gray", Color => gray, Icon => 7);
   Insert_Treeitem(Tree => Tree, Parent_Node => Parent_node, New_Node => New_node,
                   Where => last, Text => "green", Color => green, Icon => 8);

   -- expand tree
   Expand(Control => Tree, At_Node => Parent_Node);

   -- list
   Createex(Control => list, Parent => Win_right, Left => 0, Top => 0,
            Width => 0, Height => 0, View => Gwindows.Common_Controls.Report_View,
            Sort => Sort_custom);
   Dock(list, Gwindows.Base.Fill);
   On_Header_Click_Handler(List, Do_On_Header_Click'Unrestricted_Access);
   Set_Image_List(list, Imagelist_treelist);

   -- extended-styles
   Set_Extended_Style(Control => List, Style => Grid);
   Set_Extended_Style(Control => List, Style => headerdragdrop);
   Set_Extended_Style(Control => List, Style => fullrowselect);

   -- insert columns
   Insert_Column(Control => List, Text => "Icon", Index => 0, Width => 50);
   Insert_Column(Control => List, Text => "Text Color", Index => 1, Width => 100);
   Insert_Column(Control => List, Text => "Back Color", Index => 2, Width => 100);

   -- inserting, setting image and color for item
   Insert_Listitem(List, 0, 3, "black", black);
   Insert_Listitem(List, 1, 4, "red", red);
   Insert_Listitem(List, 2, 5, "blue", blue);
   Insert_Listitem(List, 3, 6, "yellow", yellow);
   Insert_Listitem(List, 4, 7, "gray", gray);
   Insert_Listitem(List, 5, 8, "green", green);

   -- Buttons
   Load_Bitmap(Bitmap_play, "#304", Transparent_Button);
   Create(Button => Button_play, Parent => Win_Bottom, Top => 20, Left => 130,
          Width => 35, Height => 35, Image => Bitmap_play);
   On_Click_Handler(Button_play, Do_On_Button_Click'Unrestricted_Access);
   Load_Bitmap(Bitmap_stop, "#305", Transparent_Button);
   Create(Button => Button_stop, Parent => Win_Bottom, Top => 20, Left => 180,
          Width => 35, Height => 35, Image => Bitmap_stop);
   On_Click_Handler(Button_stop, Do_On_Button_Click'Unrestricted_Access);
   Load_Bitmap(Bitmap_dog1, "#301", Transparent_Button);
   Load_Bitmap(Bitmap_dog2, "#302", Transparent_Button);
   Load_Bitmap(Bitmap_mouse, "#303", Transparent_Button);
   Create(Button => Button_Flash, Parent => Win_Bottom, Top => 20, Left => 280,
          Width => 35, Height => 35, Image => Bitmap_dog1);
   Init_Flash(Button_Flash, Silent_Image => Bitmap_Mouse,
              Activ_Image => Bitmap_Dog2);

   -- docking
   Dock_Children(Win_Left);
   Dock_Children(Win_right);
   Dock_Children(Win_Top);
   Dock_Children(Main_Window);

   --splitbars
   Create(Splitbar => Split_Lr, Parent => Win_top, First_Window => Win_Left,
          Second_Window => Win_Right, Splitbar_Pos => 200, Width => 2,
          Refresh_At_End => True, Direction => Hor, Move_Line_Color => blue);
   Create(Splitbar => Split_tb, Parent => content_window, First_Window => Win_top,
          Second_Window => Win_bottom, Splitbar_Pos => 200, Width => 8,
          Refresh_At_End => True, Direction => Ver, Move_Line_Color => green);

   -- Message-Loop
   Gwindows.Application.Message_Loop;
end Main;

end Example_Pkg;
