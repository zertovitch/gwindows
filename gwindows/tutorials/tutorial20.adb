with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Packing_Boxes; use GWindows.Packing_Boxes;
with GWindows.Base;
with GWindows.Application;

procedure Tutorial20 is
   Main_Window : Main_Window_Type;
   Box         : Packing_Box_Type;
   Box2        : Packing_Box_Type;
   Box3        : Packing_Box_Type;
   Box4        : Packing_Box_Type;
   Button1     : Button_Type;
   Button2     : Button_Type;
   Button3     : Button_Type;
   Button4     : Button_Type;
   Button5     : Button_Type;
   Button6     : Button_Type;
   Edit1       : Edit_Box_Type;
   Edit2       : Edit_Box_Type;
   Edit3       : Edit_Box_Type;

begin
   Create (Main_Window, "Packing Boxes in a Window",
           Width => 500, Height => 250);

   Create (Box, Main_Window, 0, 0, 100, 0, Vertical);
   Dock (Box, GWindows.Base.At_Left);

   Fill_Width (Box);
   Fill_Height (Box);
   Padding (Box, 5);
   Insets (Box, (10, 10, 10, 10));

   Create (Box2, Main_Window, 0, 0, 0, 50, Horizontal);
   Dock (Box2, GWindows.Base.At_Top);

   Fill_Width (Box2);
   Fill_Height (Box2);
   Padding (Box2, 5);
   Insets (Box2, (10, 10, 10, 10));

   Create (Box3, Main_Window, 0, 0, 100, 0, Vertical);
   Dock (Box3, GWindows.Base.At_Left);

   Fill_Width (Box3);
   Padding (Box3, 5);
   Insets (Box3, (10, 15, 10, 10));

   Create (Box4, Main_Window, 0, 0, 0, 0, Vertical);
   Dock (Box4, GWindows.Base.Fill);

   Fill_Width (Box4);
   Padding (Box4, 5);
   Insets (Box4, (10, 10, 10, 10));

   Dock_Children (Main_Window);

   Create (Button1, Box, "Button1", 0, 0, 0, 0);
   Create (Button2, Box, "Button2", 0, 0, 0, 0);
   Create (Button3, Box, "Button3", 0, 0, 0, 0);

   Pack (Box);

   Create (Button4, Box2, "Button3", 0, 0, 0, 0);
   Create (Button5, Box2, "Button4", 0, 0, 0, 0);
   Create (Button6, Box2, "Button5", 0, 0, 0, 0);

   Pack (Box2);

   Create_Label (Box3, "Field 1", 0, 0, 0, 30);
   Create_Label (Box3, "Field 2", 0, 0, 0, 30);
   Create_Label (Box3, "Field 3", 0, 0, 0, 30);

   Pack (Box3);

   Create (Edit1, Box4, "", 0, 0, 0, 30);
   Create (Edit2, Box4, "", 0, 0, 0, 30);
   Create (Edit3, Box4, "", 0, 0, 0, 30);

   Pack (Box4);

   Visible (Main_Window);
   GWindows.Application.Message_Loop;
end Tutorial20;
