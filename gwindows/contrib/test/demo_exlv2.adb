with Demo_exlv_Pkg; use Demo_exlv_Pkg;

with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Common_Controls;
with GWindows.GStrings;
with GWindows.Colors;
with GWindows.Static_Controls;
with GWindows.Base;
with GWindows.Cursors;
with GWindows.Types;
with GWindows.Drawing_Objects;

--  Demonstrates the handling of payload
--  in GWindows.Common_Controls.Ex_List_View.

procedure Demo_Exlv2 is
   use GWindows.Windows.Main;
   use GWindows.GStrings;
   use GWindows.Colors;
   use GWindows.Static_Controls;

   Font: GWindows.Drawing_Objects.Font_Type;
   Main: Main_Window_Type;
   List: My_List_View_Type;
   Labelr,
   Labelg,
   Labelb: GWindows.Static_Controls.Label_Type;

   procedure Do_On_List_Click(Window: in out GWindows.Base.Base_Window_Type'Class)is
      use My_List_View_Pkg;
      L_Item, L_Subitem: Integer := -1;
      Position: GWindows.Types.Point_Type := GWindows.Cursors.Get_Cursor_Position;
      Payload: My_List_View_Pkg.Data_Access;
   begin
      Item_At_Position(Control => My_List_View_Type(Window),
                       Position => GWindows.Base.Point_To_Client(Window, Position),
                       Item     => L_Item,
                       SubItem  => L_Subitem);
      if L_Item = -1 or L_Subitem = -1 then
         return;
      end if;

      --  Get payload
      Payload := Item_Data(Control => My_List_View_Type(Window),
                           Index => L_Item);
      if Payload /= null then
         if L_Subitem = 0 then
            Text(Labelr, To_GString_From_String("RGB red =" & Color_Range'Image(Payload.Rgb0.Red)));
            Text(Labelg, To_GString_From_String("RGB green =" & Color_Range'Image(Payload.Rgb0.Green)));
            Text(Labelb, To_GString_From_String("RGB blue =" & Color_Range'Image(Payload.Rgb0.Blue)));
         elsif L_Subitem = 1 then
            Text(Labelr, To_GString_From_String("RGB red =" & Color_Range'Image(Payload.Rgb1.Red)));
            Text(Labelg, To_GString_From_String("RGB green =" & Color_Range'Image(Payload.Rgb1.Green)));
            Text(Labelb, To_GString_From_String("RGB blue =" & Color_Range'Image(Payload.Rgb1.Blue)));
         end if;
      end if;

   end Do_On_List_Click;

   procedure Do_On_Free_Payload(Control: in out My_List_View_Pkg.Ex_List_View_Control_Type;
                                Payload: out My_List_View_Pkg.Data_Access)is
                                pragma Unreferenced (Control);
   begin
      Demo_exlv_Pkg.Free_payload(Payload);
   end Do_On_Free_Payload;

begin
   --  Font
   GWindows.Drawing_Objects.Create_Stock_Font(Font, GWindows.Drawing_Objects.ANSI_Variable_Width);

   --  Main window creation
   Create(Main, "Test ex_list_view - Payload", 0, 0, 600, 500);
   Center(Main);
   Set_Font(Main, Font);

   --  List_View creation
   Create(Control => List,
          Parent  => Main,
          Left    => 20,
          Top     => 20,
          Width   => 330,
          Height  => 400,
          View => GWindows.Common_Controls.Report_View);

   --  On-click handler. Alternatively, we could override the On_Click method.
   On_Click_Handler(List, Do_On_List_Click'Unrestricted_Access);

   --  On-free-payload handler. Alternatively, we could override the On_Free_Payload method.
   On_Free_Payload_Handler(Control => List,
                           Event => Do_On_Free_Payload'Unrestricted_Access);

   --  Styles
   Set_Extended_Style (Control => List, Style => GWindows.Common_Controls.Grid);
   Set_Extended_Style (Control => List, Style => GWindows.Common_Controls.Full_Row_Select);
   Set_Extended_Style (Control => List, Style => GWindows.Common_Controls.Header_Drag_Drop);

   --  Columns
   Insert_Column (Control => List,
                  Text    => "Column 0",
                  Index   => 0,
                  Width   => 150);
   Insert_Column (Control => List,
                  Text    => "Column 1",
                  Index   => 1,
                  Width   => 150);

   --  Test data
   declare
      L_Index: Natural;
      L_Sorted_Index: Integer;
   begin
      for I in 0..99 loop
         L_Index := I;
         Insert_Item (Control => List,
                      Text => To_GString_From_String("Item" & Natural'Image(I)),
                      Index => L_Index,
                      Sorted_Index => L_Sorted_Index);
         Set_Sub_Item (Control => List,
                       Text    => To_GString_From_String("SubItem" & Natural'Image(I) & "/1"),
                       Index => L_Sorted_Index,
                       Sub_Index => 1);
      end loop;
   end;

   --  Set colors for color_mode=SubItem and payload
   declare
      L_Rgb: RGB_Type;
      Payload: My_List_View_Pkg.Data_Access;
   begin
      for I in 0..Item_Count(Control => List)-1 loop
         L_Rgb.Green := 200;
         L_Rgb.Blue := Color_Range(I*2);
         L_Rgb.Red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => Black,
                       Back_Color => To_Color(L_Rgb),
                       Index => I,
                       Sub_Index => 0);
         -- payload
         Payload := new Payload_Data_Type;
         Payload.Rgb0 := L_Rgb;

         L_Rgb.Blue := 250;
         L_Rgb.Green := Color_Range(I*2);
         L_Rgb.Red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => Black,
                       Back_Color => To_Color(L_Rgb),
                       Index => I,
                       Sub_Index => 1);

         -- set payload
         Payload.Rgb1 := L_Rgb;
         Item_Data(Control => List,
                   Index => I,
                   Payload => Payload);
      end loop;
   end;

   --  GUI controls
   Create_Label(Parent => Main,
                Text => "Click on subitem to see the payload data:",
                Left => 380,
                Top => 50,
                Width => 200,
                Height => 50);
   Create(Static => Labelr,
          Parent => Main,
          Text => "RGB red = ?",
          Left => 380,
          Top => 100,
          Width => 250,
          Height => 22);
   Create(Static => Labelg,
          Parent => Main,
          Text => "RGB green = ?",
          Left => 380,
          Top => 125,
          Width => 250,
          Height => 22);
   Create(Static => Labelb,
          Parent => Main,
          Text => "RGB blue = ?",
          Left => 380,
          Top => 150,
          Width => 250,
          Height => 22);

   Show(Main);
   GWindows.Application.Message_Loop;

end Demo_Exlv2;
