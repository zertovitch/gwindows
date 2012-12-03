with Demo_Exlv_Pkg; use Demo_Exlv_Pkg;

with Gwindows.Windows.Main;
with Gwindows.Application;
with Gwindows.Common_Controls;
with Gwindows.Gstrings;
with Gwindows.Colors;
with Gwindows.Static_Controls;
with Gwindows.Base;
with Gwindows.Cursors;
with Gwindows.Types;
with Gwindows.Drawing_Objects;

-- demonstrates the handling of payload
procedure Demo_Exlv2 is
   use Gwindows.Windows.Main;
   use Gwindows.Gstrings;
   use Gwindows.Colors;
   use Gwindows.Static_Controls;

   Font: Gwindows.Drawing_Objects.Font_Type;
   Main: Main_Window_Type;
   List: my_List_View_Type;
   Labelr,
   Labelg,
   Labelb: Gwindows.Static_Controls.Label_Type;

   procedure Do_On_List_Click(Window: in out Gwindows.Base.Base_Window_Type'Class)is
      use My_List_View_Pkg;
      L_Item, L_Subitem: Integer := -1;
      Position: Gwindows.Types.Point_Type := Gwindows.Cursors.Get_Cursor_Position;
      Payload: My_List_View_Pkg.Data_Access;
   begin
      Item_At_Position(Control => My_List_View_Type(Window),
                       Position => Gwindows.Base.Point_To_Client(Window, Position),
                       Item     => L_Item,
                       SubItem  => L_Subitem);
      if L_Item = -1 or L_Subitem = -1 then
         return;
      end if;

      -- get payload
      Payload := Item_Data(Control => My_List_View_Type(Window),
                           Index => L_Item);
      if Payload /= null then
         if L_Subitem = 0 then
            Text(Labelr, To_Gstring_From_String("RGB red =" & Color_Range'Image(Payload.Rgb0.Red)));
            Text(Labelg, To_Gstring_From_String("RGB green =" & Color_Range'Image(Payload.Rgb0.green)));
            Text(Labelb, To_Gstring_From_String("RGB blue =" & Color_Range'Image(Payload.Rgb0.Blue)));
         elsif L_Subitem = 1 then
            Text(Labelr, To_Gstring_From_String("RGB red =" & Color_Range'Image(Payload.Rgb1.Red)));
            Text(Labelg, To_Gstring_From_String("RGB green =" & Color_Range'Image(Payload.Rgb1.green)));
            Text(Labelb, To_Gstring_From_String("RGB blue =" & Color_Range'Image(Payload.Rgb1.Blue)));
         end if;
      end if;

   end Do_On_List_Click;

   procedure Do_On_Free_Payload(Control: in out My_List_View_pkg.Ex_List_View_Control_Type;
                                Payload: out My_List_View_Pkg.Data_access)is
                                pragma Unreferenced (Control);
   begin
      Demo_exlv_Pkg.Free_Payload(Payload);
   end Do_On_Free_Payload;

begin
   -- font
   Gwindows.Drawing_Objects.Create_Stock_Font(Font, Gwindows.Drawing_objects.ANSI_Variable_Width);

   -- main
   Create(Main, "Test ex_list_view - Payload", 0, 0, 600, 500);
   Center(Main);
   Set_Font(Main, Font);

   -- list_view
   Create(Control => List,
          Parent  => Main,
          Left    => 20,
          Top     => 20,
          Width   => 330,
          Height  => 400,
          View => Gwindows.Common_Controls.Report_View);

   -- handler
   On_Click_Handler(List, Do_On_List_Click'Unrestricted_Access);

   -- free payload
   On_Free_Payload_Handler(Control => List,
                           Event => Do_On_Free_Payload'Unrestricted_Access);

   -- styles
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.Grid);
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.Full_Row_Select);
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.Header_Drag_Drop);

   -- columns
   Insert_Column (Control => List,
                  Text    => "Column 0",
                  Index   => 0,
                  Width   => 150);
   Insert_Column (Control => List,
                  Text    => "Column 1",
                  Index   => 1,
                  Width   => 150);

   -- test data
   declare
      L_Index: Natural;
      L_Sorted_Index: Integer;
   begin
      for I in 0..99 loop
         L_Index := I;
         Insert_Item (Control => List,
                      Text => To_Gstring_From_String("Item" & natural'Image(I)),
                      Index => L_Index,
                      Sorted_Index => L_Sorted_index);
         Set_Sub_Item (Control => List,
                       Text    => To_Gstring_From_String("SubItem" & natural'Image(I) & "/1"),
                       Index => L_Sorted_Index,
                       Sub_Index => 1);
      end loop;
   end;

   -- set colors for color_mode=SubItem and payload
   declare
      L_Rgb: Rgb_Type;
      Payload: My_List_View_Pkg.Data_Access;
   begin
      for I in 0..item_Count(Control => List)-1 loop
         L_Rgb.green := 200;
         L_Rgb.Blue := Color_Range(I*2);
         L_Rgb.red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => black,
                       Back_Color => To_Color(L_Rgb),
                       Index => i,
                       Sub_Index => 0);
         -- payload
         Payload := new Payload_Data_Type;
         Payload.Rgb0 := L_Rgb;


         L_Rgb.blue := 250;
         L_Rgb.green := Color_Range(I*2);
         L_Rgb.red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => black,
                       Back_Color => To_Color(L_Rgb),
                       Index => i,
                       Sub_Index => 1);

         -- set payload
         Payload.Rgb1 := L_Rgb;
         Item_Data(Control => List,
                   Index => I,
                   Payload => Payload);
      end loop;
   end;

   -- gui controls
   Create_Label(Parent => Main,
                Text => "Click on subitem to see the payload data:",
                Left => 380,
                Top => 50,
                Width => 200,
                Height => 50);
   Create(static => Labelr,
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
   Gwindows.Application.Message_Loop;

end Demo_Exlv2;


