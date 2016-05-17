with Demo_exlv_Pkg; use Demo_exlv_Pkg;

with GWindows.Windows.Main;
with GWindows.Application;
with GWindows.Common_Controls;
with GWindows.GStrings;
with GWindows.Colors;
with GWindows.Static_Controls;
with GWindows.Buttons;
with GWindows.Base;
with GWindows.Drawing_Objects;

-- demonstrates the coloring of ex_list_view
procedure Demo_Exlv1 is
   use GWindows.Windows.Main;
   use GWindows.GStrings;
   use GWindows.Colors;
   use GWindows.Static_Controls;
   use GWindows.Buttons;

   Font: GWindows.Drawing_Objects.Font_Type;
   Main: Main_Window_Type;
   List: My_List_View_Type;
   Radio1, Radio2, Radio3: Radio_Button_Type;

   procedure Do_On_Radio_Click(Window : in out GWindows.Base.Base_Window_Type'Class)is
   begin
      case ID(Radio_Button_Type(Window)) is
         when 1 =>
            Color_Mode(Control => List,
                       Mode => My_List_View_Pkg.All_Items);
         when 2 =>
            Color_Mode(Control => List,
                       Mode => My_List_View_Pkg.Item_Alternately);
         when 3 =>
            Color_Mode(Control => List,
                       Mode => My_List_View_Pkg.Subitem);
         when others =>
            null;
      end case;
   end Do_On_Radio_Click;

begin
   -- font
   GWindows.Drawing_Objects.Create_Stock_Font(Font, GWindows.Drawing_Objects.ANSI_Variable_Width);

   -- main
   Create(Main, "Test ex_list_view - color modes (Color_Mode_Type)", 0, 0, 590, 500);
   Center(Main);
   Set_Font(Main, Font);

   -- list_view
   Create(Control => List,
          Parent  => Main,
          Left    => 20,
          Top     => 20,
          Width   => 330,
          Height  => 400,
          View => GWindows.Common_Controls.Report_View);

   -- styles
   Set_Extended_Style(Control => List, Style => My_List_View_Pkg.Grid);
   Set_Extended_Style(Control => List, Style => My_List_View_Pkg.Full_Row_Select);
   Set_Extended_Style(Control => List, Style => My_List_View_Pkg.Header_Drag_Drop);

   -- columns
   Insert_Column (Control => List,
                  Text    => "Column 0",
                  Index   => 0,
                  Width   => 150);
   Insert_Column (Control => List,
                  Text    => "Column 1",
                  Index   => 1,
                  Width   => 150);

   -- data for test
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

   -- set colors for color_mode=allitems
   Text_Color(Control => List, Color => To_Color(130,75,50));
   Back_Color(Control => List, Color => To_Color(250,240,200));

   -- set colors for color_mode=Item_Alternately
   Set_Alternately_Colors(Control => List,
                          Color1 => White,
                          Color2 => To_Color(230,230,230));

   -- set colors for color_mode=SubItem
   declare
      L_Rgb: RGB_Type := (others => 200);
   begin
      for I in 0..Item_Count(Control => List)-1 loop
         L_Rgb.Blue := Color_Range(I*2);
         L_Rgb.Red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => Black,
                       Back_Color => To_Color(L_Rgb),
                       Index => I,
                       Sub_Index => 0);
      end loop;
   end;

   -- set colors for column 1
   Subitem_Color(Control => List,
                 Text_Color => White,
                 Back_Color => To_Color(150,150,150),
                 Index => -1,
                 Sub_Index => 1);

   -- gui controls
   Create_Label(Parent => Main,
                Text => "Color-Mode:",
                Left => 380,
                Top => 50,
                Width => 100,
                Height => 25);

   Create(Button => Radio1,
          Parent => Main,
          Text => "AllItems",
          Left => 380,
          Top => 80,
          Width => 100,
          Height => 25,
          ID => 1);
   Create(Button => Radio2,
          Parent => Main,
          Text => "Item_Alternately",
          Left => 380,
          Top => 105,
          Width => 170,
          Height => 25,
          ID => 2);
   Create(Button => Radio3,
          Parent => Main,
          Text => "SubItem",
          Left => 380,
          Top => 130,
          Width => 100,
          Height => 25,
          ID => 3);

   On_Click_Handler(Radio1, Do_On_Radio_Click'Unrestricted_Access);
   On_Click_Handler(Radio2, Do_On_Radio_Click'Unrestricted_Access);
   On_Click_Handler(Radio3, Do_On_Radio_Click'Unrestricted_Access);

   -- set color_mode = AllItems on start
   State(Button => Radio1, State => Checked);
   Color_Mode(Control => List,
              Mode => My_List_View_Pkg.All_Items);

   Show(Main);
   GWindows.Application.Message_Loop;

end Demo_Exlv1;
