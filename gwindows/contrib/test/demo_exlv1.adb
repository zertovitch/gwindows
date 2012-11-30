with Demo_Exlv_Pkg; use Demo_Exlv_Pkg;

with Gwindows.Windows.Main;
with Gwindows.Application;
with Gwindows.Common_Controls;
with Gwindows.Gstrings;
with Gwindows.Colors;
with Gwindows.Static_Controls;
with Gwindows.Buttons;
with Gwindows.Base;
with Gwindows.Drawing_Objects;

-- demonstrates the coloring of ex_list_view
procedure Demo_Exlv1 is
   use Gwindows.Windows.Main;
   use Gwindows.Gstrings;
   use Gwindows.Colors;
   use Gwindows.Static_Controls;
   use Gwindows.Buttons;

   Font: Gwindows.Drawing_Objects.Font_Type;
   Main: Main_Window_Type;
   List: my_List_View_Type;
   Radio1, Radio2, Radio3: Radio_Button_Type;

   procedure Do_On_Radio_Click(window : in out GWindows.Base.Base_Window_Type'Class)is
   begin
      case Id(Radio_Button_Type(Window)) is
         when 1 =>
            Color_Mode(Control => List,
                       Mode => My_List_View_Pkg.All_Items);
         when 2 =>
            Color_mode(Control => List,
                       Mode => My_List_View_Pkg.Item_Alternately);
         when 3 =>
            Color_mode(Control => List,
                       Mode => My_List_View_Pkg.Subitem);
         when others =>
            null;
      end case;
   end Do_On_Radio_Click;

begin
   -- font
   Gwindows.Drawing_Objects.Create_Stock_Font(Font, Gwindows.Drawing_objects.ANSI_Variable_Width);

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
          View => Gwindows.Common_Controls.Report_View);

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

   -- data for test
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

   -- set colors for color_mode=allitems
   text_Color(Control => List, Color => To_Color(130,75,50));
   back_Color(Control => List, Color => to_Color(250,240,200));

   -- set colors for color_mode=Item_Alternately
   Set_Alternately_Colors(Control => List,
                          Color1 => white,
                          Color2 => To_Color(230,230,230));

   -- set colors for color_mode=SubItem
   declare
      L_Rgb: Rgb_Type := (others => 200);
   begin
      for I in 0..item_Count(Control => List)-1 loop
         L_Rgb.Blue := Color_Range(I*2);
         L_Rgb.red := Color_Range(255 - I*2);
         Subitem_Color(Control => List,
                       Text_Color => black,
                       Back_Color => To_Color(L_Rgb),
                       Index => i,
                       Sub_Index => 0);
      end loop;
   end;

   -- set colors for column 1
   Subitem_Color(Control => List,
                 Text_Color => white,
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
          Id => 1);
   Create(Button => Radio2,
          Parent => Main,
          Text => "Item_Alternately",
          Left => 380,
          Top => 105,
          Width => 170,
          Height => 25,
          Id => 2);
   Create(Button => Radio3,
          Parent => Main,
          Text => "SubItem",
          Left => 380,
          Top => 130,
          Width => 100,
          Height => 25,
          Id => 3);

   On_Click_Handler(Radio1, Do_On_Radio_Click'Unrestricted_Access);
   On_Click_Handler(Radio2, Do_On_Radio_Click'Unrestricted_Access);
   On_Click_Handler(Radio3, Do_On_Radio_Click'Unrestricted_Access);

   -- set color_mode = AllItems on start
   State(Button => Radio1, State => Checked);
   Color_Mode(Control => List,
              Mode => My_List_View_pkg.All_Items);

   Show(Main);
   Gwindows.Application.Message_Loop;

end Demo_Exlv1;


