with Demo_Exlv_Pkg; use Demo_Exlv_Pkg;

with Gwindows.Windows.Main;
with Gwindows.Application;
with Gwindows.Common_Controls;
with Gwindows.Gstrings;
with Gwindows.Drawing_Objects;
with Gwindows.Buttons;
with Gwindows.Base;

with Ada.Text_Io;
with Ada.Command_Line;

-- demonstrates the sorting and autosize of columns
procedure Demo_Exlv3 is
   use Gwindows.Windows.Main;
   use Gwindows.Gstrings;
   use Gwindows.Buttons;

   Font, font2: Gwindows.Drawing_Objects.Font_Type;
   Main: Main_Window_Type;
   List: my_List_View_Type;
   my_Gen: my_Random_Pkg.Generator;
   Check: Check_Box_Type;
   Button: Button_Type;

   function Random_Date return Gwindows.Gstring is
      Day: Positive := my_Random_Pkg.Random(my_Gen) * 2;
      month: Positive := my_Random_Pkg.Random(my_Gen);
      year: Positive := my_Random_Pkg.Random(my_Gen) * 6 + 1940;
      S_Day, S_Month: String(1..2);
      S_Year: String(1..4);
   begin
      if Day > 9 then
         S_Day := Integer'Image(Day)(2..3);
      else
         S_Day := '0' & integer'Image(Day)(2);
      end if;
      if month > 9 then
         S_month := Integer'Image(month)(2..3);
      else
         S_month := '0' & integer'Image(month)(2);
      end if;
      S_year := Integer'Image(year)(2..5);

      return To_Gstring_From_String(S_Day & "." & S_Month & "." & S_Year);
   end Random_Date;

   function My_compare(Control: in My_List_View_Pkg.Ex_List_View_Control_Type;
                       Column: in Natural;
                       Value1: in Gwindows.Gstring;
                       Value2: in Gwindows.Gstring) return Integer is
   begin
      if Column in 0..1 then
         if Value1 > Value2 then
            return 1;
         elsif Value1 < Value2 then
            return -1;
         else
            return 0;
         end if;
      else
         -- date (format is DD.MM.YYYY)
         declare -- new format is YYYYMMDD
            New_Value1: Gwindows.GString(1..8) := Value1(7..10) & Value1(4..5) & Value1(1..2);
            New_Value2: Gwindows.GString(1..8) := Value2(7..10) & Value2(4..5) & Value2(1..2);
         begin
            -- compare
            if New_Value1 > New_Value2 then
               return 1;
            elsif New_Value1 < New_Value2 then
               return -1;
            else
               return 0;
            end if;
         end;
      end if;
   end My_Compare;

   procedure Do_On_check(Window: in out Gwindows.Base.Base_Window_Type'Class)is
   begin
      if State(Check_Box_Type(Window)) = Checked then
         -- set the compare event
         On_Compare_Handler(Control => List,
                            Event => My_Compare'Unrestricted_Access);
      else
         -- remove the event
         On_Compare_Handler(Control => List,
                            Event => null);
      end if;

   end Do_On_check;

   procedure Do_On_Button_click(Window: in out Gwindows.Base.Base_Window_Type'Class)is
   begin
      for I in 0..Column_Count(List)-1 loop
         Autosize(Control => List,
                  Column => I);
      end loop;
   end do_on_button_click;

begin
   -- font
   Gwindows.Drawing_Objects.Create_Stock_Font(Font, Gwindows.Drawing_objects.ANSI_Variable_Width);

   -- main
   Create(Main, "Test ex_list_view - sorting - " & To_GString_From_String(Ada.Command_Line.Command_Name), 0, 0, 680, 500);
   Center(Main);
   Set_Font(Main, Font);

   -- list_view
   -- enable gui-sort via Parameter sort
   Create(Control => List,
          Parent  => Main,
          Left    => 20,
          Top     => 20,
          Width   => 400,
          Height  => 400,
          Sort => Gwindows.Common_Controls.Sort_custom,
          View => Gwindows.Common_Controls.Report_View);

   -- styles
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.Grid);
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.fullrowselect);
   Set_Extended_Style(Control => List, Style => My_List_View_pkg.headerdragdrop);

   -- columns
   Insert_Column (Control => List,
                  Text    => "Column 0",
                  Index   => 0,
                  Width   => 120);
   Insert_Column (Control => List,
                  Text    => "Column 1",
                  Index   => 1,
                  Width   => 120);
   Insert_Column (Control => List,
                  Text    => "Date",
                  Index   => 2,
                  Width   => 120);


   -- test data
   declare
      L_Index: Natural;
      L_Sorted_Index: Integer;
   begin
      for I in 0..99 loop
         L_Index := I;
         Insert_Item (Control => List,
                      Text => To_Gstring_From_String("Item" & natural'Image(I) & "/0"),
                      Index => L_Index,
                      Sorted_Index => L_Sorted_index);
         Set_Sub_Item (Control => List,
                       Text    => To_Gstring_From_String(natural'Image(I) & "/1"),
                       Index => L_Sorted_Index,
                       Sub_Index => 1);

         -- random date
         Set_Sub_Item (Control => List,
                       Text    => Random_Date,
                       Index => L_Sorted_Index,
                       Sub_Index => 2);
      end loop;
   end;


   -- buttons
   Create(Button => Check,
          Parent => Main,
          Text => "custom sort for date",
          Left => 450,
          Top => 50,
          Width => 200,
          Height => 25);
   On_Click_Handler(Check, Do_On_Check'Unrestricted_Access);

   Create(Button => button,
          Parent => Main,
          Text => "autosize columns",
          Left => 450,
          Top => 100,
          Width => 130,
          Height => 22);
   On_Click_Handler(button, Do_On_Button_click'Unrestricted_Access);

   -- start with sort on column 1
   Sort(List, 1, My_List_View_Pkg.down, true);

   Show(Main);
   Gwindows.Application.Message_Loop;

end Demo_Exlv3;


