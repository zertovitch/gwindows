--  This is a small demo that shows the use of the mschart20
--  MSCHART20 is part of visual basic of visual studio 6.0
--  Witten by Wiljan Derks 9 sep 2000.
--  To build this demo:
--       bindcom mschrt20.ocx mschart20
--       gnatmake -g -O2 mschart_demo
--  When visual studio is installed, the program will work

--  This example will also work if you have the MSCHART.OCX on your machine.
--  Just run the following in this dir:  bindcom mschrt.ocx mschart20

with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Interfaces.C;                      use Interfaces.C;

with GNATCOM.Initialize;
with GNATCOM.Types;                     use GNATCOM.Types;
with GNATCOM.BSTR;                      use GNATCOM.BSTR;

with GWindows.Application;              use GWindows.Application;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.Main;             use GWindows.Windows.Main;
with GWindows.ActiveX;                  use GWindows.ActiveX;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Base;                     use GWindows.Base;

with mschart20;                         use mschart20;
with mschart20.IVcAxisTitle_Interface;  use mschart20.IVcAxisTitle_Interface;
with mschart20.IVcAxis_Interface;       use mschart20.IVcAxis_Interface;
with mschart20.IVcPlot_Interface;       use mschart20.IVcPlot_Interface;
with mschart20.uDMSChart_Interface;     use mschart20.uDMSChart_Interface;

procedure Mschart_Demo is
   pragma Linker_Options ("-mwindows");
   Window          : Main_Window_Type;
   Chart_Container : ActiveX_Type;
   Chart           : uDMSChart_Type;

   Sine_ID : constant := 1;

   procedure Do_Size (Win : in out GWindows.Base.Base_Window_Type'Class;
                      W : in     Integer;
                      H : in     Integer) is
      pragma Warnings (Off, Win);
   begin
      Size (Chart_Container, W, H);
   end Do_Size;

   procedure Setup_Sine is
      Steps : constant := 50;
      Plot : IVcPlot_Type;
      Axis : IVcAxis_Type;
      Title : IVcAxisTitle_Type;
   begin
      Put_chartType (Chart, VtChChartType2dXY);
      Put_ColumnCount (Chart, 4);
      Put_RowCount (Chart, Steps);
      Attach (Plot, Get_Plot (Chart));
      Put_UniformAxis (Plot, VARIANT_BOOL_FALSE);
      Attach (Axis, Get_Axis (Plot, VtChAxisIdX));
      Attach (Title, Get_AxisTitle (Axis));
      Put_Text (Title, To_BSTR ("Plot of Sin (X) and Cos (X)"));
      Put_ShowLegend (Chart, VARIANT_BOOL_TRUE);
      Put_Column (Chart, 1);
      Put_ColumnLabel (Chart, To_BSTR ("Sin (X)"));
      Put_Column (Chart, 2);
      Put_ColumnLabel (Chart, To_BSTR ("X"));
      Put_Column (Chart, 3);
      Put_ColumnLabel (Chart, To_BSTR ("Cos (X)"));
      Put_Column (Chart, 4);
      Put_ColumnLabel (Chart, To_BSTR ("X"));
      for I in 1 .. Steps loop
         declare
            X : Float := Float (I - 1) * 2.0 * Pi / Float (Steps - 1);
         begin
            Put_Row (Chart, short (I));
            Put_Column (Chart, 1);
            Put_Data (Chart, To_BSTR (Float'Image (X)));
            Put_Column (Chart, 2);
            Put_Data (Chart, To_BSTR (Float'Image (Sin (X))));
            Put_Column (Chart, 3);
            Put_Data (Chart, To_BSTR (Float'Image (X)));
            Put_Column (Chart, 4);
            Put_Data (Chart, To_BSTR (Float'Image (Cos (X))));
         end;
      end loop;
   end Setup_Sine;

   procedure Do_Menu (Win  : in out Base_Window_Type'Class;
                      Item : Integer) is
      pragma Warnings (Off, Win);
   begin
      if Item = Sine_ID then
         Setup_Sine;
      elsif Item >= 100 then
         Put_chartType (Chart, Interfaces.C.long (Item - 100));
      end if;
   end Do_Menu;

   Window_Menu : Menu_Type := Create_Menu;
begin
   GNATCOM.Initialize.Initialize_COM;
   Create (Window, "MS Chart demo", Width => 600, Height => 450);

   declare
      Chart_Type_Sub : Menu_Type := Create_Popup;
      procedure Add (S : GWindows.GString;
                     Chart_Type : VtChChartType) is
      begin
         Append_Item (Chart_Type_Sub,
                      S,
                      Integer (Chart_Type) + 100);
      end Add;
   begin
      Add ("3dBar", VtChChartType3dBar);
      Add ("2dBar", VtChChartType2dBar);
      Add ("3dLine", VtChChartType3dLine);
      Add ("2dLine", VtChChartType2dLine);
      Add ("3dArea", VtChChartType3dArea);
      Add ("2dArea", VtChChartType2dArea);
      Add ("3dStep", VtChChartType3dStep);
      Add ("2dStep", VtChChartType2dStep);
      Add ("3dCombination", VtChChartType3dCombination);
      Add ("2dCombination", VtChChartType2dCombination);
      Add ("2dPie", VtChChartType2dPie);
      Add ("2dXY", VtChChartType2dXY);
      Append_Menu (Window_Menu, "Chart type", Chart_Type_Sub);
      Append_Item (Window_Menu, "Sine and cosine functions", Sine_ID);
   end;
   Menu (Window, Window_Menu);

   Create (Chart_Container, Window, CLSID_MSChart, 0, 0, 300, 300);
   Query (Chart, Interface (Chart_Container));
   On_Size_Handler (Window, Do_Size'Unrestricted_Access);
   On_Menu_Select_Handler (Window, Do_Menu'Unrestricted_Access);
   Show (Window);
   Message_Loop;

end Mschart_Demo;
