with GWindows.Application;
with GWindows.Colors;

package body AnotherWindow_Package is

   procedure On_Create (Window : in out AnotherWindow_Type)
   is
   begin
      Text (Window, "AnotherWindow");
      Visible (Window, True);

      GWindows.Edit_Boxes.Create (Window.Edit_Box1,
                                  Parent => Window,
                                  left => 0,
                                  top => 0,
                                  width => 1,
                                  height => 1,
                                  text => "1");
      GWindows.Edit_Boxes.Dock (Window.Edit_Box1, GWindows.Base.Fill);

      GWindows.Panels.Create (Window.Split1,
                              Parent => Window,
                              left => 0,
                              top => 0,
                              width => 1,
                              height => 50);
      GWindows.Panels.Dock (Window.Split1, GWindows.Base.At_Bottom);

      GWindows.GControls.GSize_Bars.Create (Window.Split_Bar1,
                                            Parent => Window.Split1,
                                            location => GWindows.Base.At_Top);
      GWindows.GControls.GSize_Bars.Live_Resize (Window.Split_Bar1, False);
      GWindows.GControls.GSize_Bars.Move_Bar_Color (Window.Split_Bar1,
                                                    GWindows.Colors.Blue);


      GWindows.Edit_Boxes.Create (Window.Edit_Box2,
                                  Parent => Window.Split1,
                                  left => 0,
                                  top => 0,
                                  width => 1,
                                  height => 1,
                                  text => "3");
      GWindows.Edit_Boxes.Dock (Window.Edit_Box2, GWindows.Base.Fill);

      GWindows.Panels.Create (Window.Split2,
                              Parent => Window,
                              left => 0,
                              top => 0,
                              width => 50,
                              height => 1);
      GWindows.Panels.Dock (Window.Split2, GWindows.Base.At_Right);

      GWindows.GControls.GSize_Bars.Create (Window.Split_Bar2,
                                            Parent   => Window.Split2,
                                            Text     => "Tools Tray",
                                            Width    => 25,
                                            Location => GWindows.Base.At_Left);
      GWindows.GControls.GSize_Bars.Live_Resize (Window.Split_Bar2, True);
      GWindows.GControls.GSize_Bars.Border (Window.Split_Bar2, True);
      GWindows.GControls.GSize_Bars.Background_Color
        (Window.Split_Bar2,
         GWindows.Colors.Black);
      GWindows.GControls.GSize_Bars.Text_Color (Window.Split_Bar2,
                                                GWindows.Colors.White);
      GWindows.Drawing_Objects.Create_Font (Window.Label_Font,
                                            Name => "Times New Roman",
                                            Size => 16,
                                            Weight => 900);
      GWindows.GControls.GSize_Bars.Set_Font (Window.Split_Bar2,
                                              Window.Label_Font);

      GWindows.Packing_Boxes.Create
        (Window.Pack_Box1,
         Parent => Window.Split2,
         left => 0,
         top => 0,
         width => 1,
         height => 1,
         direction => GWindows.Packing_Boxes.Vertical_From_Bottom);
      GWindows.Packing_Boxes.Dock (Window.Pack_Box1, GWindows.Base.Fill);
      GWindows.Packing_Boxes.Fill_Width (Window.Pack_Box1, True);

      GWindows.Edit_Boxes.Create (Window.Edit_Box3,
                                  Parent => Window.Pack_Box1,
                                  left => 0,
                                  top => 0,
                                  width => 1,
                                  height => 100,
                                  text => "3");

      GWindows.Panels.Create (Window.Split3,
                              Parent => Window.Pack_Box1,
                              left => 0,
                              top => 0,
                              width => 1,
                              height => 75);

      GWindows.GControls.GSize_Bars.Create (Window.Split_Bar3,
                                            Parent => Window.Split3,
                                            height => 15,
                                            location => GWindows.Base.At_Top);

      GWindows.GControls.GSize_Bars.Live_Resize (Window.Split_Bar3, True);

      GWindows.Static_Controls.Create
        (Window.Split_Text,
         Parent => Window.Split_Bar3,
         left => 0,
         top => 0,
         width => 1,
         height => 1,
         alignment => GWindows.Static_Controls.Center,
         text => "=====");
      GWindows.Static_Controls.Dock (Window.Split_Text, GWindows.Base.Fill);
      GWindows.Static_Controls.Enabled (Window.Split_Text, False);

      GWindows.Edit_Boxes.Create (Window.Edit_Box4,
                                  Parent => Window.Split3,
                                  left => 0,
                                  top => 0,
                                  width => 1,
                                  height => 1,
                                  text => "4");
      GWindows.Edit_Boxes.Dock (Window.Edit_Box4, GWindows.Base.Fill);

      GWindows.Edit_Boxes.Create (Window.Edit_Box5,
                                  Parent => Window.Pack_Box1,
                                  left => 0,
                                  top => 0,
                                  width => 1,
                                  height => 100,
                                  text => "5");
      GWindows.Windows.On_Create (GWindows.Windows.Window_Type (Window));
   end On_Create;

begin
   Create (AnotherWindow);
end AnotherWindow_Package;
