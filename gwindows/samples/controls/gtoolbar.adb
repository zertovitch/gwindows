with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Windows; use GWindows.Windows;
with GWindows.Packing_Boxes; use GWindows.Packing_Boxes;
with GWindows.Buttons; use GWindows.Buttons;
with GWindows.Base; use GWindows.Base;
with GWindows.Application;

procedure GToolBar is
   Main_Win  : Main_Window_Type;
   Panel_Win : Packing_Box_Type;

   B1        : Button_Type;
   B2        : Button_Type;
   B3        : Button_Type;

   Is_Docked : Boolean := True;

   procedure Do_Docker;
   --  Toggle location of tool bar

   procedure Do_Double_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Switch tool bar on double click of bar

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);
   --  Switch tool bar back to main window on floating tool bar close

   procedure Do_Docker
   is
   begin
      if Is_Docked then
         declare
            Float_Win : Window_Access := new Window_Type;
         begin
            Create_As_Tool_Window
              (Float_Win.all, Main_Win, "Stuff",
               Width => Width (Main_Win),
               Height => 60,
               Is_Dynamic => True);
            Visible (Float_Win.all);
            On_Close_Handler (Float_Win.all, Do_Close'Unrestricted_Access);

            Is_Docked := False;

            Parent (Panel_Win, Float_Win.all);
         end;
      else
         declare
            Float_Win : Pointer_To_Base_Window_Class := Parent (Panel_Win);
         begin
            Is_Docked := True;

            Parent (Panel_Win, Main_Win);
            Close (Float_Win.all);
         end;
      end if;
   end Do_Docker;

   procedure Do_Double_Click
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States)
   is
   begin
      if Is_Docked then
         Do_Docker;
      end if;
   end Do_Double_Click;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
   begin
      if Is_Docked = False then
         Do_Docker;
         Can_Close := True;
      end if;
   end Do_Close;

begin
   Create (Main_Win, "GToolBar");
   Create (Panel_Win, Main_Win, 0, 0, 1, 40, Horizontal);
   On_Left_Mouse_Button_Double_Click_Handler
     (Panel_Win,
      Do_Double_Click'Unrestricted_Access);
   Border (Panel_Win);
   Dock (Panel_Win, GWindows.Base.At_Top);
   Padding (Panel_Win, 5);
   Insets (Panel_Win, (3, 3, 3, 3));
   Create (B1, Panel_Win, "B1", 1, 1, 30, 30);
   Create (B2, Panel_Win, "B2", 1, 1, 30, 30);
   Create (B3, Panel_Win, "B3", 1, 1, 30, 30);

   Visible (Main_Win);
   Dock_Children (Main_Win);

   GWindows.Application.Message_Loop;
end GToolBar;
