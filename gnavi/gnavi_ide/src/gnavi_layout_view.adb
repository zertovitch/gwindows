with GWindows.Drawing_Objects;
with GWindows.Drawing;
with GWindows.Colors;
with GWindows.Types;
with GWindows.GStrings;

with GNAVI_Layout_View.Controls;

package body GNAVI_Layout_View is

   Frame_Side          : constant := 3;
   Frame_Size          : constant := 2 * Frame_Side;
   Handle_Size         : constant := 6;
   Title_Text_Offset_X : constant := 5;
   Title_Text_Offset_Y : constant := 5;
   Window_Offset_X     : constant := 10;
   Window_Offset_Y     : constant := 10;

   -------------------------------------------------------------------------
   --  Private Specs
   -------------------------------------------------------------------------

   procedure Create_Child_Controls
     (Parent     : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Parent_XML : in     GNAVI_Window.Control_Element);
   --  Create Child Controls for Parent

   procedure Do_Paint_Title
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);
   --  Paint in title

   -------------------------------------------------------------------------
   --  Private Body
   -------------------------------------------------------------------------

   procedure Create_Child_Controls
     (Parent     : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Parent_XML : in     GNAVI_Window.Control_Element)
   is
      use GNAVI_Window;
      use type GNAVI_Window.Control_Element;
      use GWindows.Base;
   begin
      if Has_Child_Controls (Parent_XML) then
         declare
            N : Control_Element := First_Control
              (Child_Controls (Parent_XML));
            C : GWindows.Base.Pointer_To_Base_Window_Class;
         begin
            while N /= null loop
               C := GNAVI_Layout_View.Controls.Create_Control (Parent, N);

               if C /= null then
                  Run_Mode (C.all, GWindows.Base.Development_Create_Complete);
               end if;

               if Has_Child_Controls (N) then
                  Create_Child_Controls (C, N);
               end if;

               N := Next_Control (N);
            end loop;
         end;
      end if;
   end Create_Child_Controls;

   procedure Do_Paint_Title
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
   pragma Unreferenced (Area);
      use GWindows.Drawing;
      use GWindows.Base;
      Title : constant GWindows.GString :=
        Text (Parent (Window).all);
   begin
      Background_Mode (Canvas, Transparent);
      Text_Color (Canvas, GWindows.Colors.White);
      Put (Canvas, Title_Text_Offset_X, Title_Text_Offset_Y, Title);
   end Do_Paint_Title;

   -------------------------------------------------------------------------
   --  Layout_View_Type - Public Methods
   -------------------------------------------------------------------------

   procedure Edit_Window
     (Control : in out  Layout_View_Type;
      Parent  : in out GWindows.Base.Base_Window_Type'Class;
      Window  : in out GNAVI_Window.GNAVI_Window_Type)
   is
      use GNAVI_Window;
      use GWindows.Panels;

      Host   : constant Pointer_To_Panel_Class := new Panel_Type;
      Top    : constant Control_Element := Window_Element (Window);
      W      : Integer := Get_Init_Property (Top, "Width");
      H      : Integer := Get_Init_Property (Top, "Height");
      T      : constant GWindows.GString := Get_Init_Property (Top, "Text");
      B      : constant GWindows.GString := Get_Init_Property (Top, "Border");
      F      : constant GWindows.GString := Get_Init_Property
        (Top, "Set_Standard_Font");
      Data   : constant Control_Data_Access := new Control_Data_Type;
   begin
      Control.Win_XML := Window'Unchecked_Access;
      Data.Element := Top;
      Data.Win_XML := Window'Unchecked_Access;

      Run_Mode (Host.all, GWindows.Base.Development_Create_Start);

      if W = 0 then
         W := 500;
      end if;

      if H = 0 then
         H := 300;
      end if;

      Create_As_Control (Control, Parent,
                         Trim_Ends (T),
                         Window_Offset_X, Window_Offset_Y, W, H,
                         Is_Dynamic => True);

      Control.Sizer.Object := Control'Unchecked_Access;
      Create (Control.Sizer, Parent, "",
              W + Window_Offset_X, H + Window_Offset_Y,
              Handle_Size, Handle_Size,
              False, False, True, False);
      Background_Color (Control.Sizer, GWindows.Colors.Black);

      Create (Host.all, Control.Client,
              0, 0, 0, 0,
              Is_Dynamic => True);
      Dock (Host.all, GWindows.Base.Fill);
      Dock_Children (Control.Client);

      if F /= "" then
         Set_Standard_Font (Host.all,
                            GWindows.Drawing_Objects.Stock_Font_Type'Value
                            (GNAVI_Window.Strip_Type (F)));
      else
         Set_Standard_Font (Host.all, GWindows.Drawing_Objects.System);
      end if;

      if B = "True" then
         Border (Host.all);
      end if;

      Run_Mode (Host.all, GWindows.Base.Development_Create_Complete);

      Custom_Data (Host.all, GWindows.Base.Pointer_To_Base_Data_Class (Data));
      Create_Child_Controls
        (GWindows.Base.Pointer_To_Base_Window_Class (Host), Top);

      Dock_Children (Host.all);

      Show (Host.all);

      Run_Mode (Host.all, GWindows.Base.Development_Running);
   end Edit_Window;

   -------------------------------------------------------------------------
   --  Layout_View_Type - Event Framework Methods
   -------------------------------------------------------------------------

   procedure On_Create (Window : in out Layout_View_Type)
   is
      use GWindows.Panels;
   begin
      On_Paint_Handler (Window.Title_Panel, Do_Paint_Title'Access);
      Create (Window.Title_Panel, Window,
              0, 0, 1, 25);
      Background_Color (Window.Title_Panel, GWindows.Colors.Dark_Blue);
      Dock (Window.Title_Panel, GWindows.Base.At_Top);

      Create (Window.Client_Frame, Window,
              0, 0, 1, 1);
      Dock (Window.Client_Frame, GWindows.Base.Fill);

      Dock_Children (Window);

      Create (Window.Client, Window.Client_Frame,
              0, 0,
              Width (Window.Client_Frame) - Frame_Size,
              Height (Window.Client_Frame) - Frame_Size);
      Center (Window.Client);
   end On_Create;

   procedure On_Size (Window : in out Layout_View_Type;
                      Width  : in     Integer;
                      Height : in     Integer)
   is
      use GWindows.GStrings;
      use GWindows.Panels;
      use GNAVI_Window;

      Win : constant Control_Element := Window_Element (Window.Win_XML.all);
      W   : constant Integer := GWindows.Panels.Width (Window.Client_Frame);
      H   : constant Integer := GWindows.Panels.Height (Window.Client_Frame);
   begin
      Dock_Children (Window);
      GWindows.Panels.Width (Window.Client, W - Frame_Size);
      GWindows.Panels.Height (Window.Client, H - Frame_Size);
      Center (Window.Client);

      Set_Init_Property (Window.Win_XML.all, Win, "Width", Image (Width));
      Set_Init_Property (Window.Win_XML.all, Win, "Height", Image (Height));
   end On_Size;

   -------------------------------------------------------------------------
   --  Preview_View_Type - Public Methods
   -------------------------------------------------------------------------

   procedure Preview_Window (Window : in out GNAVI_Window.GNAVI_Window_Type)
   is
      use GNAVI_Window;

      Parent : constant Pointer_To_Preview_View_Class := new Preview_View_Type;

      Top    : constant Control_Element := Window_Element (Window);
      W      : constant Integer := Get_Init_Property (Top, "Width");
      H      : constant Integer := Get_Init_Property (Top, "Height");
      T      : constant GWindows.GString := Get_Init_Property (Top, "Text");
      B      : constant GWindows.GString := Get_Init_Property (Top, "Border");
      F      : constant GWindows.GString := Get_Init_Property
        (Top, "Set_Standard_Font");
   begin
      Run_Mode (Parent.all, GWindows.Base.Development_Create_Start);

      Create (Parent.all,
              Is_Dynamic => True);

      Text (Parent.all, Trim_Ends (T));

      if F /= "" then
         Set_Standard_Font (Parent.all,
                            GWindows.Drawing_Objects.Stock_Font_Type'Value
                            (GNAVI_Window.Strip_Type (F)));
      end if;

      if W > 0 then
         Width (Parent.all, W);
      end if;

      if H > 0 then
         Height (Parent.all, H);
      end if;

      if B = "True" then
         Border (Parent.all);
      end if;

      Run_Mode (Parent.all, GWindows.Base.Development_Create_Complete);

      Create_Child_Controls
        (GWindows.Base.Pointer_To_Base_Window_Class (Parent), Top);

      Show (Parent.all);

      Run_Mode (Parent.all, GWindows.Base.Development_Running);
   end Preview_Window;

   -------------------------------------------------------------------------
   --  Preview_View_Type - Event Framework Methods
   -------------------------------------------------------------------------

   procedure On_Lost_Focus (Window : in out Preview_View_Type)
   is
   begin
      Close (Window);
   end On_Lost_Focus;

   -------------------------------------------------------------------------
   --  Sizer_Type - Event Framework Methods
   -------------------------------------------------------------------------

   procedure On_Left_Mouse_Button_Down
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Keys);
   begin
      Window.In_Size := True;
      Capture_Mouse (Window);
   end On_Left_Mouse_Button_Down;

   procedure On_Left_Mouse_Button_Up
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      pragma Warnings (Off, X);
      pragma Warnings (Off, Y);
      pragma Warnings (Off, Keys);
   begin
      Window.In_Size := False;
      GWindows.Base.Release_Mouse;
   end On_Left_Mouse_Button_Up;

   procedure On_Mouse_Move
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States)
   is
      use GWindows.Panels;
   begin
      if Window.In_Size then
         if Width (Window.Object.Client_Frame) + X > Frame_Size then
            Width (Window.Object.all, Width (Window.Object.all) + X);
            Left (Window, Left (Window) + X);
         end if;

         if Height (Window.Object.Client_Frame) + Y > Frame_Size then
            Height (Window.Object.all, Height (Window.Object.all) + Y);
            Top (Window, Top (Window) + Y);
         end if;
      end if;
   end On_Mouse_Move;

end GNAVI_Layout_View;
