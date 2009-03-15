with GWindows.Windows;
with GWindows.Base;
with GWindows.Panels;
with GWindows.GControls;

with GNAVI_Window;

package GNAVI_Layout_View is

   -------------------------------------------------------------------------
   --  Layout_View_Type
   -------------------------------------------------------------------------

   type Layout_View_Type is new GWindows.Windows.Window_Type with private;
   type Layout_View_Access is access all Layout_View_Type;
   type Pointer_To_Layout_View_Class is access all Layout_View_Type'Class;

   -------------------------------------------------------------------------
   --  Layout_View_Type - Public_Methods
   -------------------------------------------------------------------------

   procedure Edit_Window
     (Control : in out Layout_View_Type;
      Parent  : in out GWindows.Base.Base_Window_Type'Class;
      Window  : in out GNAVI_Window.GNAVI_Window_Type);

   -------------------------------------------------------------------------
   --  Layout_View_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Layout_View_Type);
   --  Setup controls

   procedure On_Size (Window : in out Layout_View_Type;
                      Width  : in     Integer;
                      Height : in     Integer);

   -------------------------------------------------------------------------
   --  Preview_View_Type
   -------------------------------------------------------------------------

   type Preview_View_Type is new GWindows.Windows.Window_Type with private;
   type Preview_View_Access is access all Preview_View_Type;
   type Pointer_To_Preview_View_Class is access all Preview_View_Type'Class;

   -------------------------------------------------------------------------
   --  Preview_View_Type - Public Methods
   -------------------------------------------------------------------------

   procedure Preview_Window (Window : in out GNAVI_Window.GNAVI_Window_Type);

   -------------------------------------------------------------------------
   --  Preview_View_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Lost_Focus (Window : in out Preview_View_Type);

   -------------------------------------------------------------------------
   --  Control_Data_Type - Custom data attached to controls
   -------------------------------------------------------------------------

   type Control_Data_Type is new GWindows.Base.Base_Data_Type with
      record
         Resizes_Children : Boolean := False;
         Win_XML          : GNAVI_Window.Pointer_To_GNAVI_Window_Class;
         Element          : GNAVI_Window.Control_Element;
      end record;
   type Control_Data_Access is access all Control_Data_Type;

private
   type Sizer_Type is new GWindows.GControls.GControl_Type with
      record
         Object  : Layout_View_Access;
         In_Size : Boolean := False;
      end record;

   procedure On_Left_Mouse_Button_Down
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Mouse_Move
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up
     (Window : in out Sizer_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     GWindows.Windows.Mouse_Key_States);

   type Layout_View_Type is new GWindows.Windows.Window_Type with
      record
         Win_XML       : GNAVI_Window.Pointer_To_GNAVI_Window_Class;
         Title_Panel   : GWindows.Panels.Panel_Type;
         Client_Frame  : GWindows.Panels.Panel_Type;
         Client        : GWindows.Panels.Panel_Type;
         Sizer         : Sizer_Type;
      end record;

   type Preview_View_Type is new GWindows.Windows.Window_Type with
      record
         null;
      end record;

end GNAVI_Layout_View;
