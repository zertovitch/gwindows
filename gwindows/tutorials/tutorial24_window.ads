with GWindows.Base;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Image_Lists;              use GWindows.Image_Lists;
with GWindows.Windows.Main; use GWindows.Windows;
with GWindows.Types;

package Tutorial24_Window is

   -------------------------------------------
   --  List View with some Drag capability  --
   -------------------------------------------

   type LV_with_Drag is new List_View_Control_Type with private;

   overriding
   procedure On_Notify (
      Window       : in out LV_with_Drag;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
   );

   -------------------------------------------
   --  Tree View with some Drag capability  --
   -------------------------------------------

   type TV_with_Drag is new Tree_View_Control_Type with private;

   overriding
   procedure On_Notify (
      Window       : in out TV_with_Drag;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult
   );

   -------------------
   --  Demo window  --
   -------------------

   type My_Window_Type is
     new GWindows.Windows.Main.Main_Window_Type with record
        Some_list       : LV_with_Drag;
        Some_tree       : TV_with_Drag;
        Some_edit_box   : Edit_Box_Type;
        Status          : Status_Bar_Type;
        Dragging        : Boolean := False;
        Drag_Image_List : Image_List_Type;
     end record;

   overriding
   procedure On_Create (Window : in out My_Window_Type);

   --  The On_Left_Mouse_Button_Up method is not defined (thus cannot
   --  be overriden) for the controls LV_with_Drag and TV_with_Drag.
   --  Then we override it for the parent window

   overriding
   procedure On_Left_Mouse_Button_Up (Window : in out My_Window_Type;
                                      X      : in     Integer;
                                      Y      : in     Integer;
                                      Keys   : in     Mouse_Key_States);

   overriding
   procedure On_Mouse_Move (Window : in out My_Window_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     Mouse_Key_States);

   type My_Window_Access is access all My_Window_Type;
   type Pointer_To_My_Window_Class is access all My_Window_Type'Class;

private

   type LV_with_Drag is new List_View_Control_Type with null record;
   type TV_with_Drag is new Tree_View_Control_Type with null record;

end Tutorial24_Window;
