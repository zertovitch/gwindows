with GWindows.Base;
with GNAVI_Window;

package GNAVI_Layout_View.Controls is

   function Create_Control
     (Parent      :        GWindows.Base.Pointer_To_Base_Window_Class;
      Control_XML :        GNAVI_Window.Control_Element)
     return GWindows.Base.Pointer_To_Base_Window_Class;
   --  Create Control on Parent

end GNAVI_Layout_View.Controls;
