with GWindows.Base;              use GWindows.Base;
with GWindows.Drawing;           use GWindows.Drawing;
with GWindows.Types;             use GWindows.Types;
with GWindows.Windows;           use GWindows.Windows;

package body GWindows.Colored_Control_Window is

   WM_ERASEBKGND        : constant := 20;
   WM_CTLCOLOREDIT      : constant := 307;
--     WM_CTLCOLORBTN       : constant := 309;
   WM_CTLCOLORSCROLLBAR : constant := 311;
   WM_CTLCOLORSTATIC    : constant := 312;

   procedure GetClientRect
     (hwnd            : in  GWindows.Types.Handle;
      Rect            : out GWindows.Types.Rectangle_Type);
   pragma Import (StdCall, GetClientRect, "GetClientRect");

   IMAGE_BITMAP : constant := 0;
   function CopyImage (hImage    : in GWindows.Types.Handle;
                       uType     : in Integer;
                       cxDesired : in Integer;
                       cyDesired : in Integer;
                       fuFlags   : in Integer) return GWindows.Types.Handle;
   pragma Import (StdCall, CopyImage, "CopyImage");

   procedure On_Message (Window       : in out Colored_Control_Window_Type;
                         message      : in     Interfaces.C.unsigned;
                         wParam       : in     Interfaces.C.int;
                         lParam       : in     Interfaces.C.int;
                         Return_Value : in out Interfaces.C.long) is
      Canvas : Canvas_Type;
      Area   : Rectangle_Type;
      Control : Pointer_To_Base_Window_Class :=
        Window_From_Handle (long (lParam));
   begin
      if message = WM_ERASEBKGND then
         Handle (Canvas, GWindows.Types.Handle (wParam));
         GetClientRect (Handle (Window), Area);
         if Handle (Window.Bitmap) /= 0 then
            if Handle (Window.Stretched) = 0 or
              Window.Width /= Area.Right - Area.Left or
              Window.Height /= Area.Bottom - Area.Top then
               Delete (Window.Stretched);
               Handle (Window.Stretched,
                       CopyImage (Handle (Window.Bitmap), IMAGE_BITMAP,
                                  Area.Right - Area.Left,
                                  Area.Bottom - Area.Top,
                                  0));
               Window.Width := Area.Right - Area.Left;
               Window.Height := Area.Bottom - Area.Top;
            end if;
            Paint_Bitmap (Canvas, Window.Stretched,
                          0, 0, Area.Right - Area.Left,
                          Area.Bottom - Area.Top);
         else
            Fill_Rectangle (Canvas, Area, Window.Brush);
         end if;
         Handle (Canvas, 0);
         Return_Value := 1;
      elsif Control /= null then
         if message = WM_CTLCOLORSCROLLBAR and then
           Control.all in Colored_Scroll_Bar_Type'Class then
            Return_Value :=
              Handle (Colored_Scroll_Bar_Type (Control.all).Brush);
         elsif message = WM_CTLCOLORSTATIC and then
           Control.all in Colored_Label_Type'Class then
            Handle (Canvas, GWindows.Types.Handle (wParam));
            Background_Color (Canvas, Colored_Label_Type (Control.all).Color);
            Handle (Canvas, 0);
            Return_Value := Handle (Colored_Label_Type (Control.all).Brush);
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT) and then
           Control.all in Colored_Multi_Line_Edit_Box_Type'Class then
            Handle (Canvas, GWindows.Types.Handle (wParam));
            Background_Color
              (Canvas, Colored_Multi_Line_Edit_Box_Type (Control.all).Color);
            Handle (Canvas, 0);
            Return_Value :=
              Handle (Colored_Multi_Line_Edit_Box_Type (Control.all).Brush);
         else
            On_Message (Window_Type (Window), message, wParam,
                        lParam, Return_Value);
         end if;
      else
         On_Message (Window_Type (Window), message, wParam,
                     lParam, Return_Value);
      end if;
   end On_Message;

   procedure Initialize (Window : in out Colored_Control_Window_Type) is
   begin
      Create_Solid_Brush (Window.Brush, Window.Color);
      Initialize (Window_Type (Window));
   end Initialize;

   procedure Color (Window : in out Colored_Control_Window_Type;
                    Color : Color_Type) is
   begin
      if Color /= Window.Color then
         Window.Color := Color;
         Delete (Window.Brush);
         Create_Solid_Brush (Window.Brush, Window.Color);
         Redraw (Window, True);
      end if;
   end Color;

   procedure Background
     (Window : in out Colored_Control_Window_Type;
      Bitmap : GString) is
   begin
      Delete (Window.Bitmap);
      Handle (Window.Bitmap, 0);
      Delete (Window.Stretched);
      Handle (Window.Stretched, 0);
      if Bitmap /= "" then
         Load_Bitmap_From_File (Window.Bitmap, Bitmap);
      end if;
      Redraw (Window, True);
   end Background;

   procedure Initialize (Label  : in out Colored_Label_Type) is
   begin
      Create_Solid_Brush (Label.Brush, Label.Color);
      Initialize (Label_Type (Label));
   end Initialize;

   procedure Color (Label : in out Colored_Label_Type;
                    Color : Color_Type) is
   begin
      if Color /= Label.Color then
         Label.Color := Color;
         Delete (Label.Brush);
         Create_Solid_Brush (Label.Brush, Label.Color);
         Redraw (Label, True);
      end if;
   end Color;

   procedure Initialize (Edit  : in out Colored_Multi_Line_Edit_Box_Type) is
   begin
      Create_Solid_Brush (Edit.Brush, Edit.Color);
      Initialize (Multi_Line_Edit_Box_Type (Edit));
   end Initialize;

   procedure Color (Edit : in out Colored_Multi_Line_Edit_Box_Type;
                    Color : Color_Type) is
   begin
      if Color /= Edit.Color then
         Edit.Color := Color;
         Delete (Edit.Brush);
         Create_Solid_Brush (Edit.Brush, Edit.Color);
         Redraw (Edit, True);
      end if;
   end Color;

   procedure Initialize (Scroll_Bar : in out Colored_Scroll_Bar_Type) is
   begin
      Create_Solid_Brush (Scroll_Bar.Brush, Scroll_Bar.Color);
      Initialize (GWindows.Scroll_Bars.Scroll_Bar_Type (Scroll_Bar));
   end Initialize;

   procedure Color (Scroll_Bar : in out Colored_Scroll_Bar_Type;
                    Color : Color_Type) is
   begin
      if Color /= Scroll_Bar.Color then
         Scroll_Bar.Color := Color;
         Delete (Scroll_Bar.Brush);
         Create_Solid_Brush (Scroll_Bar.Brush, Scroll_Bar.Color);
         Redraw (Scroll_Bar, True);
      end if;
   end Color;

end GWindows.Colored_Control_Window;
