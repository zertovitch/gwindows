with GWindows.Drawing;           use GWindows.Drawing;

package body GWindows.Colored_Control_Window is

   WM_ERASEBKGND        : constant := 20;
   WM_CTLCOLOREDIT      : constant := 307;
   WM_CTLCOLORBTN       : constant := 309;
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
                         wParam       : in     GWindows.Types.Wparam;
                         lParam       : in     GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult) is
      Canvas : Canvas_Type;
      Area   : Rectangle_Type;
      Control : Pointer_To_Base_Window_Class :=
        Window_From_Handle (To_Handle (lParam));
   begin
      if message = WM_ERASEBKGND then
         Handle (Canvas, GWindows.Types.To_Handle (wParam));
         GetClientRect (Handle (Window), Area);
         if Handle (Window.Bitmap) /= Null_Handle then
            if Handle (Window.Stretched) = Null_Handle or
              Window.Width /= Area.Right - Area.Left or
              Window.Height /= Area.Bottom - Area.Top
            then
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
         Handle (Canvas, Null_Handle);
         Return_Value := 1;
      elsif Control /= null then
         if message = WM_CTLCOLORSCROLLBAR and then
           Control.all in Colored_Scroll_Bar_Type'Class
         then
            Return_Value := To_Lresult
              (Handle (Colored_Scroll_Bar_Type (Control.all).Brush));
         elsif message = WM_CTLCOLORSTATIC and then
           Control.all in Colored_Label_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color (Canvas, Colored_Label_Type (Control.all).Color);
            Text_Color (Canvas, Colored_Label_Type (Control.all).Text);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Label_Type (Control.all).Brush));
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT) and then
           Control.all in Colored_Edit_Box_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color
               (Canvas, Colored_Edit_Box_Type (Control.all).Color);
            Text_Color (Canvas, Colored_Edit_Box_Type (Control.all).Text);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Edit_Box_Type (Control.all).Brush));
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT) and then
           Control.all in Colored_Multi_Line_Edit_Box_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color
              (Canvas, Colored_Multi_Line_Edit_Box_Type (Control.all).Color);
            Text_Color
              (Canvas, Colored_Multi_Line_Edit_Box_Type (Control.all).Text);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Multi_Line_Edit_Box_Type (Control.all).Brush));
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT) and then
           Control.all in Colored_Combo_Box_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color
              (Canvas, Colored_Combo_Box_Type (Control.all).Color);
            Text_Color (Canvas, Colored_Combo_Box_Type (Control.all).Text);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Combo_Box_Type (Control.all).Brush));
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT) and then
           Control.all in Colored_Drop_Down_Combo_Box_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color
              (Canvas, Colored_Drop_Down_Combo_Box_Type (Control.all).Color);
            Text_Color
              (Canvas, Colored_Drop_Down_Combo_Box_Type (Control.all).Text);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Drop_Down_Combo_Box_Type (Control.all).Brush));
         elsif (message = WM_CTLCOLORSTATIC or
                message = WM_CTLCOLOREDIT or
                message = WM_CTLCOLORBTN) and then
               Control.all in Colored_Check_Box_Type'Class
         then
            Handle (Canvas, GWindows.Types.To_Handle (wParam));
            Background_Color
              (Canvas, Colored_Check_Box_Type (Control.all).Color);
            Text_Color
              (Canvas, Colored_Check_Box_Type (Control.all).Color);
            Handle (Canvas, Null_Handle);
            Return_Value := To_Lresult
              (Handle (Colored_Check_Box_Type (Control.all).Brush));
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
      Handle (Window.Bitmap, Null_Handle);
      Delete (Window.Stretched);
      Handle (Window.Stretched, Null_Handle);
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

   procedure Text_Color (Label : in out Colored_Label_Type;
                         Color :        Color_Type) is
   begin
      if Color /= Label.Text then
         Label.Text := Color;
         Redraw (Label, True);
      end if;
   end Text_Color;

   procedure Initialize (Edit : in out Colored_Edit_Box_Type) is
   begin
      Create_Solid_Brush (Edit.Brush, Edit.Color);
      Initialize (Edit_Box_Type (Edit));
   end Initialize;

   procedure Color (Edit  : in out Colored_Edit_Box_Type;
                    Color :        Color_Type) is
   begin
      if Color /= Edit.Color then
         Edit.Color := Color;
         Delete (Edit.Brush);
         Create_Solid_Brush (Edit.Brush, Edit.Color);
         Redraw (Edit, True);
      end if;
   end Color;

   procedure Text_Color (Edit  : in out Colored_Edit_Box_Type;
                         Color :        Color_Type) is
   begin
      if Color /= Edit.Text then
         Edit.Text := Color;
         Redraw (Edit, True);
      end if;
   end Text_Color;

   procedure Default_Color (Edit : in out Colored_Edit_Box_Type) is
   begin
      if Read_Only (Edit) then
         Color (Edit, System_Color (COLOR_BTNFACE));
      else
         Color (Edit, White);
      end if;
   end Default_Color;

   procedure Read_Only (Edit  : in out Colored_Edit_Box_Type;
                        State :        Boolean := True) is
   begin
      Read_Only (Edit_Box_Type (Edit), State);
      Default_Color (Edit);
   end Read_Only;

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

   procedure Text_Color (Edit  : in out Colored_Multi_Line_Edit_Box_Type;
                         Color :        Color_Type) is
   begin
      if Color /= Edit.Text then
         Edit.Text := Color;
         Redraw (Edit, True);
      end if;
   end Text_Color;

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

   procedure Initialize (Box : in out Colored_Combo_Box_Type) is
   begin
      Create_Solid_Brush (Box.Brush, Box.Color);
      Initialize (Combo_Box_Type (Box));
   end Initialize;

   procedure Color (Box   : in out Colored_Combo_Box_Type;
                    Color :        Color_Type) is
   begin
      if Color /= Box.Color then
         Box.Color := Color;
         Delete (Box.Brush);
         Create_Solid_Brush (Box.Brush, Box.Color);
         Redraw (Box, True);
      end if;
   end Color;

   procedure Text_Color (Box   : in out Colored_Combo_Box_Type;
                         Color :        Color_Type) is
   begin
      if Box.Text /= Color then
         Box.Text := Color;
         Redraw (Box, True);
      end if;
   end Text_Color;

   procedure Default_Color (Box : in out Colored_Combo_Box_Type) is
   begin
      if Enabled (Box) then
         Color (Box, White);
      else
         Color (Box, System_Color (COLOR_BTNFACE));
      end if;
   end Default_Color;

   function Color (Box   : Colored_Combo_Box_Type)
                   return Color_Type is
   begin
      return Box.Color;
   end Color;

   function Text_Color (Box   : Colored_Combo_Box_Type)
                        return Color_Type is
   begin
      return Box.Text;
   end Text_Color;

   function Color_Brush (Box   : Colored_Combo_Box_Type)
                         return GWindows.Types.Handle is
   begin
      return Handle (Box.Brush);
   end Color_Brush;

   procedure Initialize (Box : in out Colored_Drop_Down_Combo_Box_Type) is
   begin
      Create_Solid_Brush (Box.Brush, Box.Color);
      Initialize (Drop_Down_Combo_Box_Type (Box));
   end Initialize;

   procedure Color (Box   : in out Colored_Drop_Down_Combo_Box_Type;
                    Color :        Color_Type) is
   begin
      if Color /= Box.Color then
         Box.Color := Color;
         Delete (Box.Brush);
         Create_Solid_Brush (Box.Brush, Box.Color);
         Redraw (Box, True);
      end if;
   end Color;

   procedure Text_Color (Box   : in out Colored_Drop_Down_Combo_Box_Type;
                         Color :        Color_Type) is
   begin
      if Box.Text /= Color then
         Box.Text := Color;
         Redraw (Box, True);
      end if;
   end Text_Color;

   procedure Default_Color (Box : in out Colored_Drop_Down_Combo_Box_Type) is
   begin
      if Enabled (Box) then
         Color (Box, White);
      else
         Color (Box, System_Color (COLOR_BTNFACE));
      end if;
   end Default_Color;

   function Color (Box   : Colored_Drop_Down_Combo_Box_Type)
                   return Color_Type is
   begin
      return Box.Color;
   end Color;

   function Text_Color (Box   : Colored_Drop_Down_Combo_Box_Type)
                        return Color_Type is
   begin
      return Box.Text;
   end Text_Color;

   function Color_Brush (Box   : Colored_Drop_Down_Combo_Box_Type)
                         return GWindows.Types.Handle is
   begin
      return Handle (Box.Brush);
   end Color_Brush;

   procedure Initialize (Box : in out Colored_Check_Box_Type) is
   begin
      Box.Read_Only := False;
      Create_Solid_Brush (Box.Brush, Box.Color);
      Initialize (Check_Box_Type (Box));
   end Initialize;

   procedure Color (Box   : in out Colored_Check_Box_Type;
                    Color :        Color_Type) is
   begin
      if Color /= Box.Color then
         Box.Color := Color;
         Delete (Box.Brush);
         Create_Solid_Brush (Box.Brush, Box.Color);
         Redraw (Box, True);
      end if;
   end Color;

   procedure Default_Color (Box : in out Colored_Check_Box_Type) is
   begin
      Color (Box, System_Color (COLOR_BTNFACE));
   end Default_Color;

   function Color (Box : Colored_Check_Box_Type) return Color_Type is
   begin
      return Box.Color;
   end Color;

   function Color_Brush (Box : Colored_Check_Box_Type)
                         return GWindows.Types.Handle is
   begin
      return Handle (Box.Brush);
   end Color_Brush;

   procedure Read_Only (Box   : in out Colored_Check_Box_Type;
                        State :        Boolean := True) is
   begin
      Box.Read_Only := State;
      Default_Color (Box);
   end Read_Only;

   procedure On_Command
     (Box       : in out Colored_Check_Box_Type;
      Code      : in     Integer;
      ID        : in     Integer;
      Control   : in     Pointer_To_Base_Window_Class) is
      BN_CLICKED : constant := 0;
   begin
      if Code /= BN_CLICKED or else not Box.Read_Only then
         On_Command (Check_Box_Type (Box), Code, ID, Control);
      end if;
   end On_Command;

end GWindows.Colored_Control_Window;
