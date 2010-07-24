with Ada.Strings.Wide_Fixed;    use Ada.Strings.Wide_Fixed;
with Interfaces.C;              use Interfaces.C;

package body GWindows.Colored_Button is

   GWL_STYLE : constant := -16;
   BS_OWNERDRAW : constant := 11;

   Pen_Black : Pen_Type;
   Pen_White : Pen_Type;
   Pen_Gray : Pen_Type;

   procedure Put_Text_Formatted
      (Canvas : in out GWindows.Drawing.Canvas_Type;
       Text : GString;
       Area : Rectangle_Type) is
      Height : Integer :=
         Integer (Float (Text_Output_Size (Canvas, "Test").Height) * 0.95);
      Lines : Integer := Integer ((Area.Bottom - Area.Top + 1) / Height);
      Breaks : array (1 .. Lines) of Integer;
      Next_Space : Integer;
      Total_Break : Integer := 0;
      Start : Integer := Text'First;
      Width : Integer;
      Offset : Integer;
   begin
      while Total_Break < Lines and Start <= Text'Last loop
         Next_Space := Index (Text (Start .. Text'Last), " ");
         exit when Next_Space = 0;
         Total_Break := Total_Break + 1;
         Breaks (Total_Break) := Next_Space;
         loop
            Next_Space := Index (Text (Next_Space + 1 .. Text'Last), " ");
            if Next_Space = 0 then
               Next_Space := Text'Last + 1;
            end if;
            exit when
               Text_Output_Size
                  (Canvas, Text (Start .. Next_Space - 1)).Width >
                  (Area.Right - Area.Left + 1);
            Breaks (Total_Break) := Next_Space;
            exit when Next_Space > Text'Last;
         end loop;
         Start := Breaks (Total_Break) + 1;
      end loop;
      if Total_Break < Lines and Start <= Text'Last then
         Total_Break := Total_Break + 1;
         Breaks (Total_Break) := Text'Last + 1;
      end if;
      Offset := (Area.Bottom - Area.Top + 1 -
                    Total_Break * Height) / 2;
      Start := Text'First;
      for I in 1 .. Total_Break loop
         Width := Text_Output_Size (Canvas,
                                    Text (Start .. Breaks (I) - 1)).Width;
         Put (Canvas,
              Area.Left + (Area.Right - Area.Left + 1) / 2 - Width / 2,
              Offset + Area.Top + (I - 1) * Height,
              Text (Start .. Breaks (I) - 1),
              Area);
         Start := Breaks (I) + 1;
      end loop;
   end Put_Text_Formatted;

   procedure On_Draw_Item
      (Window          : in out Colored_Button_Type;
       Canvas          : in out GWindows.Drawing.Canvas_Type;
       Item_ID         : in     Integer;
       Item_Action     : in     Interfaces.C.unsigned;
       Item_State      : in     Interfaces.C.unsigned;
       Item_Rect       : in     GWindows.Types.Rectangle_Type;
       Item_Data       : in     Integer;
       Control         : in     Pointer_To_Base_Window_Class) is
      pragma Warnings (Off, Item_ID);
      pragma Warnings (Off, Item_Action);
      pragma Warnings (Off, Item_State);
      pragma Warnings (Off, Item_Rect);
      pragma Warnings (Off, Item_Data);
      pragma Warnings (Off, Control);

      W : Integer := Width (Window);
      H : Integer := Height (Window);
      Font : Font_Type;
   begin
      Get_Font (Window, Font);
      Select_Object (Canvas, Window.Brush);
      Select_Object (Canvas, Pen_Black);
      Rectangle (Canvas, 0, 0, W, H);
      Select_Object (Canvas, Pen_White);
      Line (Canvas, 0, H - 2, 0, 0);
      Line (Canvas, 0, 0, W - 1, 0);
      Select_Object (Canvas, Pen_Gray);
      Line (Canvas, 1, H - 2, W - 2, H - 2);
      Line (Canvas, W - 2, H - 2, W - 2, 0);
      Background_Mode (Canvas, Transparent);
      Text_Color (Canvas, Window.Text);
      Select_Object (Canvas, Font);
      Put_Text_Formatted (Canvas, Text (Window), (2, 2, W - 2, H - 2));
      Handle (Font, GWindows.Types.Null_Handle);
   end On_Draw_Item;

   procedure SetWindowLong
      (hwnd    : GWindows.Types.Handle;
       nIndex  : Interfaces.C.int := GWL_STYLE;
       newLong : Interfaces.C.unsigned);
   pragma Import (StdCall, SetWindowLong,
                  "SetWindowLong" & Character_Mode_Identifier);

   function GetWindowLong
      (hwnd   : GWindows.Types.Handle;
       nIndex : Interfaces.C.int := GWL_STYLE)
    return Interfaces.C.unsigned;
   pragma Import (StdCall, GetWindowLong,
                  "GetWindowLong" & Character_Mode_Identifier);

   procedure On_Create (Button : in out Colored_Button_Type) is
   begin
      Create_Solid_Brush (Button.Brush, Button.Color);
      SetWindowLong (Handle (Button),
                     newLong =>
                        GetWindowLong (Handle (Button)) or BS_OWNERDRAW);
   end On_Create;

   procedure Color (Button : in out Colored_Button_Type;
                    Color : Color_Type) is
   begin
      if Color /= Button.Color then
         Button.Color := Color;
         Create_Solid_Brush (Button.Brush, Button.Color);
         Redraw (Button, Erase => True, Redraw_Now => True);
      end if;
   end Color;

   procedure Text_Color (Button : in out Colored_Button_Type;
                         Color : Color_Type) is
   begin
      if Color /= Button.Text then
         Button.Text := Color;
         Redraw (Button, Erase => True, Redraw_Now => True);
      end if;
   end Text_Color;

begin
   Create_Pen (Pen_Black, Solid, 1, To_Color (0, 0, 0));
   Create_Pen (Pen_White, Solid, 1, To_Color (255, 255, 255));
   Create_Pen (Pen_Gray, Solid, 1, To_Color (127, 127, 127));
end GWindows.Colored_Button;
