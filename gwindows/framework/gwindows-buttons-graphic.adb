with GWindows.Types;

package body GWindows.Buttons.Graphic is

   use Interfaces.C;

   GWL_STYLE   : constant := -16;

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

   BS_ICON      : constant := 16#40#;
   BS_BITMAP    : constant := 16#80#;
   BS_FLAT      : constant := 16#8000#;
   BM_SETIMAGE  : constant := 247;
   IMAGE_BITMAP : constant := 0;
   IMAGE_ICON   : constant := 1;
   --  WS_BORDER   : constant := 8388608;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Icon_Button_Type) is
   begin
     SetWindowLong (Handle (Window),
                    newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_ICON or BS_FLAT);
   end On_Create;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon (
      Window : in out Icon_Button_Type;
      Icon   : GWindows.Drawing_Objects.Icon_Type
   )
   is
      use GWindows.Drawing_Objects;
      --  http://msdn.microsoft.com/en-us/library/bb761822(VS.85).aspx
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Window);
                             Umsg   : Interfaces.C.int      := BM_SETIMAGE;
                             Wparam : GWindows.Types.Wparam := IMAGE_ICON;
                             Lparam : GWindows.Types.Lparam :=
                               GWindows.Types.Lparam (
                                 GWindows.Types.To_Lresult (Handle (Icon)))
                            );
      pragma Import (Stdcall, Sendmessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Icon;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Bitmap_Button_Type) is
   begin
     SetWindowLong (Handle (Window),
                    newLong =>
                       GetWindowLong (Handle (Window)) or
                       BS_BITMAP or BS_FLAT);
   end On_Create;

   --------------
   -- Set_Bitmap --
   --------------

   procedure Set_Bitmap (
      Window : in out Bitmap_Button_Type;
      Bitmap : GWindows.Drawing_Objects.Bitmap_Type
   )
   is
      use GWindows.Drawing_Objects;
      --  http://msdn.microsoft.com/en-us/library/bb761822(VS.85).aspx
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Window);
                             Umsg   : Interfaces.C.int      := BM_SETIMAGE;
                             Wparam : GWindows.Types.Wparam := IMAGE_BITMAP;
                             Lparam : GWindows.Types.Lparam :=
                                GWindows.Types.Lparam (
                                   GWindows.Types.To_Lresult (Handle (Bitmap)))
                            );
      pragma Import (Stdcall, Sendmessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Bitmap;

end GWindows.Buttons.Graphic;
