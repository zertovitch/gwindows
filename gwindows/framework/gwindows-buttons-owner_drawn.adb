package body GWindows.Buttons.Owner_Drawn is

   ------------------
   -- On_Draw_Item --
   ------------------

   procedure On_Draw_Item
     (Window          : in out Owner_Drawn_Button_Type;
      Canvas          : in out GWindows.Drawing.Canvas_Type;
      Item_ID         : in     Integer;
      Item_Action     : in     Interfaces.C.unsigned;
      Item_State      : in     Interfaces.C.unsigned;
      Item_Rect       : in     GWindows.Types.Rectangle_Type;
      Item_Data       : in     Integer;
      Control         : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
   pragma Unreferenced (Control, Item_Data, Item_Action, Item_ID);
      use type Interfaces.C.unsigned;

      ODS_SELECTED               : constant := 1;
      --  ODS_GRAYED                 : constant := 2;
      --  ODS_DISABLED               : constant := 4;
      --  ODS_CHECKED                : constant := 8;
      ODS_FOCUS                  : constant := 16;
      --  ODS_DEFAULT                : constant := 32;
   begin
      On_Paint (Owner_Drawn_Button_Type'Class (Window),
                Canvas,
                Item_Rect,
                (Item_State and ODS_SELECTED) = ODS_SELECTED,
                (Item_State and ODS_FOCUS) = ODS_FOCUS);
   end On_Draw_Item;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint
     (Window   : in out Owner_Drawn_Button_Type;
      Canvas   : in out GWindows.Drawing.Canvas_Type;
      Area     : in     GWindows.Types.Rectangle_Type;
      Selected : in     Boolean;
      Focused  : in     Boolean)
   is
   begin
      Fire_On_Paint (Window, Canvas, Area, Selected, Focused);
   end On_Paint;

   ----------------------
   -- On_Paint_Handler --
   ----------------------

   procedure On_Paint_Handler
     (Window  : in out Owner_Drawn_Button_Type;
      Handler : in     Paint_Event)
   is
   begin
      Window.On_Paint_Event := Handler;
   end On_Paint_Handler;

   -------------------
   -- Fire_On_Paint --
   -------------------

   procedure Fire_On_Paint
     (Window   : in out Owner_Drawn_Button_Type;
      Canvas   : in out GWindows.Drawing.Canvas_Type;
      Area     : in     GWindows.Types.Rectangle_Type;
      Selected : in     Boolean;
      Focused  : in     Boolean)
   is
      use GWindows.Base;
   begin
      if Window.On_Paint_Event /= null then
         Window.On_Paint_Event (Base_Window_Type'Class (Window),
                                Canvas,
                                Area,
                                Selected,
                                Focused);
      end if;
   end Fire_On_Paint;

   -------------------
   -- On_Pre_Create --
   -------------------

   procedure On_Pre_Create
     (Window    : in out Owner_Drawn_Button_Type;
      dwStyle   : in out Interfaces.C.unsigned;
      dwExStyle : in out Interfaces.C.unsigned)
   is
   pragma Unreferenced (dwExStyle, Window);
      use type Interfaces.C.unsigned;

      BS_OWNERDRAW : constant := 11;
   begin
      dwStyle := dwStyle or BS_OWNERDRAW;
   end On_Pre_Create;

end GWindows.Buttons.Owner_Drawn;
