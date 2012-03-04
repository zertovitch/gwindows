--  Owner Drawn Button GWindows Custom Control

with Interfaces.C;

with GWindows.Buttons;
with GWindows.Drawing;
with GWindows.Types;
with GWindows.Base;

package GWindows.Buttons.Owner_Drawn is

   -------------------------------------------------------------------------
   --  Owner_Drawn_Button_Type
   -------------------------------------------------------------------------

   type Owner_Drawn_Button_Type is
     new GWindows.Buttons.Button_Type with private;
   type Owner_Drawn_Button_Access is access all Owner_Drawn_Button_Type;
   type Pointer_To_Owner_Drawn_Button_Class is
     access all Owner_Drawn_Button_Type'Class;

   type Paint_Event is access
     procedure (Window   : in out GWindows.Base.Base_Window_Type'Class;
                Canvas   : in out GWindows.Drawing.Canvas_Type;
                Area     : in     GWindows.Types.Rectangle_Type;
                Selected : in     Boolean;
                Focused  : in     Boolean);

   -------------------------------------------------------------------------
   --  Owner_Drawn_Button_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Paint_Handler (Window  : in out Owner_Drawn_Button_Type;
                               Handler : in     Paint_Event);
   procedure Fire_On_Paint (Window   : in out Owner_Drawn_Button_Type;
                            Canvas   : in out GWindows.Drawing.Canvas_Type;
                            Area     : in     GWindows.Types.Rectangle_Type;
                            Selected : in     Boolean;
                            Focused  : in     Boolean);

   -------------------------------------------------------------------------
   --  Owner_Drawn_Button_Type - Events
   -------------------------------------------------------------------------

   procedure On_Paint (Window   : in out Owner_Drawn_Button_Type;
                       Canvas   : in out GWindows.Drawing.Canvas_Type;
                       Area     : in     GWindows.Types.Rectangle_Type;
                       Selected : in     Boolean;
                       Focused : in     Boolean);
   --  Called when application requests painting of the Button

   -------------------------------------------------------------------------
   --  Owner_Drawn_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------

   procedure On_Pre_Create (Window    : in out Owner_Drawn_Button_Type;
                            dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

   procedure On_Draw_Item
     (Window          : in out Owner_Drawn_Button_Type;
      Canvas          : in out GWindows.Drawing.Canvas_Type;
      Item_ID         : in     Integer;
      Item_Action     : in     Interfaces.C.unsigned;
      Item_State      : in     Interfaces.C.unsigned;
      Item_Rect       : in     GWindows.Types.Rectangle_Type;
      Item_Data       : in     Integer;
      Control         : in     GWindows.Base.Pointer_To_Base_Window_Class);

private
   type Owner_Drawn_Button_Type is new GWindows.Buttons.Button_Type with
      record
         On_Paint_Event : Paint_Event := null;
      end record;
end GWindows.Buttons.Owner_Drawn;
