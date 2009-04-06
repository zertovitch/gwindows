with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GWindows;              use GWindows;
with GWindows.ActiveX;      use GWindows.ActiveX;
with GWindows.Base;
with Interfaces.C;

package GWindows.Html is

   type Html_Type is new ActiveX_Type with private;

   type Action_Event is access procedure (Html : Html_Type'Class);

   procedure Create
               (Html          : in out Html_Type;
                Parent        : in out GWindows.Base.Base_Window_Type'Class;
                Top, Left     :        Natural;
                Width, Height :        Natural);

   procedure Accept_File_Drag_And_Drop (Html  : Html_Type;
                                        State : Boolean := True);

   procedure Url (Html : Html_Type;
                  Url  : GString);

   procedure On_Mouse_Activate_Handler (Html    : in out Html_Type;
                                        Handler :        Action_Event);

private

   type Html_Type is new ActiveX_Type with
      record
         Browser                   : Dispinterface_Type;
         On_Mouse_Activate_Handler : Action_Event;
      end record;

   procedure On_Message
     (Html         : in out Html_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     Interfaces.C.int;
      lParam       : in     Interfaces.C.int;
      Return_Value : in out Interfaces.C.long);

end GWindows.Html;
