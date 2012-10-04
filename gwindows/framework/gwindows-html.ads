with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GWindows;              use GWindows;
with GWindows.ActiveX;      use GWindows.ActiveX;
with GWindows.Base;
with Interfaces.C;
with GWindows.Types;

package GWindows.Html is

   type Html_Type is new ActiveX_Type with private;

   type Action_Event is access procedure (Html : Html_Type'Class);

   procedure Create
               (Html          : in out Html_Type;
                Parent        : in out GWindows.Base.Base_Window_Type'Class;
                Left, Top     :        Natural;
                Width, Height :        Natural);

   procedure Create
     (Html          : in out Html_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left, Top     :        Natural;
      Width, Height :        Natural;
      FileName      : in     Unbounded_String);

   procedure Accept_File_Drag_And_Drop (Html  : Html_Type;
                                        State : Boolean := True);

   procedure Url (Html : Html_Type;
                  Url  : GString);

   procedure On_Mouse_Activate_Handler (Html    : in out Html_Type;
                                        Handler :        Action_Event);

   procedure GoBack (Html : in out Html_Type);

   function FileName (Html : in Html_Type) return String;

private

   type Html_Type is new ActiveX_Type with
      record
         Browser                   : Dispinterface_Type;
         On_Mouse_Activate_Handler : Action_Event;
         FileName                  : Unbounded_String;
      end record;

   procedure On_Message
     (Html         : in out Html_Type;
      message      : Interfaces.C.unsigned;
      wParam       : GWindows.Types.Wparam;
      lParam       : GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

end GWindows.Html;
