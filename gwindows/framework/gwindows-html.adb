with GNATCOM.Initialize;
with GNATCOM.VARIANT;                   use GNATCOM.VARIANT;
with GWindows.GStrings;                 use GWindows.GStrings;

package body GWindows.Html is

   procedure Create
               (Html          : in out Html_Type;
                Parent        : in out GWindows.Base.Base_Window_Type'Class;
                Top, Left     :        Natural;
                Width, Height :        Natural) is
   begin
      GNATCOM.Initialize.Initialize_COM;
      Create (ActiveX_Type (Html), Parent, "Shell.Explorer",
              Left, Top, Width, Height);
      Query (Html.Browser, Interfac (Html));
   end Create;

   procedure Accept_File_Drag_And_Drop (Html  : Html_Type;
                                        State : Boolean := True) is
   begin
      Put (Html.Browser, "RegisterAsDropTarget", To_VARIANT (State));
   end Accept_File_Drag_And_Drop;

   procedure Url (Html : Html_Type;
                  Url  : GString) is
   begin
      if Url /= "" then
         Invoke (Html.Browser, "navigate",
                 (1 => To_VARIANT_From_GString (Url)));
      end if;
   end Url;

   procedure On_Mouse_Activate_Handler (Html    : in out Html_Type;
                                        Handler :        Action_Event) is
   begin
      Html.On_Mouse_Activate_Handler := Handler;
   end On_Mouse_Activate_Handler;

   procedure On_Message
     (Html         : in out Html_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     Interfaces.C.int;
      lParam       : in     Interfaces.C.int;
      Return_Value : in out Interfaces.C.long) is
      use Interfaces.C;
      WM_MOUSEACTIVATE : constant := 33;
   begin
      if message = WM_MOUSEACTIVATE then
         if Html.On_Mouse_Activate_Handler /= null then
            Html.On_Mouse_Activate_Handler (Html);
         end if;
      end if;
      On_Message (ActiveX_Type (Html), message, wParam, lParam, Return_Value);
   end On_Message;

end GWindows.Html;
