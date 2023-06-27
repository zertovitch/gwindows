with GNATCOM.Initialize;
with GNATCOM.VARIANT;
with GWindows.GStrings;

package body GWindows.Html is

   procedure Create
               (Html          : in out Html_Type;
                Parent        : in out GWindows.Base.Base_Window_Type'Class;
                Left, Top     :        Natural;
                Width, Height :        Natural) is
   begin
      GNATCOM.Initialize.Initialize_COM;
      ActiveX.Create
         (ActiveX.ActiveX_Type (Html), Parent, "Shell.Explorer",
          Left, Top, Width, Height);
      GNATCOM.Dispinterface.Query (Html.Browser, Interfac (Html));
      Html.FileName := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Create;

   procedure Create
     (Html          : in out Html_Type;
      Parent        : in out GWindows.Base.Base_Window_Type'Class;
      Left, Top     :        Natural;
      Width, Height :        Natural;
      FileName      : in     Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Create (Html, Parent, Left, Top, Width, Height);
      Html.FileName := FileName;
   end Create;

   procedure Accept_File_Drag_And_Drop (Html  : Html_Type;
                                        State : Boolean := True) is
   begin
      GNATCOM.Dispinterface.Put
         (Html.Browser,
          "RegisterAsDropTarget",
          GNATCOM.VARIANT.To_VARIANT (State));
   end Accept_File_Drag_And_Drop;

   procedure Url (Html : Html_Type;
                  Url  : GString) is
   begin
      if Url /= "" then
         GNATCOM.Dispinterface.Invoke
            (Html.Browser, "navigate",
                (1 => GStrings.To_VARIANT_From_GString (Url)));
      end if;
   end Url;

   procedure On_Mouse_Activate_Handler (Html    : in out Html_Type;
                                        Handler :        Action_Event) is
   begin
      Html.On_Mouse_Activate_Handler := Handler;
   end On_Mouse_Activate_Handler;

   procedure On_Message
     (Html         : in out Html_Type;
      message      : Interfaces.C.unsigned;
      wParam       : GWindows.Types.Wparam;
      lParam       : GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
   is
      use Interfaces.C;
      WM_MOUSEACTIVATE : constant := 33;
   begin
      if message = WM_MOUSEACTIVATE then
         if Html.On_Mouse_Activate_Handler /= null then
            Html.On_Mouse_Activate_Handler (Html);
         end if;
      end if;
      ActiveX.On_Message
         (ActiveX.ActiveX_Type (Html), message, wParam, lParam, Return_Value);
   end On_Message;

   procedure GoBack
     (Html : in out Html_Type)
   is
   begin
      GNATCOM.Dispinterface.Invoke (Html.Browser, "GoBack");
   exception when others =>  --  Exception generated when back at top.
         null;
   end GoBack;

   function FileName (Html : in Html_Type) return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Html.FileName);
   end FileName;

end GWindows.Html;
