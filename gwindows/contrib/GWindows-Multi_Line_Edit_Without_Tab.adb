with GWindows.Base;              use GWindows.Base;

package body GWindows.Multi_Line_Edit_Without_Tab is

   VK_TAB      : constant := 9;
   VK_SHIFT    : constant := 16;
   WM_KEYDOWN  : constant := 256;

   function GetAsyncKeyState (vKey : Interfaces.C.int) return
      Interfaces.C.short;
   pragma Import (Stdcall, GetAsyncKeyState, "GetAsyncKeyState");

   procedure On_Message
      (Window       : in out Multi_Line_Edit_Without_Tab_Type;
       message      : Interfaces.C.unsigned;
       wParam       : GWindows.Types.Wparam;
       lParam       : GWindows.Types.Lparam;
       Return_Value : in out GWindows.Types.Lresult) is
      use GWindows.Types;

      Parent_Window : Pointer_To_Base_Window_Class := Parent (Window);
      Next_Window : Pointer_To_Base_Window_Class;
   begin
      if message = WM_KEYDOWN and then
         wParam = VK_TAB
      then
         if GetAsyncKeyState (VK_SHIFT) < 0 then
            Next_Window := Previous_Tab_Stop (Parent_Window.all, Window);
            if Next_Window /= null then
               Focus (Next_Window.all);
            end if;
         else
            Next_Window := Next_Tab_Stop (Parent_Window.all, Window);
            if Next_Window /= null then
               Focus (Next_Window.all);
            end if;
         end if;
         Return_Value := 0;
      else
         On_Message (Base_Window_Type (Window),
                     message,
                     wParam,
                     lParam,
                     Return_Value);
      end if;
   end On_Message;

end GWindows.Multi_Line_Edit_Without_Tab;
