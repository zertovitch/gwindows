with GWindows.Edit_Boxes;       use GWindows.Edit_Boxes;
with Interfaces.C;              use Interfaces.C;
with GWindows.Types;

package GWindows.Multi_Line_Edit_Without_Tab is

   type Multi_Line_Edit_Without_Tab_Type is new Multi_Line_Edit_Box_Type
   with null record;
   type Multi_Line_Edit_Without_Tab_Access is access
      all Multi_Line_Edit_Without_Tab_Type;

   overriding
   procedure On_Message
      (Window       : in out Multi_Line_Edit_Without_Tab_Type;
       message      : Interfaces.C.unsigned;
       wParam       : GWindows.Types.Wparam;
       lParam       : GWindows.Types.Lparam;
       Return_Value : in out GWindows.Types.Lresult);

end GWindows.Multi_Line_Edit_Without_Tab;
