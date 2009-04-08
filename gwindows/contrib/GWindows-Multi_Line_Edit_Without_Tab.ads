with GWindows.Edit_Boxes;       use GWindows.Edit_Boxes;
with Interfaces.C;              use Interfaces.C;

package GWindows.Multi_Line_Edit_Without_Tab is

   type Multi_Line_Edit_Without_Tab_Type is new Multi_Line_Edit_Box_Type
   with null record;
   type Multi_Line_Edit_Without_Tab_Access is access
      all Multi_Line_Edit_Without_Tab_Type;

   procedure On_Message
      (Window       : in out Multi_Line_Edit_Without_Tab_Type;
       message      : in     Interfaces.C.unsigned;
       wParam       : in     Interfaces.C.int;
       lParam       : in     Interfaces.C.int;
       Return_Value : in out Interfaces.C.long);

end GWindows.Multi_Line_Edit_Without_Tab;
