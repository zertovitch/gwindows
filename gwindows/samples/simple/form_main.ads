with GWindows.Windows;
with GWindows.Edit_Boxes;

package Form_Main is

   type Form_Main_Type is
     new GWindows.Windows.Window_Type with
      record
         Last_Name  : GWindows.Edit_Boxes.Edit_Box_Type;
         First_Name : GWindows.Edit_Boxes.Edit_Box_Type;
         Address    : GWindows.Edit_Boxes.Edit_Box_Type;
         City       : GWindows.Edit_Boxes.Edit_Box_Type;
         State      : GWindows.Edit_Boxes.Edit_Box_Type;
         Zip        : GWindows.Edit_Boxes.Edit_Box_Type;
         Country    : GWindows.Edit_Boxes.Edit_Box_Type;
      end record;

   type Form_Main_Access is access all Form_Main_Type;

   procedure On_Create (Window : in out Form_Main_Type);
   --  Handles creating window

   procedure On_Menu_Select
     (Window : in out Form_Main_Type;
      Item   : in     Integer);
   --  Handles menu selections

end Form_Main;
