with GNATCOM.Initialize;
with GNATCOM.IErrorInfo;

with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Databases.Controls;
use GWindows.Databases.Controls; use GWindows.Databases;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Edit_Boxes; use GWindows.Edit_Boxes;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.Base; use GWindows.Base;
with GWindows.Application;

procedure DB_View is
   Main_Window : Main_Window_Type;
   DB_Control  : Database_Control_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   --  Setup Controls
   Create (Main_Window, "Database Window", Width => 400, Height => 500);
   Keyboard_Support (Main_Window);
   Center (Main_Window);

   Create_As_Control (DB_Control, Main_Window, "", 0, 0, 0, 30);
   Dock (DB_Control, At_Top);

   --  Open Database
   Open (DB_Control.Database,
         "Provider=Microsoft.Jet.OLEDB.4.0; " &
         "Data Source=..\tutorials\adotest.mdb");

   Open (DB_Control.Recordset,
         DB_Control.Database,
         "SELECT * FROM People",
         Dynamic,
         Optimistic);

   --  Create labels, controls and bindings

   for N in 1 .. Field_Count (DB_Control.Recordset) loop
      Create_Label (Main_Window,
                    Field_Name (DB_Control.Recordset, N),
                    10, N * 40, 100, 25);
      declare
         Box  : constant Pointer_To_Edit_Box_Class := new Edit_Box_Type;
         Perm : Permissions_Type := Read_Write;
      begin
         Create (Box.all, Main_Window,
                 "", 120, N * 40, 200, 30, Is_Dynamic => True);

         if not Field_Updatable (DB_Control.Recordset, N) then
            Perm := Read_Only;
            Read_Only (Box.all);
         end if;

         Bind_Text_Control (DB_Control.Recordset,
                            Field_Name (DB_Control.Recordset, N),
                            Pointer_To_Base_Window_Class (Box),
                            Perm);
      end;
   end loop;

   --  Get Started
   Fill_Bindings (DB_Control.Recordset);
   Dock_Children (Main_Window);
   Visible (Main_Window, True);

   GWindows.Application.Message_Loop;
exception
   when others =>
      Message_Box ("Error",
                   To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
                   Icon => Error_Icon);
end DB_View;
