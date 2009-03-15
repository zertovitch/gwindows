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

procedure Tutorial19 is
   Main_Window : Main_Window_Type;
   ID          : aliased Edit_Box_Type;
   LastName    : aliased Edit_Box_Type;
   FirstName   : aliased Edit_Box_Type;
   Address     : aliased Edit_Box_Type;
   City        : aliased Edit_Box_Type;
   State       : aliased Edit_Box_Type;
   Zip         : aliased Edit_Box_Type;
   Country     : aliased Edit_Box_Type;
   DB_Control  : Database_Control_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   --  Setup Controls
   Create (Main_Window, "Database Window", Width => 400, Height => 500);
   Keyboard_Support (Main_Window);
   Center (Main_Window);

   Create_As_Control (DB_Control, Main_Window, "", 0, 0, 0, 30);
   Dock (DB_Control, At_Top);

   Create_Label (Main_Window, "ID", 10, 40, 100, 25);
   Create_Label (Main_Window, "Last Name", 10, 80, 100, 25);
   Create_Label (Main_Window, "First Name", 10, 120, 100, 25);
   Create_Label (Main_Window, "Address", 10, 160, 100, 25);
   Create_Label (Main_Window, "City", 10, 200, 100, 25);
   Create_Label (Main_Window, "State", 10, 240, 100, 25);
   Create_Label (Main_Window, "Zip", 10, 280, 100, 25);
   Create_Label (Main_Window, "Country", 10, 320, 100, 25);

   Create (ID, Main_Window, "",
           120, 40, 200, 30);
   Read_Only (ID);
   Create (LastName, Main_Window, "",
           120, 80, 200, 30);
   Create (FirstName, Main_Window, "",
           120, 120, 200, 30);
   Create (Address, Main_Window, "",
           120, 160, 200, 30);
   Create (City, Main_Window, "",
           120, 200, 200, 30);
   Create (State, Main_Window, "",
           120, 240, 200, 30);
   Create (Zip, Main_Window, "",
           120, 280, 200, 30);
   Create (Country, Main_Window, "",
           120, 320, 200, 30);

   --  Setup Database and Bindings

   Open (DB_Control.Database,
         "Provider=Microsoft.Jet.OLEDB.4.0; " &
         "Data Source=adotest.mdb");

   Open (DB_Control.Recordset,
         DB_Control.Database,
         "SELECT * FROM People",
         Dynamic,
         Optimistic);

   Bind_Text_Control (DB_Control.Recordset,
                      "ID",
                      ID'Unchecked_Access,
                      Read_Only);
   Bind_Text_Control (DB_Control.Recordset,
                      "LastName",
                      LastName'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "FirstName",
                      FirstName'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "Address",
                      Address'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "City",
                      City'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "State",
                      State'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "Zip",
                      Zip'Unchecked_Access);
   Bind_Text_Control (DB_Control.Recordset,
                      "Country",
                      Country'Unchecked_Access);

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
end Tutorial19;
