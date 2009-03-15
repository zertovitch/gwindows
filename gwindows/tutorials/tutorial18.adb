with GNATCOM.Initialize;
with GWindows.GStrings.IO; use GWindows.GStrings.IO;
with GWindows.Databases; use GWindows.Databases;

procedure Tutorial18 is
   Connection : Database_Type;
   Recordset  : Recordset_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   Open (Connection,
         "Provider=Microsoft.Jet.OLEDB.4.0; " &
         "Data Source=adotest.mdb");

   Open (Recordset,
         Connection,
         "SELECT * FROM People",
         Dynamic,
         Optimistic);

   while not EOF (Recordset) loop
      for N in 1 .. Field_Count (Recordset) loop
         Put_Line
           (Field_Name (Recordset, N) & " = " & Field_Value (Recordset, N));
      end loop;

      if Field_Value (Recordset, "LastName") = "TestLName" then
         Put_Line ("This record is being deleted");
         Delete (Recordset);
      end if;
      Move_Next (Recordset);
      New_Line;
   end loop;

   Add_New (Recordset);
   Field_Value (Recordset, "LastName", "TestLName");
   Field_Value (Recordset, "FirstName", "TestLName");
   Update (Recordset);

   Close (Recordset);
   Close (Connection);
end Tutorial18;
