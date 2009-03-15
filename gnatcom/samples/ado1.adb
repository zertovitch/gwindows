with Ada.Exceptions;
with GNAT.OS_Lib;
with GNAT.IO; use GNAT.IO;

with GNATCOM.Types;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.Initialize;

with ADO.uConnection_Interface; use ADO.uConnection_Interface;
with ADO.uCommand_Interface; use ADO.uCommand_Interface;
with ADO.uRecordset_Interface; use ADO.uRecordset_Interface;
with ADO.Fields_Interface; use ADO.Fields_Interface;
with ADO.Field_Interface; use ADO.Field_Interface;

procedure ADO1 is
   use type GNATCOM.Types.VARIANT_BOOL;

   Connection : uConnection_Type;
   Command    : uCommand_Type;
   Recordset  : uRecordset_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   Put_Line ("Create ADO Engine");
   Create (Connection, ADO.CLSID_Connection);

   Put_Line ("ADO Version is : " & To_Ada (Get_Version (Connection)));
   New_Line;
   Put_Line ("Open Connection to Database");

   Put_ConnectionString
     (Connection,
      To_BSTR ("Provider=Microsoft.Jet.OLEDB.4.0; " &
               "Data Source=res\adotest.mdb"));

   Open (Connection, null, null, null, 0);

   Create (Command, ADO.CLSID_Command);
   PutRef_ActiveConnection (Command, Pointer (Connection));

   Put_CommandText (Command, To_BSTR ("SELECT * FROM People"));

   Create (Recordset, ADO.CLSID_Recordset);

   Open (Recordset,
         Source     => To_VARIANT_From_Dispinterface (Command),
         CursorType => ADO.AdOpenForwardOnly,
         LockType   => ADO.AdLockReadOnly,
         Options    => 0,
         Free       => False);

   MoveFirst (Recordset);

   while Get_EOF (Recordset) /= GNATCOM.Types.VARIANT_BOOL_TRUE loop
      declare
         Fields : Fields_Type;
      begin
         Attach (Fields, Get_Fields (Recordset));

         for N in 0 .. Integer (Get_Count (Fields)) - 1 loop
            declare
               Field : Field_Type;
            begin
               Attach (Field, Get_Item (Fields, To_VARIANT (N)));
               Put_Line (To_Ada (Get_Name (Field)) & " = " &
                         To_Ada (Get_Value (Field)));
            end;
         end loop;
      end;
      MoveNext (Recordset);
      New_Line;
   end loop;

   Put_Line ("Close Connections");
   Close (Recordset);
   Close (Connection);

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      GNAT.OS_Lib.OS_Exit (1);
end ADO1;

