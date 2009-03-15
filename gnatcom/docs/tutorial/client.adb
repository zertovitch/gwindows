with Ada.Exceptions;
with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.BSTR;

with GNATClient.IGNATMessage_Interface; use GNATClient.IGNATMessage_Interface;
with GNATClient.IGNATStat_Interface; use GNATClient.IGNATStat_Interface;

procedure Client is
   Messages : IGNATMessage_Type;
   Stats    : IGNATStat_Type;
begin
   GNATCOM.Initialize.Initialize_COM;

   Create (Messages, GNATClient.CLSID_GNATCOMClass);

   Beep (Messages);

   MessageBox (Messages, GNATCOM.BSTR.To_BSTR ("Hello World!"));

   Query (Stats, Messages);

   Put_Line ("IGNATMessage methods called" &
             Integer (Calls (Stats))'Img & " times.");
exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end Client;
