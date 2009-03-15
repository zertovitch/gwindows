with GNATCOM.Initialize;
with GNATCOM.Types;
with GNATCOM.BSTR;
with AdaToVB.UAdaToVBClass_Interface; use AdaToVB.UAdaToVBClass_Interface;

procedure Client is
   Object : UAdaToVBClass_Type;
   Msg    : aliased GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR ("Test");
begin
   GNATCOM.Initialize.Initialize_COM;

   Create (Object, AdaToVB.CLSID_AdaToVBClass);

   Display (Object, Msg'Unchecked_Access);
   GNATCOM.BSTR.Free (Msg);
end Client;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.1
--  date: 2001/03/30 15:03:54;  author: fofanov;  state: Exp;
--  Initial version
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
