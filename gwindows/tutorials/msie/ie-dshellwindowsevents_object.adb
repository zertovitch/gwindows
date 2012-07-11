package body IE.DShellWindowsEvents_Object is

   procedure WindowRegistered
     (This    : DShellWindowsEvents_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DShellWindowsEvents_WindowRegistered,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => lCookie),
         Free);
   end WindowRegistered;

   procedure WindowRevoked
     (This    : DShellWindowsEvents_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DShellWindowsEvents_WindowRevoked,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => lCookie),
         Free);
   end WindowRevoked;

end IE.DShellWindowsEvents_Object;

