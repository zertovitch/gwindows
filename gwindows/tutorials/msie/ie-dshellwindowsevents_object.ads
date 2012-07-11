with GNATCOM.Dispinterface;

package IE.DShellWindowsEvents_Object is

   type DShellWindowsEvents_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure WindowRegistered
     (This    : DShellWindowsEvents_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  A new window was registered.

   procedure WindowRevoked
     (This    : DShellWindowsEvents_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  A new window was revoked.

end IE.DShellWindowsEvents_Object;

