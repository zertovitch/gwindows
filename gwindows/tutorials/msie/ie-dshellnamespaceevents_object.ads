with GNATCOM.Dispinterface;

package IE.DShellNameSpaceEvents_Object is

   type DShellNameSpaceEvents_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure FavoritesSelectionChange
     (This              : DShellNameSpaceEvents_Type;
      cItems            : GNATCOM.Types.VARIANT;
      hItem             : GNATCOM.Types.VARIANT;
      strName           : GNATCOM.Types.VARIANT;
      strUrl            : GNATCOM.Types.VARIANT;
      cVisits           : GNATCOM.Types.VARIANT;
      strDate           : GNATCOM.Types.VARIANT;
      fAvailableOffline : GNATCOM.Types.VARIANT;
      Free              : Boolean := True);

   procedure SelectionChange
     (This : DShellNameSpaceEvents_Type);

   procedure DoubleClick
     (This : DShellNameSpaceEvents_Type);

   procedure Initialized
     (This : DShellNameSpaceEvents_Type);

end IE.DShellNameSpaceEvents_Object;

