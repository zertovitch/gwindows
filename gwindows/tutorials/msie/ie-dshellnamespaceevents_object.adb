package body IE.DShellNameSpaceEvents_Object is

   procedure FavoritesSelectionChange
     (This              : DShellNameSpaceEvents_Type;
      cItems            : GNATCOM.Types.VARIANT;
      hItem             : GNATCOM.Types.VARIANT;
      strName           : GNATCOM.Types.VARIANT;
      strUrl            : GNATCOM.Types.VARIANT;
      cVisits           : GNATCOM.Types.VARIANT;
      strDate           : GNATCOM.Types.VARIANT;
      fAvailableOffline : GNATCOM.Types.VARIANT;
      Free              : Boolean := True)
   is
   begin
      Invoke
        (This,
         DShellNameSpaceEvents_FavoritesSelectionChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fAvailableOffline,
          2 => strDate,
          3 => cVisits,
          4 => strUrl,
          5 => strName,
          6 => hItem,
          7 => cItems),
         Free);
   end FavoritesSelectionChange;

   procedure SelectionChange
     (This : DShellNameSpaceEvents_Type)
   is
   begin
      Invoke (This, DShellNameSpaceEvents_SelectionChange);
   end SelectionChange;

   procedure DoubleClick
     (This : DShellNameSpaceEvents_Type)
   is
   begin
      Invoke (This, DShellNameSpaceEvents_DoubleClick);
   end DoubleClick;

   procedure Initialized
     (This : DShellNameSpaceEvents_Type)
   is
   begin
      Invoke (This, DShellNameSpaceEvents_Initialized);
   end Initialized;

end IE.DShellNameSpaceEvents_Object;

