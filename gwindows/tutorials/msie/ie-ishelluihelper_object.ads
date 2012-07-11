with GNATCOM.Dispinterface;

package IE.IShellUIHelper_Object is

   type IShellUIHelper_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure ResetFirstBootMode
     (This : IShellUIHelper_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper_Type);

   procedure AddFavorite
     (This  : IShellUIHelper_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure AddDesktopComponent
     (This   : IShellUIHelper_Type;
      URL    : GNATCOM.Types.VARIANT;
      uType  : GNATCOM.Types.VARIANT;
      Left   : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Top    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Width  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Height : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True);

   function IsSubscribed
     (This : IShellUIHelper_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure NavigateAndFind
     (This           : IShellUIHelper_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True);

   procedure AutoScan
     (This            : IShellUIHelper_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True);

   function ShowBrowserUI
     (This     : IShellUIHelper_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

end IE.IShellUIHelper_Object;

