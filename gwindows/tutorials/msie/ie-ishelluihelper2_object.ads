with GNATCOM.Dispinterface;

package IE.IShellUIHelper2_Object is

   type IShellUIHelper2_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure ResetFirstBootMode
     (This : IShellUIHelper2_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper2_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper2_Type);

   procedure AddFavorite
     (This  : IShellUIHelper2_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure AddDesktopComponent
     (This   : IShellUIHelper2_Type;
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
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure NavigateAndFind
     (This           : IShellUIHelper2_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper2_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper2_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True);

   procedure AutoScan
     (This            : IShellUIHelper2_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper2_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True);

   function ShowBrowserUI
     (This     : IShellUIHelper2_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure AddSearchProvider
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure RunOnceShown
     (This : IShellUIHelper2_Type);

   procedure SkipRunOnce
     (This : IShellUIHelper2_Type);

   procedure CustomizeSettings
     (This       : IShellUIHelper2_Type;
      fSQM       : GNATCOM.Types.VARIANT;
      fPhishing  : GNATCOM.Types.VARIANT;
      bstrLocale : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);

   function SqmEnabled
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   function PhishingEnabled
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   function BrandImageUri
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   procedure SkipTabsWelcome
     (This : IShellUIHelper2_Type);

   procedure DiagnoseConnection
     (This : IShellUIHelper2_Type);

   procedure CustomizeClearType
     (This : IShellUIHelper2_Type;
      fSet : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function IsSearchProviderInstalled
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function IsSearchMigrated
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   function DefaultSearchProvider
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper2_Type;
      fComplete : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);

   function RunOnceHasShown
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

   function SearchGuideUrl
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT;

end IE.IShellUIHelper2_Object;

