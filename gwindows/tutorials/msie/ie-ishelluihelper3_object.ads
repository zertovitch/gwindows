with GNATCOM.Dispinterface;

package IE.IShellUIHelper3_Object is

   type IShellUIHelper3_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure ResetFirstBootMode
     (This : IShellUIHelper3_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper3_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper3_Type);

   procedure AddFavorite
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure AddDesktopComponent
     (This   : IShellUIHelper3_Type;
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
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure NavigateAndFind
     (This           : IShellUIHelper3_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper3_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper3_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True);

   procedure AutoScan
     (This            : IShellUIHelper3_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper3_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True);

   function ShowBrowserUI
     (This     : IShellUIHelper3_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure AddSearchProvider
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure RunOnceShown
     (This : IShellUIHelper3_Type);

   procedure SkipRunOnce
     (This : IShellUIHelper3_Type);

   procedure CustomizeSettings
     (This       : IShellUIHelper3_Type;
      fSQM       : GNATCOM.Types.VARIANT;
      fPhishing  : GNATCOM.Types.VARIANT;
      bstrLocale : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);

   function SqmEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   function PhishingEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   function BrandImageUri
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   procedure SkipTabsWelcome
     (This : IShellUIHelper3_Type);

   procedure DiagnoseConnection
     (This : IShellUIHelper3_Type);

   procedure CustomizeClearType
     (This : IShellUIHelper3_Type;
      fSet : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function IsSearchProviderInstalled
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function IsSearchMigrated
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   function DefaultSearchProvider
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper3_Type;
      fComplete : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);

   function RunOnceHasShown
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   function SearchGuideUrl
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddService
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function IsServiceInstalled
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Verb : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function InPrivateFilteringEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddToFavoritesBar
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT;
      uType : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);

   procedure BuildNewTabPage
     (This : IShellUIHelper3_Type);

   procedure SetRecentlyClosedVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   procedure SetActivitiesVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   procedure ContentDiscoveryReset
     (This : IShellUIHelper3_Type);

   function IsSuggestedSitesEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT;

   procedure EnableSuggestedSites
     (This    : IShellUIHelper3_Type;
      fEnable : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);

   procedure NavigateToSuggestedSites
     (This            : IShellUIHelper3_Type;
      bstrRelativeUrl : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);

   procedure ShowTabsHelp
     (This : IShellUIHelper3_Type);

   procedure ShowInPrivateHelp
     (This : IShellUIHelper3_Type);

end IE.IShellUIHelper3_Object;

