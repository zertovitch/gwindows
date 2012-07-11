package body IE.IShellUIHelper3_Object is

   procedure ResetFirstBootMode
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_ResetFirstBootMode);
   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_ResetSafeMode);
   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_RefreshOfflineDesktop);
   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddFavorite,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Title,
          2 => URL),
         Free);
   end AddFavorite;

   procedure AddChannel
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddChannel,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddChannel;

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
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddDesktopComponent,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Height,
          2 => Width,
          3 => Top,
          4 => Left,
          5 => uType,
          6 => URL),
         Free);
   end AddDesktopComponent;

   function IsSubscribed
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper3_IsSubscribed,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end IsSubscribed;

   procedure NavigateAndFind
     (This           : IShellUIHelper3_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_NavigateAndFind,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => varTargetFrame,
          2 => strQuery,
          3 => URL),
         Free);
   end NavigateAndFind;

   procedure ImportExportFavorites
     (This          : IShellUIHelper3_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_ImportExportFavorites,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => strImpExpPath,
          2 => fImport),
         Free);
   end ImportExportFavorites;

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper3_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AutoCompleteSaveForm,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Form),
         Free);
   end AutoCompleteSaveForm;

   procedure AutoScan
     (This            : IShellUIHelper3_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AutoScan,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarTargetFrame,
          2 => strFailureUrl,
          3 => strSearch),
         Free);
   end AutoScan;

   procedure AutoCompleteAttach
     (This     : IShellUIHelper3_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AutoCompleteAttach,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Reserved),
         Free);
   end AutoCompleteAttach;

   function ShowBrowserUI
     (This     : IShellUIHelper3_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper3_ShowBrowserUI,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarIn,
          2 => bstrName),
         Free);
   end ShowBrowserUI;

   procedure AddSearchProvider
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddSearchProvider,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddSearchProvider;

   procedure RunOnceShown
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_RunOnceShown);
   end RunOnceShown;

   procedure SkipRunOnce
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_SkipRunOnce);
   end SkipRunOnce;

   procedure CustomizeSettings
     (This       : IShellUIHelper3_Type;
      fSQM       : GNATCOM.Types.VARIANT;
      fPhishing  : GNATCOM.Types.VARIANT;
      bstrLocale : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_CustomizeSettings,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrLocale,
          2 => fPhishing,
          3 => fSQM),
         Free);
   end CustomizeSettings;

   function SqmEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_SqmEnabled);
   end SqmEnabled;

   function PhishingEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_PhishingEnabled);
   end PhishingEnabled;

   function BrandImageUri
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_BrandImageUri);
   end BrandImageUri;

   procedure SkipTabsWelcome
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_SkipTabsWelcome);
   end SkipTabsWelcome;

   procedure DiagnoseConnection
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_DiagnoseConnection);
   end DiagnoseConnection;

   procedure CustomizeClearType
     (This : IShellUIHelper3_Type;
      fSet : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_CustomizeClearType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fSet),
         Free);
   end CustomizeClearType;

   function IsSearchProviderInstalled
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper3_IsSearchProviderInstalled,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end IsSearchProviderInstalled;

   function IsSearchMigrated
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_IsSearchMigrated);
   end IsSearchMigrated;

   function DefaultSearchProvider
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_DefaultSearchProvider);
   end DefaultSearchProvider;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper3_Type;
      fComplete : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_RunOnceRequiredSettingsComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fComplete),
         Free);
   end RunOnceRequiredSettingsComplete;

   function RunOnceHasShown
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_RunOnceHasShown);
   end RunOnceHasShown;

   function SearchGuideUrl
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_SearchGuideUrl);
   end SearchGuideUrl;

   procedure AddService
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddService,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddService;

   function IsServiceInstalled
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.VARIANT;
      Verb : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper3_IsServiceInstalled,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Verb,
          2 => URL),
         Free);
   end IsServiceInstalled;

   function InPrivateFilteringEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_InPrivateFilteringEnabled);
   end InPrivateFilteringEnabled;

   procedure AddToFavoritesBar
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT;
      uType : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_AddToFavoritesBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => uType,
          2 => Title,
          3 => URL),
         Free);
   end AddToFavoritesBar;

   procedure BuildNewTabPage
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_BuildNewTabPage);
   end BuildNewTabPage;

   procedure SetRecentlyClosedVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_SetRecentlyClosedVisible,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fVisible),
         Free);
   end SetRecentlyClosedVisible;

   procedure SetActivitiesVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_SetActivitiesVisible,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fVisible),
         Free);
   end SetActivitiesVisible;

   procedure ContentDiscoveryReset
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_ContentDiscoveryReset);
   end ContentDiscoveryReset;

   function IsSuggestedSitesEnabled
     (This : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper3_IsSuggestedSitesEnabled);
   end IsSuggestedSitesEnabled;

   procedure EnableSuggestedSites
     (This    : IShellUIHelper3_Type;
      fEnable : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_EnableSuggestedSites,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fEnable),
         Free);
   end EnableSuggestedSites;

   procedure NavigateToSuggestedSites
     (This            : IShellUIHelper3_Type;
      bstrRelativeUrl : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper3_NavigateToSuggestedSites,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrRelativeUrl),
         Free);
   end NavigateToSuggestedSites;

   procedure ShowTabsHelp
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_ShowTabsHelp);
   end ShowTabsHelp;

   procedure ShowInPrivateHelp
     (This : IShellUIHelper3_Type)
   is
   begin
      Invoke (This, IShellUIHelper3_ShowInPrivateHelp);
   end ShowInPrivateHelp;

end IE.IShellUIHelper3_Object;

