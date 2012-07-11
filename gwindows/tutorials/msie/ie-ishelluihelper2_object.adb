package body IE.IShellUIHelper2_Object is

   procedure ResetFirstBootMode
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_ResetFirstBootMode);
   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_ResetSafeMode);
   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_RefreshOfflineDesktop);
   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper2_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AddFavorite,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Title,
          2 => URL),
         Free);
   end AddFavorite;

   procedure AddChannel
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AddChannel,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddChannel;

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
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AddDesktopComponent,
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
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper2_IsSubscribed,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end IsSubscribed;

   procedure NavigateAndFind
     (This           : IShellUIHelper2_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_NavigateAndFind,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => varTargetFrame,
          2 => strQuery,
          3 => URL),
         Free);
   end NavigateAndFind;

   procedure ImportExportFavorites
     (This          : IShellUIHelper2_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_ImportExportFavorites,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => strImpExpPath,
          2 => fImport),
         Free);
   end ImportExportFavorites;

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper2_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AutoCompleteSaveForm,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Form),
         Free);
   end AutoCompleteSaveForm;

   procedure AutoScan
     (This            : IShellUIHelper2_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AutoScan,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarTargetFrame,
          2 => strFailureUrl,
          3 => strSearch),
         Free);
   end AutoScan;

   procedure AutoCompleteAttach
     (This     : IShellUIHelper2_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AutoCompleteAttach,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Reserved),
         Free);
   end AutoCompleteAttach;

   function ShowBrowserUI
     (This     : IShellUIHelper2_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper2_ShowBrowserUI,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarIn,
          2 => bstrName),
         Free);
   end ShowBrowserUI;

   procedure AddSearchProvider
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_AddSearchProvider,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddSearchProvider;

   procedure RunOnceShown
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_RunOnceShown);
   end RunOnceShown;

   procedure SkipRunOnce
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_SkipRunOnce);
   end SkipRunOnce;

   procedure CustomizeSettings
     (This       : IShellUIHelper2_Type;
      fSQM       : GNATCOM.Types.VARIANT;
      fPhishing  : GNATCOM.Types.VARIANT;
      bstrLocale : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_CustomizeSettings,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrLocale,
          2 => fPhishing,
          3 => fSQM),
         Free);
   end CustomizeSettings;

   function SqmEnabled
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_SqmEnabled);
   end SqmEnabled;

   function PhishingEnabled
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_PhishingEnabled);
   end PhishingEnabled;

   function BrandImageUri
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_BrandImageUri);
   end BrandImageUri;

   procedure SkipTabsWelcome
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_SkipTabsWelcome);
   end SkipTabsWelcome;

   procedure DiagnoseConnection
     (This : IShellUIHelper2_Type)
   is
   begin
      Invoke (This, IShellUIHelper2_DiagnoseConnection);
   end DiagnoseConnection;

   procedure CustomizeClearType
     (This : IShellUIHelper2_Type;
      fSet : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_CustomizeClearType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fSet),
         Free);
   end CustomizeClearType;

   function IsSearchProviderInstalled
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper2_IsSearchProviderInstalled,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end IsSearchProviderInstalled;

   function IsSearchMigrated
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_IsSearchMigrated);
   end IsSearchMigrated;

   function DefaultSearchProvider
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_DefaultSearchProvider);
   end DefaultSearchProvider;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper2_Type;
      fComplete : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper2_RunOnceRequiredSettingsComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fComplete),
         Free);
   end RunOnceRequiredSettingsComplete;

   function RunOnceHasShown
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_RunOnceHasShown);
   end RunOnceHasShown;

   function SearchGuideUrl
     (This : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellUIHelper2_SearchGuideUrl);
   end SearchGuideUrl;

end IE.IShellUIHelper2_Object;

