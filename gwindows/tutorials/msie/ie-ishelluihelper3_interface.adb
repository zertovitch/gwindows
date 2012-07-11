with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellUIHelper3_Interface is

   procedure Initialize (This : in out IShellUIHelper3_Type) is
   begin
      Set_IID (This, IID_IShellUIHelper3);
   end Initialize;

   function Pointer (This : IShellUIHelper3_Type)
     return Pointer_To_IShellUIHelper3
   is
   begin
      return To_Pointer_To_IShellUIHelper3 (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellUIHelper3_Type;
                     Pointer : in     Pointer_To_IShellUIHelper3)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure ResetFirstBootMode
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetFirstBootMode
         (Pointer (This)));

   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetSafeMode
         (Pointer (This)));

   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RefreshOfflineDesktop
         (Pointer (This)));

   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddFavorite
         (Pointer (This),
          URL,
          Title));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

   end AddFavorite;

   procedure AddChannel
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddChannel
         (Pointer (This),
          URL));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

   end AddChannel;

   procedure AddDesktopComponent
     (This   : IShellUIHelper3_Type;
      URL    : GNATCOM.Types.BSTR;
      uType  : GNATCOM.Types.BSTR;
      Left   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Top    : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Width  : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Height : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free   : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddDesktopComponent
         (Pointer (This),
          URL,
          uType,
          Left,
          Top,
          Width,
          Height));

      if Free then
               GNATCOM.IInterface.Free (URL);
               GNATCOM.IInterface.Free (uType);
      
      end if;

   end AddDesktopComponent;

   function IsSubscribed
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsSubscribed
         (Pointer (This),
          URL,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

      return RetVal;
   end IsSubscribed;

   procedure NavigateAndFind
     (This           : IShellUIHelper3_Type;
      URL            : GNATCOM.Types.BSTR;
      strQuery       : GNATCOM.Types.BSTR;
      varTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free           : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NavigateAndFind
         (Pointer (This),
          URL,
          strQuery,
          varTargetFrame));

      if Free then
               GNATCOM.IInterface.Free (URL);
               GNATCOM.IInterface.Free (strQuery);
      
      end if;

   end NavigateAndFind;

   procedure ImportExportFavorites
     (This          : IShellUIHelper3_Type;
      fImport       : GNATCOM.Types.VARIANT_BOOL;
      strImpExpPath : GNATCOM.Types.BSTR;
      Free          : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ImportExportFavorites
         (Pointer (This),
          fImport,
          strImpExpPath));

      if Free then
               GNATCOM.IInterface.Free (strImpExpPath);
      
      end if;

   end ImportExportFavorites;

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper3_Type;
      Form : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AutoCompleteSaveForm
         (Pointer (This),
          Form));

   end AutoCompleteSaveForm;

   procedure AutoScan
     (This            : IShellUIHelper3_Type;
      strSearch       : GNATCOM.Types.BSTR;
      strFailureUrl   : GNATCOM.Types.BSTR;
      pvarTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AutoScan
         (Pointer (This),
          strSearch,
          strFailureUrl,
          pvarTargetFrame));

      if Free then
               GNATCOM.IInterface.Free (strSearch);
               GNATCOM.IInterface.Free (strFailureUrl);
      
      end if;

   end AutoScan;

   procedure AutoCompleteAttach
     (This     : IShellUIHelper3_Type;
      Reserved : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AutoCompleteAttach
         (Pointer (This),
          Reserved));

   end AutoCompleteAttach;

   function ShowBrowserUI
     (This     : IShellUIHelper3_Type;
      bstrName : GNATCOM.Types.BSTR;
      pvarIn   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ShowBrowserUI
         (Pointer (This),
          bstrName,
          pvarIn,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (bstrName);
      
      end if;

      return RetVal;
   end ShowBrowserUI;

   procedure AddSearchProvider
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddSearchProvider
         (Pointer (This),
          URL));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

   end AddSearchProvider;

   procedure RunOnceShown
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RunOnceShown
         (Pointer (This)));

   end RunOnceShown;

   procedure SkipRunOnce
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SkipRunOnce
         (Pointer (This)));

   end SkipRunOnce;

   procedure CustomizeSettings
     (This       : IShellUIHelper3_Type;
      fSQM       : GNATCOM.Types.VARIANT_BOOL;
      fPhishing  : GNATCOM.Types.VARIANT_BOOL;
      bstrLocale : GNATCOM.Types.BSTR;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CustomizeSettings
         (Pointer (This),
          fSQM,
          fPhishing,
          bstrLocale));

      if Free then
               GNATCOM.IInterface.Free (bstrLocale);
      
      end if;

   end CustomizeSettings;

   function SqmEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SqmEnabled
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end SqmEnabled;

   function PhishingEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PhishingEnabled
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end PhishingEnabled;

   function BrandImageUri
     (This     : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.BrandImageUri
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end BrandImageUri;

   procedure SkipTabsWelcome
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SkipTabsWelcome
         (Pointer (This)));

   end SkipTabsWelcome;

   procedure DiagnoseConnection
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DiagnoseConnection
         (Pointer (This)));

   end DiagnoseConnection;

   procedure CustomizeClearType
     (This : IShellUIHelper3_Type;
      fSet : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CustomizeClearType
         (Pointer (This),
          fSet));

   end CustomizeClearType;

   function IsSearchProviderInstalled
     (This      : IShellUIHelper3_Type;
      URL       : GNATCOM.Types.BSTR;
      Free      : Boolean := True)
     return Interfaces.C.unsigned_long
   is
       RetVal : aliased Interfaces.C.unsigned_long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsSearchProviderInstalled
         (Pointer (This),
          URL,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

      return RetVal;
   end IsSearchProviderInstalled;

   function IsSearchMigrated
     (This       : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsSearchMigrated
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end IsSearchMigrated;

   function DefaultSearchProvider
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DefaultSearchProvider
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end DefaultSearchProvider;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper3_Type;
      fComplete : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RunOnceRequiredSettingsComplete
         (Pointer (This),
          fComplete));

   end RunOnceRequiredSettingsComplete;

   function RunOnceHasShown
     (This    : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RunOnceHasShown
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end RunOnceHasShown;

   function SearchGuideUrl
     (This     : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SearchGuideUrl
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end SearchGuideUrl;

   procedure AddService
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddService
         (Pointer (This),
          URL));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

   end AddService;

   function IsServiceInstalled
     (This      : IShellUIHelper3_Type;
      URL       : GNATCOM.Types.BSTR;
      Verb      : GNATCOM.Types.BSTR;
      Free      : Boolean := True)
     return Interfaces.C.unsigned_long
   is
       RetVal : aliased Interfaces.C.unsigned_long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsServiceInstalled
         (Pointer (This),
          URL,
          Verb,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (URL);
               GNATCOM.IInterface.Free (Verb);
      
      end if;

      return RetVal;
   end IsServiceInstalled;

   function InPrivateFilteringEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InPrivateFilteringEnabled
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end InPrivateFilteringEnabled;

   procedure AddToFavoritesBar
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : GNATCOM.Types.BSTR;
      uType : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddToFavoritesBar
         (Pointer (This),
          URL,
          Title,
          uType));

      if Free then
               GNATCOM.IInterface.Free (URL);
               GNATCOM.IInterface.Free (Title);
      
      end if;

   end AddToFavoritesBar;

   procedure BuildNewTabPage
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.BuildNewTabPage
         (Pointer (This)));

   end BuildNewTabPage;

   procedure SetRecentlyClosedVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetRecentlyClosedVisible
         (Pointer (This),
          fVisible));

   end SetRecentlyClosedVisible;

   procedure SetActivitiesVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetActivitiesVisible
         (Pointer (This),
          fVisible));

   end SetActivitiesVisible;

   procedure ContentDiscoveryReset
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ContentDiscoveryReset
         (Pointer (This)));

   end ContentDiscoveryReset;

   function IsSuggestedSitesEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsSuggestedSitesEnabled
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end IsSuggestedSitesEnabled;

   procedure EnableSuggestedSites
     (This    : IShellUIHelper3_Type;
      fEnable : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnableSuggestedSites
         (Pointer (This),
          fEnable));

   end EnableSuggestedSites;

   procedure NavigateToSuggestedSites
     (This            : IShellUIHelper3_Type;
      bstrRelativeUrl : GNATCOM.Types.BSTR;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NavigateToSuggestedSites
         (Pointer (This),
          bstrRelativeUrl));

      if Free then
               GNATCOM.IInterface.Free (bstrRelativeUrl);
      
      end if;

   end NavigateToSuggestedSites;

   procedure ShowTabsHelp
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ShowTabsHelp
         (Pointer (This)));

   end ShowTabsHelp;

   procedure ShowInPrivateHelp
     (This : IShellUIHelper3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ShowInPrivateHelp
         (Pointer (This)));

   end ShowInPrivateHelp;

end IE.IShellUIHelper3_Interface;

