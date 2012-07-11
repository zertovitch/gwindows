with GNATCOM.Dispinterface;

package IE.IShellUIHelper3_Interface is

   type IShellUIHelper3_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IShellUIHelper3_Type);

   function Pointer (This : IShellUIHelper3_Type)
     return Pointer_To_IShellUIHelper3;

   procedure Attach (This    : in out IShellUIHelper3_Type;
                     Pointer : in     Pointer_To_IShellUIHelper3);

   procedure ResetFirstBootMode
     (This : IShellUIHelper3_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper3_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper3_Type);

   procedure AddFavorite
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

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
      Free   : Boolean := True);

   function IsSubscribed
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure NavigateAndFind
     (This           : IShellUIHelper3_Type;
      URL            : GNATCOM.Types.BSTR;
      strQuery       : GNATCOM.Types.BSTR;
      varTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper3_Type;
      fImport       : GNATCOM.Types.VARIANT_BOOL;
      strImpExpPath : GNATCOM.Types.BSTR;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper3_Type;
      Form : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure AutoScan
     (This            : IShellUIHelper3_Type;
      strSearch       : GNATCOM.Types.BSTR;
      strFailureUrl   : GNATCOM.Types.BSTR;
      pvarTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper3_Type;
      Reserved : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   function ShowBrowserUI
     (This     : IShellUIHelper3_Type;
      bstrName : GNATCOM.Types.BSTR;
      pvarIn   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure AddSearchProvider
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

   procedure RunOnceShown
     (This : IShellUIHelper3_Type);

   procedure SkipRunOnce
     (This : IShellUIHelper3_Type);

   procedure CustomizeSettings
     (This       : IShellUIHelper3_Type;
      fSQM       : GNATCOM.Types.VARIANT_BOOL;
      fPhishing  : GNATCOM.Types.VARIANT_BOOL;
      bstrLocale : GNATCOM.Types.BSTR;
      Free       : Boolean := True);

   function SqmEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function PhishingEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function BrandImageUri
     (This     : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR;

   procedure SkipTabsWelcome
     (This : IShellUIHelper3_Type);

   procedure DiagnoseConnection
     (This : IShellUIHelper3_Type);

   procedure CustomizeClearType
     (This : IShellUIHelper3_Type;
      fSet : GNATCOM.Types.VARIANT_BOOL);

   function IsSearchProviderInstalled
     (This      : IShellUIHelper3_Type;
      URL       : GNATCOM.Types.BSTR;
      Free      : Boolean := True)
     return Interfaces.C.unsigned_long;

   function IsSearchMigrated
     (This       : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function DefaultSearchProvider
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper3_Type;
      fComplete : GNATCOM.Types.VARIANT_BOOL);

   function RunOnceHasShown
     (This    : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function SearchGuideUrl
     (This     : IShellUIHelper3_Type)
     return GNATCOM.Types.BSTR;

   procedure AddService
     (This : IShellUIHelper3_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

   function IsServiceInstalled
     (This      : IShellUIHelper3_Type;
      URL       : GNATCOM.Types.BSTR;
      Verb      : GNATCOM.Types.BSTR;
      Free      : Boolean := True)
     return Interfaces.C.unsigned_long;

   function InPrivateFilteringEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure AddToFavoritesBar
     (This  : IShellUIHelper3_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : GNATCOM.Types.BSTR;
      uType : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True);

   procedure BuildNewTabPage
     (This : IShellUIHelper3_Type);

   procedure SetRecentlyClosedVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT_BOOL);

   procedure SetActivitiesVisible
     (This     : IShellUIHelper3_Type;
      fVisible : GNATCOM.Types.VARIANT_BOOL);

   procedure ContentDiscoveryReset
     (This : IShellUIHelper3_Type);

   function IsSuggestedSitesEnabled
     (This      : IShellUIHelper3_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure EnableSuggestedSites
     (This    : IShellUIHelper3_Type;
      fEnable : GNATCOM.Types.VARIANT_BOOL);

   procedure NavigateToSuggestedSites
     (This            : IShellUIHelper3_Type;
      bstrRelativeUrl : GNATCOM.Types.BSTR;
      Free            : Boolean := True);

   procedure ShowTabsHelp
     (This : IShellUIHelper3_Type);

   procedure ShowInPrivateHelp
     (This : IShellUIHelper3_Type);

end IE.IShellUIHelper3_Interface;

