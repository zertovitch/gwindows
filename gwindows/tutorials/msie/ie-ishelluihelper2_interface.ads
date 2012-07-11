with GNATCOM.Dispinterface;

package IE.IShellUIHelper2_Interface is

   type IShellUIHelper2_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IShellUIHelper2_Type);

   function Pointer (This : IShellUIHelper2_Type)
     return Pointer_To_IShellUIHelper2;

   procedure Attach (This    : in out IShellUIHelper2_Type;
                     Pointer : in     Pointer_To_IShellUIHelper2);

   procedure ResetFirstBootMode
     (This : IShellUIHelper2_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper2_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper2_Type);

   procedure AddFavorite
     (This  : IShellUIHelper2_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

   procedure AddDesktopComponent
     (This   : IShellUIHelper2_Type;
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
     (This  : IShellUIHelper2_Type;
      URL   : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure NavigateAndFind
     (This           : IShellUIHelper2_Type;
      URL            : GNATCOM.Types.BSTR;
      strQuery       : GNATCOM.Types.BSTR;
      varTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper2_Type;
      fImport       : GNATCOM.Types.VARIANT_BOOL;
      strImpExpPath : GNATCOM.Types.BSTR;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper2_Type;
      Form : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure AutoScan
     (This            : IShellUIHelper2_Type;
      strSearch       : GNATCOM.Types.BSTR;
      strFailureUrl   : GNATCOM.Types.BSTR;
      pvarTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper2_Type;
      Reserved : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   function ShowBrowserUI
     (This     : IShellUIHelper2_Type;
      bstrName : GNATCOM.Types.BSTR;
      pvarIn   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure AddSearchProvider
     (This : IShellUIHelper2_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

   procedure RunOnceShown
     (This : IShellUIHelper2_Type);

   procedure SkipRunOnce
     (This : IShellUIHelper2_Type);

   procedure CustomizeSettings
     (This       : IShellUIHelper2_Type;
      fSQM       : GNATCOM.Types.VARIANT_BOOL;
      fPhishing  : GNATCOM.Types.VARIANT_BOOL;
      bstrLocale : GNATCOM.Types.BSTR;
      Free       : Boolean := True);

   function SqmEnabled
     (This      : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function PhishingEnabled
     (This      : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function BrandImageUri
     (This     : IShellUIHelper2_Type)
     return GNATCOM.Types.BSTR;

   procedure SkipTabsWelcome
     (This : IShellUIHelper2_Type);

   procedure DiagnoseConnection
     (This : IShellUIHelper2_Type);

   procedure CustomizeClearType
     (This : IShellUIHelper2_Type;
      fSet : GNATCOM.Types.VARIANT_BOOL);

   function IsSearchProviderInstalled
     (This      : IShellUIHelper2_Type;
      URL       : GNATCOM.Types.BSTR;
      Free      : Boolean := True)
     return Interfaces.C.unsigned_long;

   function IsSearchMigrated
     (This       : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function DefaultSearchProvider
     (This      : IShellUIHelper2_Type)
     return GNATCOM.Types.BSTR;

   procedure RunOnceRequiredSettingsComplete
     (This      : IShellUIHelper2_Type;
      fComplete : GNATCOM.Types.VARIANT_BOOL);

   function RunOnceHasShown
     (This    : IShellUIHelper2_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function SearchGuideUrl
     (This     : IShellUIHelper2_Type)
     return GNATCOM.Types.BSTR;

end IE.IShellUIHelper2_Interface;

