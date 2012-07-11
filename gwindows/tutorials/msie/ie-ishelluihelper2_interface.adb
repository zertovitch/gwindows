with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellUIHelper2_Interface is

   procedure Initialize (This : in out IShellUIHelper2_Type) is
   begin
      Set_IID (This, IID_IShellUIHelper2);
   end Initialize;

   function Pointer (This : IShellUIHelper2_Type)
     return Pointer_To_IShellUIHelper2
   is
   begin
      return To_Pointer_To_IShellUIHelper2 (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellUIHelper2_Type;
                     Pointer : in     Pointer_To_IShellUIHelper2)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure ResetFirstBootMode
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetFirstBootMode
         (Pointer (This)));

   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetSafeMode
         (Pointer (This)));

   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RefreshOfflineDesktop
         (Pointer (This)));

   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper2_Type;
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
     (This : IShellUIHelper2_Type;
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
     (This  : IShellUIHelper2_Type;
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
     (This           : IShellUIHelper2_Type;
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
     (This          : IShellUIHelper2_Type;
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
     (This : IShellUIHelper2_Type;
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
     (This            : IShellUIHelper2_Type;
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
     (This     : IShellUIHelper2_Type;
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
     (This     : IShellUIHelper2_Type;
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
     (This : IShellUIHelper2_Type;
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
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RunOnceShown
         (Pointer (This)));

   end RunOnceShown;

   procedure SkipRunOnce
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SkipRunOnce
         (Pointer (This)));

   end SkipRunOnce;

   procedure CustomizeSettings
     (This       : IShellUIHelper2_Type;
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
     (This      : IShellUIHelper2_Type)
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
     (This      : IShellUIHelper2_Type)
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
     (This     : IShellUIHelper2_Type)
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
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SkipTabsWelcome
         (Pointer (This)));

   end SkipTabsWelcome;

   procedure DiagnoseConnection
     (This : IShellUIHelper2_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DiagnoseConnection
         (Pointer (This)));

   end DiagnoseConnection;

   procedure CustomizeClearType
     (This : IShellUIHelper2_Type;
      fSet : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CustomizeClearType
         (Pointer (This),
          fSet));

   end CustomizeClearType;

   function IsSearchProviderInstalled
     (This      : IShellUIHelper2_Type;
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
     (This       : IShellUIHelper2_Type)
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
     (This      : IShellUIHelper2_Type)
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
     (This      : IShellUIHelper2_Type;
      fComplete : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RunOnceRequiredSettingsComplete
         (Pointer (This),
          fComplete));

   end RunOnceRequiredSettingsComplete;

   function RunOnceHasShown
     (This    : IShellUIHelper2_Type)
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
     (This     : IShellUIHelper2_Type)
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

end IE.IShellUIHelper2_Interface;

