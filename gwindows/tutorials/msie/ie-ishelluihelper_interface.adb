with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellUIHelper_Interface is

   procedure Initialize (This : in out IShellUIHelper_Type) is
   begin
      Set_IID (This, IID_IShellUIHelper);
   end Initialize;

   function Pointer (This : IShellUIHelper_Type)
     return Pointer_To_IShellUIHelper
   is
   begin
      return To_Pointer_To_IShellUIHelper (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellUIHelper_Type;
                     Pointer : in     Pointer_To_IShellUIHelper)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure ResetFirstBootMode
     (This : IShellUIHelper_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetFirstBootMode
         (Pointer (This)));

   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetSafeMode
         (Pointer (This)));

   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RefreshOfflineDesktop
         (Pointer (This)));

   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper_Type;
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
     (This : IShellUIHelper_Type;
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
     (This   : IShellUIHelper_Type;
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
     (This  : IShellUIHelper_Type;
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
     (This           : IShellUIHelper_Type;
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
     (This          : IShellUIHelper_Type;
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
     (This : IShellUIHelper_Type;
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
     (This            : IShellUIHelper_Type;
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
     (This     : IShellUIHelper_Type;
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
     (This     : IShellUIHelper_Type;
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

end IE.IShellUIHelper_Interface;

