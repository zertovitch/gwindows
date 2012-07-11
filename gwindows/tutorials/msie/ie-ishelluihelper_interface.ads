with GNATCOM.Dispinterface;

package IE.IShellUIHelper_Interface is

   type IShellUIHelper_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IShellUIHelper_Type);

   function Pointer (This : IShellUIHelper_Type)
     return Pointer_To_IShellUIHelper;

   procedure Attach (This    : in out IShellUIHelper_Type;
                     Pointer : in     Pointer_To_IShellUIHelper);

   procedure ResetFirstBootMode
     (This : IShellUIHelper_Type);

   procedure ResetSafeMode
     (This : IShellUIHelper_Type);

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper_Type);

   procedure AddFavorite
     (This  : IShellUIHelper_Type;
      URL   : GNATCOM.Types.BSTR;
      Title : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free  : Boolean := True);

   procedure AddChannel
     (This : IShellUIHelper_Type;
      URL  : GNATCOM.Types.BSTR;
      Free : Boolean := True);

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
      Free   : Boolean := True);

   function IsSubscribed
     (This  : IShellUIHelper_Type;
      URL   : GNATCOM.Types.BSTR;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT_BOOL;

   procedure NavigateAndFind
     (This           : IShellUIHelper_Type;
      URL            : GNATCOM.Types.BSTR;
      strQuery       : GNATCOM.Types.BSTR;
      varTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free           : Boolean := True);

   procedure ImportExportFavorites
     (This          : IShellUIHelper_Type;
      fImport       : GNATCOM.Types.VARIANT_BOOL;
      strImpExpPath : GNATCOM.Types.BSTR;
      Free          : Boolean := True);

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper_Type;
      Form : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   procedure AutoScan
     (This            : IShellUIHelper_Type;
      strSearch       : GNATCOM.Types.BSTR;
      strFailureUrl   : GNATCOM.Types.BSTR;
      pvarTargetFrame : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True);

   procedure AutoCompleteAttach
     (This     : IShellUIHelper_Type;
      Reserved : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);

   function ShowBrowserUI
     (This     : IShellUIHelper_Type;
      bstrName : GNATCOM.Types.BSTR;
      pvarIn   : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

end IE.IShellUIHelper_Interface;

