package body IE.IShellUIHelper_Object is

   procedure ResetFirstBootMode
     (This : IShellUIHelper_Type)
   is
   begin
      Invoke (This, IShellUIHelper_ResetFirstBootMode);
   end ResetFirstBootMode;

   procedure ResetSafeMode
     (This : IShellUIHelper_Type)
   is
   begin
      Invoke (This, IShellUIHelper_ResetSafeMode);
   end ResetSafeMode;

   procedure RefreshOfflineDesktop
     (This : IShellUIHelper_Type)
   is
   begin
      Invoke (This, IShellUIHelper_RefreshOfflineDesktop);
   end RefreshOfflineDesktop;

   procedure AddFavorite
     (This  : IShellUIHelper_Type;
      URL   : GNATCOM.Types.VARIANT;
      Title : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_AddFavorite,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Title,
          2 => URL),
         Free);
   end AddFavorite;

   procedure AddChannel
     (This : IShellUIHelper_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_AddChannel,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end AddChannel;

   procedure AddDesktopComponent
     (This   : IShellUIHelper_Type;
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
         IShellUIHelper_AddDesktopComponent,
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
     (This : IShellUIHelper_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper_IsSubscribed,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end IsSubscribed;

   procedure NavigateAndFind
     (This           : IShellUIHelper_Type;
      URL            : GNATCOM.Types.VARIANT;
      strQuery       : GNATCOM.Types.VARIANT;
      varTargetFrame : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_NavigateAndFind,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => varTargetFrame,
          2 => strQuery,
          3 => URL),
         Free);
   end NavigateAndFind;

   procedure ImportExportFavorites
     (This          : IShellUIHelper_Type;
      fImport       : GNATCOM.Types.VARIANT;
      strImpExpPath : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_ImportExportFavorites,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => strImpExpPath,
          2 => fImport),
         Free);
   end ImportExportFavorites;

   procedure AutoCompleteSaveForm
     (This : IShellUIHelper_Type;
      Form : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_AutoCompleteSaveForm,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Form),
         Free);
   end AutoCompleteSaveForm;

   procedure AutoScan
     (This            : IShellUIHelper_Type;
      strSearch       : GNATCOM.Types.VARIANT;
      strFailureUrl   : GNATCOM.Types.VARIANT;
      pvarTargetFrame : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_AutoScan,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarTargetFrame,
          2 => strFailureUrl,
          3 => strSearch),
         Free);
   end AutoScan;

   procedure AutoCompleteAttach
     (This     : IShellUIHelper_Type;
      Reserved : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellUIHelper_AutoCompleteAttach,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Reserved),
         Free);
   end AutoCompleteAttach;

   function ShowBrowserUI
     (This     : IShellUIHelper_Type;
      bstrName : GNATCOM.Types.VARIANT;
      pvarIn   : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellUIHelper_ShowBrowserUI,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarIn,
          2 => bstrName),
         Free);
   end ShowBrowserUI;

end IE.IShellUIHelper_Object;

