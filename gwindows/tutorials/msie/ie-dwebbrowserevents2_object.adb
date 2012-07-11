package body IE.DWebBrowserEvents2_Object is

   procedure StatusTextChange
     (This : DWebBrowserEvents2_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_StatusTextChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Text),
         Free);
   end StatusTextChange;

   procedure ProgressChange
     (This        : DWebBrowserEvents2_Type;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_ProgressChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => ProgressMax,
          2 => Progress),
         Free);
   end ProgressChange;

   procedure CommandStateChange
     (This    : DWebBrowserEvents2_Type;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_CommandStateChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Enable,
          2 => Command),
         Free);
   end CommandStateChange;

   procedure DownloadBegin
     (This : DWebBrowserEvents2_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents2_DownloadBegin);
   end DownloadBegin;

   procedure DownloadComplete
     (This : DWebBrowserEvents2_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents2_DownloadComplete);
   end DownloadComplete;

   procedure TitleChange
     (This : DWebBrowserEvents2_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_TitleChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Text),
         Free);
   end TitleChange;

   procedure PropertyChange
     (This       : DWebBrowserEvents2_Type;
      szProperty : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_PropertyChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => szProperty),
         Free);
   end PropertyChange;

   procedure BeforeNavigate2
     (This            : DWebBrowserEvents2_Type;
      pDisp           : GNATCOM.Types.VARIANT;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_BeforeNavigate2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => Headers,
          3 => PostData,
          4 => TargetFrameName,
          5 => Flags,
          6 => URL,
          7 => pDisp),
         Free);
   end BeforeNavigate2;

   procedure NewWindow2
     (This   : DWebBrowserEvents2_Type;
      ppDisp : GNATCOM.Types.VARIANT;
      Cancel : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_NewWindow2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => ppDisp),
         Free);
   end NewWindow2;

   procedure NavigateComplete2
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_NavigateComplete2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL,
          2 => pDisp),
         Free);
   end NavigateComplete2;

   procedure DocumentComplete
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_DocumentComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL,
          2 => pDisp),
         Free);
   end DocumentComplete;

   procedure OnQuit
     (This : DWebBrowserEvents2_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents2_OnQuit);
   end OnQuit;

   procedure OnVisible
     (This    : DWebBrowserEvents2_Type;
      Visible : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnVisible,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Visible),
         Free);
   end OnVisible;

   procedure OnToolBar
     (This    : DWebBrowserEvents2_Type;
      ToolBar : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnToolBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => ToolBar),
         Free);
   end OnToolBar;

   procedure OnMenuBar
     (This    : DWebBrowserEvents2_Type;
      MenuBar : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnMenuBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => MenuBar),
         Free);
   end OnMenuBar;

   procedure OnStatusBar
     (This      : DWebBrowserEvents2_Type;
      StatusBar : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnStatusBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => StatusBar),
         Free);
   end OnStatusBar;

   procedure OnFullScreen
     (This       : DWebBrowserEvents2_Type;
      FullScreen : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnFullScreen,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => FullScreen),
         Free);
   end OnFullScreen;

   procedure OnTheaterMode
     (This        : DWebBrowserEvents2_Type;
      TheaterMode : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_OnTheaterMode,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => TheaterMode),
         Free);
   end OnTheaterMode;

   procedure WindowSetResizable
     (This      : DWebBrowserEvents2_Type;
      Resizable : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowSetResizable,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Resizable),
         Free);
   end WindowSetResizable;

   procedure WindowSetLeft
     (This : DWebBrowserEvents2_Type;
      Left : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowSetLeft,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Left),
         Free);
   end WindowSetLeft;

   procedure WindowSetTop
     (This : DWebBrowserEvents2_Type;
      Top  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowSetTop,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Top),
         Free);
   end WindowSetTop;

   procedure WindowSetWidth
     (This  : DWebBrowserEvents2_Type;
      Width : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowSetWidth,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Width),
         Free);
   end WindowSetWidth;

   procedure WindowSetHeight
     (This   : DWebBrowserEvents2_Type;
      Height : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowSetHeight,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Height),
         Free);
   end WindowSetHeight;

   procedure WindowClosing
     (This          : DWebBrowserEvents2_Type;
      IsChildWindow : GNATCOM.Types.VARIANT;
      Cancel        : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowClosing,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => IsChildWindow),
         Free);
   end WindowClosing;

   procedure ClientToHostWindow
     (This : DWebBrowserEvents2_Type;
      CX   : GNATCOM.Types.VARIANT;
      CY   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_ClientToHostWindow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => CY,
          2 => CX),
         Free);
   end ClientToHostWindow;

   procedure SetSecureLockIcon
     (This           : DWebBrowserEvents2_Type;
      SecureLockIcon : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_SetSecureLockIcon,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => SecureLockIcon),
         Free);
   end SetSecureLockIcon;

   procedure FileDownload
     (This           : DWebBrowserEvents2_Type;
      ActiveDocument : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_FileDownload,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => ActiveDocument),
         Free);
   end FileDownload;

   procedure NavigateError
     (This       : DWebBrowserEvents2_Type;
      pDisp      : GNATCOM.Types.VARIANT;
      URL        : GNATCOM.Types.VARIANT;
      Frame      : GNATCOM.Types.VARIANT;
      StatusCode : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_NavigateError,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => StatusCode,
          3 => Frame,
          4 => URL,
          5 => pDisp),
         Free);
   end NavigateError;

   procedure PrintTemplateInstantiation
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_PrintTemplateInstantiation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pDisp),
         Free);
   end PrintTemplateInstantiation;

   procedure PrintTemplateTeardown
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_PrintTemplateTeardown,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pDisp),
         Free);
   end PrintTemplateTeardown;

   procedure UpdatePageStatus
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      nPage : GNATCOM.Types.VARIANT;
      fDone : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_UpdatePageStatus,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fDone,
          2 => nPage,
          3 => pDisp),
         Free);
   end UpdatePageStatus;

   procedure PrivacyImpactedStateChange
     (This      : DWebBrowserEvents2_Type;
      bImpacted : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_PrivacyImpactedStateChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bImpacted),
         Free);
   end PrivacyImpactedStateChange;

   procedure NewWindow3
     (This           : DWebBrowserEvents2_Type;
      ppDisp         : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      dwFlags        : GNATCOM.Types.VARIANT;
      bstrUrlContext : GNATCOM.Types.VARIANT;
      bstrUrl        : GNATCOM.Types.VARIANT;
      Free           : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_NewWindow3,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrUrl,
          2 => bstrUrlContext,
          3 => dwFlags,
          4 => Cancel,
          5 => ppDisp),
         Free);
   end NewWindow3;

   procedure SetPhishingFilterStatus
     (This                 : DWebBrowserEvents2_Type;
      PhishingFilterStatus : GNATCOM.Types.VARIANT;
      Free                 : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_SetPhishingFilterStatus,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => PhishingFilterStatus),
         Free);
   end SetPhishingFilterStatus;

   procedure WindowStateChanged
     (This               : DWebBrowserEvents2_Type;
      dwWindowStateFlags : GNATCOM.Types.VARIANT;
      dwValidFlagsMask   : GNATCOM.Types.VARIANT;
      Free               : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_WindowStateChanged,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => dwValidFlagsMask,
          2 => dwWindowStateFlags),
         Free);
   end WindowStateChanged;

   procedure NewProcess
     (This       : DWebBrowserEvents2_Type;
      lCauseFlag : GNATCOM.Types.VARIANT;
      pWB2       : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_NewProcess,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => pWB2,
          3 => lCauseFlag),
         Free);
   end NewProcess;

   procedure ThirdPartyUrlBlocked
     (This    : DWebBrowserEvents2_Type;
      URL     : GNATCOM.Types.VARIANT;
      dwCount : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_ThirdPartyUrlBlocked,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => dwCount,
          2 => URL),
         Free);
   end ThirdPartyUrlBlocked;

   procedure RedirectXDomainBlocked
     (This        : DWebBrowserEvents2_Type;
      pDisp       : GNATCOM.Types.VARIANT;
      StartURL    : GNATCOM.Types.VARIANT;
      RedirectURL : GNATCOM.Types.VARIANT;
      Frame       : GNATCOM.Types.VARIANT;
      StatusCode  : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents2_RedirectXDomainBlocked,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => StatusCode,
          2 => Frame,
          3 => RedirectURL,
          4 => StartURL,
          5 => pDisp),
         Free);
   end RedirectXDomainBlocked;

end IE.DWebBrowserEvents2_Object;

