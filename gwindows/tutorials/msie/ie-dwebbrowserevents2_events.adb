package body IE.DWebBrowserEvents2_Events is

   procedure Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      pragma Unreferenced (wFlags);
   begin
      case dispidMember is
         when DWebBrowserEvents2_StatusTextChange =>
            StatusTextChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_ProgressChange =>
            ProgressChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_CommandStateChange =>
            CommandStateChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_DownloadBegin =>
            DownloadBegin
              (DWebBrowserEvents2_Event'Class (Event_Object.all));
         when DWebBrowserEvents2_DownloadComplete =>
            DownloadComplete
              (DWebBrowserEvents2_Event'Class (Event_Object.all));
         when DWebBrowserEvents2_TitleChange =>
            TitleChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_PropertyChange =>
            PropertyChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_BeforeNavigate2 =>
            BeforeNavigate2
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (6),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_NewWindow2 =>
            NewWindow2
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_NavigateComplete2 =>
            NavigateComplete2
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_DocumentComplete =>
            DocumentComplete
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnQuit =>
            OnQuit
              (DWebBrowserEvents2_Event'Class (Event_Object.all));
         when DWebBrowserEvents2_OnVisible =>
            OnVisible
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnToolBar =>
            OnToolBar
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnMenuBar =>
            OnMenuBar
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnStatusBar =>
            OnStatusBar
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnFullScreen =>
            OnFullScreen
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_OnTheaterMode =>
            OnTheaterMode
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowSetResizable =>
            WindowSetResizable
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowSetLeft =>
            WindowSetLeft
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowSetTop =>
            WindowSetTop
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowSetWidth =>
            WindowSetWidth
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowSetHeight =>
            WindowSetHeight
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowClosing =>
            WindowClosing
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_ClientToHostWindow =>
            ClientToHostWindow
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_SetSecureLockIcon =>
            SetSecureLockIcon
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_FileDownload =>
            FileDownload
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_NavigateError =>
            NavigateError
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_PrintTemplateInstantiation =>
            PrintTemplateInstantiation
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_PrintTemplateTeardown =>
            PrintTemplateTeardown
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_UpdatePageStatus =>
            UpdatePageStatus
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_PrivacyImpactedStateChange =>
            PrivacyImpactedStateChange
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_NewWindow3 =>
            NewWindow3
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_SetPhishingFilterStatus =>
            SetPhishingFilterStatus
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_WindowStateChanged =>
            WindowStateChanged
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_NewProcess =>
            NewProcess
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_ThirdPartyUrlBlocked =>
            ThirdPartyUrlBlocked
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents2_RedirectXDomainBlocked =>
            RedirectXDomainBlocked
              (DWebBrowserEvents2_Event'Class (Event_Object.all),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when others =>
            null;
      end case;
   end Invoke;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Events.Event_Object.Create
        (Invoke'Access,
         IID_DWebBrowserEvents2,
         From);
   end Create;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Events.Set_Events
        (This,
         For_Object,
         IID_DWebBrowserEvents2,
         Event_Interface,
         Free);
   end Set_Events;

   procedure StatusTextChange
     (This : DWebBrowserEvents2_Event;
      Text : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end StatusTextChange;

   procedure ProgressChange
     (This        : DWebBrowserEvents2_Event;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ProgressChange;

   procedure CommandStateChange
     (This    : DWebBrowserEvents2_Event;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end CommandStateChange;

   procedure DownloadBegin
     (This : DWebBrowserEvents2_Event)
   is
   begin
      null;
   end DownloadBegin;

   procedure DownloadComplete
     (This : DWebBrowserEvents2_Event)
   is
   begin
      null;
   end DownloadComplete;

   procedure TitleChange
     (This : DWebBrowserEvents2_Event;
      Text : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end TitleChange;

   procedure PropertyChange
     (This       : DWebBrowserEvents2_Event;
      szProperty : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end PropertyChange;

   procedure BeforeNavigate2
     (This            : DWebBrowserEvents2_Event;
      pDisp           : GNATCOM.Types.VARIANT;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end BeforeNavigate2;

   procedure NewWindow2
     (This   : DWebBrowserEvents2_Event;
      ppDisp : GNATCOM.Types.VARIANT;
      Cancel : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NewWindow2;

   procedure NavigateComplete2
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NavigateComplete2;

   procedure DocumentComplete
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end DocumentComplete;

   procedure OnQuit
     (This : DWebBrowserEvents2_Event)
   is
   begin
      null;
   end OnQuit;

   procedure OnVisible
     (This    : DWebBrowserEvents2_Event;
      Visible : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnVisible;

   procedure OnToolBar
     (This    : DWebBrowserEvents2_Event;
      ToolBar : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnToolBar;

   procedure OnMenuBar
     (This    : DWebBrowserEvents2_Event;
      MenuBar : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnMenuBar;

   procedure OnStatusBar
     (This      : DWebBrowserEvents2_Event;
      StatusBar : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnStatusBar;

   procedure OnFullScreen
     (This       : DWebBrowserEvents2_Event;
      FullScreen : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnFullScreen;

   procedure OnTheaterMode
     (This        : DWebBrowserEvents2_Event;
      TheaterMode : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end OnTheaterMode;

   procedure WindowSetResizable
     (This      : DWebBrowserEvents2_Event;
      Resizable : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowSetResizable;

   procedure WindowSetLeft
     (This : DWebBrowserEvents2_Event;
      Left : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowSetLeft;

   procedure WindowSetTop
     (This : DWebBrowserEvents2_Event;
      Top  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowSetTop;

   procedure WindowSetWidth
     (This  : DWebBrowserEvents2_Event;
      Width : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowSetWidth;

   procedure WindowSetHeight
     (This   : DWebBrowserEvents2_Event;
      Height : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowSetHeight;

   procedure WindowClosing
     (This          : DWebBrowserEvents2_Event;
      IsChildWindow : GNATCOM.Types.VARIANT;
      Cancel        : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowClosing;

   procedure ClientToHostWindow
     (This : DWebBrowserEvents2_Event;
      CX   : GNATCOM.Types.VARIANT;
      CY   : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ClientToHostWindow;

   procedure SetSecureLockIcon
     (This           : DWebBrowserEvents2_Event;
      SecureLockIcon : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end SetSecureLockIcon;

   procedure FileDownload
     (This           : DWebBrowserEvents2_Event;
      ActiveDocument : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FileDownload;

   procedure NavigateError
     (This       : DWebBrowserEvents2_Event;
      pDisp      : GNATCOM.Types.VARIANT;
      URL        : GNATCOM.Types.VARIANT;
      Frame      : GNATCOM.Types.VARIANT;
      StatusCode : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NavigateError;

   procedure PrintTemplateInstantiation
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end PrintTemplateInstantiation;

   procedure PrintTemplateTeardown
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end PrintTemplateTeardown;

   procedure UpdatePageStatus
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      nPage : GNATCOM.Types.VARIANT;
      fDone : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end UpdatePageStatus;

   procedure PrivacyImpactedStateChange
     (This      : DWebBrowserEvents2_Event;
      bImpacted : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end PrivacyImpactedStateChange;

   procedure NewWindow3
     (This           : DWebBrowserEvents2_Event;
      ppDisp         : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      dwFlags        : GNATCOM.Types.VARIANT;
      bstrUrlContext : GNATCOM.Types.VARIANT;
      bstrUrl        : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NewWindow3;

   procedure SetPhishingFilterStatus
     (This                 : DWebBrowserEvents2_Event;
      PhishingFilterStatus : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end SetPhishingFilterStatus;

   procedure WindowStateChanged
     (This               : DWebBrowserEvents2_Event;
      dwWindowStateFlags : GNATCOM.Types.VARIANT;
      dwValidFlagsMask   : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowStateChanged;

   procedure NewProcess
     (This       : DWebBrowserEvents2_Event;
      lCauseFlag : GNATCOM.Types.VARIANT;
      pWB2       : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NewProcess;

   procedure ThirdPartyUrlBlocked
     (This    : DWebBrowserEvents2_Event;
      URL     : GNATCOM.Types.VARIANT;
      dwCount : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ThirdPartyUrlBlocked;

   procedure RedirectXDomainBlocked
     (This        : DWebBrowserEvents2_Event;
      pDisp       : GNATCOM.Types.VARIANT;
      StartURL    : GNATCOM.Types.VARIANT;
      RedirectURL : GNATCOM.Types.VARIANT;
      Frame       : GNATCOM.Types.VARIANT;
      StatusCode  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end RedirectXDomainBlocked;

end IE.DWebBrowserEvents2_Events;
