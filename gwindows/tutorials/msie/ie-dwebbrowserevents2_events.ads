with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Iinterface;

package IE.DWebBrowserEvents2_Events is

   type DWebBrowserEvents2_Event is
     new GNATCOM.Events.Event_Object.Event_Type with null record;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);

   procedure StatusTextChange
     (This : DWebBrowserEvents2_Event;
      Text : GNATCOM.Types.VARIANT);

   procedure ProgressChange
     (This        : DWebBrowserEvents2_Event;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT);

   procedure CommandStateChange
     (This    : DWebBrowserEvents2_Event;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT);

   procedure DownloadBegin
     (This : DWebBrowserEvents2_Event);

   procedure DownloadComplete
     (This : DWebBrowserEvents2_Event);

   procedure TitleChange
     (This : DWebBrowserEvents2_Event;
      Text : GNATCOM.Types.VARIANT);

   procedure PropertyChange
     (This       : DWebBrowserEvents2_Event;
      szProperty : GNATCOM.Types.VARIANT);

   procedure BeforeNavigate2
     (This            : DWebBrowserEvents2_Event;
      pDisp           : GNATCOM.Types.VARIANT;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT);

   procedure NewWindow2
     (This   : DWebBrowserEvents2_Event;
      ppDisp : GNATCOM.Types.VARIANT;
      Cancel : GNATCOM.Types.VARIANT);

   procedure NavigateComplete2
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT);

   procedure DocumentComplete
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT);

   procedure OnQuit
     (This : DWebBrowserEvents2_Event);

   procedure OnVisible
     (This    : DWebBrowserEvents2_Event;
      Visible : GNATCOM.Types.VARIANT);

   procedure OnToolBar
     (This    : DWebBrowserEvents2_Event;
      ToolBar : GNATCOM.Types.VARIANT);

   procedure OnMenuBar
     (This    : DWebBrowserEvents2_Event;
      MenuBar : GNATCOM.Types.VARIANT);

   procedure OnStatusBar
     (This      : DWebBrowserEvents2_Event;
      StatusBar : GNATCOM.Types.VARIANT);

   procedure OnFullScreen
     (This       : DWebBrowserEvents2_Event;
      FullScreen : GNATCOM.Types.VARIANT);

   procedure OnTheaterMode
     (This        : DWebBrowserEvents2_Event;
      TheaterMode : GNATCOM.Types.VARIANT);

   procedure WindowSetResizable
     (This      : DWebBrowserEvents2_Event;
      Resizable : GNATCOM.Types.VARIANT);

   procedure WindowSetLeft
     (This : DWebBrowserEvents2_Event;
      Left : GNATCOM.Types.VARIANT);

   procedure WindowSetTop
     (This : DWebBrowserEvents2_Event;
      Top  : GNATCOM.Types.VARIANT);

   procedure WindowSetWidth
     (This  : DWebBrowserEvents2_Event;
      Width : GNATCOM.Types.VARIANT);

   procedure WindowSetHeight
     (This   : DWebBrowserEvents2_Event;
      Height : GNATCOM.Types.VARIANT);

   procedure WindowClosing
     (This          : DWebBrowserEvents2_Event;
      IsChildWindow : GNATCOM.Types.VARIANT;
      Cancel        : GNATCOM.Types.VARIANT);

   procedure ClientToHostWindow
     (This : DWebBrowserEvents2_Event;
      CX   : GNATCOM.Types.VARIANT;
      CY   : GNATCOM.Types.VARIANT);

   procedure SetSecureLockIcon
     (This           : DWebBrowserEvents2_Event;
      SecureLockIcon : GNATCOM.Types.VARIANT);

   procedure FileDownload
     (This           : DWebBrowserEvents2_Event;
      ActiveDocument : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT);

   procedure NavigateError
     (This       : DWebBrowserEvents2_Event;
      pDisp      : GNATCOM.Types.VARIANT;
      URL        : GNATCOM.Types.VARIANT;
      Frame      : GNATCOM.Types.VARIANT;
      StatusCode : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT);

   procedure PrintTemplateInstantiation
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT);

   procedure PrintTemplateTeardown
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT);

   procedure UpdatePageStatus
     (This  : DWebBrowserEvents2_Event;
      pDisp : GNATCOM.Types.VARIANT;
      nPage : GNATCOM.Types.VARIANT;
      fDone : GNATCOM.Types.VARIANT);

   procedure PrivacyImpactedStateChange
     (This      : DWebBrowserEvents2_Event;
      bImpacted : GNATCOM.Types.VARIANT);

   procedure NewWindow3
     (This           : DWebBrowserEvents2_Event;
      ppDisp         : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      dwFlags        : GNATCOM.Types.VARIANT;
      bstrUrlContext : GNATCOM.Types.VARIANT;
      bstrUrl        : GNATCOM.Types.VARIANT);

   procedure SetPhishingFilterStatus
     (This                 : DWebBrowserEvents2_Event;
      PhishingFilterStatus : GNATCOM.Types.VARIANT);

   procedure WindowStateChanged
     (This               : DWebBrowserEvents2_Event;
      dwWindowStateFlags : GNATCOM.Types.VARIANT;
      dwValidFlagsMask   : GNATCOM.Types.VARIANT);

   procedure NewProcess
     (This       : DWebBrowserEvents2_Event;
      lCauseFlag : GNATCOM.Types.VARIANT;
      pWB2       : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT);

   procedure ThirdPartyUrlBlocked
     (This    : DWebBrowserEvents2_Event;
      URL     : GNATCOM.Types.VARIANT;
      dwCount : GNATCOM.Types.VARIANT);

   procedure RedirectXDomainBlocked
     (This        : DWebBrowserEvents2_Event;
      pDisp       : GNATCOM.Types.VARIANT;
      StartURL    : GNATCOM.Types.VARIANT;
      RedirectURL : GNATCOM.Types.VARIANT;
      Frame       : GNATCOM.Types.VARIANT;
      StatusCode  : GNATCOM.Types.VARIANT);

end IE.DWebBrowserEvents2_Events;
