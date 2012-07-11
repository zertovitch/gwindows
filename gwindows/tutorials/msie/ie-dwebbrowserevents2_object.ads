with GNATCOM.Dispinterface;

package IE.DWebBrowserEvents2_Object is

   type DWebBrowserEvents2_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure StatusTextChange
     (This : DWebBrowserEvents2_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Statusbar text changed.

   procedure ProgressChange
     (This        : DWebBrowserEvents2_Type;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);
   --  Fired when download progress is updated.

   procedure CommandStateChange
     (This    : DWebBrowserEvents2_Type;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  The enabled state of a command changed.

   procedure DownloadBegin
     (This : DWebBrowserEvents2_Type);
   --  Download of a page started.

   procedure DownloadComplete
     (This : DWebBrowserEvents2_Type);
   --  Download of page complete.

   procedure TitleChange
     (This : DWebBrowserEvents2_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Document title changed.

   procedure PropertyChange
     (This       : DWebBrowserEvents2_Type;
      szProperty : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  Fired when the PutProperty method has been called.

   procedure BeforeNavigate2
     (This            : DWebBrowserEvents2_Type;
      pDisp           : GNATCOM.Types.VARIANT;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);
   --  Fired before navigate occurs in the given WebBrowser (window or frameset
   --  element). The processing of this navigation may be modified.

   procedure NewWindow2
     (This   : DWebBrowserEvents2_Type;
      ppDisp : GNATCOM.Types.VARIANT;
      Cancel : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);
   --  A new, hidden, non-navigated WebBrowser window is needed.

   procedure NavigateComplete2
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when the document being navigated to becomes visible and enters
   --  the navigation stack.

   procedure DocumentComplete
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when the document being navigated to reaches ReadyState_Complete.

   procedure OnQuit
     (This : DWebBrowserEvents2_Type);
   --  Fired when application is quiting.

   procedure OnVisible
     (This    : DWebBrowserEvents2_Type;
      Visible : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Fired when the window should be shown/hidden

   procedure OnToolBar
     (This    : DWebBrowserEvents2_Type;
      ToolBar : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Fired when the toolbar  should be shown/hidden

   procedure OnMenuBar
     (This    : DWebBrowserEvents2_Type;
      MenuBar : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Fired when the menubar should be shown/hidden

   procedure OnStatusBar
     (This      : DWebBrowserEvents2_Type;
      StatusBar : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);
   --  Fired when the statusbar should be shown/hidden

   procedure OnFullScreen
     (This       : DWebBrowserEvents2_Type;
      FullScreen : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  Fired when fullscreen mode should be on/off

   procedure OnTheaterMode
     (This        : DWebBrowserEvents2_Type;
      TheaterMode : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);
   --  Fired when theater mode should be on/off

   procedure WindowSetResizable
     (This      : DWebBrowserEvents2_Type;
      Resizable : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);
   --  Fired when the host window should allow/disallow resizing

   procedure WindowSetLeft
     (This : DWebBrowserEvents2_Type;
      Left : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Fired when the host window should change its Left coordinate

   procedure WindowSetTop
     (This : DWebBrowserEvents2_Type;
      Top  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Fired when the host window should change its Top coordinate

   procedure WindowSetWidth
     (This  : DWebBrowserEvents2_Type;
      Width : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when the host window should change its width

   procedure WindowSetHeight
     (This   : DWebBrowserEvents2_Type;
      Height : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);
   --  Fired when the host window should change its height

   procedure WindowClosing
     (This          : DWebBrowserEvents2_Type;
      IsChildWindow : GNATCOM.Types.VARIANT;
      Cancel        : GNATCOM.Types.VARIANT;
      Free          : Boolean := True);
   --  Fired when the WebBrowser is about to be closed by script

   procedure ClientToHostWindow
     (This : DWebBrowserEvents2_Type;
      CX   : GNATCOM.Types.VARIANT;
      CY   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Fired to request client sizes be converted to host window sizes

   procedure SetSecureLockIcon
     (This           : DWebBrowserEvents2_Type;
      SecureLockIcon : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);
   --  Fired to indicate the security level of the current web page contents

   procedure FileDownload
     (This           : DWebBrowserEvents2_Type;
      ActiveDocument : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);
   --  Fired to indicate the File Download dialog is opening

   procedure NavigateError
     (This       : DWebBrowserEvents2_Type;
      pDisp      : GNATCOM.Types.VARIANT;
      URL        : GNATCOM.Types.VARIANT;
      Frame      : GNATCOM.Types.VARIANT;
      StatusCode : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  Fired when a binding error occurs (window or frameset element).

   procedure PrintTemplateInstantiation
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when a print template is instantiated.

   procedure PrintTemplateTeardown
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when a print template destroyed.

   procedure UpdatePageStatus
     (This  : DWebBrowserEvents2_Type;
      pDisp : GNATCOM.Types.VARIANT;
      nPage : GNATCOM.Types.VARIANT;
      fDone : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  Fired when a page is spooled. When it is fired can be changed by a
   --  custom template.

   procedure PrivacyImpactedStateChange
     (This      : DWebBrowserEvents2_Type;
      bImpacted : GNATCOM.Types.VARIANT;
      Free      : Boolean := True);
   --  Fired when the global privacy impacted state changes

   procedure NewWindow3
     (This           : DWebBrowserEvents2_Type;
      ppDisp         : GNATCOM.Types.VARIANT;
      Cancel         : GNATCOM.Types.VARIANT;
      dwFlags        : GNATCOM.Types.VARIANT;
      bstrUrlContext : GNATCOM.Types.VARIANT;
      bstrUrl        : GNATCOM.Types.VARIANT;
      Free           : Boolean := True);
   --  A new, hidden, non-navigated WebBrowser window is needed.

   procedure SetPhishingFilterStatus
     (This                 : DWebBrowserEvents2_Type;
      PhishingFilterStatus : GNATCOM.Types.VARIANT;
      Free                 : Boolean := True);
   --  Fired to indicate the progress and status of the Phishing Filter
   --  analysis of the current web page

   procedure WindowStateChanged
     (This               : DWebBrowserEvents2_Type;
      dwWindowStateFlags : GNATCOM.Types.VARIANT;
      dwValidFlagsMask   : GNATCOM.Types.VARIANT;
      Free               : Boolean := True);
   --  Fired to indicate that the browser window's visibility or enabled state
   --  has changed.

   procedure NewProcess
     (This       : DWebBrowserEvents2_Type;
      lCauseFlag : GNATCOM.Types.VARIANT;
      pWB2       : GNATCOM.Types.VARIANT;
      Cancel     : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  A new, hidden, non-navigated process is created to handle the
   --  navigation.

   procedure ThirdPartyUrlBlocked
     (This    : DWebBrowserEvents2_Type;
      URL     : GNATCOM.Types.VARIANT;
      dwCount : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Fired when a third-party URL is blocked.

   procedure RedirectXDomainBlocked
     (This        : DWebBrowserEvents2_Type;
      pDisp       : GNATCOM.Types.VARIANT;
      StartURL    : GNATCOM.Types.VARIANT;
      RedirectURL : GNATCOM.Types.VARIANT;
      Frame       : GNATCOM.Types.VARIANT;
      StatusCode  : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);
   --  Fired when a x-domain redirect is blocked.

end IE.DWebBrowserEvents2_Object;

