with GNATCOM.Dispinterface;

package IE.DWebBrowserEvents_Object is

   type DWebBrowserEvents_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure BeforeNavigate
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);
   --  Fired when a new hyperlink is being navigated to.

   procedure NavigateComplete
     (This : DWebBrowserEvents_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Fired when the document being navigated to becomes visible and enters
   --  the navigation stack.

   procedure StatusTextChange
     (This : DWebBrowserEvents_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Statusbar text changed.

   procedure ProgressChange
     (This        : DWebBrowserEvents_Type;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);
   --  Fired when download progress is updated.

   procedure DownloadComplete
     (This : DWebBrowserEvents_Type);
   --  Download of page complete.

   procedure CommandStateChange
     (This    : DWebBrowserEvents_Type;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  The enabled state of a command changed

   procedure DownloadBegin
     (This : DWebBrowserEvents_Type);
   --  Download of a page started.

   procedure NewWindow
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);
   --  Fired when a new window should be created.

   procedure TitleChange
     (This : DWebBrowserEvents_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Document title changed.

   procedure FrameBeforeNavigate
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);
   --  Fired when a new hyperlink is being navigated to in a frame.

   procedure FrameNavigateComplete
     (This : DWebBrowserEvents_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Fired when a new hyperlink is being navigated to in a frame.

   procedure FrameNewWindow
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT;
      Free            : Boolean := True);
   --  Fired when a new window should be created.

   procedure Quit
     (This   : DWebBrowserEvents_Type;
      Cancel : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);
   --  Fired when application is quiting.

   procedure WindowMove
     (This : DWebBrowserEvents_Type);
   --  Fired when window has been moved.

   procedure WindowResize
     (This : DWebBrowserEvents_Type);
   --  Fired when window has been sized.

   procedure WindowActivate
     (This : DWebBrowserEvents_Type);
   --  Fired when window has been activated.

   procedure PropertyChange
     (This     : DWebBrowserEvents_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);
   --  Fired when the PutProperty method has been called.

end IE.DWebBrowserEvents_Object;

