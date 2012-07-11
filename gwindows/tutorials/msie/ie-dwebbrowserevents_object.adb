package body IE.DWebBrowserEvents_Object is

   procedure BeforeNavigate
     (This            : DWebBrowserEvents_Type;
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
         DWebBrowserEvents_BeforeNavigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => Headers,
          3 => PostData,
          4 => TargetFrameName,
          5 => Flags,
          6 => URL),
         Free);
   end BeforeNavigate;

   procedure NavigateComplete
     (This : DWebBrowserEvents_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_NavigateComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end NavigateComplete;

   procedure StatusTextChange
     (This : DWebBrowserEvents_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_StatusTextChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Text),
         Free);
   end StatusTextChange;

   procedure ProgressChange
     (This        : DWebBrowserEvents_Type;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_ProgressChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => ProgressMax,
          2 => Progress),
         Free);
   end ProgressChange;

   procedure DownloadComplete
     (This : DWebBrowserEvents_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents_DownloadComplete);
   end DownloadComplete;

   procedure CommandStateChange
     (This    : DWebBrowserEvents_Type;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_CommandStateChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Enable,
          2 => Command),
         Free);
   end CommandStateChange;

   procedure DownloadBegin
     (This : DWebBrowserEvents_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents_DownloadBegin);
   end DownloadBegin;

   procedure NewWindow
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_NewWindow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Processed,
          2 => Headers,
          3 => PostData,
          4 => TargetFrameName,
          5 => Flags,
          6 => URL),
         Free);
   end NewWindow;

   procedure TitleChange
     (This : DWebBrowserEvents_Type;
      Text : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_TitleChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Text),
         Free);
   end TitleChange;

   procedure FrameBeforeNavigate
     (This            : DWebBrowserEvents_Type;
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
         DWebBrowserEvents_FrameBeforeNavigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel,
          2 => Headers,
          3 => PostData,
          4 => TargetFrameName,
          5 => Flags,
          6 => URL),
         Free);
   end FrameBeforeNavigate;

   procedure FrameNavigateComplete
     (This : DWebBrowserEvents_Type;
      URL  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_FrameNavigateComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => URL),
         Free);
   end FrameNavigateComplete;

   procedure FrameNewWindow
     (This            : DWebBrowserEvents_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_FrameNewWindow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Processed,
          2 => Headers,
          3 => PostData,
          4 => TargetFrameName,
          5 => Flags,
          6 => URL),
         Free);
   end FrameNewWindow;

   procedure Quit
     (This   : DWebBrowserEvents_Type;
      Cancel : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_Quit,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Cancel),
         Free);
   end Quit;

   procedure WindowMove
     (This : DWebBrowserEvents_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents_WindowMove);
   end WindowMove;

   procedure WindowResize
     (This : DWebBrowserEvents_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents_WindowResize);
   end WindowResize;

   procedure WindowActivate
     (This : DWebBrowserEvents_Type)
   is
   begin
      Invoke (This, DWebBrowserEvents_WindowActivate);
   end WindowActivate;

   procedure PropertyChange
     (This     : DWebBrowserEvents_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         DWebBrowserEvents_PropertyChange,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Property),
         Free);
   end PropertyChange;

end IE.DWebBrowserEvents_Object;

