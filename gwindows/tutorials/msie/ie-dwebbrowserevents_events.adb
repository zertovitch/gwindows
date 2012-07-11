package body IE.DWebBrowserEvents_Events is

   procedure Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
   begin
      case dispidMember is
         when DWebBrowserEvents_BeforeNavigate=>
            BeforeNavigate
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_NavigateComplete=>
            NavigateComplete
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_StatusTextChange=>
            StatusTextChange
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_ProgressChange=>
            ProgressChange
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_DownloadComplete=>
            DownloadComplete
              (DWebBrowserEvents_Event'Class (Event_Object.all));
         when DWebBrowserEvents_CommandStateChange=>
            CommandStateChange
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_DownloadBegin=>
            DownloadBegin
              (DWebBrowserEvents_Event'Class (Event_Object.all));
         when DWebBrowserEvents_NewWindow=>
            NewWindow
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_TitleChange=>
            TitleChange
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_FrameBeforeNavigate=>
            FrameBeforeNavigate
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_FrameNavigateComplete=>
            FrameNavigateComplete
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_FrameNewWindow=>
            FrameNewWindow
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_Quit=>
            Quit
              (DWebBrowserEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DWebBrowserEvents_WindowMove=>
            WindowMove
              (DWebBrowserEvents_Event'Class (Event_Object.all));
         when DWebBrowserEvents_WindowResize=>
            WindowResize
              (DWebBrowserEvents_Event'Class (Event_Object.all));
         when DWebBrowserEvents_WindowActivate=>
            WindowActivate
              (DWebBrowserEvents_Event'Class (Event_Object.all));
         when DWebBrowserEvents_PropertyChange=>
            PropertyChange
              (DWebBrowserEvents_Event'Class (Event_Object.all),
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
         IID_DWebBrowserEvents,
         From);
   end Create;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.IInterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Events.Set_Events
        (This,
         For_Object,
         IID_DWebBrowserEvents,
         Event_Interface,
         Free);
   end Set_Events;

   procedure BeforeNavigate
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end BeforeNavigate;

   procedure NavigateComplete
     (This : DWebBrowserEvents_Event;
      URL  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NavigateComplete;

   procedure StatusTextChange
     (This : DWebBrowserEvents_Event;
      Text : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end StatusTextChange;

   procedure ProgressChange
     (This        : DWebBrowserEvents_Event;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ProgressChange;

   procedure DownloadComplete
     (This : DWebBrowserEvents_Event)
   is
   begin
      null;
   end DownloadComplete;

   procedure CommandStateChange
     (This    : DWebBrowserEvents_Event;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end CommandStateChange;

   procedure DownloadBegin
     (This : DWebBrowserEvents_Event)
   is
   begin
      null;
   end DownloadBegin;

   procedure NewWindow
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end NewWindow;

   procedure TitleChange
     (This : DWebBrowserEvents_Event;
      Text : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end TitleChange;

   procedure FrameBeforeNavigate
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FrameBeforeNavigate;

   procedure FrameNavigateComplete
     (This : DWebBrowserEvents_Event;
      URL  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FrameNavigateComplete;

   procedure FrameNewWindow
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FrameNewWindow;

   procedure Quit
     (This   : DWebBrowserEvents_Event;
      Cancel : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end Quit;

   procedure WindowMove
     (This : DWebBrowserEvents_Event)
   is
   begin
      null;
   end WindowMove;

   procedure WindowResize
     (This : DWebBrowserEvents_Event)
   is
   begin
      null;
   end WindowResize;

   procedure WindowActivate
     (This : DWebBrowserEvents_Event)
   is
   begin
      null;
   end WindowActivate;

   procedure PropertyChange
     (This     : DWebBrowserEvents_Event;
      Property : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end PropertyChange;

end IE.DWebBrowserEvents_Events;

