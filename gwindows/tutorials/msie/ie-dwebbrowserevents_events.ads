with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
with GNATCOM.IInterface;

package IE.DWebBrowserEvents_Events is

   type DWebBrowserEvents_Event is
     new GNATCOM.Events.Event_Object.Event_Type with null record;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.IInterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);

   procedure BeforeNavigate
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT);

   procedure NavigateComplete
     (This : DWebBrowserEvents_Event;
      URL  : GNATCOM.Types.VARIANT);

   procedure StatusTextChange
     (This : DWebBrowserEvents_Event;
      Text : GNATCOM.Types.VARIANT);

   procedure ProgressChange
     (This        : DWebBrowserEvents_Event;
      Progress    : GNATCOM.Types.VARIANT;
      ProgressMax : GNATCOM.Types.VARIANT);

   procedure DownloadComplete
     (This : DWebBrowserEvents_Event);

   procedure CommandStateChange
     (This    : DWebBrowserEvents_Event;
      Command : GNATCOM.Types.VARIANT;
      Enable  : GNATCOM.Types.VARIANT);

   procedure DownloadBegin
     (This : DWebBrowserEvents_Event);

   procedure NewWindow
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT);

   procedure TitleChange
     (This : DWebBrowserEvents_Event;
      Text : GNATCOM.Types.VARIANT);

   procedure FrameBeforeNavigate
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Cancel          : GNATCOM.Types.VARIANT);

   procedure FrameNavigateComplete
     (This : DWebBrowserEvents_Event;
      URL  : GNATCOM.Types.VARIANT);

   procedure FrameNewWindow
     (This            : DWebBrowserEvents_Event;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT;
      TargetFrameName : GNATCOM.Types.VARIANT;
      PostData        : GNATCOM.Types.VARIANT;
      Headers         : GNATCOM.Types.VARIANT;
      Processed       : GNATCOM.Types.VARIANT);

   procedure Quit
     (This   : DWebBrowserEvents_Event;
      Cancel : GNATCOM.Types.VARIANT);

   procedure WindowMove
     (This : DWebBrowserEvents_Event);

   procedure WindowResize
     (This : DWebBrowserEvents_Event);

   procedure WindowActivate
     (This : DWebBrowserEvents_Event);

   procedure PropertyChange
     (This     : DWebBrowserEvents_Event;
      Property : GNATCOM.Types.VARIANT);

end IE.DWebBrowserEvents_Events;

