package body IE.IWebBrowser2_Object is

   procedure GoBack
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_GoBack);
   end GoBack;

   procedure GoForward
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_GoForward);
   end GoForward;

   procedure GoHome
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_GoHome);
   end GoHome;

   procedure GoSearch
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_GoSearch);
   end GoSearch;

   procedure Navigate
     (This            : IWebBrowser2_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      TargetFrameName : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      PostData        : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Headers         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_Navigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Headers,
          2 => PostData,
          3 => TargetFrameName,
          4 => Flags,
          5 => URL),
         Free);
   end Navigate;

   procedure Refresh
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_Refresh);
   end Refresh;

   procedure Refresh2
     (This  : IWebBrowser2_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_Refresh2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Level),
         Free);
   end Refresh2;

   procedure Stop
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_Stop);
   end Stop;

   function Get_Application
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Application);
   end Get_Application;

   function Get_Parent
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Parent);
   end Get_Parent;

   function Get_Container
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Container);
   end Get_Container;

   function Get_Document
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Document);
   end Get_Document;

   function Get_TopLevelContainer
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_TopLevelContainer);
   end Get_TopLevelContainer;

   function Get_Type
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Type);
   end Get_Type;

   function Get_Left
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Left);
   end Get_Left;

   procedure Put_Left
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Left,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Left;

   function Get_Top
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Top);
   end Get_Top;

   procedure Put_Top
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Top,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Top;

   function Get_Width
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Width);
   end Get_Width;

   procedure Put_Width
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Width,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Width;

   function Get_Height
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Height);
   end Get_Height;

   procedure Put_Height
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Height,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Height;

   function Get_LocationName
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_LocationName);
   end Get_LocationName;

   function Get_LocationURL
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_LocationURL);
   end Get_LocationURL;

   function Get_Busy
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Busy);
   end Get_Busy;

   procedure Quit
     (This : IWebBrowser2_Type)
   is
   begin
      Invoke (This, IWebBrowser2_Quit);
   end Quit;

   procedure ClientToWindow
     (This : IWebBrowser2_Type;
      pcx  : GNATCOM.Types.VARIANT;
      pcy  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_ClientToWindow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pcy,
          2 => pcx),
         Free);
   end ClientToWindow;

   procedure PutProperty
     (This     : IWebBrowser2_Type;
      Property : GNATCOM.Types.VARIANT;
      vtValue  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_PutProperty,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => vtValue,
          2 => Property),
         Free);
   end PutProperty;

   function GetProperty
     (This     : IWebBrowser2_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IWebBrowser2_GetProperty,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Property),
         Free);
   end GetProperty;

   function Get_Name
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Name);
   end Get_Name;

   function Get_HWND
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_HWND);
   end Get_HWND;

   function Get_FullName
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_FullName);
   end Get_FullName;

   function Get_Path
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Path);
   end Get_Path;

   function Get_Visible
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Visible);
   end Get_Visible;

   procedure Put_Visible
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Visible,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Visible;

   function Get_StatusBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_StatusBar);
   end Get_StatusBar;

   procedure Put_StatusBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_StatusBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StatusBar;

   function Get_StatusText
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_StatusText);
   end Get_StatusText;

   procedure Put_StatusText
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_StatusText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StatusText;

   function Get_ToolBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_ToolBar);
   end Get_ToolBar;

   procedure Put_ToolBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_ToolBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ToolBar;

   function Get_MenuBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_MenuBar);
   end Get_MenuBar;

   procedure Put_MenuBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_MenuBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MenuBar;

   function Get_FullScreen
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_FullScreen);
   end Get_FullScreen;

   procedure Put_FullScreen
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_FullScreen,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_FullScreen;

   procedure Navigate2
     (This            : IWebBrowser2_Type;
      URL             : GNATCOM.Types.VARIANT;
      Flags           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      TargetFrameName : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      PostData        : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Headers         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_Navigate2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Headers,
          2 => PostData,
          3 => TargetFrameName,
          4 => Flags,
          5 => URL),
         Free);
   end Navigate2;

   function QueryStatusWB
     (This  : IWebBrowser2_Type;
      cmdID : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IWebBrowser2_QueryStatusWB,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => cmdID),
         Free);
   end QueryStatusWB;

   procedure ExecWB
     (This       : IWebBrowser2_Type;
      cmdID      : GNATCOM.Types.VARIANT;
      cmdexecopt : GNATCOM.Types.VARIANT;
      pvaIn      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      pvaOut     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_ExecWB,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvaOut,
          2 => pvaIn,
          3 => cmdexecopt,
          4 => cmdID),
         Free);
   end ExecWB;

   procedure ShowBrowserBar
     (This     : IWebBrowser2_Type;
      pvaClsid : GNATCOM.Types.VARIANT;
      pvarShow : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      pvarSize : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser2_ShowBrowserBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarSize,
          2 => pvarShow,
          3 => pvaClsid),
         Free);
   end ShowBrowserBar;

   function Get_ReadyState
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_ReadyState);
   end Get_ReadyState;

   function Get_Offline
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Offline);
   end Get_Offline;

   procedure Put_Offline
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Offline,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Offline;

   function Get_Silent
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Silent);
   end Get_Silent;

   procedure Put_Silent
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Silent,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Silent;

   function Get_RegisterAsBrowser
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_RegisterAsBrowser);
   end Get_RegisterAsBrowser;

   procedure Put_RegisterAsBrowser
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_RegisterAsBrowser,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_RegisterAsBrowser;

   function Get_RegisterAsDropTarget
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_RegisterAsDropTarget);
   end Get_RegisterAsDropTarget;

   procedure Put_RegisterAsDropTarget
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_RegisterAsDropTarget,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_RegisterAsDropTarget;

   function Get_TheaterMode
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_TheaterMode);
   end Get_TheaterMode;

   procedure Put_TheaterMode
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_TheaterMode,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_TheaterMode;

   function Get_AddressBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_AddressBar);
   end Get_AddressBar;

   procedure Put_AddressBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_AddressBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AddressBar;

   function Get_Resizable
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser2_Get_Resizable);
   end Get_Resizable;

   procedure Put_Resizable
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser2_Put_Resizable,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Resizable;

end IE.IWebBrowser2_Object;

