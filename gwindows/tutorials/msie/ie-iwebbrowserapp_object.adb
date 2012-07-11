package body IE.IWebBrowserApp_Object is

   procedure GoBack
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_GoBack);
   end GoBack;

   procedure GoForward
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_GoForward);
   end GoForward;

   procedure GoHome
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_GoHome);
   end GoHome;

   procedure GoSearch
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_GoSearch);
   end GoSearch;

   procedure Navigate
     (This            : IWebBrowserApp_Type;
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
         IWebBrowserApp_Navigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Headers,
          2 => PostData,
          3 => TargetFrameName,
          4 => Flags,
          5 => URL),
         Free);
   end Navigate;

   procedure Refresh
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_Refresh);
   end Refresh;

   procedure Refresh2
     (This  : IWebBrowserApp_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowserApp_Refresh2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Level),
         Free);
   end Refresh2;

   procedure Stop
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_Stop);
   end Stop;

   function Get_Application
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Application);
   end Get_Application;

   function Get_Parent
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Parent);
   end Get_Parent;

   function Get_Container
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Container);
   end Get_Container;

   function Get_Document
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Document);
   end Get_Document;

   function Get_TopLevelContainer
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_TopLevelContainer);
   end Get_TopLevelContainer;

   function Get_Type
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Type);
   end Get_Type;

   function Get_Left
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Left);
   end Get_Left;

   procedure Put_Left
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_Left,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Left;

   function Get_Top
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Top);
   end Get_Top;

   procedure Put_Top
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_Top,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Top;

   function Get_Width
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Width);
   end Get_Width;

   procedure Put_Width
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_Width,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Width;

   function Get_Height
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Height);
   end Get_Height;

   procedure Put_Height
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_Height,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Height;

   function Get_LocationName
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_LocationName);
   end Get_LocationName;

   function Get_LocationURL
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_LocationURL);
   end Get_LocationURL;

   function Get_Busy
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Busy);
   end Get_Busy;

   procedure Quit
     (This : IWebBrowserApp_Type)
   is
   begin
      Invoke (This, IWebBrowserApp_Quit);
   end Quit;

   procedure ClientToWindow
     (This : IWebBrowserApp_Type;
      pcx  : GNATCOM.Types.VARIANT;
      pcy  : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowserApp_ClientToWindow,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pcy,
          2 => pcx),
         Free);
   end ClientToWindow;

   procedure PutProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.VARIANT;
      vtValue  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowserApp_PutProperty,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => vtValue,
          2 => Property),
         Free);
   end PutProperty;

   function GetProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IWebBrowserApp_GetProperty,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Property),
         Free);
   end GetProperty;

   function Get_Name
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Name);
   end Get_Name;

   function Get_HWND
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_HWND);
   end Get_HWND;

   function Get_FullName
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_FullName);
   end Get_FullName;

   function Get_Path
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Path);
   end Get_Path;

   function Get_Visible
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_Visible);
   end Get_Visible;

   procedure Put_Visible
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_Visible,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Visible;

   function Get_StatusBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_StatusBar);
   end Get_StatusBar;

   procedure Put_StatusBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_StatusBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StatusBar;

   function Get_StatusText
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_StatusText);
   end Get_StatusText;

   procedure Put_StatusText
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_StatusText,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StatusText;

   function Get_ToolBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_ToolBar);
   end Get_ToolBar;

   procedure Put_ToolBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_ToolBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ToolBar;

   function Get_MenuBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_MenuBar);
   end Get_MenuBar;

   procedure Put_MenuBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_MenuBar,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MenuBar;

   function Get_FullScreen
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowserApp_Get_FullScreen);
   end Get_FullScreen;

   procedure Put_FullScreen
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowserApp_Put_FullScreen,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_FullScreen;

end IE.IWebBrowserApp_Object;

