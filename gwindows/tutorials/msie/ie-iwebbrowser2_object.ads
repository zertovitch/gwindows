with GNATCOM.Dispinterface;

package IE.IWebBrowser2_Object is

   type IWebBrowser2_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure GoBack
     (This : IWebBrowser2_Type);
   --  Navigates to the previous item in the history list.

   procedure GoForward
     (This : IWebBrowser2_Type);
   --  Navigates to the next item in the history list.

   procedure GoHome
     (This : IWebBrowser2_Type);
   --  Go home/start page.

   procedure GoSearch
     (This : IWebBrowser2_Type);
   --  Go Search Page.

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
      Free            : Boolean := True);
   --  Navigates to a URL or file.

   procedure Refresh
     (This : IWebBrowser2_Type);
   --  Refresh the currently viewed page.

   procedure Refresh2
     (This  : IWebBrowser2_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);
   --  Refresh the currently viewed page.

   procedure Stop
     (This : IWebBrowser2_Type);
   --  Stops opening a file.

   function Get_Application
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the application automation object if accessible, this automation
   --  object otherwise..

   function Get_Parent
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the automation object of the container/parent if one exists or
   --  this automation object.

   function Get_Container
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the container/parent automation object, if any.

   function Get_Document
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the active Document automation object, if any.

   function Get_TopLevelContainer
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns True if this is the top level object.

   function Get_Type
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the type of the contained document object.

   function Get_Left
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Left
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Top
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Top
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Width
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal dimension (pixels) of the frame window/object.

   procedure Put_Width
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal dimension (pixels) of the frame window/object.

   function Get_Height
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical dimension (pixels) of the frame window/object.

   procedure Put_Height
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical dimension (pixels) of the frame window/object.

   function Get_LocationName
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the short (UI-friendly) name of the URL/file currently viewed.

   function Get_LocationURL
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the full URL/path currently viewed.

   function Get_Busy
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Query to see if something is still in progress.

   procedure Quit
     (This : IWebBrowser2_Type);
   --  Exits application and closes the open document.

   procedure ClientToWindow
     (This : IWebBrowser2_Type;
      pcx  : GNATCOM.Types.VARIANT;
      pcy  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Converts client sizes into window sizes.

   procedure PutProperty
     (This     : IWebBrowser2_Type;
      Property : GNATCOM.Types.VARIANT;
      vtValue  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);
   --  Associates vtValue with the name szProperty in the context of the
   --  object.

   function GetProperty
     (This     : IWebBrowser2_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  Retrieve the Associated value for the property vtValue in the context of
   --  the object.

   function Get_Name
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns name of the application.

   function Get_HWND
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the HWND of the current IE window.

   function Get_FullName
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns file specification of the application, including path.

   function Get_Path
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the path to the application.

   function Get_Visible
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Determines whether the application is visible or hidden.

   procedure Put_Visible
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Determines whether the application is visible or hidden.

   function Get_StatusBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Turn on or off the statusbar.

   procedure Put_StatusBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Turn on or off the statusbar.

   function Get_StatusText
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Text of Status window.

   procedure Put_StatusText
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Text of Status window.

   function Get_ToolBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls which toolbar is shown.

   procedure Put_ToolBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls which toolbar is shown.

   function Get_MenuBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls whether menubar is shown.

   procedure Put_MenuBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls whether menubar is shown.

   function Get_FullScreen
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Maximizes window and turns off statusbar, toolbar, menubar, and
   --  titlebar.

   procedure Put_FullScreen
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Maximizes window and turns off statusbar, toolbar, menubar, and
   --  titlebar.

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
      Free            : Boolean := True);
   --  Navigates to a URL or file or pidl.

   function QueryStatusWB
     (This  : IWebBrowser2_Type;
      cmdID : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  IOleCommandTarget::QueryStatus

   procedure ExecWB
     (This       : IWebBrowser2_Type;
      cmdID      : GNATCOM.Types.VARIANT;
      cmdexecopt : GNATCOM.Types.VARIANT;
      pvaIn      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      pvaOut     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True);
   --  IOleCommandTarget::Exec

   procedure ShowBrowserBar
     (This     : IWebBrowser2_Type;
      pvaClsid : GNATCOM.Types.VARIANT;
      pvarShow : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      pvarSize : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True);
   --  Set BrowserBar to Clsid

   function Get_ReadyState
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Offline
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls if the frame is offline (read from cache)

   procedure Put_Offline
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls if the frame is offline (read from cache)

   function Get_Silent
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls if any dialog boxes can be shown

   procedure Put_Silent
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls if any dialog boxes can be shown

   function Get_RegisterAsBrowser
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Registers OC as a top-level browser (for target name resolution)

   procedure Put_RegisterAsBrowser
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Registers OC as a top-level browser (for target name resolution)

   function Get_RegisterAsDropTarget
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Registers OC as a drop target for navigation

   procedure Put_RegisterAsDropTarget
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Registers OC as a drop target for navigation

   function Get_TheaterMode
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls if the browser is in theater mode

   procedure Put_TheaterMode
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls if the browser is in theater mode

   function Get_AddressBar
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls whether address bar is shown

   procedure Put_AddressBar
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls whether address bar is shown

   function Get_Resizable
     (This : IWebBrowser2_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls whether the window is resizable

   procedure Put_Resizable
     (This : IWebBrowser2_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls whether the window is resizable

end IE.IWebBrowser2_Object;

