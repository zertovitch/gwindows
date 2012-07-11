with GNATCOM.Dispinterface;

package IE.IWebBrowserApp_Object is

   type IWebBrowserApp_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure GoBack
     (This : IWebBrowserApp_Type);
   --  Navigates to the previous item in the history list.

   procedure GoForward
     (This : IWebBrowserApp_Type);
   --  Navigates to the next item in the history list.

   procedure GoHome
     (This : IWebBrowserApp_Type);
   --  Go home/start page.

   procedure GoSearch
     (This : IWebBrowserApp_Type);
   --  Go Search Page.

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
      Free            : Boolean := True);
   --  Navigates to a URL or file.

   procedure Refresh
     (This : IWebBrowserApp_Type);
   --  Refresh the currently viewed page.

   procedure Refresh2
     (This  : IWebBrowserApp_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);
   --  Refresh the currently viewed page.

   procedure Stop
     (This : IWebBrowserApp_Type);
   --  Stops opening a file.

   function Get_Application
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the application automation object if accessible, this automation
   --  object otherwise..

   function Get_Parent
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the automation object of the container/parent if one exists or
   --  this automation object.

   function Get_Container
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the container/parent automation object, if any.

   function Get_Document
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the active Document automation object, if any.

   function Get_TopLevelContainer
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns True if this is the top level object.

   function Get_Type
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the type of the contained document object.

   function Get_Left
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Left
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Top
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Top
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Width
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal dimension (pixels) of the frame window/object.

   procedure Put_Width
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal dimension (pixels) of the frame window/object.

   function Get_Height
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical dimension (pixels) of the frame window/object.

   procedure Put_Height
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical dimension (pixels) of the frame window/object.

   function Get_LocationName
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the short (UI-friendly) name of the URL/file currently viewed.

   function Get_LocationURL
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the full URL/path currently viewed.

   function Get_Busy
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Query to see if something is still in progress.

   procedure Quit
     (This : IWebBrowserApp_Type);
   --  Exits application and closes the open document.

   procedure ClientToWindow
     (This : IWebBrowserApp_Type;
      pcx  : GNATCOM.Types.VARIANT;
      pcy  : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Converts client sizes into window sizes.

   procedure PutProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.VARIANT;
      vtValue  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);
   --  Associates vtValue with the name szProperty in the context of the
   --  object.

   function GetProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  Retrieve the Associated value for the property vtValue in the context of
   --  the object.

   function Get_Name
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns name of the application.

   function Get_HWND
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the HWND of the current IE window.

   function Get_FullName
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns file specification of the application, including path.

   function Get_Path
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the path to the application.

   function Get_Visible
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Determines whether the application is visible or hidden.

   procedure Put_Visible
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Determines whether the application is visible or hidden.

   function Get_StatusBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Turn on or off the statusbar.

   procedure Put_StatusBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Turn on or off the statusbar.

   function Get_StatusText
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Text of Status window.

   procedure Put_StatusText
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Text of Status window.

   function Get_ToolBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls which toolbar is shown.

   procedure Put_ToolBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls which toolbar is shown.

   function Get_MenuBar
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Controls whether menubar is shown.

   procedure Put_MenuBar
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Controls whether menubar is shown.

   function Get_FullScreen
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT;
   --  Maximizes window and turns off statusbar, toolbar, menubar, and
   --  titlebar.

   procedure Put_FullScreen
     (This : IWebBrowserApp_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  Maximizes window and turns off statusbar, toolbar, menubar, and
   --  titlebar.

end IE.IWebBrowserApp_Object;

