with GNATCOM.Dispinterface;

package IE.IWebBrowser_Object is

   type IWebBrowser_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure GoBack
     (This : IWebBrowser_Type);
   --  Navigates to the previous item in the history list.

   procedure GoForward
     (This : IWebBrowser_Type);
   --  Navigates to the next item in the history list.

   procedure GoHome
     (This : IWebBrowser_Type);
   --  Go home/start page.

   procedure GoSearch
     (This : IWebBrowser_Type);
   --  Go Search Page.

   procedure Navigate
     (This            : IWebBrowser_Type;
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
     (This : IWebBrowser_Type);
   --  Refresh the currently viewed page.

   procedure Refresh2
     (This  : IWebBrowser_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True);
   --  Refresh the currently viewed page.

   procedure Stop
     (This : IWebBrowser_Type);
   --  Stops opening a file.

   function Get_Application
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the application automation object if accessible, this automation
   --  object otherwise..

   function Get_Parent
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the automation object of the container/parent if one exists or
   --  this automation object.

   function Get_Container
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the container/parent automation object, if any.

   function Get_Document
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the active Document automation object, if any.

   function Get_TopLevelContainer
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns True if this is the top level object.

   function Get_Type
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns the type of the contained document object.

   function Get_Left
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Left
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Top
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Top
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Width
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  The horizontal dimension (pixels) of the frame window/object.

   procedure Put_Width
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The horizontal dimension (pixels) of the frame window/object.

   function Get_Height
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  The vertical dimension (pixels) of the frame window/object.

   procedure Put_Height
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  The vertical dimension (pixels) of the frame window/object.

   function Get_LocationName
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the short (UI-friendly) name of the URL/file currently viewed.

   function Get_LocationURL
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Gets the full URL/path currently viewed.

   function Get_Busy
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT;
   --  Query to see if something is still in progress.

end IE.IWebBrowser_Object;

