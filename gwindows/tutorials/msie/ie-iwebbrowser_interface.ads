with GNATCOM.Dispinterface;

package IE.IWebBrowser_Interface is

   type IWebBrowser_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IWebBrowser_Type);

   function Pointer (This : IWebBrowser_Type)
     return Pointer_To_IWebBrowser;

   procedure Attach (This    : in out IWebBrowser_Type;
                     Pointer : in     Pointer_To_IWebBrowser);

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
      URL             : GNATCOM.Types.BSTR;
      Flags           : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      TargetFrameName : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      PostData        : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Headers         : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True);
   --  Navigates to a URL or file.

   procedure Refresh
     (This : IWebBrowser_Type);
   --  Refresh the currently viewed page.

   procedure Refresh2
     (This  : IWebBrowser_Type;
      Level : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);
   --  Refresh the currently viewed page.

   procedure Stop
     (This : IWebBrowser_Type);
   --  Stops opening a file.

   function Get_Application
     (This   : IWebBrowser_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Returns the application automation object if accessible, this automation
   --  object otherwise..

   function Get_Parent
     (This   : IWebBrowser_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Returns the automation object of the container/parent if one exists or
   --  this automation object.

   function Get_Container
     (This   : IWebBrowser_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Returns the container/parent automation object, if any.

   function Get_Document
     (This   : IWebBrowser_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Returns the active Document automation object, if any.

   function Get_TopLevelContainer
     (This  : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT_BOOL;
   --  Returns True if this is the top level object.

   function Get_Type
     (This  : IWebBrowser_Type)
     return GNATCOM.Types.BSTR;
   --  Returns the type of the contained document object.

   function Get_Left
     (This : IWebBrowser_Type)
     return Interfaces.C.long;
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Left
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long);
   --  The horizontal position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Top
     (This : IWebBrowser_Type)
     return Interfaces.C.long;
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   procedure Put_Top
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long);
   --  The vertical position (pixels) of the frame window relative to the
   --  screen/container.

   function Get_Width
     (This : IWebBrowser_Type)
     return Interfaces.C.long;
   --  The horizontal dimension (pixels) of the frame window/object.

   procedure Put_Width
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long);
   --  The horizontal dimension (pixels) of the frame window/object.

   function Get_Height
     (This : IWebBrowser_Type)
     return Interfaces.C.long;
   --  The vertical dimension (pixels) of the frame window/object.

   procedure Put_Height
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long);
   --  The vertical dimension (pixels) of the frame window/object.

   function Get_LocationName
     (This         : IWebBrowser_Type)
     return GNATCOM.Types.BSTR;
   --  Gets the short (UI-friendly) name of the URL/file currently viewed.

   function Get_LocationURL
     (This        : IWebBrowser_Type)
     return GNATCOM.Types.BSTR;
   --  Gets the full URL/path currently viewed.

   function Get_Busy
     (This  : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT_BOOL;
   --  Query to see if something is still in progress.

end IE.IWebBrowser_Interface;
