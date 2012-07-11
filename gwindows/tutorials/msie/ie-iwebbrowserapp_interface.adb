with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IWebBrowserApp_Interface is

   procedure Initialize (This : in out IWebBrowserApp_Type) is
   begin
      Set_IID (This, IID_IWebBrowserApp);
   end Initialize;

   function Pointer (This : IWebBrowserApp_Type)
     return Pointer_To_IWebBrowserApp
   is
   begin
      return To_Pointer_To_IWebBrowserApp (Address (This));
   end Pointer;

   procedure Attach (This    : in out IWebBrowserApp_Type;
                     Pointer : in     Pointer_To_IWebBrowserApp)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GoBack
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoBack
         (Pointer (This)));

   end GoBack;

   procedure GoForward
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoForward
         (Pointer (This)));

   end GoForward;

   procedure GoHome
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoHome
         (Pointer (This)));

   end GoHome;

   procedure GoSearch
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoSearch
         (Pointer (This)));

   end GoSearch;

   procedure Navigate
     (This            : IWebBrowserApp_Type;
      URL             : GNATCOM.Types.BSTR;
      Flags           : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      TargetFrameName : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      PostData        : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Headers         : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Navigate
         (Pointer (This),
          URL,
          Flags,
          TargetFrameName,
          PostData,
          Headers));

      if Free then
               GNATCOM.IInterface.Free (URL);
      
      end if;

   end Navigate;

   procedure Refresh
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   procedure Refresh2
     (This  : IWebBrowserApp_Type;
      Level : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh2
         (Pointer (This),
          Level));

   end Refresh2;

   procedure Stop
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Stop
         (Pointer (This)));

   end Stop;

   function Get_Application
     (This   : IWebBrowserApp_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Application
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Application;

   function Get_Parent
     (This   : IWebBrowserApp_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Parent
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Parent;

   function Get_Container
     (This   : IWebBrowserApp_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Container
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Container;

   function Get_Document
     (This   : IWebBrowserApp_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Document
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Document;

   function Get_TopLevelContainer
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_TopLevelContainer
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_TopLevelContainer;

   function Get_Type
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Type
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Type;

   function Get_Left
     (This : IWebBrowserApp_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Left
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Left;

   procedure Put_Left
     (This : IWebBrowserApp_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Left
         (Pointer (This),
          pl));

   end Put_Left;

   function Get_Top
     (This : IWebBrowserApp_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Top
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Top;

   procedure Put_Top
     (This : IWebBrowserApp_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Top
         (Pointer (This),
          pl));

   end Put_Top;

   function Get_Width
     (This : IWebBrowserApp_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Width
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Width;

   procedure Put_Width
     (This : IWebBrowserApp_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Width
         (Pointer (This),
          pl));

   end Put_Width;

   function Get_Height
     (This : IWebBrowserApp_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Height
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Height;

   procedure Put_Height
     (This : IWebBrowserApp_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Height
         (Pointer (This),
          pl));

   end Put_Height;

   function Get_LocationName
     (This         : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LocationName
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LocationName;

   function Get_LocationURL
     (This        : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LocationURL
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LocationURL;

   function Get_Busy
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Busy
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Busy;

   procedure Quit
     (This : IWebBrowserApp_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Quit
         (Pointer (This)));

   end Quit;

   procedure ClientToWindow
     (This : IWebBrowserApp_Type;
      pcx  : GNATCOM.Types.Pointer_To_int;
      pcy  : GNATCOM.Types.Pointer_To_int)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ClientToWindow
         (Pointer (This),
          pcx,
          pcy));

   end ClientToWindow;

   procedure PutProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.BSTR;
      vtValue  : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PutProperty
         (Pointer (This),
          Property,
          vtValue));

      if Free then
               GNATCOM.IInterface.Free (Property);
               GNATCOM.IInterface.Free (vtValue);
      
      end if;

   end PutProperty;

   function GetProperty
     (This     : IWebBrowserApp_Type;
      Property : GNATCOM.Types.BSTR;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetProperty
         (Pointer (This),
          Property,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (Property);
      
      end if;

      return RetVal;
   end GetProperty;

   function Get_Name
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Name
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Name;

   function Get_HWND
     (This  : IWebBrowserApp_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_HWND
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_HWND;

   function Get_FullName
     (This     : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_FullName
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_FullName;

   function Get_Path
     (This : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Path
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Path;

   function Get_Visible
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Visible
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Visible;

   procedure Put_Visible
     (This  : IWebBrowserApp_Type;
      pBool : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Visible
         (Pointer (This),
          pBool));

   end Put_Visible;

   function Get_StatusBar
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StatusBar
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StatusBar;

   procedure Put_StatusBar
     (This  : IWebBrowserApp_Type;
      pBool : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_StatusBar
         (Pointer (This),
          pBool));

   end Put_StatusBar;

   function Get_StatusText
     (This       : IWebBrowserApp_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StatusText
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StatusText;

   procedure Put_StatusText
     (This       : IWebBrowserApp_Type;
      StatusText : GNATCOM.Types.BSTR;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_StatusText
         (Pointer (This),
          StatusText));

      if Free then
               GNATCOM.IInterface.Free (StatusText);
      
      end if;

   end Put_StatusText;

   function Get_ToolBar
     (This  : IWebBrowserApp_Type)
     return Interfaces.C.int
   is
       RetVal : aliased Interfaces.C.int;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ToolBar
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ToolBar;

   procedure Put_ToolBar
     (This  : IWebBrowserApp_Type;
      Value : Interfaces.C.int)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ToolBar
         (Pointer (This),
          Value));

   end Put_ToolBar;

   function Get_MenuBar
     (This  : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_MenuBar
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_MenuBar;

   procedure Put_MenuBar
     (This  : IWebBrowserApp_Type;
      Value : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_MenuBar
         (Pointer (This),
          Value));

   end Put_MenuBar;

   function Get_FullScreen
     (This         : IWebBrowserApp_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_FullScreen
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_FullScreen;

   procedure Put_FullScreen
     (This         : IWebBrowserApp_Type;
      pbFullScreen : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_FullScreen
         (Pointer (This),
          pbFullScreen));

   end Put_FullScreen;

end IE.IWebBrowserApp_Interface;

