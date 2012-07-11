with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body IE.IWebBrowser_Interface is

   procedure Initialize (This : in out IWebBrowser_Type) is
   begin
      Set_IID (This, IID_IWebBrowser);
   end Initialize;

   function Pointer (This : IWebBrowser_Type)
     return Pointer_To_IWebBrowser
   is
   begin
      return To_Pointer_To_IWebBrowser (Address (This));
   end Pointer;

   procedure Attach (This    : in out IWebBrowser_Type;
                     Pointer : in     Pointer_To_IWebBrowser)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GoBack
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoBack
         (Pointer (This)));

   end GoBack;

   procedure GoForward
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoForward
         (Pointer (This)));

   end GoForward;

   procedure GoHome
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoHome
         (Pointer (This)));

   end GoHome;

   procedure GoSearch
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GoSearch
         (Pointer (This)));

   end GoSearch;

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
               GNATCOM.Iinterface.Free (URL);
      end if;

   end Navigate;

   procedure Refresh
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   procedure Refresh2
     (This  : IWebBrowser_Type;
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
     (This : IWebBrowser_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Stop
         (Pointer (This)));

   end Stop;

   function Get_Application
     (This   : IWebBrowser_Type)
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
     (This   : IWebBrowser_Type)
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
     (This   : IWebBrowser_Type)
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
     (This   : IWebBrowser_Type)
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
     (This  : IWebBrowser_Type)
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
     (This  : IWebBrowser_Type)
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
     (This : IWebBrowser_Type)
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
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Left
         (Pointer (This),
          pl));

   end Put_Left;

   function Get_Top
     (This : IWebBrowser_Type)
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
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Top
         (Pointer (This),
          pl));

   end Put_Top;

   function Get_Width
     (This : IWebBrowser_Type)
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
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Width
         (Pointer (This),
          pl));

   end Put_Width;

   function Get_Height
     (This : IWebBrowser_Type)
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
     (This : IWebBrowser_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Height
         (Pointer (This),
          pl));

   end Put_Height;

   function Get_LocationName
     (This         : IWebBrowser_Type)
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
     (This        : IWebBrowser_Type)
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
     (This  : IWebBrowser_Type)
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

end IE.IWebBrowser_Interface;
