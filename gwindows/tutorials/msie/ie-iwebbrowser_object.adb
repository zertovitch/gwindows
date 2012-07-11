package body IE.IWebBrowser_Object is

   procedure GoBack
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_GoBack);
   end GoBack;

   procedure GoForward
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_GoForward);
   end GoForward;

   procedure GoHome
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_GoHome);
   end GoHome;

   procedure GoSearch
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_GoSearch);
   end GoSearch;

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
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser_Navigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Headers,
          2 => PostData,
          3 => TargetFrameName,
          4 => Flags,
          5 => URL),
         Free);
   end Navigate;

   procedure Refresh
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_Refresh);
   end Refresh;

   procedure Refresh2
     (This  : IWebBrowser_Type;
      Level : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IWebBrowser_Refresh2,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Level),
         Free);
   end Refresh2;

   procedure Stop
     (This : IWebBrowser_Type)
   is
   begin
      Invoke (This, IWebBrowser_Stop);
   end Stop;

   function Get_Application
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Application);
   end Get_Application;

   function Get_Parent
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Parent);
   end Get_Parent;

   function Get_Container
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Container);
   end Get_Container;

   function Get_Document
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Document);
   end Get_Document;

   function Get_TopLevelContainer
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_TopLevelContainer);
   end Get_TopLevelContainer;

   function Get_Type
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Type);
   end Get_Type;

   function Get_Left
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Left);
   end Get_Left;

   procedure Put_Left
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser_Put_Left,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Left;

   function Get_Top
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Top);
   end Get_Top;

   procedure Put_Top
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser_Put_Top,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Top;

   function Get_Width
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Width);
   end Get_Width;

   procedure Put_Width
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser_Put_Width,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Width;

   function Get_Height
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Height);
   end Get_Height;

   procedure Put_Height
     (This : IWebBrowser_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IWebBrowser_Put_Height,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Height;

   function Get_LocationName
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_LocationName);
   end Get_LocationName;

   function Get_LocationURL
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_LocationURL);
   end Get_LocationURL;

   function Get_Busy
     (This : IWebBrowser_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IWebBrowser_Get_Busy);
   end Get_Busy;

end IE.IWebBrowser_Object;

