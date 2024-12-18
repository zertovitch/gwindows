with Ada.Strings.Fixed;

with GWindows.GStrings;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Core.Documents;

with GWindows.GStrings.Unbounded;

package body GNAVI_Window is

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   function Handler (Control : Control_Element;
                     Index   : Positive)
                    return DOM.Core.Node;
   --  Returns node for Handler at Index of Control

   function Init_Property (Control : Control_Element;
                           Index   : Positive)
                          return DOM.Core.Node;
   --  Returns node for Property at Index of Control

   -------------------------------------------------------------------------
   --  Local Body
   -------------------------------------------------------------------------

   function Handler (Control : Control_Element;
                     Index   : Positive)
                    return DOM.Core.Node
   is
      use DOM.Core;

      N  : constant Node := GNAVI_XML.Get_Child_Node (Control, "handlers");
      NL : Node_List;
   begin
      if N = null then
         return null;
      end if;

      NL := Elements.Get_Elements_By_Tag_Name (N, "handler");
      return Nodes.Item (NL, Index - 1);
   end Handler;

   function Init_Property (Control : Control_Element;
                           Index   : Positive)
                          return DOM.Core.Node
   is
      use DOM.Core;

      INode : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node (Control, "init");
      N     : DOM.Core.Node;
      I     : Integer := 0;
   begin
      if INode = null then
         raise Property_Out_Of_Bounds;
      end if;

      N := Nodes.First_Child (INode);

      while N /= null loop
         if N.Node_Type = Element_Node then
            I := I + 1;

            if I = Index then
               return N;
            end if;
         end if;
         N := Nodes.Next_Sibling (N);
      end loop;

      raise Property_Out_Of_Bounds;
   end Init_Property;

   -------------------------------------------------------------------------
   --  Window
   -------------------------------------------------------------------------

   function Window_Element (Window : GNAVI_Window_Type)
                           return Control_Element
   is
   begin
      return GNAVI_XML.Get_Child_Node (Root (Window), "window");
   end Window_Element;

   function Window_Name (Window : GNAVI_Window_Type) return GWindows.GString
   is
   begin
      return Control_Name (Window_Element (Window));
   end Window_Name;

   function Window_Type (Window : GNAVI_Window_Type) return GWindows.GString
   is
   begin
      return Control_Type (Window_Element (Window));
   end Window_Type;

   function Has_Controls (Window : GNAVI_Window_Type) return Boolean
   is
   begin
      return Has_Child_Controls (Window_Element (Window));
   end Has_Controls;

   function Controls (Window : GNAVI_Window_Type) return Control_Element
   is
   begin
      return Child_Controls (Window_Element (Window));
   end Controls;

   function Has_Child_Controls (Control : Control_Element)
                               return Boolean
   is
      use DOM.Core;

      NL : DOM.Core.Element := Child_Controls (Control);
   begin
      if NL = null then
         return False;
      else
         NL := First_Control (NL);
         if NL = null then
            return False;
         end if;
      end if;

      return True;
   end Has_Child_Controls;

   function Child_Controls (Control : Control_Element)
                           return Control_Element
   is
      use DOM.Core;
      NL : DOM.Core.Node;
   begin
      if Control = null then
         return null;
      end if;

      NL := Nodes.First_Child (Control);

      while NL /= null loop
         if Nodes.Node_Name (NL) = "controls" then
            return NL;
         else
            NL := Nodes.Next_Sibling (NL);
         end if;
      end loop;

      return null;
   end Child_Controls;

   function First_Control (Controls : Control_Element)
                          return Control_Element
   is
      use DOM.Core;
      NL : DOM.Core.Node;
   begin
      if Controls = null then
         return null;
      end if;

      NL := Nodes.First_Child (Controls);

      while NL /= null loop
         if Nodes.Node_Name (NL) = "control" then
            return NL;
         else
            NL := Nodes.Next_Sibling (NL);
         end if;
      end loop;

      return null;
   end First_Control;

   function Previous_Control (Control : Control_Element)
                             return Control_Element
   is
      use DOM.Core;
      NL : DOM.Core.Node;
   begin
      if Control = null then
         return null;
      end if;

      NL := Nodes.Previous_Sibling (Control);

      while NL /= null loop
         if Nodes.Node_Name (NL) = "control" then
            return NL;
         else
            NL := Nodes.Previous_Sibling (NL);
         end if;
      end loop;

      return null;
   end Previous_Control;

   function Next_Control (Control : Control_Element)
                         return Control_Element
   is
      use DOM.Core;
      NL : DOM.Core.Node;
   begin
      if Control = null then
         return null;
      end if;

      NL := Nodes.Next_Sibling (Control);

      while NL /= null loop
         if Nodes.Node_Name (NL) = "control" then
            return NL;
         else
            NL := Nodes.Next_Sibling (NL);
         end if;
      end loop;

      return null;
   end Next_Control;

   function Parent_Control (Control : Control_Element)
                           return Control_Element
   is
      use DOM.Core;
   begin
      return Nodes.Parent_Node (Nodes.Parent_Node (Control));
   end Parent_Control;

   function Control_Name (Control : Control_Element)
                         return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Control, "name"));
   end Control_Name;

   function Control_Type (Control : Control_Element)
                         return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Control, "type"));
   end Control_Type;

   function Control (Window : GNAVI_Window_Type;
                     Index  : Positive)
                    return Control_Element
   is
      use DOM.Core;

      NL : constant Node_List :=
        Elements.Get_Elements_By_Tag_Name (Window_Element (Window), "control");
   begin
      if Index > Nodes.Length (NL) then
         return null;
      end if;

      return Nodes.Item (NL, Index - 1);
   end Control;

   procedure Add_Control (Window        : in out GNAVI_Window_Type;
                          Parent        : in     Control_Element;
                          Name          : in     GWindows.GString;
                          Control_Index : in     Positive)
   is
      use DOM.Core;
      use GWindows.GStrings;
      use GNAVI_Controls;

      New_Node      : DOM.Core.Node :=
        Documents.Create_Element (Document (Window), "control");

      Controls_Node : DOM.Core.Node := GNAVI_XML.Get_Child_Node (Parent,
                                                                 "controls");
   begin
      if Controls_Node = null then
         declare
            New_Controls_Node : constant Node :=
              Documents.Create_Element (Document (Window), "controls");
         begin
            Controls_Node := Nodes.Append_Child (Parent,
                                                 New_Controls_Node);
         end;
      end if;

      New_Node := Nodes.Append_Child (Controls_Node, New_Node);
      Elements.Set_Attribute (New_Node, "name", To_String (Name));
      Elements.Set_Attribute (New_Node, "type",
                              To_String (Control_Type (Control_Index)));

      for I in 1 .. Control_Create_Properties_Count (Control_Index) loop
         Elements.Set_Attribute
           (New_Node,
            To_String (Control_Create_Property_Name (Control_Index, I)),
            To_String (Control_Create_Property_Value (Control_Index, I)));
      end loop;

      Write (Window);
   end Add_Control;

   procedure Delete_Control (Window       : in out GNAVI_Window_Type;
                             Control      : in     Control_Element)
   is
      use DOM.Core;

      Parent        :          DOM.Core.Node := Parent_Control (Control);
      Controls_Node : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node (Parent,
                                                                 "controls");
      Control_Node  : DOM.Core.Node := Control;
   begin
      if Controls_Node /= null then
         Parent := Nodes.Remove_Child (Controls_Node, Control);
         Nodes.Free (Control_Node);
      end if;

      Write (Window);
   end Delete_Control;

   -------------------------------------------------------------------------
   --  Controls in Window - Handlers
   -------------------------------------------------------------------------

   function Handler_Count (Control : Control_Element)
                          return Natural
   is
      use DOM.Core;

      N  : constant Node := GNAVI_XML.Get_Child_Node (Control, "handlers");
      NL : Node_List;
   begin
      if N = null then
         return 0;
      end if;

      NL := Elements.Get_Elements_By_Tag_Name (N, "handler");
      return Nodes.Length (NL);
   end Handler_Count;

   function Handler_Name (Control : Control_Element;
                          Index   : Positive)
                         return GWindows.GString
   is
      use DOM.Core;
   begin
      return GWindows.GStrings.To_GString_From_String
        (Elements.Get_Attribute (Handler (Control, Index), "name"));
   end Handler_Name;

   function Handler_Type (Control : Control_Element;
                          Index   : Positive)
                         return GWindows.GString
   is
      use DOM.Core;
   begin
      return GWindows.GStrings.To_GString_From_String
        (Elements.Get_Attribute (Handler (Control, Index), "type"));
   end Handler_Type;

   function Handler_Event (Control : Control_Element;
                           Index   : Positive)
                          return GWindows.GString
   is
      use DOM.Core;
   begin
      return GWindows.GStrings.To_GString_From_String
        (Elements.Get_Attribute (Handler (Control, Index), "event"));
   end Handler_Event;

   procedure Set_Handler (Window       : in out GNAVI_Window_Type;
                          Control      : in     Control_Element;
                          Name         : in     GWindows.GString;
                          Event_Type   : in     GWindows.GString;
                          Handler_Type : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Handlers_Node    : DOM.Core.Node :=
        GNAVI_XML.Get_Child_Node (Control, "handlers");

      New_Handler_Node : DOM.Core.Node :=
        Documents.Create_Element (Document (Window), "handler");

      Replaced_Node    : DOM.Core.Node := null;
   begin
      if Handlers_Node = null then
         declare
            New_Handlers_Node : constant Node :=
              Documents.Create_Element (Document (Window), "handlers");
         begin
            Handlers_Node := Nodes.Append_Child (Control,
                                                 New_Handlers_Node);
         end;
      end if;

      for I in 1 .. Handler_Count (Control) loop
         if Handler_Event (Control, I) = Event_Type then
            Replaced_Node := Nodes.Replace_Child (Handlers_Node,
                                                  New_Handler_Node,
                                                  Handler (Control, I));
            exit;
         end if;
      end loop;

      if Replaced_Node = null then
         New_Handler_Node := Nodes.Append_Child (Handlers_Node,
                                                 New_Handler_Node);
      end if;

      Elements.Set_Attribute (New_Handler_Node, "name",
                              To_String (Name));

      Elements.Set_Attribute (New_Handler_Node, "event",
                              To_String (Event_Type));

      Elements.Set_Attribute (New_Handler_Node, "type",
                              To_String (Handler_Type));

      Write (Window);
   end Set_Handler;

   -------------------------------------------------------------------------
   --  Controls in Window - Create properties
   -------------------------------------------------------------------------

   function Create_Property_Count (Control : Control_Element)
                                  return Natural
   is
      use DOM.Core;

      NL : constant DOM.Core.Named_Node_Map := Nodes.Attributes (Control);
   begin
      return Nodes.Length (NL);
   end Create_Property_Count;

   function Create_Property_Name (Control : Control_Element;
                                  Index   : Positive)
                                 return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;

      NL : constant DOM.Core.Named_Node_Map := Nodes.Attributes (Control);
   begin
      return To_GString_From_String
        (Nodes.Node_Name (Nodes.Item (NL, Index - 1)));
   end Create_Property_Name;

   function Create_Property_Value (Control : Control_Element;
                                   Index   : Positive)
                                  return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;

      NL : constant DOM.Core.Named_Node_Map := Nodes.Attributes (Control);
   begin
      return To_GString_From_String
        (Nodes.Node_Value (Nodes.Item (NL, Index - 1)));
   end Create_Property_Value;

   procedure Set_Create_Property (Window  : in out GNAVI_Window_Type;
                                  Control : in     Control_Element;
                                  Name    : in     GWindows.GString;
                                  Value   : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;
   begin
      Elements.Set_Attribute (Control,
                              To_String (Name),
                              To_String (Value));

      Write (Window);
   end Set_Create_Property;

   -------------------------------------------------------------------------
   --  Controls in Window - Init properties
   -------------------------------------------------------------------------

   function Init_Property_Count (Control : Control_Element)
                                return Natural
   is
      use DOM.Core;

      INode : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node (Control, "init");
      N     : DOM.Core.Node;
      I     : Integer := 0;
   begin
      if INode = null then
         return 0;
      end if;

      N := Nodes.First_Child (INode);

      while N /= null loop
         if N.Node_Type = Element_Node then
            I := I + 1;
         end if;
         N := Nodes.Next_Sibling (N);
      end loop;

      return I;
   end Init_Property_Count;

   function Init_Property_Name (Control : Control_Element;
                                Index   : Positive)
                               return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;
   begin
      return
        To_GString_From_String (Nodes.Node_Name
                                (Init_Property (Control, Index)));
   end Init_Property_Name;

   function Init_Property_Value (Control : Control_Element;
                                 Index   : Positive)
                                return GWindows.GString
   is
      use DOM.Core;
      use GWindows.GStrings;
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Init_Property (Control, Index), "value"));
   end Init_Property_Value;

   procedure Set_Init_Property (Window  : in out GNAVI_Window_Type;
                                Control : in     Control_Element;
                                Name    : in     GWindows.GString;
                                Value   : in     GWindows.GString)
   is
      use DOM.Core;
      use GWindows.GStrings;

      Init_Node    : DOM.Core.Node :=
        GNAVI_XML.Get_Child_Node (Control, "init");

      New_Property_Node : DOM.Core.Node :=
        Documents.Create_Element (Document (Window), To_String (Name));

      Replaced_Node    : DOM.Core.Node := null;
   begin
      if Init_Node = null then
         declare
            New_Init_Node : constant Node :=
              Documents.Create_Element (Document (Window), "init");
         begin
            Init_Node := Nodes.Append_Child (Control,
                                             New_Init_Node);
         end;
      end if;

      for I in 1 .. Init_Property_Count (Control) loop
         if Init_Property_Name (Control, I) = Name then
            Replaced_Node := Nodes.Replace_Child (Init_Node,
                                                  New_Property_Node,
                                                  Init_Property (Control, I));
            exit;
         end if;
      end loop;

      if Replaced_Node = null then
         New_Property_Node := Nodes.Append_Child (Init_Node,
                                                  New_Property_Node);
      end if;

      Elements.Set_Attribute (New_Property_Node, "value",
                              To_String (Value));

      Write (Window);
   end Set_Init_Property;

   -------------------------------------------------------------------------
   --  Controls in Window - All Properties in one view
   -------------------------------------------------------------------------

   function All_Properties (Control : Control_Element)
                           return GNAVI_Controls.Detail_Array
   is
      use GWindows.GStrings;
      use GWindows.GStrings.Unbounded;
      use type GNAVI_Controls.Detail_Kind_Type;

      Control_ID : constant Natural :=
        GNAVI_Controls.Find_Control_By_Type (Control_Type (Control));

   begin
      if Control_ID = 0 then
         declare
            Result : GNAVI_Controls.Detail_Array (1 .. 0);
         begin
            return Result;
         end;
      else
         declare
            Result : GNAVI_Controls.Detail_Array :=
              GNAVI_Controls.Control_Properties (Control_ID);
         begin
            for N in 1 .. Result'Length loop
               if Result (N).Detail_Kind = GNAVI_Controls.Create_Property then
                  for L in 1 .. Create_Property_Count (Control) loop
                     if
                       Result (N).Detail_Name
                       =
                       Create_Property_Name (Control, L)
                     then
                        Result (N).Detail_Value :=
                          To_GString_Unbounded
                          (Create_Property_Value (Control, L));
                     end if;
                  end loop;
               else
                  for L in 1 .. Init_Property_Count (Control) loop
                     if
                       Result (N).Detail_Name
                       =
                       Init_Property_Name (Control, L)
                     then
                        Result (N).Detail_Value :=
                          To_GString_Unbounded
                          (Init_Property_Value (Control, L));
                     end if;
                  end loop;
               end if;
            end loop;

            return Result;
         end;
      end if;
   end All_Properties;

   function All_Property_Count (Control : Control_Element)
                               return Natural
   is
   begin
      return All_Properties (Control)'Length;
   end All_Property_Count;

   function All_Property_Name (Control : Control_Element;
                               Index   : Positive)
                              return GWindows.GString
   is
   begin
      return GWindows.GStrings.To_GString_From_Unbounded
        (All_Properties (Control) (Index).Detail_Name);
   end All_Property_Name;

   function All_Property_Value (Control : Control_Element;
                                Index   : Positive)
                               return GWindows.GString
   is
   begin
      return GWindows.GStrings.To_GString_From_Unbounded
        (All_Properties (Control) (Index).Detail_Value);
   end All_Property_Value;

   procedure Set_All_Property (Window  : in out GNAVI_Window_Type;
                               Control : in     Control_Element;
                               Name    : in     GWindows.GString;
                               Value   : in     GWindows.GString)
   is
      use GWindows.GStrings;
      use GWindows.GStrings.Unbounded;
      use type GNAVI_Controls.Detail_Kind_Type;

      Properties : constant GNAVI_Controls.Detail_Array := All_Properties (Control);
   begin
      for I in 1 .. Properties'Length loop
         if Properties (I).Detail_Name = Name then
            if
              Properties (I).Detail_Kind = GNAVI_Controls.Create_Property
            then
               Set_Create_Property (Window, Control, Name, Value);
               exit;
            else
               Set_Init_Property (Window, Control, Name, Value);
            end if;
         end if;
      end loop;
   end Set_All_Property;

   -------------------------------------------------------------------------
   --  Controls in Window - All Handlers in one view
   -------------------------------------------------------------------------

   function All_Handler_Count (Control : Control_Element)
                              return Natural
   is
      HA : constant GNAVI_Controls.Detail_Array := All_Handlers (Control);
   begin
      return HA'Length;
   end All_Handler_Count;

   function All_Handler_Name (Control : Control_Element;
                              Index   : Positive)
                             return GWindows.GString
   is
      HA : constant GNAVI_Controls.Detail_Array := All_Handlers (Control);
   begin
      return GWindows.GStrings.To_GString_From_Unbounded
        (HA (Index).Detail_Value);
   end All_Handler_Name;

   function All_Handler_Type (Control : Control_Element;
                              Index   : Positive)
                             return GWindows.GString
   is
      HA : constant GNAVI_Controls.Detail_Array := All_Handlers (Control);
   begin
      return GWindows.GStrings.To_GString_From_Unbounded
        (HA (Index).Detail_Type);
   end All_Handler_Type;

   function All_Handler_Event (Control : Control_Element;
                               Index   : Positive)
                              return GWindows.GString
   is
      HA : constant GNAVI_Controls.Detail_Array := All_Handlers (Control);
   begin
      return GWindows.GStrings.To_GString_From_Unbounded
        (HA (Index).Detail_Name);
   end All_Handler_Event;

   procedure Set_All_Handler (Window       : in out GNAVI_Window_Type;
                              Control      : in     Control_Element;
                              Name         : in     GWindows.GString;
                              Event_Type   : in     GWindows.GString;
                              Handler_Type : in     GWindows.GString)
   is
      use DOM.Core;
   begin
      if Name /= "" then
         Set_Handler (Window, Control, Name, Event_Type, Handler_Type);
      else
         declare
            Handlers_Node : DOM.Core.Node :=
              GNAVI_XML.Get_Child_Node (Control, "handlers");

            Handler_Node  : DOM.Core.Node;
         begin
            if Handlers_Node = null then
               return;
            else
               for I in 1 .. Handler_Count (Control) loop
                  if Handler_Event (Control, I) = Event_Type then
                     Handler_Node := Handler (Control, I);

                     Handlers_Node :=
                       Nodes.Remove_Child (Handlers_Node,
                                           Handler_Node);

                     Nodes.Free (Handler_Node);
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Set_All_Handler;

   function All_Handlers (Control : Control_Element)
                         return GNAVI_Controls.Detail_Array
   is
      use GWindows.GStrings;
      use GWindows.GStrings.Unbounded;

      Control_ID : constant Natural :=
        GNAVI_Controls.Find_Control_By_Type (Control_Type (Control));
   begin
      if Control_ID = 0 then
         declare
            R : GNAVI_Controls.Detail_Array (1 .. Handler_Count (Control));
         begin
            for N in 1 .. Handler_Count (Control) loop
               R (N).Detail_Name  :=
                 To_GString_Unbounded (Handler_Event (Control, N));
               R (N).Detail_Value :=
                 To_GString_Unbounded (Handler_Name (Control, N));
               R (N).Detail_Type  :=
                 To_GString_Unbounded (Handler_Type (Control, N));
               R (N).Detail_Kind  := GNAVI_Controls.Event;
            end loop;

            return R;
         end;
      else
         declare
            Result : GNAVI_Controls.Detail_Array :=
              GNAVI_Controls.Control_Events (Control_ID);
         begin
            for N in 1 .. Result'Length loop
               for L in 1 .. Handler_Count (Control) loop
                  if Result (N).Detail_Name = Handler_Event (Control, L) then
                     Result (N).Detail_Value :=
                       To_GString_Unbounded (Handler_Name (Control, L));
                  end if;
               end loop;
            end loop;

            return Result;
         end;
      end if;
   end All_Handlers;

   function Copy_Control (Control : in Control_Element)
                         return Control_Element
   is
      use DOM.Core;
   begin
      return Nodes.Clone_Node (Control, True);
   end Copy_Control;

   procedure Cut_Control (Window  : in out GNAVI_Window_Type;
                          Control : in     Control_Element;
                          Result  :    out Control_Element)
   is
      use DOM.Core;

      Parent        : constant DOM.Core.Node := Parent_Control (Control);
      Controls_Node : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node (Parent,
                                                                 "controls");
      --  Control_Node  : DOM.Core.Node := Control;
   begin
      if Controls_Node /= null then
         Result := Nodes.Remove_Child (Controls_Node, Control);
      else
         Result := null;
      end if;

      Write (Window);
   end Cut_Control;

   procedure Free_Control (Control : in out Control_Element)
   is
      use DOM.Core;
   begin
      Nodes.Free (Control);
   end Free_Control;

   procedure Insert_Control_Before
     (Window  : in out GNAVI_Window_Type;
      Control : in     Control_Element;
      Parent  : in     Control_Element;
      Before  : in     Control_Element := Null_Control_Element)
   is
      use DOM.Core;

      Dummy         : DOM.Core.Node;
      Controls_Node : DOM.Core.Node := GNAVI_XML.Get_Child_Node (Parent,
                                                                 "controls");
   begin
      if Controls_Node = null then
         declare
            New_Controls_Node : constant Node :=
              Documents.Create_Element (Document (Window), "controls");
         begin
            Controls_Node := Nodes.Append_Child (Parent,
                                                 New_Controls_Node);
         end;
      end if;

      Dummy := Nodes.Insert_Before (Controls_Node, Control, Before);
      Write (Window);
   end Insert_Control_Before;

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return GWindows.GString
   is
      use GWindows.GStrings;
      use DOM.Core;
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Control,
                                 To_String (Name)));
   end Get_Create_Property;

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return Integer
   is
      use GWindows.GStrings;

      Result : constant String := To_String (Get_Create_Property (Control, Name));
   begin
      if Result = "" then
         return 0;
      end if;

      return Integer'Value (Result);
   end Get_Create_Property;

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return Boolean
   is
      Result : constant GWindows.GString := Get_Create_Property (Control, Name);
   begin
      return Result = "True";
   end Get_Create_Property;

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return GWindows.GString
   is
      use GWindows.GStrings;
      use DOM.Core;

      INode : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node (Control, "init");
   begin
      if INode = null then
         return "";
      else
         declare
            N  : constant Node := GNAVI_XML.Get_Child_Node (INode, Name);
         begin
            if N = null then
               return "";
            else
               return To_GString_From_String
                 (Elements.Get_Attribute (N, "value"));
            end if;
         end;
      end if;
   end Get_Init_Property;

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return Integer
   is
      use GWindows.GStrings;

      Result : constant String := To_String (Get_Init_Property (Control, Name));
   begin
      if Result = "" then
         return 0;
      end if;

      return Integer'Value (Result);
   end Get_Init_Property;

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return Boolean
   is
      Result : constant GWindows.GString := Get_Init_Property (Control, Name);
   begin
      return Result = "True";
   end Get_Init_Property;

   function Strip_Type (Property_Name : GWindows.GString) return String
   is
      use GWindows.GStrings;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      R : constant String := To_String (Property_Name);
   begin
      return R (Index (R, ".", Backward) + 1 .. R'Last);
   end Strip_Type;

   function Trim_Ends (Text : GWindows.GString) return GWindows.GString
   is
   begin
      if Text'Length >= 2 then
         return Text (Text'First + 1 .. Text'Last - 1);
      else
         return Text;
      end if;
   end Trim_Ends;

end GNAVI_Window;
