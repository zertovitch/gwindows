with GNAVI_Datastore;
with GNAVI_XML;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with GWindows.GStrings;

package body GNAVI_Controls is
   use DOM.Core;
   use GWindows.GStrings;

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   Controls_XML  : GNAVI_Datastore.GNAVI_Datastore_Type;
   Controls_File : constant GWindows.GString := "controls.xml";
   Windows_List  : DOM.Core.Node_List;
   Controls_List : DOM.Core.Node_List;
   Types_List    : DOM.Core.Node_List;

   function Get_Create_Properties (Index : Positive)
                                  return DOM.Core.Node_List;
   --  Get possible create properties for control at index

   function Get_Init_Properties (Index : Positive)
                                return DOM.Core.Node_List;
   --  Get possible init properties for control at index

   function Get_Events (Index : Positive)
                       return DOM.Core.Node_List;
   --  Get possible init properties for control at index

   -------------------------------------------------------------------------
   --  Local Body
   -------------------------------------------------------------------------

   function Get_Create_Properties (Index : Positive)
                                  return DOM.Core.Node_List
   is
      use DOM.Core;

      P : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node
        (Nodes.Item (Controls_List, Index - 1), "create_properties");

      Result : DOM.Core.Node_List;
   begin
      if P /= null then
         Result :=  Elements.Get_Elements_By_Tag_Name (P, "property");
      end if;

      return Result;
   end Get_Create_Properties;

   function Get_Init_Properties (Index : Positive)
                                return DOM.Core.Node_List
   is
      use DOM.Core;

      P : constant DOM.Core.Node := GNAVI_XML.Get_Child_Node
        (Nodes.Item (Controls_List, Index - 1), "init_properties");

      Result : DOM.Core.Node_List;
   begin
      if P /= null then
         Result :=  Elements.Get_Elements_By_Tag_Name (P, "property");
      end if;

      return Result;
   end Get_Init_Properties;

   function Get_Events (Index : Positive)
                       return DOM.Core.Node_List
   is
   begin
      return
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_XML.Get_Child_Node
         (Nodes.Item (Controls_List, Index - 1), "events"),
         "event");
   end Get_Events;

   -------------------------------------------------------------------------
   --  Control - Body
   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      GNAVI_Datastore.Open (Controls_XML, Controls_File);

      Windows_List :=
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_Datastore.Root (Controls_XML), "window");
      Controls_List :=
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_Datastore.Root (Controls_XML), "control");
      Types_List :=
        Elements.Get_Elements_By_Tag_Name
        (GNAVI_Datastore.Root (Controls_XML), "type");
   end Init;

   function Control_Count return Natural
   is
   begin
      return Nodes.Length (Controls_List);
   end Control_Count;

   function Control_Display_Name (Index : Positive) return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Nodes.Item (Controls_List, Index - 1),
                                 "display_name"));
   end Control_Display_Name;

   function Control_Type (Index : Positive) return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Nodes.Item (Controls_List, Index - 1),
                                 "type"));
   end Control_Type;

   function Control_Category (Index : Positive) return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute (Nodes.Item (Controls_List, Index - 1),
                                 "category"));
   end Control_Category;

   function Find_Control_By_Type (Type_String : GWindows.GString)
                                 return Natural
   is
   begin
      for N in 1 .. Control_Count loop
         if Control_Type (N) = Type_String then
            return N;
         end if;
      end loop;

      return 0;
   end Find_Control_By_Type;

   function Find_Control_By_Display_Name (Name : GWindows.GString)
                                         return Natural
   is
   begin
      for N in 1 .. Control_Count loop
         if Control_Display_Name (N) = Name then
            return N;
         end if;
      end loop;

      return 0;
   end Find_Control_By_Display_Name;

   -------------------------------------------------------------------------
   --  Controls - Create Properites Body
   -------------------------------------------------------------------------

   function Control_Create_Properties_Count (Index : Positive) return Natural
   is
      use DOM.Core;

      CP : constant DOM.Core.Node_List := Get_Create_Properties (Index);
   begin
      return Nodes.Length (CP);
   end Control_Create_Properties_Count;

   function Control_Create_Property_Name (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Create_Properties (Control_Index),
                      Property_Index - 1),
          "name"));
   end Control_Create_Property_Name;

   function Control_Create_Property_Type (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Create_Properties (Control_Index),
                      Property_Index - 1),
          "type"));
   end Control_Create_Property_Type;

   function Control_Create_Property_Default (Control_Index  : Positive;
                                             Property_Index : Positive)
                                            return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Create_Properties (Control_Index),
                      Property_Index - 1),
          "default"));
   end Control_Create_Property_Default;

   function Control_Create_Property_Value (Control_Index  : Positive;
                                           Property_Index : Positive)
                                          return GWindows.GString
   is
      --  Needs to be modified to handle non Ada types
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Create_Properties (Control_Index),
                      Property_Index - 1),
          "default"));
   end Control_Create_Property_Value;

   -------------------------------------------------------------------------
   --  Controls - Init Properites Body
   -------------------------------------------------------------------------

   function Control_Init_Properties_Count (Index : Positive) return Natural
   is
      use DOM.Core;

      IP : constant DOM.Core.Node_List := Get_Init_Properties (Index);
   begin
      return Nodes.Length (IP);
   end Control_Init_Properties_Count;

   function Control_Init_Property_Name (Control_Index  : Positive;
                                        Property_Index : Positive)
                                       return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Init_Properties (Control_Index),
                      Property_Index - 1),
          "name"));
   end Control_Init_Property_Name;

   function Control_Init_Property_Type (Control_Index  : Positive;
                                        Property_Index : Positive)
                                       return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Init_Properties (Control_Index),
                      Property_Index - 1),
          "type"));
   end Control_Init_Property_Type;

   function Control_Init_Property_Default (Control_Index  : Positive;
                                           Property_Index : Positive)
                                          return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Init_Properties (Control_Index),
                      Property_Index - 1),
          "default"));
   end Control_Init_Property_Default;

   function Control_Init_Property_Value (Control_Index  : Positive;
                                         Property_Index : Positive)
                                        return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Init_Properties (Control_Index),
                      Property_Index - 1),
          "default"));
   end Control_Init_Property_Value;

   -------------------------------------------------------------------------
   --  Controls - Events
   -------------------------------------------------------------------------

   function Control_Events_Count (Index : Positive) return Natural
   is
   begin
      return Nodes.Length (Get_Events (Index));
   end Control_Events_Count;

   function Control_Event_Name (Control_Index  : Positive;
                                Event_Index : Positive)
                               return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Events (Control_Index),
                      Event_Index - 1),
          "name"));
   end Control_Event_Name;

   function Control_Event_Type (Control_Index  : Positive;
                                Event_Index : Positive)
                               return GWindows.GString
   is
   begin
      return To_GString_From_String
        (Elements.Get_Attribute
         (Nodes.Item (Get_Events (Control_Index),
                      Event_Index - 1),
          "type"));
   end Control_Event_Type;

   function Control_Events (Index : Positive) return Detail_Array
   is
      use GWindows.GStrings;

      Result : Detail_Array (1 .. Control_Events_Count (Index));
   begin
      for N in 1 .. Control_Events_Count (Index) loop
         Result (N).Detail_Name :=
           To_GString_Unbounded (Control_Event_Name (Index, N));
         Result (N).Detail_Type :=
           To_GString_Unbounded (Control_Event_Type (Index, N));
         Result (N).Detail_Kind := Event;
      end loop;

      return Result;
   end Control_Events;

   function Control_Properties (Index : Positive) return Detail_Array
   is
      use GWindows.GStrings;

      R0 :          Detail_Array (1 .. 2);
      R1 : constant Detail_Array := Control_Create_Properties (Index);
      R2 : constant Detail_Array := Control_Init_Properties (Index);
   begin
      R0 (1).Detail_Name := To_GString_Unbounded ("name");
      R0 (1).Detail_Type := To_GString_Unbounded ("GString");
      R0 (1).Detail_Kind := Create_Property;
      R0 (2).Detail_Name := To_GString_Unbounded ("type");
      R0 (2).Detail_Type := To_GString_Unbounded ("GString");
      R0 (2).Detail_Kind := Create_Property;

      return R0 & R1 & R2;
   end Control_Properties;

   function Control_Create_Properties (Index : Positive) return Detail_Array
   is
      use GWindows.GStrings;

      Result : Detail_Array (1 .. Control_Create_Properties_Count (Index));
   begin
      for N in 1 .. Control_Create_Properties_Count (Index) loop
         Result (N).Detail_Name :=
           To_GString_Unbounded (Control_Create_Property_Name (Index, N));
         Result (N).Detail_Type :=
           To_GString_Unbounded (Control_Create_Property_Type (Index, N));
         Result (N).Detail_Value :=
           To_GString_Unbounded (Control_Create_Property_Value (Index, N));
         Result (N).Detail_Kind := Create_Property;
      end loop;

      return Result;
   end Control_Create_Properties;

   function Control_Init_Properties (Index : Positive) return Detail_Array
   is
      use GWindows.GStrings;

      Result : Detail_Array (1 .. Control_Init_Properties_Count (Index));
   begin
      for N in 1 .. Control_Init_Properties_Count (Index) loop
         Result (N).Detail_Name :=
           To_GString_Unbounded (Control_Init_Property_Name (Index, N));
         Result (N).Detail_Type :=
           To_GString_Unbounded (Control_Init_Property_Type (Index, N));
         Result (N).Detail_Value :=
           To_GString_Unbounded (Control_Init_Property_Value (Index, N));
         Result (N).Detail_Kind := Init_Property;
      end loop;

      return Result;
   end Control_Init_Properties;

end GNAVI_Controls;
