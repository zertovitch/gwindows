--  Access to GNAVI's Control/Window Database

with GWindows;

package GNAVI_Controls is

   procedure Init;
   --  Load GNAVI_Controls with XML file

   type Detail_Kind_Type is (Init_Property, Create_Property, Event);

   type Detail_Record is
      record
         Detail_Name  : GWindows.GString_Unbounded;
         Detail_Type  : GWindows.GString_Unbounded;
         Detail_Value : GWindows.GString_Unbounded;
         Detail_Kind  : Detail_Kind_Type;
      end record;

   type Detail_Array is array (Natural range <>) of Detail_Record;

   -------------------------------------------------------------------------
   --  Controls
   -------------------------------------------------------------------------

   function Control_Count return Natural;

   function Control_Display_Name (Index : Positive) return GWindows.GString;

   function Control_Type (Index : Positive) return GWindows.GString;

   function Control_Category (Index : Positive) return GWindows.GString;

   function Find_Control_By_Type (Type_String : GWindows.GString)
                                 return Natural;

   function Find_Control_By_Display_Name (Name : GWindows.GString)
                                         return Natural;

   function Control_Properties (Index : Positive) return Detail_Array;
   --  Returns all properties create and init for control at Index

   -------------------------------------------------------------------------
   --  Controls - Create Properties
   -------------------------------------------------------------------------

   function Control_Create_Properties (Index : Positive) return Detail_Array;

   function Control_Create_Properties_Count (Index : Positive) return Natural;

   function Control_Create_Property_Name (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString;

   function Control_Create_Property_Type (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString;

   function Control_Create_Property_Default (Control_Index  : Positive;
                                             Property_Index : Positive)
                                            return GWindows.GString;

   function Control_Create_Property_Value (Control_Index  : Positive;
                                           Property_Index : Positive)
                                          return GWindows.GString;
   --  Same as default but correct value for ICG based XML

   -------------------------------------------------------------------------
   --  Controls - Init Properties
   -------------------------------------------------------------------------

   function Control_Init_Properties (Index : Positive) return Detail_Array;

   function Control_Init_Properties_Count (Index : Positive) return Natural;

   function Control_Init_Property_Name (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString;

   function Control_Init_Property_Type (Control_Index  : Positive;
                                          Property_Index : Positive)
                                         return GWindows.GString;

   function Control_Init_Property_Default (Control_Index  : Positive;
                                             Property_Index : Positive)
                                            return GWindows.GString;

   function Control_Init_Property_Value (Control_Index  : Positive;
                                           Property_Index : Positive)
                                          return GWindows.GString;
   --  Same as default but correct value for ICG based XML

   -------------------------------------------------------------------------
   --  Controls - Events
   -------------------------------------------------------------------------

   function Control_Events (Index : Positive) return Detail_Array;

   function Control_Events_Count (Index : Positive) return Natural;

   function Control_Event_Name (Control_Index  : Positive;
                                Event_Index : Positive)
                               return GWindows.GString;

   function Control_Event_Type (Control_Index  : Positive;
                                Event_Index : Positive)
                               return GWindows.GString;

end GNAVI_Controls;
