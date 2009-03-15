with GWindows;

package GNAVI_Window_Classes is

   procedure Init;
   --  Initialize Datastore

   function Count return Natural;
   --  Returns the number of Window_Classes in datastore

   function Display_Name (Index : in Positive)
                         return GWindows.GString;
   --  Returns the Display Name for Index

   function Description (Index : in Positive)
                        return GWindows.GString;
   --  Returns the Description for Index

   function Template (Index : in Positive)
                     return GWindows.GString;
   --  Returns the Template for Index

   function Window_Type (Index : in Positive)
                        return GWindows.GString;
   --  Returns the Window Type for Index

   procedure Generate_Window (Index          : in Positive;
                              Window_Name    : in GWindows.GString;
                              Project_Name   : in GWindows.GString  := "";
                              Directory_Name : in GWindows.GString  := "");
   --  Generate XML from template for Index with Window_Name
   --  in Project_Name if not "" and in Directory_Name

end GNAVI_Window_Classes;
