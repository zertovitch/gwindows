with GWindows;

package GNAVI_Project_Classes is

   procedure Init;
   --  Initialize Datastore

   function Count return Natural;
   --  Returns the number of Project_Classes in datastore

   function Display_Name (Index : in Positive)
                         return GWindows.GString;
   --  Returns the Display Name for Index

   function Description (Index : in Positive)
                        return GWindows.GString;
   --  Returns the Description for Index

   function Template (Index : in Positive)
                     return GWindows.GString;
   --  Returns the Template for Index


   procedure Generate_Project (Index          : in Positive;
                               Project_Name   : in GWindows.GString;
                               Directory_Name : in GWindows.GString := "");
   --  Generate XML from template for Index with Project_Name
   --  in Project_Name if not "" and in Directory_Name

end GNAVI_Project_Classes;
