--  Office_Applications facilitates the production of "Office"-like
--  applications with the GWindows framework.
--
--  Office_Applications is used in the following open-source projects:
--
--    AZip   : https://azip.sourceforge.io/
--    LEA    : https://l-e-a.sourceforge.io/
--    TeXCAD : https://texcad.sourceforge.io/
--
--  Mirrors of those projects are located here: https://github.com/zertovitch

with GWindows.Windows.MDI;

package Office_Applications is

   -------------------------------------------------------------------
   --  "Classic Office" window layout: multiple sub-windows within  --
   --  a main window. Eventually the sub-windows are maximized,     --
   --  so only one sub-window is visible at a time. For instance    --
   --  in Notepad++ the sub-windows are always maximized            --
   -------------------------------------------------------------------

   ------------------------------------------------
   --  Main "Classic Office" application window  --
   ------------------------------------------------

   type Classic_Main_Window_Type is
      new GWindows.Windows.MDI.MDI_Main_Window_Type with null record;

   --  Close the document (usually blank) that might be created on
   --  application's startup. If that document was modified by the
   --  user, we give up closing it.
   --
   procedure Close_Initial_Document
     (Main_Window : in out Classic_Main_Window_Type);

   ----------------------------------------------
   --  Child "Classic Office" document window  --
   ----------------------------------------------

   type Classic_Document_Window_Type is
      abstract new GWindows.Windows.MDI.MDI_Child_Window_Type with
         record
            --  New document appearing on startup is closed
            --  if it was kept virgin when the user opened
            --  another document.
            Extra_First_Doc : Boolean := False;
         end record;

   function Is_Document_Modified
     (Child_Window : Classic_Document_Window_Type) return Boolean is abstract;

end Office_Applications;
