with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
with GWindows.Types;           use GWindows.Types;

package GWindows.Drawing_EMF is

   type EMF_Type is new Drawing_Object_Type with private;

   procedure Read_EMF (Emf       : in out EMF_Type;
                       File_Name : in     GString);

   function Get_Dimension_EMF (Emf : EMF_Type)
      return GWindows.Types.Rectangle_Type;

   procedure Delete (Emf : in out EMF_Type);

   function Valid (Emf : EMF_Type) return Boolean;

private

   type EMF_Type is new Drawing_Object_Type with null record;

end GWindows.Drawing_EMF;
