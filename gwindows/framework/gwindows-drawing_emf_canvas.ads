with GWindows.Drawing;         use GWindows.Drawing;
with GWindows.Drawing_EMF;     use GWindows.Drawing_EMF;
with GWindows.Types;           use GWindows.Types;

package GWindows.Drawing_EMF_Canvas is

   type EMF_Canvas_Type is new Canvas_Type with private;

   procedure Create_EMF_Canvas (Canvas    : in out EMF_Canvas_Type;
                                File_Name : in     GString := "";
                                File_Desc : in     GString := "");

   procedure Close_EMF_Canvas (Canvas : in out EMF_Canvas_Type;
                               Emf    :    out EMF_Type);

   procedure Finalize (Canvas : in out EMF_Canvas_Type);

   procedure Play_EMF_Canvas
     (Canvas : in out Canvas_Type;
      Emf    : in     EMF_Type;
      Rect   : in     GWindows.Types.Rectangle_Type);

private

   type EMF_Canvas_Type is new Canvas_Type with null record;

end GWindows.Drawing_EMF_Canvas;
