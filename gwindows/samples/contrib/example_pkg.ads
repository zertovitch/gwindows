with Gwindows.Application;
with Gwindows.Base;
with Interfaces.C;
with Gwindows.Message_Boxes; use Gwindows.Message_Boxes;
with Ada.Text_Io; use Ada;
with Gwindows.Gstrings; use Gwindows.Gstrings;
with Gwindows.Common_Controls; use Gwindows.Common_Controls;
with Gwindows.Drawing_Objects; use Gwindows.Drawing_Objects;
with Gwindows.Windows; use Gwindows.Windows;
with Gwindows.Colors; use Gwindows.Colors;

with Gwindows.Windows.Ex_Windows.Ex_Main; use Gwindows.Windows.Ex_Windows.Ex_Main;
with Gwindows.Common_Controls.Ex_tb; use Gwindows.Common_Controls.Ex_Tb;
with Gwindows.Image_Lists.Ex_Image_Lists; use Gwindows.Image_Lists.Ex_Image_Lists;
--with Gwindows.Common_Controls.Ex_Tv; use Gwindows.Common_Controls.Ex_Tv;
-- generic package for extended list view controls
with Gwindows.Common_Controls.Ex_Lv_Generic; --use Gwindows.Common_Controls.Ex_lv;
with Gwindows.Common_Controls.Ex_Tv_Generic;
with Gwindows.Buttons.Ex_Buttons; use Gwindows.Buttons.Ex_Buttons;
with Splitbar; use Splitbar;

package Example_Pkg is
   -- Instantiation of generic extended list view controls
   package Int_Ex_Lv is new Gwindows.Common_Controls.Ex_Lv_Generic(Integer);
   use Int_Ex_Lv;

   -- Instantiation of generic extended tree view controls
   package Int_Ex_Tv is new Gwindows.Common_Controls.Ex_Tv_Generic(Integer);
   use Int_Ex_Tv;

   procedure Main;

end Example_Pkg;
