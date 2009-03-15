with GWindows.Drawing_Objects;
with GWindows.Static_Controls;
with GWindows.Packing_Boxes;
with GWindows.GControls.GSize_Bars;
with GWindows.Panels;
with GWindows.Edit_Boxes;
with GWindows.Buttons;
with GWindows.Windows.Main;
with GWindows.Base;

package AnotherWindow_Package is

   -------------------------------------------------------------------------
   --  AnotherWindow Specs
   -------------------------------------------------------------------------

   type AnotherWindow_Type is
     new GWindows.Windows.Main.Main_Window_Type with
      record
         Edit_Box1  : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Split1     : aliased GWindows.Panels.Panel_Type;
         Split_Bar1 : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         Edit_Box2  : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Split2     : aliased GWindows.Panels.Panel_Type;
         Split_Bar2 : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         Pack_Box1  : aliased GWindows.Packing_Boxes.Packing_Box_Type;
         Edit_Box3  : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Split3     : aliased GWindows.Panels.Panel_Type;
         Split_Bar3 : aliased GWindows.GControls.GSize_Bars.GSize_Bar_Type;
         Split_Text : aliased GWindows.Static_Controls.Label_Type;
         Edit_Box4  : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Edit_Box5  : aliased GWindows.Edit_Boxes.Multi_Line_Edit_Box_Type;
         Label_Font : GWindows.Drawing_Objects.Font_Type;
      end record;

   type AnotherWindow_Access is
     access all AnotherWindow_Type;

   type Pointer_To_AnotherWindow_Class is
     access all AnotherWindow_Type'Class;

   procedure On_Create (Window : in out AnotherWindow_Type);

   AnotherWindow : AnotherWindow_Type;

end AnotherWindow_Package;
