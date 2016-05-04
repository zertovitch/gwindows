with Ada.Unchecked_Deallocation;
with Ada.Numerics.Discrete_Random;

with GWindows.Common_Controls.Ex_List_View;
with GWindows.Colors;

package Demo_exlv_Pkg is

   type Payload_Data_Type is
      record
         Rgb0: GWindows.Colors.RGB_Type;
         Rgb1: GWindows.Colors.RGB_Type;
      end record;
   type Payload_Data_Access is access all Payload_Data_Type;

   -- instantiation with user defined payload type
   package My_List_View_Pkg is new Gwindows.Common_Controls.Ex_List_View(Payload_Data_type);
   type My_List_View_Type is new My_List_View_Pkg.Ex_List_View_Control_Type with null record;

   procedure Free_payload is new Ada.Unchecked_Deallocation(Payload_Data_Type,
                                                            My_List_View_Pkg.Data_Access);

   subtype My_number is Positive range 1..12;
   package My_Random_Pkg is new Ada.Numerics.Discrete_Random (My_number);

end Demo_exlv_Pkg;
