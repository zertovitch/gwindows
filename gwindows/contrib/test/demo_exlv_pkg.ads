with Ada.Unchecked_Deallocation;
with Ada.Numerics.Discrete_Random;

with Gwindows.Common_Controls.Ex_List_View;
with Gwindows.Colors;

Package Demo_exlv_Pkg is

   type Payload_Data_type is
      record
         Rgb0: Gwindows.Colors.Rgb_Type;
         Rgb1: Gwindows.Colors.Rgb_Type;
      end record;
   type Payload_Data_Access is access all Payload_Data_Type;

   -- instantiation with user defined payload type
   package My_List_View_Pkg is new Gwindows.Common_Controls.Ex_List_View(Payload_Data_type);
   type My_List_View_Type is new My_List_View_Pkg.Ex_List_View_Control_Type with null record;

   procedure Free_payload is new Ada.Unchecked_Deallocation(Payload_data_Type,
                                                            My_List_View_Pkg.Data_Access);

   subtype my_number is positive range 1..12;
   package my_Random_Pkg is new Ada.Numerics.Discrete_Random (my_number);

end Demo_exlv_Pkg;
