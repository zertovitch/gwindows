with Interfaces.C;
use Interfaces;

with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Types;
with GNATCOM.BSTR;
with GNATCOM.VARIANT;
with GNATCOM.SafeArray;
--  with GNATCOM.GUID;
use GNATCOM;

procedure SafeArray_Test is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   function Get_BSTR is
      new GNATCOM.SafeArray.Get_Element (GNATCOM.Types.BSTR);
   function Get_VARIANT is
      new GNATCOM.SafeArray.Get_Element (GNATCOM.Types.VARIANT);

   function Get_Int is
      new GNATCOM.SafeArray.Get_Element (Interfaces.C.int);
   procedure Put_Int is
      new GNATCOM.SafeArray.Put_Element (Interfaces.C.int);

   function Get_Multi_Int is
      new GNATCOM.SafeArray.Get_Value (Interfaces.C.int);
   procedure Put_Multi_Int is
      new GNATCOM.SafeArray.Put_Value (Interfaces.C.int);

   type RECT is
      record
         Left, Top, Right, Bottom : Integer;
      end record;

   function Get_RECT is
      new GNATCOM.SafeArray.Get_Element (RECT);
   procedure Put_RECT is
      new GNATCOM.SafeArray.Put_Element (RECT);

   Test_Array : GNATCOM.Types.Pointer_To_SAFEARRAY;
   SA_ERROR   : exception;
begin
   Initialize_COM;

   Put_Line ("-- Start SafeArray_Test");
   New_Line;

   Put_Line ("Test   : Create single dimensioned SAFEARRAY of BSTRs");
   Test_Array := SafeArray.Create (GNATCOM.Types. VT_BSTR, 0, 100);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Put BSTRs in Array");
   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      SafeArray.Put_BSTR (Test_Array, N, BSTR.To_BSTR (N'Img));
   end loop;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Put Array in VARIANT");
   declare
      VArray : GNATCOM.Types.VARIANT :=
        GNATCOM.VARIANT.To_VARIANT (Test_Array, GNATCOM.Types.VT_BSTR);
   begin
      Put_Line ("++ PASS");
      New_Line;

      Put_Line ("Test   : Get Array out of VARIANT");
      declare
         SArray : GNATCOM.Types.Pointer_To_SAFEARRAY;
      begin
         SArray := GNATCOM.VARIANT.To_Pointer_To_SAFEARRAY (VArray);
         Put_Line ("++ PASS");
         New_Line;
      end;
   end;


   Put_Line ("Test   : Get BSTRs in Array");
   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         Temp : String := BSTR.To_Ada (Get_BSTR (Test_Array, N));
      begin
         if Temp /= N'Img then
            Put_Line ("++ FAIL");
            raise SA_ERROR;
         end if;
      end;
   end loop;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Free Array");
   SafeArray.Free (Test_Array);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : SAFEARRAY of VARIANTs");
   Test_Array := SafeArray.Create (GNATCOM.Types. VT_VARIANT, 0, 100);

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
         SafeArray.Put_VARIANT (Test_Array,
                                N,
                                VARIANT.To_VARIANT (N'Img));
   end loop;

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         Temp : String := VARIANT.To_Ada (Get_VARIANT (Test_Array, N));
      begin
         if Temp /= N'Img then
            Put_Line ("++ FAIL");
            raise SA_ERROR;
         end if;
      end;
   end loop;

   SafeArray.Free (Test_Array);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : SAFEARRAY of Ints");
   Test_Array := SafeArray.Create (GNATCOM.Types. VT_I4, 0, 100);

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
         Put_Int (Test_Array, N, Interfaces.C.int (N));
   end loop;

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         use type Interfaces.C.int;

         Temp : Interfaces.C.int  := Get_Int (Test_Array, N);
      begin
         if Temp /= Interfaces.C.int (N) then
            Put_Line ("++ FAIL");
            raise SA_ERROR;
         end if;
      end;
   end loop;

   SafeArray.Free (Test_Array);
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Multi Dimensional SAFEARRAY of Ints");
   Test_Array := SafeArray.Create (GNATCOM.Types. VT_I4, ((1, 100), (1, 100)));

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
         Put_Multi_Int (Test_Array, (N, N), Interfaces.C.int (N));
   end loop;

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         use type Interfaces.C.int;

         Temp : Interfaces.C.int  := Get_Multi_Int (Test_Array, (N, N));
      begin
         if Temp /= Interfaces.C.int (N) then
            Put_Line ("++ FAIL");
            raise SA_ERROR;
         end if;
      end;
   end loop;

   Put_Line ("++ PASS");
   New_Line;


   Put_Line ("Test   : Get Dimensions");
   if SafeArray.Get_Dimensions (Test_Array) = 2 then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL");
   end if;
   New_Line;

   Put_Line ("Test   : Get Upper Bound");
   if SafeArray.Get_Upper_Bound (Test_Array, 2) = 100 then
      Put_Line ("++ PASS");
   else
      Put_Line ("++ FAIL");
   end if;
   New_Line;

   SafeArray.Free (Test_Array);

   Put_Line ("Test   : Custom Create");
   Test_Array := SafeArray.Create_Custom ((RECT'Size + 7) / 8, 0, 100);

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         My_Rect : RECT := (N, N, N, N);
      begin
         Put_RECT (Test_Array, N, My_Rect);
      end;
   end loop;

   for N in
     SafeArray.Get_Lower_Bound (Test_Array) ..
     SafeArray.Get_Upper_Bound (Test_Array)
   loop
      declare
         My_Rect : RECT  := Get_RECT (Test_Array, N);
      begin
         if
           (My_Rect.Left /= N) or
           (My_Rect.Right /= N) or
           (My_Rect.Top /= N) or
           (My_Rect.Bottom /= N)
         then
            Put_Line ("++ FAIL");
            raise SA_ERROR;
         end if;
      end;
   end loop;

   SafeArray.Free (Test_Array);
   Put_Line ("++ PASS");
   New_Line;

--    Put_Line ("Test   : VT_RECORD style create");
--    declare
--       LIBID_DxVBLib : GNATCOM.Types.GUID :=
--         GNATCOM.GUID.To_GUID ("{E1211242-8E94-11D1-8808-00C04FC2C602}");
--       RECT_Index    : constant := 224; --  Element Index in thin binding
--    begin
--       Test_Array := SafeArray.Create (Lib_ID      => LIBID_DxVBLib,
--                                       Ver_Maj     => 1,
--                                       Ver_Min     => 0,
--                                       Index       => RECT_Index,
--                                       Lower_Bound => 0,
--                                       Elements    => 100);
--    end;

--    for N in
--      SafeArray.Get_Lower_Bound (Test_Array) ..
--      SafeArray.Get_Upper_Bound (Test_Array)
--    loop
--       declare
--          My_Rect : RECT := (N, N, N, N);
--       begin
--          Put_RECT (Test_Array, N, My_Rect);
--       end;
--    end loop;

--    for N in
--      SafeArray.Get_Lower_Bound (Test_Array) ..
--      SafeArray.Get_Upper_Bound (Test_Array)
--    loop
--       declare
--          My_Rect : RECT  := Get_RECT (Test_Array, N);
--       begin
--          if
--            (My_Rect.Left /= N) or
--            (My_Rect.Right /= N) or
--            (My_Rect.Top /= N) or
--            (My_Rect.Bottom /= N)
--          then
--             Put_Line ("++ FAIL");
--             raise SA_ERROR;
--          end if;
--       end;
--    end loop;

--    SafeArray.Free (Test_Array);
--    Put_Line ("++ PASS");
   New_Line;

   Put_Line ("-- Completed SafeArray_Test");
   New_Line;

end SafeArray_Test;
