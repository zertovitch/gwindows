with GWindows.GStrings;

package body GWindows.Drawing_EMF is

   -------------------------------------------------------------------------
   --  Read_EMF
   -------------------------------------------------------------------------
   procedure Read_EMF (Emf       : in out EMF_Type;
                       File_Name : in     GString)
   is
      function GetEnhMetaFile (lpFilename    : Pointer_To_GChar_C)
        return GWindows.Types.Handle;
      pragma Import (Stdcall, GetEnhMetaFile, "GetEnhMetaFileW");
      Filename : GString_C := GWindows.GStrings.To_GString_C (File_Name);
   begin
      Delete (Emf);
      Handle (Emf, GetEnhMetaFile (Filename (0)'Unchecked_Access));
   end Read_EMF;

   type ENHMETAHEADER is
      record
         iType          : Interfaces.C.unsigned_long;
         nSize          : Interfaces.C.unsigned_long;
         rclBounds      : GWindows.Types.Rectangle_Type;
         rclFrame       : GWindows.Types.Rectangle_Type;
         dSignature     : Interfaces.C.unsigned_long;
         nVersion       : Interfaces.C.unsigned_long;
         nBytes         : Interfaces.C.unsigned_long;
         nRecords       : Interfaces.C.unsigned_long;
         nHandles       : Interfaces.C.unsigned_long;
         sReserved      : Interfaces.C.unsigned_long;
         nDescription   : Interfaces.C.unsigned_long;
         offDescription : Interfaces.C.unsigned_long;
         nPalEntries    : Interfaces.C.unsigned_long;
         szlDevice      : GWindows.Types.Size_Type;
         szlMillimeters : GWindows.Types.Size_Type;
      end record;

   function Get_Dimension_EMF (Emf : in EMF_Type)
     return GWindows.Types.Rectangle_Type is
      use Interfaces.C;
      procedure GetEnhMetaFileHeader
        (HENHMETAFILE : in     GWindows.Types.Handle;
         cbBuffer     : in     Interfaces.C.unsigned;
         emh          : in out ENHMETAHEADER);
      pragma Import (Stdcall, GetEnhMetaFileHeader, "GetEnhMetaFileHeader");
      emh    : ENHMETAHEADER;
      Result : GWindows.Types.Rectangle_Type;
   begin
      GetEnhMetaFileHeader (Handle (Emf),
                            ENHMETAHEADER'Size / 8,
                            emh);
      Result := emh.rclBounds;
      Result.Bottom := abs (Result.Bottom + Result.Top);
      Result.Right  := abs (Result.Right + Result.Left);
      Result.Top    := 0;
      Result.Left   := 0;
      return Result;
   end Get_Dimension_EMF;

   procedure Delete (Emf : in out EMF_Type)
   is
      use type Interfaces.C.long;
      procedure DeleteEnhMetaFile (HENHMETAFILE : GWindows.Types.Handle);
      pragma Import (StdCall, DeleteEnhMetaFile, "DeleteEnhMetaFile");
   begin
      if Handle (Emf) /= GWindows.Types.Null_Handle then
         DeleteEnhMetaFile (Handle (Emf));
         Handle (Emf, GWindows.Types.Null_Handle);
      end if;
   end Delete;

   function Valid (Emf : EMF_Type) return Boolean is
      use type Interfaces.C.long;
   begin
      return Handle (Emf) /= GWindows.Types.Null_Handle;
   end Valid;

end GWindows.Drawing_EMF;
