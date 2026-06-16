with GWindows.GStrings;
with Win32_Types;

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
      pragma Machine_Attribute (GetEnhMetaFile, "ms_abi");
      Filename : GString_C := GWindows.GStrings.To_GString_C (File_Name);
   begin
      Delete (Emf);
      Handle (Emf, GetEnhMetaFile (Filename (0)'Unchecked_Access));
   end Read_EMF;

   type ENHMETAHEADER is
      record
         iType          : Win32_Types.Unsigned_Long;
         nSize          : Win32_Types.Unsigned_Long;
         rclBounds      : GWindows.Types.Rectangle_Type;
         rclFrame       : GWindows.Types.Rectangle_Type;
         dSignature     : Win32_Types.Unsigned_Long;
         nVersion       : Win32_Types.Unsigned_Long;
         nBytes         : Win32_Types.Unsigned_Long;
         nRecords       : Win32_Types.Unsigned_Long;
         nHandles       : Win32_Types.Unsigned_Long;
         sReserved      : Win32_Types.Unsigned_Long;
         nDescription   : Win32_Types.Unsigned_Long;
         offDescription : Win32_Types.Unsigned_Long;
         nPalEntries    : Win32_Types.Unsigned_Long;
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
      pragma Machine_Attribute (GetEnhMetaFileHeader, "ms_abi");
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
      procedure DeleteEnhMetaFile (HENHMETAFILE : GWindows.Types.Handle);
      pragma Import (StdCall, DeleteEnhMetaFile, "DeleteEnhMetaFile");
   pragma Machine_Attribute (DeleteEnhMetaFile, "ms_abi");
   begin
      if Handle (Emf) /= GWindows.Types.Null_Handle then
         DeleteEnhMetaFile (Handle (Emf));
         Handle (Emf, GWindows.Types.Null_Handle);
      end if;
   end Delete;

   function Valid (Emf : EMF_Type) return Boolean is
   begin
      return Handle (Emf) /= GWindows.Types.Null_Handle;
   end Valid;

end GWindows.Drawing_EMF;
