with GWindows.GStrings;

package body GWindows.Drawing_EMF_Canvas is

   -------------------------------------------------------------------------
   --  Create_MetaFile_Canvas
   -------------------------------------------------------------------------

   procedure Create_EMF_Canvas (Canvas    : in out EMF_Canvas_Type;
                                File_Name : in     GString := "";
                                File_Desc : in     GString := "") is
      type PRec is access all GWindows.Types.Rectangle_Type;
      function CreateEnhMetaFile (hdcRef        : GWindows.Types.Handle;
                                  lpFilename    : Pointer_To_GChar_C;
                                  lpRect        : PRec;
                                  lpDescription : Pointer_To_GChar_C)
        return GWindows.Types.Handle;
      pragma Import (Stdcall, CreateEnhMetaFile, "CreateEnhMetaFileW");
      Filename : GString_C := GWindows.GStrings.To_GString_C (File_Name);
      Filedesc : GString_C := GWindows.GStrings.To_GString_C (File_Desc);
      lpFilename    : Pointer_To_GChar_C;
      lpDescription : Pointer_To_GChar_C;
   begin
      Finalize (Canvas);
      if File_Name /= "" then
         lpFilename := Filename (0)'Unchecked_Access;
      else
         lpFilename := null;
      end if;
      if File_Desc /= "" then
         lpDescription := Filedesc (0)'Unchecked_Access;
      else
         lpDescription := null;
      end if;
      Handle (Canvas, CreateEnhMetaFile (GWindows.Types.Null_Handle,
                                         lpFilename, null, lpDescription));
   end Create_EMF_Canvas;

   -------------------------------------------------------------------------
   --  Close_EMF_Canvas
   -------------------------------------------------------------------------

   procedure Close_EMF_Canvas
     (Canvas : in out EMF_Canvas_Type;
      Emf    :    out EMF_Type)
   is
      use type Interfaces.C.long;
      function CloseEnhMetaFile (hdc : GWindows.Types.Handle)
        return GWindows.Types.Handle;
      pragma Import (Stdcall, CloseEnhMetaFile, "CloseEnhMetaFile");
   begin
      if Handle (Canvas) /= GWindows.Types.Null_Handle then
         Delete (Emf);
         Handle (Emf, CloseEnhMetaFile (Handle (Canvas)));
         Handle (Canvas, GWindows.Types.Null_Handle);
      end if;
   end Close_EMF_Canvas;

   procedure Finalize (Canvas : in out EMF_Canvas_Type) is
      Emf : EMF_Type;
   begin
      Close_EMF_Canvas (Canvas, Emf);
   end Finalize;

   -------------------------------------------------------------------------
   --  Play_EMF_Canvas
   -------------------------------------------------------------------------

   procedure Play_EMF_Canvas
     (Canvas : in out Canvas_Type;
      Emf    : in     EMF_Type;
      Rect   : in     GWindows.Types.Rectangle_Type)
   is
      procedure PlayEnhMetaFile (Hdc          : GWindows.Types.Handle;
                                 HENHMETAFILE : GWindows.Types.Handle;
                                 Rect         : GWindows.Types.Rectangle_Type);
      pragma Import (Stdcall, PlayEnhMetaFile, "PlayEnhMetaFile");
   begin
      PlayEnhMetaFile (GWindows.Drawing.Handle (Canvas),
                       Handle (Emf),
                       Rect);
   end Play_EMF_Canvas;

end GWindows.Drawing_EMF_Canvas;
