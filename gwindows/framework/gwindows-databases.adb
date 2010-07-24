------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . D A T A B A S E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;

with GWindows.GStrings;                 use GWindows.GStrings;

with ADO.uConnection_Interface;
use ADO.uConnection_Interface;
with ADO.uRecordset_Interface;
use ADO.uRecordset_Interface;
with ADO.Fields_Interface;
use ADO.Fields_Interface;
with ADO.Field_Interface;
use ADO.Field_Interface;

with GNATCOM.Dispinterface;
use GNATCOM.Dispinterface;
with GNATCOM.VARIANT;

package body GWindows.Databases is
   ---------------------
   -- Trans functions --
   ---------------------

   procedure Begin_Trans (Database : in out Database_Type) is
      n : Integer;
   begin
      n := Integer (BeginTrans (Database.Connection));
   end Begin_Trans;

   procedure Commit_Trans (Database : in out Database_Type) is
   begin
      CommitTrans (Database.Connection);
   end Commit_Trans;

   procedure Rollback_Trans (Database : in out Database_Type) is
   begin
      RollbackTrans (Database.Connection);
   end Rollback_Trans;

   -------------
   -- Add_New --
   -------------

   procedure Add_New (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Add_New (Recordset_Type'Class (Recordset), Change);

      if Change then
         AddNew (Recordset.Recordset);
         On_Add_New (Recordset_Type'Class (Recordset));
      end if;
   end Add_New;

   ---------------
   -- Edit_Mode --
   ---------------

   function Edit_Mode (Recordset : Recordset_Type) return Boolean
   is
      Result : Integer := Integer (Get_EditMode (Recordset.Recordset));
   begin
      if Result = ADO.adEditInProgress or Result = ADO.adEditAdd then
         return True;
      else
         return False;
      end if;
   end Edit_Mode;

   ---------
   -- BOF --
   ---------

   function BOF (Recordset : Recordset_Type) return Boolean is
      use type GNATCOM.Types.VARIANT_BOOL;
   begin
      return Get_BOF (Recordset.Recordset) = GNATCOM.Types.VARIANT_BOOL_TRUE;
   end BOF;

   -----------
   -- Close --
   -----------

   procedure Close (Database : in out Database_Type) is
   begin
      Close (Database.Connection);
   end Close;

   -----------
   -- Close --
   -----------

   procedure Close (Recordset : in out Recordset_Type) is
   begin
      Close (Recordset.Recordset);
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Delete (Recordset_Type'Class (Recordset), Change);

      if Change then
         Delete (Recordset.Recordset, ADO.adAffectCurrent);
         On_Delete (Recordset_Type'Class (Recordset));
      end if;
   end Delete;

   ---------
   -- EOF --
   ---------

   function EOF (Recordset : Recordset_Type) return Boolean is
      use type GNATCOM.Types.VARIANT_BOOL;
   begin
      return Get_EOF (Recordset.Recordset) = GNATCOM.Types.VARIANT_BOOL_TRUE;
   end EOF;

   -------------
   -- Execute --
   -------------

   function Execute
     (Database : in Database_Type;
      Query    : in GString)
      return Natural
   is
      Result  : ADO.Pointer_To_uRecordset;
      Records : aliased GNATCOM.Types.VARIANT;
   begin
      Records.vt := GNATCOM.Types.VT_I4;
      Result := Execute (Database.Connection,
                         To_BSTR_From_GString (Query),
                         Records'Unchecked_Access, 0);

      return GNATCOM.VARIANT.To_Ada (Records);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Database : in out Database_Type;
      Query    : in     GString)
   is
      Result  : ADO.Pointer_To_uRecordset;
      Records : aliased GNATCOM.Types.VARIANT;
   begin
      Records.vt := GNATCOM.Types.VT_I4;
      Result := Execute (Database.Connection,
                         To_BSTR_From_GString (Query),
                         Records'Unchecked_Access, 0);
   end Execute;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Recordset : Recordset_Type) return Natural is
      Fields : Fields_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      return Natural (Get_Count (Fields));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Recordset : Recordset_Type;
      Index     : Positive)
      return GString
   is
      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      return To_GString_From_BSTR (Get_Name (Field));
   end Field_Name;

   -----------------
   -- Field_Value --
   -----------------

   function Field_Value
     (Recordset : Recordset_Type;
      Index     : Positive)
      return GString
   is
      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      return To_GString_From_VARIANT (Get_Value (Field));
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   function Field_Value
     (Recordset : Recordset_Type;
      Name      : GString)
      return GString
   is
      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               To_VARIANT_From_GString (Name)));

      return To_GString_From_VARIANT (Get_Value (Field));
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   function Field_Value
     (Recordset : Recordset_Type;
      Index     : Positive)
      return GNATCOM.Types.VARIANT
   is
      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      return Get_Value (Field);
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   function Field_Value
     (Recordset : Recordset_Type;
      Name      : GString)
      return GNATCOM.Types.VARIANT
   is
      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               To_VARIANT_From_GString (Name)));

      return Get_Value (Field);
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   procedure Field_Value
     (Recordset : in out Recordset_Type;
      Index     : in     Positive;
      Value     : in     GString)
   is
      Fields : Fields_Type;
      Field  : Field_Type;
      Val    : GNATCOM.Types.VARIANT := To_VARIANT_From_GString (Value);
      Change : Boolean;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      On_Before_Field_Change
        (Recordset_Type'Class (Recordset),
         To_GString_From_BSTR (Get_Name (Field)),
         Val,
         Change);

      if Change then
         Put_Value (Field, Val, False);

         On_Field_Change
           (Recordset_Type'Class (Recordset),
            To_GString_From_BSTR (Get_Name (Field)),
            Val);
      end if;

      GNATCOM.VARIANT.Clear (Val);

   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   procedure Field_Value
     (Recordset : in out Recordset_Type;
      Name      : in     GString;
      Value     : in     GString)
   is
      Fields : Fields_Type;
      Field  : Field_Type;
      Val    : GNATCOM.Types.VARIANT := To_VARIANT_From_GString (Value);
      Change : Boolean;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               To_VARIANT_From_GString (Name)));

      On_Before_Field_Change
        (Recordset_Type'Class (Recordset),
         To_GString_From_BSTR (Get_Name (Field)),
         Val,
         Change);

      if Change then
         Put_Value (Field, Val, False);

         On_Field_Change
           (Recordset_Type'Class (Recordset),
            To_GString_From_BSTR (Get_Name (Field)),
            Val);
      end if;

      GNATCOM.VARIANT.Clear (Val);
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   procedure Field_Value
     (Recordset : in out Recordset_Type;
      Index     : in     Positive;
      Value     : in     GNATCOM.Types.VARIANT;
      Clear     : in     Boolean               := True)
   is
      Fields : Fields_Type;
      Field  : Field_Type;
      Val    : GNATCOM.Types.VARIANT := Value;
      Change : Boolean;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      On_Before_Field_Change
        (Recordset_Type'Class (Recordset),
         To_GString_From_BSTR (Get_Name (Field)),
         Val,
         Change);

      if Change then
         Put_Value (Field, Val, False);

         On_Field_Change
           (Recordset_Type'Class (Recordset),
            To_GString_From_BSTR (Get_Name (Field)),
            Val);
      end if;

      if Clear then
         GNATCOM.VARIANT.Clear (Val);
      end if;
   end Field_Value;

   -----------------
   -- Field_Value --
   -----------------

   procedure Field_Value
     (Recordset : in out Recordset_Type;
      Name      : in     GString;
      Value     : in     GNATCOM.Types.VARIANT;
      Clear     : in     Boolean               := True)
   is
      Fields : Fields_Type;
      Field  : Field_Type;
      Val    : GNATCOM.Types.VARIANT := Value;
      Change : Boolean;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               To_VARIANT_From_GString (Name)));

      On_Before_Field_Change
        (Recordset_Type'Class (Recordset),
         To_GString_From_BSTR (Get_Name (Field)),
         Val,
         Change);

      if Change then
         Put_Value (Field, Val, False);

         On_Field_Change
           (Recordset_Type'Class (Recordset),
            To_GString_From_BSTR (Get_Name (Field)),
            Val);
      end if;

      if Clear then
         GNATCOM.VARIANT.Clear (Val);
      end if;
   end Field_Value;

   ---------------
   -- Interface --
   ---------------

   function Interfac
     (This : Database_Type)
      return GNATCOM.Dispinterface.Dispinterface_Type
   is
      Result : GNATCOM.Dispinterface.Dispinterface_Type;
   begin
      Query (Result, This.Connection);
      return Result;
   end Interfac;

   ---------------
   -- Interface --
   ---------------

   function Interfac (This : Recordset_Type)
                      return GNATCOM.Dispinterface.Dispinterface_Type
   is
      Result : GNATCOM.Dispinterface.Dispinterface_Type;
   begin
      Query (Result, This.Recordset);
      return Result;
   end Interfac;

   ----------
   -- Move --
   ----------

   procedure Move
     (Recordset : in out Recordset_Type;
      Count     : in     Integer;
      From      : in     Move_From_Type := Current)
   is
      type Move_Array is array (Move_From_Type) of Integer;

      Move_Values : Move_Array :=
        (Current => ADO.adBookmarkCurrent,
         First   => ADO.adBookmarkFirst,
         Last    => ADO.adBookmarkLast);

      Change : Boolean;
   begin
      On_Before_Change (Recordset_Type'Class (Recordset), Change);

      if Change then
         Move (Recordset.Recordset,
               Interfaces.C.long (Count),
               GNATCOM.VARIANT.To_VARIANT (Move_Values (From)));
         On_Change (Recordset_Type'Class (Recordset));
      end if;
   end Move;

   ----------------
   -- Move_First --
   ----------------

   procedure Move_First (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Change (Recordset_Type'Class (Recordset), Change);

      if Change then
         MoveFirst (Recordset.Recordset);
         On_Change (Recordset_Type'Class (Recordset));
      end if;
   end Move_First;

   ---------------
   -- Move_Last --
   ---------------

   procedure Move_Last (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Change (Recordset_Type'Class (Recordset), Change);

      if Change then
         MoveLast (Recordset.Recordset);
         On_Change (Recordset_Type'Class (Recordset));
      end if;
   end Move_Last;

   ---------------
   -- Move_Next --
   ---------------

   procedure Move_Next (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Change (Recordset_Type'Class (Recordset), Change);

      if Change then
         MoveNext (Recordset.Recordset);
         On_Change (Recordset_Type'Class (Recordset));
      end if;
   end Move_Next;

   -------------------
   -- Move_Previous --
   -------------------

   procedure Move_Previous (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Change (Recordset_Type'Class (Recordset), Change);

      if Change then
         MovePrevious (Recordset.Recordset);
         On_Change (Recordset_Type'Class (Recordset));
      end if;
   end Move_Previous;

   ----------
   -- Open --
   ----------

   procedure Open
     (Database          : in out Database_Type;
      Connection_String : in     GString;
      User_ID           : in     GString        := "";
      Password          : in     GString        := "")
   is
   begin
      Create (Database.Connection, ADO.CLSID_Connection);
      Open (Database.Connection,
            To_BSTR_From_GString (Connection_String),
            To_BSTR_From_GString (User_ID),
            To_BSTR_From_GString (Password), 0);
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Recordset : in out Recordset_Type;
      Database  : in     Database_Type'Class;
      Query     : in     GString;
      Cursor    : in     Cursor_Type;
      Lock      : in     Lock_Type)
   is
      type Cursor_Array is array (Cursor_Type) of Interfaces.C.long;
      type Lock_Array is array (Lock_Type) of Interfaces.C.long;

      Cursor_Values : Cursor_Array :=
        (Dynamic      => ADO.adOpenDynamic,
         Forward_Only => ADO.adOpenForwardOnly,
         Keyset       => ADO.adOpenKeyset,
         Static       => ADO.adOpenStatic);

      Lock_Values   : Lock_Array :=
        (Batch_Optimistic => ADO.adLockBatchOptimistic,
         Optimistic       => ADO.adLockOptimistic,
         Pessimistic      => ADO.adLockPessimistic,
         Read_Only        => ADO.adLockReadOnly);
   begin
      Create (Recordset.Recordset, ADO.CLSID_Recordset);

      Open (Recordset.Recordset,
            To_VARIANT_From_GString (Query),
            To_VARIANT_From_Dispinterface (Interfac (Database)),
            Cursor_Values (Cursor),
            Lock_Values (Lock),
            0);
   end Open;

   ------------------
   -- Record_Count --
   ------------------

   function Record_Count (Recordset : Recordset_Type) return Integer is
   begin
      return Integer (Get_RecordCount (Recordset.Recordset));
   end Record_Count;

   -------------
   -- Requery --
   -------------

   procedure Requery (Recordset : in out Recordset_Type) is
   begin
      Requery (Recordset.Recordset, 0);
   end Requery;

   ------------
   -- Resync --
   ------------

   procedure Resync (Recordset : in out Recordset_Type) is
   begin
      Resync (Recordset.Recordset, 0, 0);
   end Resync;

   -------------------
   -- Cancel_Update --
   -------------------

   procedure Cancel_Update (Recordset : in out Recordset_Type)
   is
   begin
      CancelUpdate (Recordset.Recordset);
      On_Change (Recordset_Type'Class (Recordset));
   end Cancel_Update;

   ------------
   -- Update --
   ------------

   procedure Update (Recordset : in out Recordset_Type) is
      Change : Boolean;
   begin
      On_Before_Update (Recordset_Type'Class (Recordset), Change);

      if Change then
         Update (Recordset.Recordset);
         On_Update (Recordset_Type'Class (Recordset));
      end if;
   end Update;

   ----------------------
   -- On_Before_Change --
   ----------------------

   procedure On_Before_Change (Recordset : in out Recordset_Type;
                               Change    :    out Boolean)
   is
   begin
      Fire_On_Before_Change (Recordset, Change);
   end On_Before_Change;

   procedure On_Change (Recordset : in out Recordset_Type)
   is
   begin
      Fire_On_Change (Recordset);
   end On_Change;

   ----------------------------
   -- On_Before_Field_Change --
   ----------------------------

   procedure On_Before_Field_Change (Recordset : in out Recordset_Type;
                                     Name      : in     GString;
                                     Value     : in out GNATCOM.Types.VARIANT;
                                     Change    :    out Boolean)
   is
   begin
      Fire_On_Before_Field_Change (Recordset, Name, Value, Change);
   end On_Before_Field_Change;

   ---------------------
   -- On_Field_Change --
   ---------------------

   procedure On_Field_Change (Recordset : in out Recordset_Type;
                              Name      : in     GString;
                              Value     : in     GNATCOM.Types.VARIANT)
   is
   begin
      Fire_On_Field_Change (Recordset, Name, Value);
   end On_Field_Change;

   ----------------------
   -- On_Before_Update --
   ----------------------

   procedure On_Before_Update (Recordset : in out Recordset_Type;
                               Change    :    out Boolean)
   is
   begin
      Fire_On_Before_Update (Recordset, Change);
   end On_Before_Update;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update (Recordset : in out Recordset_Type)
   is
   begin
      Fire_On_Update (Recordset);
   end On_Update;

   -----------------------
   -- On_Before_Add_New --
   -----------------------

   procedure On_Before_Add_New (Recordset : in out Recordset_Type;
                                Change    :    out Boolean)
   is
   begin
      Fire_On_Before_Add_New (Recordset, Change);
   end On_Before_Add_New;

   ----------------
   -- On_Add_New --
   ----------------

   procedure On_Add_New (Recordset : in out Recordset_Type)
   is
   begin
      Fire_On_Add_New (Recordset);
   end On_Add_New;

   ----------------------
   -- On_Before_Delete --
   ----------------------

   procedure On_Before_Delete (Recordset : in out Recordset_Type;
                               Change    :    out Boolean)
   is
   begin
      Fire_On_Before_Delete (Recordset, Change);
   end On_Before_Delete;

   ---------------
   -- On_Delete --
   ---------------

   procedure On_Delete (Recordset : in out Recordset_Type)
   is
   begin
      Fire_On_Delete (Recordset);
   end On_Delete;

   ------------------------------
   -- On_Before_Change_Handler --
   ------------------------------

   procedure On_Before_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event)
   is
   begin
      Recordset.On_Before_Change_Event := Handler;
   end On_Before_Change_Handler;

   ---------------------------
   -- Fire_On_Before_Change --
   ---------------------------

   procedure Fire_On_Before_Change
     (Recordset : in out Recordset_Type;
      Change    :    out Boolean)
   is
   begin
      if Recordset.On_Before_Change_Event /= null then
         Recordset.On_Before_Change_Event
           (Recordset_Type'Class (Recordset), Change);
      else
         Change := True;
      end if;
   end Fire_On_Before_Change;

   -----------------------
   -- On_Change_Handler --
   -----------------------

   procedure On_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event)
   is
   begin
      Recordset.On_Change_Event := Handler;
   end On_Change_Handler;

   --------------------
   -- Fire_On_Change --
   --------------------

   procedure Fire_On_Change (Recordset : in out Recordset_Type)
   is
   begin
      if Recordset.On_Change_Event /= null then
         Recordset.On_Change_Event (Recordset_Type'Class (Recordset));
      end if;
   end Fire_On_Change;

   ------------------------------------
   -- On_Before_Field_Change_Handler --
   ------------------------------------

   procedure On_Before_Field_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Field_Change_Event)
   is
   begin
      Recordset.On_Before_Field_Change_Event := Handler;
   end On_Before_Field_Change_Handler;

   ---------------------------------
   -- Fire_On_Before_Field_Change --
   ---------------------------------

   procedure Fire_On_Before_Field_Change
     (Recordset : in out Recordset_Type;
      Name      : in     GString;
      Value     : in out GNATCOM.Types.VARIANT;
      Change    :    out Boolean)
   is
   begin
      if Recordset.On_Before_Field_Change_Event /= null then
         Recordset.On_Before_Field_Change_Event
           (Recordset_Type'Class (Recordset), Name, Value, Change);
      else
         Change := True;
      end if;
   end Fire_On_Before_Field_Change;

   -----------------------------
   -- On_Field_Change_Handler --
   -----------------------------

   procedure On_Field_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Field_Change_Event)
   is
   begin
      Recordset.On_Field_Change_Event := Handler;
   end On_Field_Change_Handler;

   --------------------------
   -- Fire_On_Field_Change --
   --------------------------

   procedure Fire_On_Field_Change (Recordset : in out Recordset_Type;
                                   Name      : in     GString;
                                   Value     : in     GNATCOM.Types.VARIANT)
   is
   begin
      if Recordset.On_Field_Change_Event /= null then
         Recordset.On_Field_Change_Event
           (Recordset_Type'Class (Recordset), Name, Value);
      end if;
   end Fire_On_Field_Change;

   ------------------------------
   -- On_Before_Update_Handler --
   ------------------------------

   procedure On_Before_Update_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event)
   is
   begin
      Recordset.On_Before_Update_Event := Handler;
   end On_Before_Update_Handler;

   ---------------------------
   -- Fire_On_Before_Update --
   ---------------------------

   procedure Fire_On_Before_Update (Recordset : in out Recordset_Type;
                                    Change    :    out Boolean)
   is
   begin
      if Recordset.On_Before_Update_Event /= null then
         Recordset.On_Before_Update_Event
           (Recordset_Type'Class (Recordset), Change);
      else
         Change := True;
      end if;
   end Fire_On_Before_Update;

   -----------------------
   -- On_Update_Handler --
   -----------------------

   procedure On_Update_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event)
   is
   begin
      Recordset.On_Update_Event := Handler;
   end On_Update_Handler;

   --------------------
   -- Fire_On_Update --
   --------------------

   procedure Fire_On_Update (Recordset : in out Recordset_Type)
   is
   begin
      if Recordset.On_Update_Event /= null then
         Recordset.On_Update_Event (Recordset_Type'Class (Recordset));
      end if;
   end Fire_On_Update;

   -------------------------------
   -- On_Before_Add_New_Handler --
   -------------------------------

   procedure On_Before_Add_New_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event)
   is
   begin
      Recordset.On_Before_Add_New_Event := Handler;
   end On_Before_Add_New_Handler;

   ----------------------------
   -- Fire_On_Before_Add_New --
   ----------------------------

   procedure Fire_On_Before_Add_New (Recordset : in out Recordset_Type;
                                     Change    :    out Boolean)
   is
   begin
      if Recordset.On_Before_Add_New_Event /= null then
         Recordset.On_Before_Add_New_Event
           (Recordset_Type'Class (Recordset), Change);
      else
         Change := True;
      end if;
   end Fire_On_Before_Add_New;

   ------------------------
   -- On_Add_New_Handler --
   ------------------------

   procedure On_Add_New_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event)
   is
   begin
      Recordset.On_Add_New_Event := Handler;
   end On_Add_New_Handler;

   --------------------
   -- Fire_On_Add_New --
   --------------------

   procedure Fire_On_Add_New (Recordset : in out Recordset_Type)
   is
   begin
      if Recordset.On_Add_New_Event /= null then
         Recordset.On_Add_New_Event (Recordset_Type'Class (Recordset));
      end if;
   end Fire_On_Add_New;

   ------------------------------
   -- On_Before_Delete_Handler --
   ------------------------------

   procedure On_Before_Delete_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event)
   is
   begin
      Recordset.On_Before_Delete_Event := Handler;
   end On_Before_Delete_Handler;

   ---------------------------
   -- Fire_On_Before_Delete --
   ---------------------------

   procedure Fire_On_Before_Delete (Recordset : in out Recordset_Type;
                                    Change    :    out Boolean)
   is
   begin
      if Recordset.On_Before_Delete_Event /= null then
         Recordset.On_Before_Delete_Event
           (Recordset_Type'Class (Recordset), Change);
      else
         Change := True;
      end if;
   end Fire_On_Before_Delete;

   -----------------------
   -- On_Delete_Handler --
   -----------------------

   procedure On_Delete_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event)
   is
   begin
      Recordset.On_Delete_Event := Handler;
   end On_Delete_Handler;

   --------------------
   -- Fire_On_Delete --
   --------------------

   procedure Fire_On_Delete (Recordset : in out Recordset_Type)
   is
   begin
      if Recordset.On_Delete_Event /= null then
         Recordset.On_Delete_Event (Recordset_Type'Class (Recordset));
      end if;
   end Fire_On_Delete;

   ---------------------
   -- Field_Updatable --
   ---------------------

   function Field_Updatable (Recordset : in Recordset_Type;
                             Index     : in Positive)
                            return Boolean
   is
      use type Interfaces.C.unsigned;

      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               GNATCOM.VARIANT.To_VARIANT (Index - 1)));

      return
        (Interfaces.C.unsigned (Get_Attributes (Field))
        and
        ADO.adFldUpdatable) = ADO.adFldUpdatable;
   end Field_Updatable;

   function Field_Updatable (Recordset : in Recordset_Type;
                             Name      : in GString)
                            return Boolean
   is
      use type Interfaces.C.unsigned;

      Fields : Fields_Type;
      Field  : Field_Type;
   begin
      Attach (Fields, Get_Fields (Recordset.Recordset));
      Attach (Field, Get_Item (Fields,
                               To_VARIANT_From_GString (Name)));

      return
        (Interfaces.C.unsigned (Get_Attributes (Field))
        and
        ADO.adFldUpdatable) = ADO.adFldUpdatable;
   end Field_Updatable;

end GWindows.Databases;
