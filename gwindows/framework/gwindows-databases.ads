------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     G W I N D O W S . D A T A B A S E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2019 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;

with GNATCOM.Dispinterface;
with GNATCOM.Types;

with ADO.uConnection_Interface;
with ADO.uRecordset_Interface;

package GWindows.Databases is

   -------------------------------------------------------------------------
   --  Database_Type
   -------------------------------------------------------------------------
   --  Database connection object

   type Database_Type is
     new Ada.Finalization.Limited_Controlled with private;

   function Interfac (This : Database_Type)
                      return GNATCOM.Dispinterface.Dispinterface_Type;

   -------------------------------------------------------------------------
   --  Database_Type - Methods
   -------------------------------------------------------------------------

   procedure Open (Database          : in out Database_Type;
                   Connection_String : in     GString;
                   User_ID           : in     GString        := "";
                   Password          : in     GString        := "");
   --  Open connection to database

   procedure Close (Database : in out Database_Type);
   --  Close connection to database

   function Execute (Database : in Database_Type;
                     Query    : in GString)
                    return Natural;
   --  Executes a Query and returns number of affected rows

   procedure Execute (Database : in out Database_Type;
                      Query    : in     GString);
   --  Executes a Query

   procedure Command_Timeout
     (Database : in out Database_Type;
      Seconds  :        Natural);
   --  Changes the command timeout, in seconds.
   --  "How long to wait while executing a command before terminating
   --  the attempt and generating an error."
   --  0 means wait indefinitely. 30 is the preset value.

   function Command_Timeout (Database : in Database_Type) return Natural;

   procedure Connection_Timeout
     (Database : in out Database_Type;
      Seconds  :        Natural);
   --  Changes the connection timeout, in seconds.
   --  "How long to wait while establishing a connection before terminating
   --  the attempt and generating an error."
   --  0 means wait indefinitely. 15 is the preset value.

   function Connection_Timeout (Database : in Database_Type) return Natural;

   -------------------------------------------------------------------------
   --  Trans functions
   -------------------------------------------------------------------------
   procedure Begin_Trans (Database : in out Database_Type);

   procedure Commit_Trans (Database : in out Database_Type);

   procedure Rollback_Trans (Database : in out Database_Type);

   -------------------------------------------------------------------------
   --  Recordset_Type
   -------------------------------------------------------------------------

   type Recordset_Type is
     new Ada.Finalization.Limited_Controlled with private;

   function Interfac (This : Recordset_Type)
                      return GNATCOM.Dispinterface.Dispinterface_Type;

   -------------------------------------------------------------------------
   --  Recordset_Type - Properties
   -------------------------------------------------------------------------

   function Edit_Mode (Recordset : Recordset_Type) return Boolean;
   --  Returns true if current record is in edit mode

   function BOF (Recordset : Recordset_Type) return Boolean;
   --  Current record before first record

   function EOF (Recordset : Recordset_Type) return Boolean;
   --  Current record after last record

   function Record_Count (Recordset : Recordset_Type) return Integer;
   --  Number of records in recorset or -1 if this can not be determined

   function Field_Count (Recordset : Recordset_Type) return Natural;
   --  Number of fields in each record

   function Field_Name (Recordset : Recordset_Type;
                        Index     : Positive)
                        return GString;
   --  Name of field at index

   function Field_Updatable (Recordset : in Recordset_Type;
                             Index     : in Positive)
                            return Boolean;
   function Field_Updatable (Recordset : in Recordset_Type;
                             Name      : in GString)
                            return Boolean;
   --  If field can be updated

   function Field_Value (Recordset : Recordset_Type;
                         Index     : Positive)
                        return GString;
   function Field_Value (Recordset : Recordset_Type;
                         Name      : GString)
                        return GString;
   function Field_Value (Recordset : Recordset_Type;
                         Index     : Positive)
                        return GNATCOM.Types.VARIANT;
   function Field_Value (Recordset : Recordset_Type;
                         Name      : GString)
                        return GNATCOM.Types.VARIANT;
   procedure Field_Value (Recordset : in out Recordset_Type;
                          Index     : in     Positive;
                          Value     : in     GString);
   procedure Field_Value (Recordset : in out Recordset_Type;
                          Name      : in     GString;
                          Value     : in     GString);
   procedure Field_Value (Recordset : in out Recordset_Type;
                          Index     : in     Positive;
                          Value     : in     GNATCOM.Types.VARIANT;
                          Clear     : in     Boolean               := True);
   procedure Field_Value (Recordset : in out Recordset_Type;
                          Name      : in     GString;
                          Value     : in     GNATCOM.Types.VARIANT;
                          Clear     : in     Boolean               := True);
   --  Field value of this record

   -------------------------------------------------------------------------
   --  Recordset_Type - Methods
   -------------------------------------------------------------------------

   type Cursor_Type is (Dynamic, Forward_Only, Keyset, Static);

   type Lock_Type is (Batch_Optimistic,
                      Optimistic,
                      Pessimistic,
                      Read_Only);

   procedure Open (Recordset : in out Recordset_Type;
                   Database  : in     Database_Type'Class;
                   Query     : in     GString;
                   Cursor    : in     Cursor_Type;
                   Lock      : in     Lock_Type);
   --  Open recordset

   procedure Close (Recordset : in out Recordset_Type);
   --  Close recordset

   procedure Requery (Recordset : in out Recordset_Type);
   --  Rerun recordset's query

   procedure Resync (Recordset : in out Recordset_Type);
   --  Refresh recordset data

   procedure Add_New (Recordset : in out Recordset_Type);
   --  Add a new record to recordset
   --  Call Update to post changes to the new record to the database

   procedure Update (Recordset : in out Recordset_Type);
   --  Post changes to current record

   procedure Cancel_Update (Recordset : in out Recordset_Type);
   --  Cancel an Add_New in progress or changes being made
   --  to the current record

   procedure Delete (Recordset : in out Recordset_Type);
   --  Delete current record

   procedure Move_First (Recordset : in out Recordset_Type);
   procedure Move_Last (Recordset : in out Recordset_Type);
   procedure Move_Next (Recordset : in out Recordset_Type);
   procedure Move_Previous (Recordset : in out Recordset_Type);
   --  Move to XXXX record

   type Move_From_Type is (Current, First, Last);

   procedure Move (Recordset : in out Recordset_Type;
                   Count     : in     Integer;
                   From      : in     Move_From_Type := Current);
   --  Move cursor count records from

   -------------------------------------------------------------------------
   --  Recordset_Type - Event Types
   -------------------------------------------------------------------------

   type Before_Change_Event is access
     procedure (Recordset : in out Recordset_Type'Class;
                Change    : out    Boolean);

   type Change_Event is access
     procedure (Recordset : in out Recordset_Type'Class);

   type Before_Field_Change_Event is access
     procedure (Recordset : in out Recordset_Type'Class;
                Name      : in     GString;
                Value     : in out GNATCOM.Types.VARIANT;
                Change    :    out Boolean);

   type Field_Change_Event is access
     procedure (Recordset : in out Recordset_Type'Class;
                Name      : in     GString;
                Value     : in     GNATCOM.Types.VARIANT);

   -------------------------------------------------------------------------
   --  Recordset_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Before_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event);
   procedure Fire_On_Before_Change (Recordset : in out Recordset_Type;
                                    Change    :    out Boolean);

   procedure On_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event);
   procedure Fire_On_Change (Recordset : in out Recordset_Type);

   procedure On_Before_Field_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Field_Change_Event);
   procedure Fire_On_Before_Field_Change
     (Recordset : in out Recordset_Type;
      Name      : in     GString;
      Value     : in out GNATCOM.Types.VARIANT;
      Change    :    out Boolean);

   procedure On_Field_Change_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Field_Change_Event);
   procedure Fire_On_Field_Change (Recordset : in out Recordset_Type;
                                   Name      : in     GString;
                                   Value     : in     GNATCOM.Types.VARIANT);

   procedure On_Before_Update_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event);
   procedure Fire_On_Before_Update (Recordset : in out Recordset_Type;
                                    Change    :    out Boolean);

   procedure On_Update_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event);
   procedure Fire_On_Update (Recordset : in out Recordset_Type);

   procedure On_Before_Add_New_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event);
   procedure Fire_On_Before_Add_New (Recordset : in out Recordset_Type;
                                     Change    :    out Boolean);

   procedure On_Add_New_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event);
   procedure Fire_On_Add_New (Recordset : in out Recordset_Type);

   procedure On_Before_Delete_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Before_Change_Event);
   procedure Fire_On_Before_Delete (Recordset : in out Recordset_Type;
                                     Change    :    out Boolean);

   procedure On_Delete_Handler
     (Recordset  : in out Recordset_Type;
      Handler    : in     Change_Event);
   procedure Fire_On_Delete (Recordset : in out Recordset_Type);

   -------------------------------------------------------------------------
   --  Recordset_Type - Events
   -------------------------------------------------------------------------

   procedure On_Before_Change (Recordset : in out Recordset_Type;
                               Change    :    out Boolean);
   --  Request to current change record

   procedure On_Change (Recordset : in out Recordset_Type);
   --  Current record has changed

   procedure On_Before_Field_Change (Recordset : in out Recordset_Type;
                                     Name      : in     GString;
                                     Value     : in out GNATCOM.Types.VARIANT;
                                     Change    :    out Boolean);
   --  Request to change field value

   procedure On_Field_Change (Recordset : in out Recordset_Type;
                              Name      : in     GString;
                              Value     : in     GNATCOM.Types.VARIANT);
   --  Field value has changed

   procedure On_Before_Update (Recordset : in out Recordset_Type;
                               Change    :    out Boolean);
   --  Request to update record

   procedure On_Update (Recordset : in out Recordset_Type);
   --  Current record has been updated

   procedure On_Before_Add_New (Recordset : in out Recordset_Type;
                                Change    :    out Boolean);
   --  Request to add a new record

   procedure On_Add_New (Recordset : in out Recordset_Type);
   --  Current record represents a new record to be added

   procedure On_Before_Delete (Recordset : in out Recordset_Type;
                               Change    :    out Boolean);
   --  Request to update record

   procedure On_Delete (Recordset : in out Recordset_Type);
   --  Current record has been updated

   -------------------------------------------------------------------------
   --  Objectless utilities
   -------------------------------------------------------------------------

   function Simple_Query (Query             : in     GString;
                          Connection_String : in     GString;
                          User_ID           : in     GString        := "";
                          Password          : in     GString        := "")
                         return GString;
   --  Returns an atomic information from a database, that is
   --  the first element (first column of first row) of a query's result

private

   type Database_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Connection : ADO.uConnection_Interface.uConnection_Type;
      end record;

   type Recordset_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         Recordset : ADO.uRecordset_Interface.uRecordset_Type;

         On_Before_Change_Event       : Before_Change_Event       := null;
         On_Change_Event              : Change_Event              := null;
         On_Before_Field_Change_Event : Before_Field_Change_Event := null;
         On_Field_Change_Event        : Field_Change_Event        := null;
         On_Before_Update_Event       : Before_Change_Event       := null;
         On_Update_Event              : Change_Event              := null;
         On_Before_Add_New_Event      : Before_Change_Event       := null;
         On_Add_New_Event             : Change_Event              := null;
         On_Before_Delete_Event       : Before_Change_Event       := null;
         On_Delete_Event              : Change_Event              := null;
      end record;

end GWindows.Databases;
