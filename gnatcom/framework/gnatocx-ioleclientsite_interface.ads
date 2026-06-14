with GNATCOM.Iinterface;
with Win32_Types;

package GNATOCX.IOleClientSite_Interface is

   type IOleClientSite_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IOleClientSite_Type);

   function Pointer (This : IOleClientSite_Type)
     return Pointer_To_IOleClientSite;

   procedure Attach (This    : in out IOleClientSite_Type;
                     Pointer : in     Pointer_To_IOleClientSite);

   procedure SaveObject
     (This : IOleClientSite_Type);

   procedure GetMoniker
     (This           : IOleClientSite_Type;
      dwAssign       : Win32_Types.Unsigned_Long;
      dwWhichMoniker : Win32_Types.Unsigned_Long;
      ppmk           : Pointer_To_Pointer_To_IMoniker);

   procedure GetContainer
     (This        : IOleClientSite_Type;
      ppContainer : Pointer_To_Pointer_To_IOleContainer);

   procedure ShowObject
     (This : IOleClientSite_Type);

   procedure OnShowWindow
     (This  : IOleClientSite_Type;
      fShow : Win32_Types.Long);

   procedure RequestNewObjectLayout
     (This : IOleClientSite_Type);

end GNATOCX.IOleClientSite_Interface;
