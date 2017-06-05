--------------------------------------------------------------------------------------
--        x_Cleaner erases all data from selected volume with special algorithm
--
--        Copyright (C) 2016  George Ivanov
--
--        This program is free software: you can redistribute it and/or modify
--        it under the terms of the GNU General Public License as published by
--        the Free Software Foundation, either version 3 of the License, or
--        (at your option) any later version."
--
--        This program is distributed in the hope that it will be useful,
--        but WITHOUT ANY WARRANTY; without even the implied warranty of
--        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--        GNU General Public License for more details.
--
--        You should have received a copy of the GNU General Public License
--        along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Real_Time,
     Ada.Containers.Vectors;

with Ada.Numerics,
     Ada.Numerics.Float_Random,
     Ada.Numerics.Elementary_Functions;
---
with Win32; use Win32;
with Win32.Winbase; use Win32.Winbase;
with Win32.Windef; use Win32.Windef;
with Win32.Winioctl; use Win32.Winioctl;

with Win32.Winuser; use Win32.Winuser;
with Win32.Shellapi;
---
with Interfaces;
with Interfaces.C;
---
with GWindows,
     GWindows.Base,
     GWindows.GStrings,
     GWindows.Message_Boxes,
     GWindows.Application,
     GWindows.Buttons,
     GWindows.Constants,
     GWindows.Drawing_Objects,
     GWindows.Windows,
     GWindows.Static_Controls;
---
with System.Machine_Code; use System.Machine_Code;

package body Drive_Devices is

   package IC renames Interfaces.C;

   use Ada.Strings.Unbounded;
   use Win32.Winnt;

   Sys_Drives : Drives := new Drives_Record;

   package body Drive_Device is separate;

   -- ACCESSORS --

   function Get_Drives return Drives
   is

   begin
      return Sys_Drives;
   end Get_Drives;


   function Get_Vector(Drives : access Drives_Record'Class) return Drives_Vector
   is
   begin
      return Drives.Drives;
   end Get_Vector;

   function Get_Selected_Index(Drives : access Drives_Record) return Integer
   is
   begin
      return Drives.Selected;
   end Get_Selected_Index;

   -- SETTERS --

   procedure Set_Selected_Index(Drives : access Drives_Record;
                                Index  : Integer)
   is
   begin
      Drives.Selected := Index;
   end Set_Selected_Index;

   ---------------------------------------
   -- CONSTRUCTORS --
   ---------------------------------------

   procedure Init
   is
      use GWindows.Base,
          GWindows.Message_Boxes;

      use type IC.unsigned_long;

      Ret_Drive  : Drive_Device.Drive := null;
      Drive_Letter:Character := 'A';
      Drive_Mask:DWORD       := GetLogicalDrives;
   begin
      while Drive_Mask /= 0 loop
         if (Drive_Mask and 1) = 1 and then
           Drive_Device.Init(Drive_Letter,Ret_Drive) then
            Sys_Drives.Drives.Append(Ret_Drive.all);
         end if;

         Drive_Letter:=Character'Succ(Drive_Letter);

         Drive_Mask:=Interfaces.C.unsigned_long
           (Interfaces.Shift_Right(Interfaces.Unsigned_32(Drive_Mask),1));

      end loop;

   exception
      when Init_Drive_Error =>
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Error during drives initialization with drive " & Drive_Letter,
                     Icon     => Error_Icon);

         raise Init_Drive_Error;
   end Init;

end Drive_Devices;
