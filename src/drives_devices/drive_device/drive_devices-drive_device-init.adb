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

separate (Drive_Devices.Drive_Device)


function Init(Letter : Character; The_Drive : out Drive) return Boolean
is
   use System,IC;
   Drive_Name:String:= Letter & ":\" & ASCII.NUL; -- open File System to get partition size
   Volume_Name_C:IC.char_array(1..MAX_PATH+1);

   File_System_Name_C:IC.char_array(1..MAX_PATH+1);
   function To_LPSTR is
     new Ada.Unchecked_Conversion(Address, LPSTR);
   function To_LPCSTR is
     new Ada.Unchecked_Conversion(Address, LPCSTR);


   Volume_Info : Volume_Information;

begin
   if GetVolumeInformation
     (lpRootPathName           => To_LPCSTR(Drive_Name'Address),
      lpVolumeNameBuffer       => To_LPSTR(Volume_Name_C'Address),
      nVolumeNameSize          => MAX_PATH+1,
      lpVolumeSerialNumber     => null,
      lpMaximumComponentLength => null,
      lpFileSystemFlags        => null,
      lpFileSystemNameBuffer   => To_LPSTR(File_System_Name_C'Address),
      nFileSystemNameSize      => MAX_PATH+1) = Win32.TRUE
     and then
       Get_Volume_Information(Drive_Letter => Letter,
                              Volume_Info  => Volume_Info) = Win32.True
   --         Get_Drive_Geometry(Drive_Letter => Letter,
   --                            D_Geometry   => D_Geometry) = Win32.True
   --       and then
   --         D_Geometry.MediaType = FixedMedia
   then
      declare
         use ASCII,Interfaces;
         function To_Unsigned_32 is
           new Ada.Unchecked_Conversion(Address, Unsigned_32);

         Drive_Size:ULONGLONG:=0;
         Volume_Name:String:=To_Ada(Volume_Name_C);
         File_System_Name : String := To_Ada(File_System_Name_C);
      begin

         --Asm("int $3");
         Asm("finit" & LF & HT &
               "fildq %1" & LF & HT &
               "fistpq %0", -- Get the result
             Outputs  => ULONGLONG'Asm_Output("=m",Drive_Size),-- %0
             Inputs   =>
               (LONGLONG'Asm_Input
                  ("m",Volume_Info.Size.QuadPart)));-- %1


         The_Drive:=new Drive_Record'(Label_Length => Volume_Name'Length,
                                      FS_Length    => File_System_Name'Length,
                                      Letter       => Letter,
                                      Label        => Volume_Name,
                                      File_System  => File_System_Name,
                                      M_Type       => Volume_Info.Geometry.MediaType,
                                      Size         => Drive_Size,
                                      Is_Cleaning  => False);
         return True;
      end;
   else
      return False;
   end if;
end Init;
