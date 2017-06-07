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
with GWindows;use GWindows;
with Win32; use Win32;

separate (Drive_Devices.Drive_Device)


function Get_Volume_Information(Drive_Letter : GCharacter;
                                Volume_Info  : in out Volume_Information)
                                return Win32.BOOL
is
   use System,IC;
   IOCTL_DISK_GET_LENGTH_INFO : constant := 16#7405C#;

   Drive_Name:GString:="\\.\" & Drive_Letter & ":" & GCharacter'Val (0);

   H_Device:HANDLE:=CreateFileW
     (lpFileName            => To_PCWSTR(Drive_Name'Address),
      dwDesiredAccess       => GENERIC_READ,
      dwShareMode           => FILE_SHARE_READ + FILE_SHARE_WRITE,
      lpSecurityAttributes  => null,
      dwCreationDisposition => OPEN_EXISTING,
      dwFlagsAndAttributes  => 0,
      hTemplateFile         => Null_Address);
begin
   if H_Device = INVALID_HANDLE_VALUE then
      return Win32.FALSE;
   else
      declare
         Junk:DWORD;
         Size:DWORD:=IC.unsigned_long(Interfaces.Shift_Right
                                      (Interfaces.Unsigned_32(Volume_Info.Geometry'Size),3));
         Result:BOOL:=DeviceIoControl
           (hDevice         => H_Device,
            dwIoControlCode => IOCTL_DISK_GET_DRIVE_GEOMETRY,
            lpInBuffer      => Null_Address,
            nInBufferSize   => 0,
            lpOutBuffer     => Volume_Info.Geometry'Address,
            nOutBufferSize  =>
              IC.unsigned_long(Interfaces.Shift_Right(Interfaces.Unsigned_32(Volume_Info.Geometry'Size),3)),
              -- >> 3, /8 bits to get size in bytes
            lpBytesReturned => To_PULONG(Junk'Address),
            lpOverlapped    => null);
      begin
         if Result = Win32.TRUE then
            Result := DeviceIoControl
              (hDevice         => H_Device,
               dwIoControlCode => IOCTL_DISK_GET_LENGTH_INFO,
               lpInBuffer      => Null_Address,
               nInBufferSize   => 0,
               lpOutBuffer     => Volume_Info.Size'Address,
               nOutBufferSize  =>
                 IC.unsigned_long(Interfaces.Shift_Right(Interfaces.Unsigned_32( Volume_Info.Size'Size),3)),
                 -- >> 3, /8 bits to get size in bytes
               lpBytesReturned => To_PULONG(Junk'Address),
               lpOverlapped    => null);
         end if;

         if CloseHandle(H_Device) /= Win32.TRUE then
            null;
         end if;
         return Result;
      end;

   end if;

end Get_Volume_Information;
