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

separate (Drive_Devices)


package body Drive_Device is


   -- inner function --
   -- Get_Drive_Geometry --

   type Volume_Information is
      record
         Geometry : DISK_GEOMETRY;
         Size     : LARGE_INTEGER(QuadPart_kind);
      end record;



   function Get_Volume_Information(Drive_Letter : Character;
                                   Volume_Info  : in out Volume_Information)
                                   return Win32.BOOL is separate;

   -- Clear --

   procedure Clear(Drive            : Drive_Record;
                   Drive_Index      : Natural;
                   Chosen_Algorithm : in Algorithm := HMG_IS5;
                   Buf_Size_Mb_Mult : Win32.ULONG := 1048576)
   is
      use GWindows.Message_Boxes;
      use type IC.unsigned_long;

      Ret_Val : Message_Box_Result := Message_Box(GWindows.Base.Base_Window_Type(Main_Window.Get_X_Main.all),
                                                  "Attention!","All data will be erased. Continue?",
                                                  Yes_No_Box,Question_Icon);
      ---
   begin -- for Clear_Data
      case Ret_Val is
         when Yes =>
            declare
               Clr_Drv:Clear_Drive_Device.Clear_Drive := Clear_Drive_Device.Init(Drive,
                                                                                 Buf_Size_Mb_Mult * 1,
                                                                                 Drive_Index);
            begin
               Clr_Drv.Clear(Clear_Algorithm => Chosen_Algorithm);
            end;
            when others =>
            null;
      end case;

   end Clear;



   -- Init --

   function Init(Letter : Character; The_Drive : out Drive) return Boolean is separate;

   -- ACCESSORS --

   function Get_Letter(Self : Drive_Record) return Character
   is
   begin
      return Self.Letter;
   end Get_Letter;

   function Get_Size(Self : Drive_Record) return Win32.ULONGLONG
   is
   begin
      return Self.Size;
   end Get_Size;


   -- "=" --

   function "="(Left,Right : Drive_Record) return Boolean
   is
   begin
      if Left.Label_Length = Right.Label_Length and then
        Left.Letter = Right.Letter and then
        Left.Label = Right.Label and then
        Left.M_Type = Right.M_Type and then
        Left.Size = Right.Size then
         return True;
      else
         return False;
      end if;
   end "=";


   ----------------------------------
   -------- PRIVATE SECTION ---------
   ----------------------------------

   package body Clear_Drive_Device is separate;


end Drive_Device;
