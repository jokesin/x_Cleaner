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
with GWindows; use GWindows;

separate (Drive_Devices)

package body Drive_Device is

   -- inner function --
   -- Get_Drive_Geometry --

   type Volume_Information is
      record
         Geometry : DISK_GEOMETRY;
         Size     : LARGE_INTEGER(QuadPart_kind);
      end record;



   function Get_Volume_Information(Drive_Letter : GCharacter;
                                   Volume_Info  : in out Volume_Information)
                                   return Win32.BOOL is separate;

   -- Clear --

   procedure Clear(Drive            : access Drive_Record;
                   Drive_Index      : Natural;
                   Chosen_Algorithm : in Algorithm := HMG_IS5;
                   Buf_Size_Mb_Mult : Win32.ULONG := 1048576)
   is
      use GWindows.Application,
          GWindows.Buttons,
          GWindows.Constants,
          GWindows.Drawing_Objects,
          GWindows.Windows,
          GWindows.Static_Controls;

      use type IC.unsigned_long,IC.long,IC.int;

      --
      function Show_Attention_Message_Dialog return Integer is
         Dialog        : Window_Type;
         OK_Button,
         Cancel_Button : Default_Button_Type;
         Font          : Font_Type;
      begin
         Create_As_Dialog(Dialog,"Attention!",Width => 190,Height => 100);
         Font.Create_Stock_Font(Default_GUI);
         Dialog.Set_Font(Font);
         Create_Label(Dialog,"All data will be erased. Continue?",10,10,
                      Dialog.Client_Area_Width - 20, 25, Center);

         OK_Button.Create(Dialog,"O&k",10,35,75,25,ID => GWindows.Constants.IDOK);
         Cancel_Button.Create(Dialog,"&Cancel",95,35,75,25,ID => GWindows.Constants.IDCANCEL);

         -- Set center pos
         Main_Window.Get_X_Main.Set_Dialog_Center_Pos(Dialog);

         return Show_Dialog(Dialog);
      end Show_Attention_Message_Dialog;
      --
      Ret_Val       : Integer;
   begin -- for Clear_Data

      Ret_Val := Show_Attention_Message_Dialog;
      case Ret_Val is
         when GWindows.Constants.IDOK =>
            Drive.Set_Cleaning_State(True);
            declare
               Clr_Drv:Clear_Drive_Device.Clear_Drive := Clear_Drive_Device.Init(Drive,
                                                                                 Buf_Size_Mb_Mult * 1,
                                                                                 Drive_Index);
            begin
               Clr_Drv.Clear(Clear_Algorithm => Chosen_Algorithm);
            end;
         when GWindows.Constants.IDCANCEL =>
            Drive.Set_Cleaning_State(False);
         when others =>
            null;
      end case;

   end Clear;

   -- Init --

   function Init(Letter : GCharacter; The_Drive : out Drive) return Boolean is separate;

   -- ACCESSORS --

   function Get_Letter(Self : Drive_Record) return GCharacter
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


   -- Is_Cleaning
   function Is_Cleaning(Drive : access Drive_Record) return Boolean is
   begin
      return Drive.Is_Cleaning;
   end Is_Cleaning;

   procedure Set_Cleaning_State(Drive : access Drive_Record;
                                State : Boolean)
   is
   begin
      Drive.Is_Cleaning := State;
   end Set_Cleaning_State;

   -- Is_Canceled --
   function Is_Canceled(Drive : access Drive_Record)
                        return Boolean is
   begin
      return Drive.Is_Canceled;
   end Is_Canceled;

   -- Set_Cancel_State
   procedure Set_Cancel_State(Drive : access Drive_Record;
                              State : Boolean) is
   begin
      Drive.Is_Canceled := State;
   end Set_Cancel_State;


   package body Clear_Drive_Device is separate;


end Drive_Device;
