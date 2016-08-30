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

with Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Exceptions,
     Ada.Finalization,
     Ada.Strings.Hash,
     Ada.Task_Identification,
     Ada.Task_Termination,
     Ada.Unchecked_Deallocation,

     Win32,
     Win32.Winioctl,
     Win32.Winnt;



with Main_Window;

with System;

package Drive_Devices is

   ---------------------------------------
   package Drive_Device is

      type Drive_Record(Label_Length:Natural;FS_Length:Natural) is tagged private;
      type Drive is access all Drive_Record;
      function Init(Letter : Character; The_Drive : out Drive) return Boolean;
      function Get_Letter(Self : Drive_Record) return Character;
      function Get_Size(Self : Drive_Record) return Win32.ULONGLONG;
      function "="(Left,Right : Drive_Record) return Boolean;

      type Algorithm is (HMG_IS5,HMG_IS5_EXT,GOST_R50739_95,SCHNEIER);

      procedure Clear(Drive            : Drive_Record;
                      Drive_Index      : Natural;
                      Chosen_Algorithm : in Algorithm := HMG_IS5;
                      Buf_Size_Mb_Mult : Win32.ULONG := 1048576);

   private

      type Drive_Record(Label_Length:Natural;FS_Length:Natural) is tagged
         record
            Letter      : Character;
            Label       : String(1..Label_Length);
            File_System : String(1..FS_Length);
            M_Type      : Win32.Winioctl.MEDIA_TYPE;
            Size        : Win32.ULONGLONG;
         end record;

      ---------------------------------------
      package Clear_Drive_Device is

         type Clear_Drive_Record;
         type Clear_Drive is access all Clear_Drive_Record;

         task type Clear_Drive_Task is
            entry Start(Drive         : Clear_Drive;
                        The_Algorithm : Algorithm);
         end Clear_Drive_Task;

         type Clear_Drive_Task_Ptr is access all Clear_Drive_Task;

         type Clear_Drive_Task_Data is
            record
               Clear_Task          : Clear_Drive_Task_Ptr;
               Task_Iter_Step_Time : Duration;
            end record;

         type Clear_Drive_Task_Data_Ptr is access all Clear_Drive_Task_Data;

         procedure Free_Task is new Ada.Unchecked_Deallocation(Object => Clear_Drive_Task,
                                                               Name   => Clear_Drive_Task_Ptr);

         procedure Free_Task_Data is new Ada.Unchecked_Deallocation(Object => Clear_Drive_Task_Data,
                                                                    Name   => Clear_Drive_Task_Data_Ptr);

         function Hash(Key : Ada.Task_Identification.Task_Id) return Ada.Containers.Hash_Type;

         package Clear_Drive_Task_Containers is
           new Ada.Containers.Indefinite_Hashed_Maps(Key_Type        => Ada.Task_Identification.Task_Id,
                                                     Element_Type    => Clear_Drive_Task_Data_Ptr,
                                                     Hash            => Hash,
                                                     Equivalent_Keys => Ada.Task_Identification."=");

         protected Coordinator is
            procedure Track(Ptr : in Clear_Drive_Task_Data_Ptr);
            procedure Set_Delay_Timeout(Delay_Timeout : Duration) with Inline;
            function Get_Common_Delay_Timeout return Duration with Inline;
         private
            Tasks : Clear_Drive_Task_Containers.Map;
            Task_Count : Natural := 0;
         end Coordinator;

         --http://unethicalblogger.com/2013/03/09/dynamic-tasks-in-ada.html
         --task termination

         type Clear_Drive_Record is new Drive_Record with
            record
               Buf_Size     : Win32.ULONG;
               Buf_Count    : Win32.ULONGLONG;
               Buf_Rem_Size : Win32.ULONGLONG;

               H_FS        : Win32.Winnt.HANDLE := System.Null_Address;
               Buf_Index   : Natural := 0;
               Pos         : Win32.Winnt.LARGE_INTEGER(Win32.Winnt.u_kind):=
                 Win32.Winnt.LARGE_INTEGER'(Which => Win32.Winnt.u_kind,u => Win32.Winnt.anonymous1_t'(0,0));
               Pass        : Positive := 1;
               Pass_Count  : Positive;
               Drive_Index : Natural;
            end record;

         procedure Clear(Self            : access Clear_Drive_Record;
                         Clear_Algorithm : Algorithm);



         function Init(Drive       : Drive_Record;
                       Buf_Size    : Win32.ULONG;
                       Drive_Index : Natural)
                       return Clear_Drive;

      private

         function Open(Self : in out Clear_Drive_Record) return Boolean with Inline;
         function Lock(Self : Clear_Drive_Record) return Boolean with Inline;
         function Unlock(Self : Clear_Drive_Record) return Boolean with Inline;
         function Close(Self : Clear_Drive_Record) return Boolean with Inline;
         procedure Format(Self : Clear_Drive_Record) with Inline;

      end Clear_Drive_Device;

   end Drive_Device;
   ---------------------------------------

   package Drives_Container is new Ada.Containers.Indefinite_Vectors(Index_Type   => Natural,
                                                                     Element_Type => Drive_Device.Drive_Record,
                                                                     "="          => Drive_Device."=");
   type Drives_Vector is new Drives_Container.Vector with null record;

   type Drives_Record is tagged private;
   type Drives is access all Drives_Record;
   procedure Init;
   function Get_Drives return Drives;

   function Get_Vector(Drives : access Drives_Record'Class) return Drives_Vector;

   procedure Set_Selected_Index(Drives : access Drives_Record;
                                Index  : Integer);
   function Get_Selected_Index(Drives : access Drives_Record) return Integer;

   Init_Drive_Error : exception;

private

   type Drives_Record is tagged
      record
         Drives   : Drives_Vector;
         Selected : Integer; --cause of possible -1
      end record;

end Drive_Devices;
