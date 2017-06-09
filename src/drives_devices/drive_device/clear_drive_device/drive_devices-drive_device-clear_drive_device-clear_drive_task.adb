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

with GWindows.GStrings; use GWindows.GStrings;

separate (Drive_Devices.Drive_Device.Clear_Drive_Device)

task body Clear_Drive_Task
is
   use GWindows.Base,
       GWindows.Message_Boxes;

   Drives : Drives_Ptr := Get_Drives;
   Data : Clear_Drive;
   --Delay_Time : Duration;-- make dynamic
   Clear_Algorithm : Algorithm;
   Drv_List : Main_Window.List_View.X_List_View:=Main_Window.Get_X_Main.Get_Volume_List;

   -- HMG_IS5_Proc --

   procedure HMG_IS5_Proc with Inline
   is
   begin
      -- Set algorithm name
      Drv_List.Set_Algorithm_Name(Data.Drive_Index,"HMG IS5 (Base)");
      -- Set pass count
      Data.Pass_Count := 1;

      -- Clean drive
      Drv_List.Set_Operation_Name(Data.Drive_Index,"Writing 0x0...");

      while ULONGLONG(Data.Buf_Index) < Data.Buf_Count loop
         exit
           when Drives.Is_Canceled(Data.Drive_Index);
         Data.HMG_IS5_Clear_Main(Drv_List);
         Data.Buf_Index:=Data.Buf_Index+1;
      end loop;
      -- Clean rest bytes
      if not Drives.Is_Canceled(Data.Drive_Index) then
         Data.HMG_IS5_Clear_Rest(Drv_List);
         Data.Is_Canceled := False;
      end if;

   end HMG_IS5_Proc;

   -- HMG_IS5_Enh_Proc --

   procedure HMG_IS5_Enh_Proc with Inline
   is
   begin
      -- Set algorithm name
      Drv_List.Set_Algorithm_Name(Data.Drive_Index,"HMG IS5 (Enhanced)");
      -- Set pass count
      Data.Pass_Count := 3;

      -- Clean drive
      for Pass in 1..3 loop
         Data.Buf_Index:=0;
         Data.Pass := Pass;

         case Pass is
            when 1 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x0...");
            when 2 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x1...");
            when 3 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) &
                                             " : Writing random values...");
         end case;

         while ULONGLONG(Data.Buf_Index) < Data.Buf_Count loop
            Data.HMG_IS5_Enh_Clear_Main(Drv_List);
            Data.Buf_Index:=Data.Buf_Index+1;
         end loop;
         -- Clean rest bytes
         Data.HMG_IS5_Enh_Clear_Rest(Drv_List);
      end loop;
   end HMG_IS5_Enh_Proc;

   -- GOST_R50739_95_Proc --

   procedure GOST_R50739_95_Proc with Inline
   is
   begin
      -- Set algorithm name
      Drv_List.Set_Algorithm_Name(Data.Drive_Index,"GOST-R-50739-95");
      -- Set pass count
      Data.Pass_Count := 2;

      -- Clean drive
      -- Algorithm passes 1,2
      for Pass in 1..2 loop
         Data.Buf_Index:=0;
         Data.Pass := Pass;

         case Pass is
            when 1 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x0...");
            when 2 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing random values...");
         end case;

         while ULONGLONG(Data.Buf_Index) < Data.Buf_Count loop
            Data.GOST_R50739_95_Clear_Main(Drv_List);
            Data.Buf_Index:=Data.Buf_Index+1;
         end loop;
         -- Clean rest bytes
         Data.GOST_R50739_95_Clear_Rest(Drv_List);
      end loop;

   end GOST_R50739_95_Proc;

   -- DoD5220_22_M_E_Proc --

   procedure DoD5220_22_M_E_Proc with Inline
   is
   begin
      -- Set algorithm name
      Drv_List.Set_Algorithm_Name(Data.Drive_Index,"US DoD 5220.22-M(E)");
      -- Set pass count
      Data.Pass_Count := 3;

      -- Clean drive
      for Pass in 1..3 loop
         Data.Buf_Index:=0;
         Data.Pass := Pass;

         case Pass is
            when 1 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x0...");
            when 2 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x1...");
            when 3 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing random values...");
         end case;

         while ULONGLONG(Data.Buf_Index) < Data.Buf_Count loop
            Data.HMG_IS5_Enh_Clear_Main(Drv_List);
            Data.Buf_Index:=Data.Buf_Index+1;
         end loop;
         -- Clean rest bytes
         Data.HMG_IS5_Enh_Clear_Rest(Drv_List);
      end loop;
   end DoD5220_22_M_E_Proc;

   -- SCHNEIER_Proc --
   procedure SCHNEIER_Proc with Inline
   is
   begin
      -- Set algorithm name
      Drv_List.Set_Algorithm_Name(Data.Drive_Index,"Bruce Schneier");
      -- Set pass count
      Data.Pass_Count := 7;

      -- Clean drive
      -- Algorithm passes 1,2
      for Pass in 1..7 loop
         Data.Buf_Index:=0;
         Data.Pass := Pass;

         case Pass is
            when 1 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x1...");
            when 2 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing 0x0...");
            when 3..7 =>
               Drv_List.Set_Operation_Name(Data.Drive_Index,Positive'Wide_Image(Pass) & " : Writing random values...");
         end case;

         while ULONGLONG(Data.Buf_Index) < Data.Buf_Count loop
            Data.SCHNEIER_Clear_Main(Drv_List);
            Data.Buf_Index:=Data.Buf_Index+1;
         end loop;
         -- Clean rest bytes
         Data.SCHNEIER_Clear_Rest(Drv_List);
      end loop;
   end SCHNEIER_Proc;

begin -- for Clear_Drive_Task

   accept Start(Drive         : Clear_Drive;
                The_Algorithm : Algorithm) do
      Data := Drive;
      Clear_Algorithm := The_Algorithm;
   end Start;

   -- Open and lock

   if Data.Open and then
     Data.Lock then

      --Clear data

      case Clear_Algorithm is
         when HMG_IS5 =>
            HMG_IS5_Proc;

         when HMG_IS5_ENH =>
            HMG_IS5_Enh_Proc;

         when GOST_R50739_95 =>
            GOST_R50739_95_Proc;

         when DoD5220_22_M_E =>
            DoD5220_22_M_E_Proc;

         when SCHNEIER =>
            SCHNEIER_Proc;
      end case;

      -- Unlock and close
      if Data.Unlock and then
        Data.Close then
         Drv_List.Set_Operation_Name(Item           => Data.Drive_Index,
                                     Operation_Name => "Formatting...");
         Data.Format;

         Display_Result_Operation:
         declare
            Result_Operation_Name : GString_Unbounded;
         begin
            if Drives.Is_Canceled(Data.Drive_Index) then
               Result_Operation_Name := To_GString_Unbounded("Canceled");
            else
               Result_Operation_Name := To_GString_Unbounded("Done");
            end if;
            Drv_List.Set_Operation_Name(Item           => Data.Drive_Index,
                                        Operation_Name => To_GString_From_Unbounded(Result_Operation_Name));
         end Display_Result_Operation;

      else
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Error while unblock and free drive's descriptor!",
                     Icon     => Error_Icon);
      end if;
   else
      Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                  "Error!","Error while opening and block drive's descriptor!",
                  Icon     => Error_Icon);
   end if;

end Clear_Drive_Task;
