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

package body Clear_Drive_Device is

   use Ada.Numerics,
       Ada.Numerics.Float_Random,
       Ada.Numerics.Elementary_Functions;

   -- Coordinator --

   protected body Coordinator is

      -- Last_Entry --

      procedure Last_Entry(C  : Ada.Task_Termination.Cause_Of_Termination;
                           ID : Ada.Task_Identification.Task_Id;
                           X  : Ada.Exceptions.Exception_Occurrence)
      is -- for Last_Entry
         Task_Data : Clear_Drive_Task_Data_Ptr := Tasks.Element(ID);
      begin
         Tasks.Delete(Key => ID);
         Free_Task(Task_Data.Clear_Task);
         Free_Task_Data(Task_Data);
      end Last_Entry;

      -- Track --

      procedure Track(Ptr : in Clear_Drive_Task_Data_Ptr)
      is
         Key : constant Ada.Task_Identification.Task_Id := Ptr.Clear_Task.all'Identity;
      begin -- for Track

         Tasks.Insert(Key      => Key,
                      New_Item => Ptr);

         --Set task termination handler
         Ada.Task_Termination.Set_Specific_Handler(Key,Last_Entry'Access);

      end Track;

      -- Set_Delay_Timeout --

      procedure Set_Delay_Timeout(Delay_Timeout : Duration) is
         -- get current task data
         Key       : constant Ada.Task_Identification.Task_Id := Ada.Task_Identification.Current_Task;
         Task_Data : Clear_Drive_Task_Data_Ptr := Tasks.Element(Key);

         procedure Update_Delay_Time (Key     : in     Ada.Task_Identification.Task_Id;
                                      Element : in out Clear_Drive_Task_Data_Ptr) is
         begin
            Element.Task_Iter_Step_Time := Delay_Timeout;
         end Update_Delay_Time;
      begin -- for Set_Delay_Timeout
         Tasks.Update_Element(Position => Tasks.Find(Key),
                              Process  => Update_Delay_Time'Access);
      end Set_Delay_Timeout;


      -- Get_Common_Delay_Timeout --

      function Get_Common_Delay_Timeout return Duration -- Get time duration without current task
      is
         use Clear_Drive_Task_Containers;
         use type Ada.Task_Identification.Task_Id,
             Ada.Containers.Count_Type;

         Current_Task_ID : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Current_Task;
         Common_Delay_Timeout : Duration := 0.0;
      begin
         if Tasks.Length = 1 then
            Common_Delay_Timeout := 0.001; -- for 1 task
         else
            for C in Tasks.Iterate loop
               if Key(C) /= Current_Task_ID then
                  Common_Delay_Timeout := Common_Delay_Timeout + Element(C).Task_Iter_Step_Time;
               end if;
            end loop;
         end if;

         return Common_Delay_Timeout;
      end Get_Common_Delay_Timeout;

   end Coordinator;

   -------------------------------------------------------------------------------------

   -- Clear --
   procedure Clear(Self            : access Clear_Drive_Record;
                   Clear_Algorithm : Algorithm)
   is
      CT : Clear_Drive_Task_Data_Ptr := new Clear_Drive_Task_Data'(Clear_Task          => new Clear_Drive_Task,
                                                                   Task_Iter_Step_Time => 0.001);
   begin

      Coordinator.Track(CT);

      CT.Clear_Task.all.Start(Clear_Drive(Self),Clear_Algorithm);

   end Clear;


   -- SUPPORT FUNCTIONS AND PROCEDURES FOR TASK--

   function To_PLONG is new Ada.Unchecked_Conversion (System.Address, PLONG);

   function Random_Ada(Seed  : Generator)
                       return Byte
     with Inline
   is
   begin
      return Byte(255.0 * Random(Seed));-- [0..255]
   end Random_Ada;

   function SetFilePointer
     (hFile                : Win32.Winnt.HANDLE;
      lDistanceToMove      : Win32.ULONG;-- we have to use ULONG to use 2gb
      lpDistanceToMoveHigh : Win32.PLONG;
      dwMoveMethod         : Win32.DWORD)
      return Win32.DWORD with Import,
     Convention => Stdcall,
     External_Name => "SetFilePointer";

   -- INNER PROCEDURES FOR TASK

   -- Consecutive_Write --

   procedure Consecutive_Write(Self : not null access Clear_Drive_Record;Buffer : BYTE_Array) with Inline
   is
      use ASCII,
          GWindows.Message_Boxes,
          GWindows.Base;

      use type IC.long,IC.unsigned_long;
      Bytes_Written : aliased DWORD;
   begin
      Asm("finit" & LF & HT &
            "fildl %1" & LF & HT &
            "fimull %2" & LF & HT &
            "fistpq %0", -- Get the result
          Outputs  => LARGE_INTEGER'Asm_Output("=m",Self.Pos),-- %0
          Inputs   => (Natural'Asm_Input("m",Self.Buf_Index),-- %1
                       ULONG'Asm_Input("m",Self.Buf_Size)));-- %2

      Self.Pos.u.LowPart := SetFilePointer(hFile                => Self.H_FS,
                                           lDistanceToMove      => Self.Pos.u.LowPart,
                                           lpDistanceToMoveHigh => To_PLONG(Self.Pos.u.HighPart'Address),
                                           dwMoveMethod         => FILE_BEGIN);
      if WriteFile(Self.H_FS,Buffer'Address,Buffer'Length,Bytes_Written'Unchecked_Access,null) /= Win32.TRUE
      then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Consecutive_Write : WriteFile error!",
                     Icon     => Error_Icon);
         return;
      end if;
   end Consecutive_Write;

   -- HMG_IS5_Clear_Main --

   procedure HMG_IS5_Clear_Main(Self        : not null access Clear_Drive_Record;
                                Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      Zero_Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others => 0);
   begin -- for HMG_IS5_Proc
      Self.Consecutive_Write(Zero_Buffer);
      Drives_List.Set_Progress_Clean_Value(Self.Drive_Index,
                                           Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * 100.0));
   end HMG_IS5_Clear_Main;


   -- HMG_IS5_Clear_Rest --

   procedure HMG_IS5_Clear_Rest(Self        : not null access Clear_Drive_Record;
                                Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      Zero_Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others => 0);
   begin
      Self.Consecutive_Write(Zero_Buffer);
      Drives_List.Set_Progress_Clean_Value(Self.Drive_Index,
                                           Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * 100.0));
   end HMG_IS5_Clear_Rest;

   -- HMG_IS5_Ext_Clear_Main --

   procedure HMG_IS5_Enh_Clear_Main(Self        : not null access Clear_Drive_Record;
                                    Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      ---
      procedure Pass_1 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>0);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_1;
      ---
      procedure Pass_2 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>1);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_2;
      ---
      procedure Pass_3 with Inline
      is
         Seed   : Generator;
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>0);
      begin
         Reset(Seed);
            for K in Buffer'Range loop
               Buffer(K) := Random_Ada(Seed);
            end loop;
            Self.Consecutive_Write(Buffer);
      end Pass_3;
      ---
   begin -- for HMG_IS5_Ext
      case Self.Pass is
         when 1 =>
            Pass_1;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when 2=>
            Pass_2;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when 3=>
            Pass_3;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when others =>
            null;
      end case;
   end HMG_IS5_Enh_Clear_Main;


   -- HMG_IS5_Ext_Clear_Rest --

   procedure HMG_IS5_Enh_Clear_Rest(Self        : not null access Clear_Drive_Record;
                                    Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      ---
      procedure Pass_1 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>0);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_1;
      ---
      procedure Pass_2 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>1);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_2;
      ---
      procedure Pass_3 with Inline
      is
         Seed   : Generator;
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>0);
      begin
         Reset(Seed);
            for K in Buffer'Range loop
               Buffer(K) := Random_Ada(Seed);
            end loop;
            Self.Consecutive_Write(Buffer);
      end Pass_3;
      ---
   begin
      case Self.Pass is
         when 1 =>
            Pass_1;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when 2=>
            Pass_2;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index, Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when 3=>
            Pass_3;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index, Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 3.0)));
         when others =>
            null;
      end case;
   end HMG_IS5_Enh_Clear_Rest;

   -- DoD5220_22_M_E --

   procedure DoD5220_22_M_E_Clear_Main(Self        : not null access Clear_Drive_Record;
                                       Drives_List : Main_Window.List_View.X_List_View)
                                       renames HMG_IS5_Enh_Clear_Main;
   procedure DoD5220_22_M_E_Clear_Rest(Self        : not null access Clear_Drive_Record;
                                       Drives_List : Main_Window.List_View.X_List_View)
                                       renames HMG_IS5_Enh_Clear_Rest;
   -- GOST_R50739_95_Clear_Main --

   procedure GOST_R50739_95_Clear_Main(Self        : not null access Clear_Drive_Record;
                                       Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      Seed   : Generator;
      Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>0);
   begin
      case Self.Pass is
         when 1 =>
            Self.Consecutive_Write(Buffer);
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 2.0)));

         when 2 =>
            Reset(Seed);
            for K in Buffer'Range loop
               Buffer(K) := Random_Ada(Seed);
            end loop;
            Self.Consecutive_Write(Buffer);
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 2.0)));
         when others =>
            null;
      end case;


   end GOST_R50739_95_Clear_Main;

   -- GOST_R50739_95_Clear_Rest --

   procedure GOST_R50739_95_Clear_Rest(Self        : not null access Clear_Drive_Record;
                                       Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      Seed   : Generator;
      Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>0);
   begin
      case Self.Pass is
         when 1 =>

            Self.Consecutive_Write(Buffer);
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 2.0)));
         when 2 =>
            Reset(Seed);
            for K in Buffer'Range loop
               Buffer(K) := Random_Ada(Seed);
            end loop;

            Self.Consecutive_Write(Buffer);
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 2.0)));
         when others =>
            null;
      end case;
   end GOST_R50739_95_Clear_Rest;

   -- SCHNEIER_Clear_Main --

   procedure SCHNEIER_Clear_Main(Self        : not null access Clear_Drive_Record;
                                 Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      ---
      procedure Pass_1 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>1);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_1;
      ---
      procedure Pass_2 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>0);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_2;
      ---
      procedure Pass_3_7 with Inline
      is
         Seed   : Generator;
         Buffer : BYTE_Array(1..Integer(Self.Buf_Size)):=(others=>0);
      begin
         Reset(Seed);
         for K in Buffer'Range loop
            Buffer(K) := Random_Ada(Seed);
         end loop;
         Self.Consecutive_Write(Buffer);
      end Pass_3_7;
      ---
   begin -- for HMG_IS5_Ext
      case Self.Pass is
         when 1 =>
            Pass_1;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when 2=>
            Pass_2;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when 3..7=>
            Pass_3_7;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when others =>
            null;
      end case;
   end SCHNEIER_Clear_Main;


   -- SCHNEIER_Clear_Rest --

   procedure SCHNEIER_Clear_Rest(Self        : not null access Clear_Drive_Record;
                                 Drives_List : Main_Window.List_View.X_List_View)
     with Inline
   is
      ---
      procedure Pass_1 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>1);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_1;
      ---
      procedure Pass_2 with Inline
      is
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>0);
      begin
         Self.Consecutive_Write(Buffer);
      end Pass_2;
      ---
      procedure Pass_3_7 with Inline
      is
         Seed   : Generator;
         Buffer : BYTE_Array(1..Integer(Self.Buf_Rem_Size)):=(others=>0);
      begin
         Reset(Seed);
            for K in Buffer'Range loop
               Buffer(K) := Random_Ada(Seed);
            end loop;
            Self.Consecutive_Write(Buffer);
      end Pass_3_7;
      ---
   begin
      case Self.Pass is
         when 1 =>
            Pass_1;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index,
               Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when 2=>
            Pass_2;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index, Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when 3..7=>
            Pass_3_7;
            Drives_List.Set_Progress_Clean_Value
              (Self.Drive_Index, Natural(100.0 * Float(Self.Pass - 1) / Float(Self.Pass_Count)) +
                 Natural(Float(Self.Buf_Index) / Float(Self.Buf_Count) * (100.0 / 7.0)));
         when others =>
            null;
      end case;
   end SCHNEIER_Clear_Rest;

   -- Clear_Drive_Task --

   --package Clear_Request is new Gtk.Main.Router.Generic_Callback_Request(User_Data => Clear_Drive_Record);

   task body Clear_Drive_Task is separate;


   -- Hash --

   function Hash(Key : Ada.Task_Identification.Task_Id)
                 return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Strings.Hash(Ada.Task_Identification.Image(Key));
   end Hash;


   -- Init --

   function Init(Drive       : access Drive_Record;
                 Buf_Size    : Win32.ULONG;
                 Drive_Index : Natural)
                 return Clear_Drive
   is
      Ret_Clear_Drive : Clear_Drive := new Clear_Drive_Record'
        (Drive_Rec    => Drive,
         Buf_Size     => Buf_Size,
         Buf_Count    => Drive.Size / Win32.ULONGLONG(Buf_Size),
         Buf_Rem_Size => Drive.Size rem Win32.ULONGLONG(Buf_Size),

         H_FS        => System.Null_Address,
         Buf_Index   => 0,
         Pos         => LARGE_INTEGER'(Which => u_kind,u =>anonymous1_t'(0,0)),
         Pass        => 1,
         Pass_Count  => 1,
         Drive_Index => Drive_Index);
      --TVC        => Main_Window.Get.Get_Tree_View_Collection);
   begin
      return Ret_Clear_Drive;
   end Init;

   ----------------------------------
   -------- PRIVATE SECTION ---------
   ----------------------------------

   -- Open --

   function Open(Self : in out Clear_Drive_Record) return Boolean
   is
      use type IC.unsigned_long;
      use System,GWindows.Base,GWindows.Message_Boxes;

      Path_C : IC.wchar_array := IC.To_C("\\.\" & Self.Drive_Rec.Letter & ":");
   begin
      Self.H_FS:= CreateFileW(lpFileName            => To_PCWSTR(Path_C'Address),
                              dwDesiredAccess       => GENERIC_READ or GENERIC_WRITE,
                              dwShareMode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                              lpSecurityAttributes  => null,
                              dwCreationDisposition => OPEN_EXISTING,
                              dwFlagsAndAttributes  => FILE_FLAG_NO_BUFFERING or FILE_FLAG_WRITE_THROUGH,
                              hTemplateFile         => Null_Address);
      if Self.H_FS = INVALID_HANDLE_VALUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Open : CreateFile error!",
                     Icon     => Error_Icon);
         return False;
      end if;
      return True;
   end Open;


   -- Lock --

   function Lock(Self : Clear_Drive_Record) return Boolean
   is
      use System,GWindows.Base,GWindows.Message_Boxes;
      Bytes_Ret : aliased DWORD;
   begin
      -- Lock volume
      if DeviceIoControl(hDevice         => Self.H_FS,
                         dwIoControlCode => FSCTL_LOCK_VOLUME,
                         lpInBuffer      => Null_Address,
                         nInBufferSize   => 0,
                         lpOutBuffer     => Null_Address,
                         nOutBufferSize  => 0,
                         lpBytesReturned => Bytes_Ret'Unchecked_Access,
                         lpOverlapped    => null) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Lock : DeviceIoControl error!",
                     Icon     => Error_Icon);
         if CloseHandle(Self.H_FS) /= Win32.TRUE then
            Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                        "Error!","Lock : CloseHandle error!",
                        Icon     => Error_Icon);
         end if;
         return False;
      end if;
      return True;
   end Lock;


   -- Unlock --

   function Unlock(Self : Clear_Drive_Record) return Boolean
   is
      use System,GWindows.Base,GWindows.Message_Boxes;
      Bytes_Ret : aliased DWORD;
   begin
      if DeviceIoControl(hDevice         => Self.H_FS,
                         dwIoControlCode => FSCTL_UNLOCK_VOLUME,
                         lpInBuffer      => Null_Address,
                         nInBufferSize   => 0,
                         lpOutBuffer     => Null_Address,
                         nOutBufferSize  => 0,
                         lpBytesReturned => Bytes_Ret'Unchecked_Access,
                         lpOverlapped    => null) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Unlock : DeviceIoControl error!",
                     Icon     => Error_Icon);
         return False;
      end if;

      return True;
   end Unlock;


   -- Close --

   function Close(Self : Clear_Drive_Record) return Boolean
   is
      use GWindows.Base,GWindows.Message_Boxes;
   begin
      if CloseHandle(Self.H_FS) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Close : CloseHandle error!",
                     Icon     => Error_Icon);
         return False;
      end if;
      return True;
   end Close;


   -- Format --

   procedure Format(Self : Clear_Drive_Record)
   is
      use System,GWindows.Base,GWindows.Message_Boxes;
      use type Interfaces.C.unsigned_long;

      Command_Line_C : Interfaces.C.wchar_array := Interfaces.C.To_C
        ("cmd /k format " & Self.Drive_Rec.Letter & ": /fs:" & Self.Drive_Rec.File_System & " /q /force");

      function To_LPSTR is new Ada.Unchecked_Conversion (System.Address, LPSTR);

      SA : aliased SECURITY_ATTRIBUTES := SECURITY_ATTRIBUTES'
        (nLength              => SECURITY_ATTRIBUTES'Size / 8,-- bytes
         lpSecurityDescriptor => Null_Address,
         bInheritHandle       => Win32.TRUE);

      -- Init startup info
      SI : aliased STARTUPINFOW := STARTUPINFOW'(cb              => STARTUPINFOW'Size / 8, -- bytes
                                                 lpReserved      => null,
                                                 lpDesktop       => null,
                                                 lpTitle         => null,
                                                 dwX             => 0,
                                                 dwY             => 0,
                                                 dwXSize         => 0,
                                                 dwYSize         => 0,
                                                 dwXCountChars   => 0,
                                                 dwYCountChars   => 0,
                                                 dwFillAttribute => 0,
                                                 dwFlags         =>
                                                   STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES,
                                                 wShowWindow     => SW_HIDE,
                                                 cbReserved2     => 0,
                                                 lpReserved2     => null,
                                                 hStdInput       => Null_Address,
                                                 hStdOutput      => Null_Address,
                                                 hStdError       => Null_Address);

      PI : aliased PROCESS_INFORMATION := PROCESS_INFORMATION'(hProcess    => Null_Address,
                                                               hThread     => Null_Address,
                                                               dwProcessId => 0,
                                                               dwThreadId  => 0);

      H_In_Read,H_In_Write : aliased Win32.Winnt.HANDLE;
      In_Buf : BYTE_Array(1..2) := (16#A#,16#D#);
      Bytes_Written : aliased DWORD;

   begin
      -- Create a pipe for STDIN
      if CreatePipe(hReadPipe        => H_In_Read'Unchecked_Access,
                    hWritePipe       => H_In_Write'Unchecked_Access,
                    lpPipeAttributes => SA'Unchecked_Access,
                    nSize            => 0) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CreatePipe error!",
                     Icon     => Error_Icon);
         return;
      end if;

      -- Ensure the write handle to the pipe STDIN is not inherited
      if SetHandleInformation(hObject => H_In_Write,
                              dwMask  => HANDLE_FLAG_INHERIT,
                              dwFlags => 0) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : Stdin SetHandleInformation!",
                     Icon     => Error_Icon);
         return;
      end if;

      -- Set StartupInfo
      SI.hStdInput  := H_In_Read;
      SI.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
      SI.hStdError  := GetStdHandle(STD_ERROR_HANDLE);

      -- Start process
      if CreateProcessW(lpApplicationName    => null,
                        lpCommandLine        => To_PWSTR(Command_Line_C'Address),
                        lpProcessAttributes  => null,
                        lpThreadAttributes   => null,
                        bInheritHandles      => Win32.TRUE,
                        dwCreationFlags      => CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                        lpEnvironment        => Null_Address,
                        lpCurrentDirectory   => null,
                        lpStartupInfo        => SI'Unchecked_Access,
                        lpProcessInformation => PI'Unchecked_Access) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CreateProcess error!",
                     Icon     => Error_Icon);
         return;
      end if;

      if CloseHandle(PI.hThread) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CloseHandle(hThread) error!",
                     Icon     => Error_Icon);
         return;
      end if;

      if CloseHandle(H_In_Read) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CloseHandle(H_In_Read) error!",
                     Icon     => Error_Icon);
         return;
      end if;

      -- Write 'ENTER' to start processing

      if WriteFile(hFile                  => H_In_Write,
                   lpBuffer               => In_Buf'Address,
                   nNumberOfBytesToWrite  => In_Buf'Length,
                   lpNumberOfBytesWritten => Bytes_Written'Unchecked_Access,
                   lpOverlapped           => null) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : WriteFile(H_In_Write) error!",
                     Icon     => Error_Icon);
         return;
      end if;

      if CloseHandle(H_In_Write) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CloseHandle(H_In_Write) error!",
                     Icon     => Error_Icon);
         return;
      end if;

      -- Wait for process to exit
      if WaitForSingleObject(PI.hProcess,INFINITE) = WAIT_FAILED then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : WaitForSingleObject error!",
                     Icon     => Error_Icon);
         return;
      end if;

      if CloseHandle(PI.hProcess) /= Win32.TRUE then
         Message_Box(Base_Window_Type(Main_Window.Get_X_Main.all),
                     "Error!","Format_Drive : CloseHandle error!",
                     Icon     => Error_Icon);
         return;
      end if;
   end Format;

end Clear_Drive_Device;
