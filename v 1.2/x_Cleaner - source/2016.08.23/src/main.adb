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

with Ada.Unchecked_Conversion;
---
with GWindows.Windows.Main,
     GWindows.Common_Controls,
     GWindows.Menus,
     GWindows.Application;

use GWindows.Windows.Main,
    GWindows.Common_Controls,
    GWindows.Menus;
---
with Win32,
     Win32.Winbase,
     Win32.Winuser,
     Win32.Windef,
     Win32.Winnt;

use Win32,
    Win32.Winbase,
    Win32.Winuser,
    Win32.Windef,
    Win32.Winnt;
---
with System.Machine_Code;
use System.Machine_Code;
---
with Main_Window,
     Callbacks;
use Main_Window,
    Callbacks;
---
with Drive_Devices;
---
with Interfaces.C;
--
procedure Main
is
   pragma Linker_Options ("resource.coff");-- resources + include "manifest" to set up 6.0 version common controls
   pragma Linker_Options("-mwindows");

   package IC renames Interfaces.C;

   use type IC.long,IC.unsigned;

   use System,
       Main_Window.List_View;

   X_Main      : X_Main_Window := Main_Window.Get_X_Main;
   X_List      : X_List_View := X_Main.Get_Volume_List;
   X_Main_Menu : Menu_Type := X_Main.Get_Main_Menu;
   File_Menu   : Menu_Type := Create_Popup;
   Help_Menu   : Menu_Type := Create_Popup;

   function Get_Not_Resizeable_Window_Style(Style : in LONG) return LONG with Inline is
      use ASCII;
      New_Style : LONG;
   begin
      Asm("notl %%edx" & LF & HT & -- ~WS_MAXIMIZEBOX
            "notl %%ecx" & LF & HT & -- ~WS_THICKFRAME
          "and %%edx, %%eax" & LF & HT & -- Style & ~WS_MAXIMIZEBOX
            "and %%ecx, %%eax" , -- Style & ~WS_THICKFRAME
          Outputs  => LONG'Asm_Output("=a",New_Style),--%0
          Inputs   => (LONG'Asm_Input("a",Style),--%1
                       (LONG'Asm_Input("d",WS_MAXIMIZEBOX)),
                       (LONG'Asm_Input("c",WS_THICKFRAME))));--%3 -- ecx
      return New_Style;
   end Get_Not_Resizeable_Window_Style;

   X_Icon : HICON := LoadIcon(GetModuleHandle(null),PCSTR(MAKEINTRESOURCE(1001)));
   function HICON_To_LONG is new Ada.Unchecked_Conversion(HICON,LONG);
   Ret_Val : LONG;
begin
   X_Main.Create("x_Cleaner v 1.2");
   Ret_Val := LONG(SetClassLong(HWND(X_Main.Handle),GCL_HICON,HICON_To_LONG(X_Icon)));
   X_Main.Size(600,300);
   X_Main.Visible(True);

   -- Disable maximize button
   Ret_Val:= SetWindowLong(HWND(X_Main.Handle),GWL_STYLE,
                           Get_Not_Resizeable_Window_Style(GetWindowLong(HWND(X_Main.Handle),GWL_STYLE)));

   -- Setup Listview
   X_List.Create(X_Main.all, 0, 0, 600, 300, Single, Report_View);
   Ret_Val:= LONG(SendMessage(HWND(X_List.Handle),LVM_SETEXTENDEDLISVIEWSTYLE,0,LVS_EX_FULLROWSELECT));

   X_List.Insert_Column("Volume",0,60);
   X_List.Insert_Column("Size (Mb)",1,80);
   X_List.Insert_Column("Algorithm",2,120);
   X_List.Insert_Column("Pass / Operation",3,170);
   X_List.Insert_Column("Cleaning process",4,162);

   X_Main.On_Context_Menu_Handler(Main_Window.List_View.Popup_Menu.Do_Context_Menu'Access);
   --
   Drive_Devices.Init;
   X_List.Add_Drives;
   --

   -- Setup menu
   X_Main_Menu := Create_Menu;
   Append_Item(File_Menu,"E&xit",IDM_Exit);
   Append_Menu(X_Main_Menu,"&Menu",File_Menu);

   Append_Item(Help_Menu,"A&bout",IDM_About);
   Append_Menu(X_Main_Menu,"&Help",Help_Menu);

   X_Main.Menu(X_Main_Menu);
   -- Setup menu handlers
   X_Main.On_Menu_Select_Handler(Menu_Select_Cb'Access);

   -- Run main loop
   GWindows.Application.Message_Loop;

end Main;
