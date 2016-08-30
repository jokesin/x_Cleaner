with Ada.Unchecked_Conversion;
---
with Win32,
     Win32.Winbase,
     Win32.Winnt;
use Win32,
    Win32.Winbase,
    Win32.Winnt;
---
with Interfaces,
     Interfaces.C;
---
with System;
---
package body Vista_Dynamic is

   package IC renames Interfaces.C;
   Is_Vista_Based : Boolean;

   type OpenThemeData_Type is access function
     (hwnd         : Win32.Windef.HWND;
      pszClassList : Win32.LPCWSTR)
      return HTHEME
     with Convention => Stdcall;

   type GetThemeBackgroundContentRect_Type is access function
     (Theme         : HTHEME;
      hdc           : Win32.Windef.HDC;
      iPartId       : Win32.INT ;
      iStateId      : Win32.INT;
      pBoundingRect : Win32.Objbase.LPCRECT;
      pContentRect  : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT
     with Convention => Stdcall;

   type DrawThemeBackground_Type is access function
     (Theme     : HTHEME;
      hdc       : Win32.Windef.HDC;
      iPartId   : Win32.INT ;
      iStateId  : Win32.INT;
      pRect     : Win32.Windef.LPRECT;
      pClipRect : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT with
     Convention => Stdcall;

   OpenThemeData                 : OpenThemeData_Type;
   GetThemeBackgroundContentRect : GetThemeBackgroundContentRect_Type;
   DrawThemeBackground           : DrawThemeBackground_Type;
   -- Is_Os_Vista_Based --

   function Is_Os_Vista_Based return Boolean is
   begin
      return Is_Vista_Based;
   end Is_Os_Vista_Based;

   -- Open_Theme_Data --

   function Open_Theme_Data(hwnd         : Win32.Windef.HWND;
                            pszClassList : Win32.LPCWSTR)
                            return HTHEME
   is
   begin
      return OpenThemeData(hwnd,pszClassList);
   end Open_Theme_Data;

   -- Get_Theme_Background_Content_Rect --

   function Get_Theme_Background_Content_Rect
     (Theme         : HTHEME;
      hdc           : Win32.Windef.HDC;
      iPartId       : Win32.INT ;
      iStateId      : Win32.INT;
      pBoundingRect : Win32.Objbase.LPCRECT;
      pContentRect  : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT
   is
   begin
      return GetThemeBackgroundContentRect(Theme,hdc,iPartId,iStateId,pBoundingRect,pContentRect);
   end Get_Theme_Background_Content_Rect;

   -- Draw_Theme_Background --

   function Draw_Theme_Background
     (Theme     : HTHEME;
      hdc       : Win32.Windef.HDC;
      iPartId   : Win32.INT ;
      iStateId  : Win32.INT;
      pRect     : Win32.Windef.LPRECT;
      pClipRect : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT
   is
   begin
      return DrawThemeBackground(Theme,hdc,iPartId,iStateId,pRect,pClipRect);
   end Draw_Theme_Background;

   --- ELABORATION ---

begin
   declare
      use type IC.unsigned_long;
      UXTHEME_DLL                        : HANDLE;
      OS_Version_Info                    : aliased OSVERSIONINFOEX;
      Lib_Name                           : IC.char_array := IC.To_C("uxtheme.dll");
      OpenThemeData_Name                 : IC.char_array := IC.To_C("OpenThemeData");
      GetThemeBackgroundContentRect_Name : IC.char_array := IC.To_C("GetThemeBackgroundContentRect");
      DrawThemeBackground_Name           : IC.char_array := IC.To_C("DrawThemeBackground");
      function To_OpenThemeData is new Ada.Unchecked_Conversion(Win32.Windef.PROC,OpenThemeData_Type);
      function To_GetThemeBackgroundContentRect is new
        Ada.Unchecked_Conversion(Win32.Windef.PROC,GetThemeBackgroundContentRect_Type);
      function To_DrawThemeBackground is new Ada.Unchecked_Conversion(Win32.Windef.PROC,DrawThemeBackground_Type);
   begin
      OS_Version_Info.dwOSVersionInfoSize := OSVERSIONINFOEX'Size / 8;-- size in bytes
      if GetVersionEx(OS_Version_Info'Unchecked_Access) /= Win32.FALSE then
         if OS_Version_Info.dwMajorVersion >= 6 then -- Vista based
            Is_Vista_Based := True;
            UXTHEME_DLL := LoadLibrary(To_PCSTR(Lib_Name'Address));
            OpenThemeData := To_OpenThemeData(GetProcAddress(UXTHEME_DLL,To_PCSTR(OpenThemeData_Name'Address)));
            GetThemeBackgroundContentRect := To_GetThemeBackgroundContentRect
              (GetProcAddress(UXTHEME_DLL,To_PCSTR(GetThemeBackgroundContentRect_Name'Address)));
            DrawThemeBackground := To_DrawThemeBackground
              (GetProcAddress(UXTHEME_DLL,To_PCSTR(DrawThemeBackground_Name'Address)));
         else -- XP or lower
            Is_Vista_Based := False;
         end if;
      end if;
   end;

end Vista_Dynamic;
