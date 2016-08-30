with Win32.Objbase,
     Win32.Windef,
     Win32.Winerror,
     Win32.Winnt;

package Vista_Dynamic is

--   pragma Linker_Options("-luxtheme");

   PP_BAR   : constant := 1;
   PP_CHUNK : constant := 3;

   subtype HTHEME is Win32.Winnt.HANDLE;

   function Is_Os_Vista_Based return Boolean;

   function Open_Theme_Data(hwnd         : Win32.Windef.HWND;
                            pszClassList : Win32.LPCWSTR)
                            return HTHEME;

   --     function OpenThemeData
--       (hwnd         : Win32.Windef.HWND;
--        pszClassList : Win32.LPCWSTR)
--        return HTHEME with Import,
--       Convention => Stdcall,
--       External_Name => "OpenThemeData";

   function Get_Theme_Background_Content_Rect
     (Theme         : HTHEME;
      hdc           : Win32.Windef.HDC;
      iPartId       : Win32.INT ;
      iStateId      : Win32.INT;
      pBoundingRect : Win32.Objbase.LPCRECT;
      pContentRect  : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT;

--     function GetThemeBackgroundContentRect
--       (Theme         : HTHEME;
--        hdc           : Win32.Windef.HDC;
--        iPartId       : Win32.INT ;
--        iStateId      : Win32.INT;
--        pBoundingRect : Win32.Objbase.LPCRECT;
--        pContentRect  : Win32.Windef.LPRECT)
--        return Win32.Winerror.HRESULT with Import,
--       Convention => Stdcall,
--       External_Name => "GetThemeBackgroundContentRect";

   function Draw_Theme_Background
     (Theme     : HTHEME;
      hdc       : Win32.Windef.HDC;
      iPartId   : Win32.INT ;
      iStateId  : Win32.INT;
      pRect     : Win32.Windef.LPRECT;
      pClipRect : Win32.Windef.LPRECT)
      return Win32.Winerror.HRESULT;

--     function DrawThemeBackground
--       (Theme     : HTHEME;
--        hdc       : Win32.Windef.HDC;
--        iPartId   : Win32.INT ;
--        iStateId  : Win32.INT;
--        pRect     : Win32.Windef.LPRECT;
--        pClipRect : Win32.Windef.LPRECT)
--        return Win32.Winerror.HRESULT with Import,
--       Convention => Stdcall,
--       External_Name => "DrawThemeBackground";

end Vista_Dynamic;
