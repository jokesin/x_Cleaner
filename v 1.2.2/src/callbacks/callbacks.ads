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

with GWindows.Base;
package Callbacks is

   -- Menu --
   IDM_About : constant := 100;
   IDM_Exit  : constant := 101;

   IDM_HMG_IS5        : constant := 200;
   IDM_HMG_IS5_EXT    : constant := 201;
   IDM_GOST_R50739_95 : constant := 202;
   IDM_SCHNEIER       : constant := 203;

   procedure Menu_Select_Cb(Window : in out GWindows.Base.Base_Window_Type'Class;
                            Item   : in     Integer);

   procedure Main_Close_Cb(Window    : in out GWindows.Base.Base_Window_Type'Class;
                           Can_Close :    out Boolean);
end Callbacks;
