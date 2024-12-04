with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_01 is

   subtype Locations is Positive;

   package Location_Lists is new Ada.Containers.Vectors (Positive, Locations);
   use Location_Lists;

   package Location_Sort is new Location_Lists.Generic_Sorting;
   use Location_Sort;

   package Histograms is new Ada.Containers.Ordered_Maps (Locations, Positive);
   use Histograms;

   procedure Read_Input (Left, Right : out Location_Lists.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_01.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Append (Left, Locations'Value (Slice (Text, First, Last)));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Append (Right, Locations'Value (Slice (Text, First, Last)));
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   Procedure Build_Hystogram (Location_List : in Location_Lists.Vector;
                              Histogram : out Histograms.Map) is

   begin -- Build_Hystogram
      Clear (Histogram);
      for L in iterate (Location_List) loop
         if Contains (Histogram, Element (L)) then
            Histogram (Element (L)) := Histogram (Element (L)) + 1;
         else
            Include (Histogram, Element (L), 1);
         end if; -- Contains (Histogram, Element (L))
      end loop; -- L in iterate (Location_List)
   end Build_Hystogram;

   Left, Right : Location_Lists.Vector;
   Sum : Natural := 0;
   Histogram : Histograms.Map;

begin -- December_01
   Read_Input (Left, Right);
   Sort (Left);
   Sort (Right);
   for L in Iterate (Left) loop
      Sum := @ + abs (Left (L) - Right (To_Index (L)));
   end Loop; -- L in Iterate (Left)
   Put_Line ("Part one:" & Sum'Image);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   Build_Hystogram (Right, Histogram);
   for L in Iterate (Left) loop
      if Contains (Histogram, (Element (L))) then
         Sum := @ + Element (L) * Histogram (Element (L));
      end if; -- Contains (Histogram, (Element (L)))
   end Loop; -- L in Iterate (Left)
   Put_Line ("Part two:" & Sum'Image);
   DJH.Execution_Time.Put_CPU_Time;
end December_01;
