with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_04 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   subtype XMAS_Characters is Character with
     Static_Predicate => XMAS_Characters in 'X' | 'M' | 'A' | 'S';

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   package Grids is new
     Ada.Containers.Ordered_Maps (Coordinates, XMAS_Characters);
   use Grids;

   procedure Read_Input (Grid : out Grids.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := 1;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_04.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         For X in Ordinates range 1 .. Length (Text) loop
            Insert (Grid, (X, Y), Element (Text, X));
         end loop; -- X in Ordinates range 1 .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Count_XMAS (Grid : in Grids.Map) return Natural is

      subtype String_4 is String (1 .. 4);

      Forward_XMAS : constant String_4 := "XMAS";
      Reverse_XMAS : constant String_4 := "SAMX";

      Test_H, Test_V, Test_L, Test_R : String_4;
      Blank : constant String_4 := "    ";
      XY : Coordinates;
      Result : Natural := 0;

   begin -- Count_XMAS
      for G in iterate (Grid) loop
         Test_H := Blank;
         Test_V := Blank;
         Test_L := Blank;
         Test_R := Blank;
         for O in Ordinates range 1 .. 4 loop
            XY.X := Key(G).X + O - 1;
            XY.Y := Key (G).Y;
            if Contains (Grid, XY) then
               Test_H (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
            XY.X := Key (G).X;
            XY.Y := Key(G).Y + O - 1;
            if Contains (Grid, XY) then
               Test_V (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
            XY.X := Key (G).X - (O - 1);
            XY.Y := Key(G).Y + O - 1;
            if Contains (Grid, XY) then
               Test_L (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
            XY.X := Key (G).X + (O - 1);
            XY.Y := Key(G).Y + O - 1;
            if Contains (Grid, XY) then
               Test_R (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
         end loop; -- O in Ordinates range 1 .. 4
         if Test_H = Forward_XMAS or Test_H = Reverse_XMAS then
            Result := @ + 1;
         end if; -- Test_H = Forward_XMAS or Test_H = Reverse_XMAS
         if Test_V = Forward_XMAS or Test_V = Reverse_XMAS then
            Result := @ + 1;
         end if; -- Test_V = Forward_XMAS or Test_V = Reverse_XMAS
         if Test_L = Forward_XMAS or Test_L = Reverse_XMAS then
            Result := @ + 1;
         end if; -- Test_L = Forward_XMAS or Test_L = Reverse_XMAS
         if Test_R = Forward_XMAS or Test_R = Reverse_XMAS then
            Result := @ + 1;
         end if; -- Test_R = Forward_XMAS or Test_R = Reverse_XMAS
      end loop; -- G in iterate (Grid)
      return Result;
   end Count_XMAS;

   function Count_MAS (Grid : in Grids.Map) return Natural is

      subtype String_3 is String (1 .. 3);

      Forward_MAS : constant String_3 := "MAS";
      Reverse_MAS : constant String_3 := "SAM";

      Test_L, Test_R : String_3;
      Blank : constant String_3 := "   ";
      XY : Coordinates;
      Result : Natural := 0;

   begin -- Count_MAS
      for G in iterate (Grid) loop
         Test_L := Blank;
         Test_R := Blank;
         for O in Ordinates range 1 .. 3 loop
            XY.X := Key (G).X - (O - 2);
            XY.Y := Key(G).Y + O - 2;
            if Contains (Grid, XY) then
               Test_L (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
            XY.X := Key (G).X + (O - 2);
            XY.Y := Key(G).Y + O - 2;
            if Contains (Grid, XY) then
               Test_R (O) := Grid (XY);
            end if; -- Contains (Grid, XY)
         end loop; -- O in Ordinates range 1 .. 3
         if (Test_L = Forward_MAS or Test_L = Reverse_MAS) and
           (Test_R = Forward_MAS or Test_R = Reverse_MAS) then
            Result := @ + 1;
         end if; -- (Test_L = Forward_MAS or Test_L = Reverse_MAS) and ...
      end loop; -- G in iterate (Grid)
      return Result;
   end Count_MAS;

   Grid : Grids.Map;

begin -- December_04
   Read_Input (Grid);
   Put_Line ("Part one:" & Count_XMAS (Grid)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Count_MAS (Grid)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_04;
