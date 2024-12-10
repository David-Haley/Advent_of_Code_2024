with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;
with DJH.Gcd_Lcm;

procedure December_10 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   Origin : constant Coordinates := (1, 1);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "+" (Left, Right : Coordinates) return Coordinates is
     ((Left.X + Right.X, Left.Y + Right.Y));

   pragma Inline_Always ("<", "+");

   type Directions is (Up, Down, Left, Right);
   Increment : constant array (Directions) of Coordinates :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   package Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Character);
   use Maps;

   package Trail_End_Sets is new  Ada.Containers.Ordered_Sets (Coordinates);
   use Trail_End_Sets;

   Trail_Head : constant Character := '0';
   Trail_End : constant Character := '9';

   procedure Read_Input (Map : out Maps.Map;
                         Trail_Head_Set : out Trail_End_Sets.Set) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_10.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Map);
      Clear (Trail_head_Set);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            if Is_In (Element (Text, X), Decimal_Digit_Set) then
               if Element (Text, X) = Trail_Head then
                  Insert (Trail_head_Set, (X, Y));
               end if; -- Element (Text, X) = Trail_Head
               Insert (Map, (X, Y), Element (Text, X));
            else
               raise Program_Error with "Invalid character '" &
                 Element (Text, X) & "' at (" & X'Img & "," & Y'Img & ")";
            end if; -- Is_In (Element (Text, X), Decimal_Digit_Set)
         end loop; -- X in Ordinates range Origin.X .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Find_Trails (Map : in Maps.Map;
                         Start : in Coordinates) return Trail_End_Sets.Set is

      Trail_Ends : Trail_End_Sets.Set := Trail_End_Sets.Empty_Set;
      Test : Coordinates;

   begin -- Find_Trails
      if Map (Start) = Trail_End then
         Include (Trail_Ends, Start);
      else
         for D in Directions loop
            Test := Start + Increment (D);
            if Contains (Map, Test) and then
              Map (Test) = Character'Succ (Map (Start)) then
               Union (Trail_Ends, Find_Trails (Map, Test));
            end if; -- Contains (Map, Test) and then ..
         end loop; -- D in Directions
      end if; -- Map (Start) = Trail_End
      return Trail_Ends;
   end Find_Trails;

   function Count_Trails (Map : in Maps.Map;
                          Start : in Coordinates) return Natural is

      Result : Natural := 0;
      Test : Coordinates;

   begin -- Count_Trails
      if Map (Start) = Trail_End then
         Result := 1;
      else
         for D in Directions loop
            Test := Start + Increment (D);
            if Contains (Map, Test) and then
              Map (Test) = Character'Succ (Map (Start)) then
               Result := @ + Count_Trails (Map, Test);
            end if; -- Contains (Map, Test) and then ..
         end loop; -- D in Directions
      end if; -- Map (Start) = Trail_End
      return Result;
   end Count_Trails;

   Map : Maps.Map;
   Trail_Head_Set : Trail_End_Sets.Set;
   Sum : Natural := 0;

begin -- December_10
   Read_Input (Map, Trail_Head_Set);
   for H in Iterate (Trail_Head_Set) loop
      Sum := @ + Natural (Length (Find_Trails (Map, Element (H))));
   end loop; -- H in Iterate (Trail_Head_Set)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum:= 0;
   for H in Iterate (Trail_Head_Set) loop
      Sum := @ + Count_Trails (Map, Element (H));
   end loop; -- H in Iterate (Trail_Head_Set)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_10;
