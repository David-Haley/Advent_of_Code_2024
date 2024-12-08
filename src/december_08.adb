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

procedure December_08 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   Origin : constant Coordinates := (1, 1);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "-" (Left, Right : Coordinates) return Coordinates is
     ((Left.X - Right.X, Left.Y - Right.Y));

   function "+" (Left, Right : Coordinates) return Coordinates is
     ((Left.X + Right.X, Left.Y + Right.Y));

   pragma Inline_Always ("<", "-", "+");

   package Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Character);
   use Maps;

   type Pairs is record
      C1, C2 : Coordinates;
   end record; -- Pairs

   package Pair_Lists is new Ada.Containers.Doubly_Linked_Lists (Pairs);
   use Pair_Lists;

   package Pair_Stores is new
     Ada.Containers.Ordered_Maps (Character, Pair_Lists.List);
   use Pair_Stores;

   package Antinode_Sets is new  Ada.Containers.Ordered_Sets (Coordinates);
   use Antinode_Sets;

   procedure Read_Input (Map : out Maps.Map;
                         Bottom_Right : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_08.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Maps.Clear (Map);
      Bottom_Right := (1, 1);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            if Element (Text, X) = '.' then
               Null; -- open space do nothing
            elsif Is_In (Element (Text, X), Letter_Set) or
              Is_In (Element (Text, X), Decimal_Digit_Set) then
               Insert (Map, (X, Y),  Element (Text, X));
            else
               raise Program_Error with "Invalid character '" &
                 Element (Text, X) & "' at (" & X'Img & "," & Y'Img & ")";
            end if; -- Element (Text, X) = '.'
            if X > Bottom_Right.X then
               Bottom_Right.X := X;
            end if; -- X > Bottom_Right.X
            if Y > Bottom_Right.Y then
               Bottom_Right.Y := Y;
            end if; -- Y > Bottom_Right.Y
         end loop; -- X in Ordinates range Origin.X .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Build_Pairs (Map : in Maps.Map;
                          Pair_Store : out Pair_Stores.Map) is

      J : Maps.Cursor;

   begin -- Build_Pairs
      Clear (Pair_Store);
      for I in Iterate (Map) loop
         J := Next (I);
         while J /= Maps.No_Element loop
            if Element (I) = Element (J) then
               if not Contains (Pair_Store, Element (I)) then
                  insert (Pair_Store, Element (I), Pair_Lists.Empty_List);
               end if; -- not Contains (Pair_Store, Element (I))
               Append (Pair_Store (Element (I)), (Key (I), Key (J)));
            end if; -- Element (I) = Element (J)
            Next (J);
         end loop; -- J /= Maps.No_Element
      end loop; -- I in Iterate (Map)
   end Build_Pairs;

   function On_Map (Bottom_Right : in Coordinates;
                    Test : in Coordinates) return Boolean is
     (Origin.X <= Test.X and Test.X <= Bottom_Right.X and
        Origin.Y <= Test.Y and Test.Y <= Bottom_Right.Y);

   pragma Inline_Always (On_Map);

   function Count_Antinodes_1 (Bottom_Right : in Coordinates;
                               Pair_Store: in Pair_Stores.Map)
                               return Count_Type is

      Antinode : Coordinates;
      Antinode_Set : Antinode_Sets.Set := Antinode_Sets.Empty_Set;

   begin -- Count_Antinodes_1
      for S in Iterate (Pair_Store) loop
         for P in Iterate (Element (S)) loop
            Antinode := Element (P).C1 - Element (P).C2 + Element (P).C1;
            if On_Map (Bottom_Right, Antinode) then
               Include (Antinode_Set, Antinode);
            end if; -- On_Map (Bottom_Right, Antinode)
            Antinode := Element (P).C2 - Element (P).C1 + Element (P).C2;
            if On_Map (Bottom_Right, Antinode) then
               Include (Antinode_Set, Antinode);
            end if; -- On_Map (Bottom_Right, Antinode)
         end loop; -- P in Iterate (Element (S))
      end loop; -- S in Iterate (Pair_Store)
      return Length (Antinode_Set);
   end Count_Antinodes_1;

   function Count_Antinodes_2 (Bottom_Right : in Coordinates;
                               Pair_Store: in Pair_Stores.Map)
                               return Count_Type is

      Package My_Gcd is new DJH.Gcd_Lcm (Ordinates);
      use My_Gcd;

      Antinode : Coordinates;
      Antinode_Set : Antinode_Sets.Set := Antinode_Sets.Empty_Set;
      Step : Coordinates;
      Divisor : Ordinates;

   begin -- Count_Antinodes_2
      for S in Iterate (Pair_Store) loop
         for P in Iterate (Element (S)) loop
            Step := Element (P).C2 - Element (P).C1;
            Divisor := Gcd (Step.X, Step.Y);
            Step := (Step.X / Divisor, Step.Y / Divisor);
            Antinode := Element (P).C1;
            while On_Map (Bottom_Right, Antinode) loop
               Include (Antinode_Set, Antinode);
               Antinode := @ + Step;
            end loop; -- On_Map (Bottom_Right, Antinode)
            Antinode := Element (P).C1;
            while On_Map (Bottom_Right, Antinode) loop
               Include (Antinode_Set, Antinode);
               Antinode := @ - Step;
            end loop; -- On_Map (Bottom_Right, Antinode)
         end loop; -- P in Iterate (Element (S))
      end loop; -- S in Iterate (Pair_Store)
      return Length (Antinode_Set);
   end Count_Antinodes_2;

   Map : Maps.Map;
   Bottom_Right : Coordinates;
   Pair_Store : Pair_Stores.Map;

begin -- December_08
   Read_Input (Map, Bottom_Right);
   Build_Pairs (Map, Pair_Store);
   Put_Line ("Part one:" & Count_Antinodes_1 (Bottom_Right, Pair_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Count_Antinodes_2 (Bottom_Right, Pair_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_08;
