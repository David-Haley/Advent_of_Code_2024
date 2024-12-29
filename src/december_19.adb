with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_19 is

   Colour_Set : constant Character_Set := To_Set ("wubrg");

   subtype Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Towel_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   use Towel_Lists;

   type Sequence_Keys is record
      Length : Positive;
      Pattern : Unbounded_String;
   end record; --  Sequence_Keys

   function Before (Left, Right : Unbounded_String) return Boolean is
     (Length (Left) > Length (Right) or
          (Length (Left) = Length (Right) and Left < Right));
   -- N.B. the intended sort order is to return longer keys before shorter keys.

   package Sequence_Maps is new
     Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                  Element_Type => Long_Natural,
                                  "<" => Before);
   use Sequence_Maps;

   procedure Read_Input (Patterns : out Sequence_Maps.Map;
                         Designs : out Towel_Lists.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_19.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Patterns);
      Clear (Designs);
      Get_Line (Input_File, Text);
      Start_At := 1;
      while Start_At < Length (Text) loop
         Find_Token (Text, Colour_Set, Start_At, Inside, First, Last);
         Insert (Patterns, (Unbounded_Slice (Text, First, Last)), 0);
         Start_At := Last + 1;
      end loop; -- Start_At < Lenght (Text)
      skip_Line (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (Designs, Text);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Is_Valid (Design : in Unbounded_String;
                      Patterns : in Sequence_Maps.Map) return Boolean is

      Matches : Boolean := False;
      Pc : Sequence_Maps.Cursor := First (Patterns);

   begin -- Is_Valid
      while not Matches and Pc /= Sequence_Maps.No_Element loop
         Matches := Index (Design, To_String (Key (Pc))) = 1;
         if Matches and then Length (Design) > Length (Key (Pc)) then
            Matches := @ and
              Is_Valid (Delete (Design, 1, Length (Key (Pc))),
                        Patterns);
         end if; -- Matches and then Length (Design) > Length (Key (Pc))
         Next (Pc);
      end loop; -- not Matches and Pc /= Sequence_Maps.No_Element
      return Matches;
   end Is_Valid;

   function Count_Valid (Designs : in Towel_Lists.List;
                         Patterns : in Sequence_Maps.Map) return Natural is

      Count : Natural := 0;

   begin -- Count_Valid
      for D in Iterate (Designs) loop
         if Is_Valid (Element (D), Patterns) then
            Count := @ + 1;
         end if; -- Is_Valid (Element (D), Patterns)
      end loop; -- D in Iterate (Designs)
      return Count;
   end Count_Valid;

   procedure Update_Patterns (Patterns : in out Sequence_Maps.Map) is

      -- Store the results for the number of combinations in each of the
      -- patterns and the set of other patterns that are contained;

      procedure Combinations (Design : in Unbounded_String;
                              Patterns : in Sequence_Maps.Map;
                              Count : out Long_Natural) is

         Returned_Count : Long_Natural;

      begin -- Combinations
         Count := 0;
         for P in Iterate (Patterns) loop
            if Design = Key (P) then
               Count := @ + 1;
            elsif Index (Design, To_String (Key (P))) = 1 then
               Combinations (Delete (Design, 1, Length (Key (P))), Patterns,
                             Returned_Count);
               Count := @ + Returned_Count;
            end if; -- Index (Design, To_String (Key (P))) = 1
         end loop; -- P in Iterate (Patterns)
      end Combinations;

      Count : Long_Natural;

   begin -- Update_Patterns
      for P in Iterate (Patterns) loop
         Combinations (Key (P), Patterns, Count);
         Patterns (P) := Count;
      end loop; -- P in Iterate (Paterns)
   end Update_Patterns;

   function Count_Combinations (Designs : in Towel_Lists.List;
                                Patterns : in Sequence_Maps.Map)
                                return Long_Natural is

      function Combinations (Design : in Unbounded_String;
                             Patterns : in Sequence_Maps.Map;
                             Cache : in out Sequence_Maps.Map)
                             return Long_Natural is

         -- Has a side effect of adding seqiences to the cache.

         Count : Long_Natural := 0;

      begin -- Combinations
         --  Put_Line ("Entry """ & Design & """");
         if Contains (Cache, Design) then
            --  Put_Line ("Cache match");
            Count := Cache (Design);
         elsif Contains (Patterns, Design) then
            --  Put_Line ("Pattern match");
            Count := Patterns (Design);
         else
            for P in Iterate (Patterns) loop
               if Index (Design, To_String (Key (P))) = 1 then
                  --  Put_Line ("Match at start """ & Key (P) & """");
                  Count := @ +
                    Combinations (Delete (Design, 1, Length (Key (P))),
                                  Patterns, Cache);
               end if; -- Index (Design, To_String (Key (P))) = 1 ...
            end loop; -- P in Iterate (Patterns)
            Insert (Cache, Design, Count);
         end if; -- Contains (Cache, Design)
         --  Put_Line ("Exit" & Count'Img);
         return Count;
      end Combinations;

      Cache : Sequence_Maps.Map;
      Total : Long_Natural := 0;

   begin -- Count_Combinations
      for D in Iterate (Designs) loop
         Clear (Cache);
         if Is_Valid (Element (D), Patterns) then
            Total := @
              + Long_Natural (Combinations (Element (D), Patterns, Cache));
         end if; -- Is_Valid (Element (D), Patterns)
      end loop; -- D in Iterate (Designs)
      return Total;
   end Count_Combinations;

   Designs : Towel_Lists.List;
   Patterns : Sequence_Maps.Map;

begin -- December_19
   Read_Input (Patterns, Designs);
   Put_Line ("Part one:" & Count_Valid (Designs, Patterns)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Update_Patterns (Patterns);
   Put_Line ("Part two:" & Count_Combinations (Designs, Patterns)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_19;
