with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_19 is

   Colour_Set : constant Character_Set := To_Set ("wubrg");

   package Towel_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   use Towel_Lists;

   package Pattern_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   use Pattern_Lists;

   package Caches is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Positive);
   use Caches;

   procedure Read_Input (Patterns : out Pattern_Lists.List;
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
         Append (Patterns, Slice (Text, First, Last));
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
                      Patterns : in Pattern_Lists.List) return Boolean is

      Matches : Boolean := False;
      Pc : Pattern_Lists.Cursor := First (Patterns);

   begin -- Is_Valid
      while not Matches and Pc /= Pattern_Lists.No_Element loop
         Matches := Index (Design, Element (Pc)) = 1;
         if Matches and then Length (Design) > Element (Pc)'Length then
            Matches := @ and
              Is_Valid (Delete (Design, 1, Element (Pc)'Length),
                        Patterns);
         end if; -- Matches and then Length (Design) > Element (Pc)'Length
         Next (Pc);
      end loop; -- not Matches and Pc /= Pattern_Lists.No_Element
      return Matches;
   end Is_Valid;

   function Count_Valid (Designs : in Towel_Lists.List;
                         Patterns : in Pattern_Lists.List) return Natural is

      Count : Natural := 0;

   begin -- Count_Valid
      for D in Iterate (Designs) loop
         if Is_Valid (Element (D), Patterns) then
            Count := @ + 1;
         end if; -- Is_Valid (Element (D), Patterns)
      end loop; -- D in Iterate (Designs)
      return Count;
   end Count_Valid;

   procedure Build_Cache (Pattern_List : in Pattern_Lists.List;
                          Cache : out Caches.Map) is

      function Combinations (Design : in Unbounded_String;
                             Patterns : in Pattern_Lists.List) return Natural is

         Count : Natural := 0;

      begin -- Combinations
         for P in Iterate (Patterns) loop
            if To_String (Design) = Element (P) then
               Count := @ + 1;
            elsif Index (Design, Element (P)) = 1 then
               Count := @ + 1 *
                 Combinations (Delete (Design, 1, Element (P)'Length),
                               Patterns);
            end if; -- To_String (Design) = Element (P)
         end loop; -- P in Iterate (Patterns)
         return Count;
      end Combinations;

   begin -- Build_Cache
      Clear (Cache);
      for P in Iterate (Pattern_List) loop
         Insert (Cache, Element (P),
                 Combinations (To_Unbounded_String (Element (P)),
                   Pattern_list));
      end loop; -- P in Iterate (Pattern_List)
   end Build_Cache;

   function Count_Combinations (Designs : in Towel_Lists.List;
                                Patterns : in Pattern_Lists.List;
                                Cache : in Caches.Map)
                                return Natural is

      function Combinations (Design : in Unbounded_String;
                             Cache : in Caches.Map) return Natural is

         Count : Natural := 0;

      begin -- Combinations
         for C in Iterate (Cache) loop
            if To_String (Design) = Key (C) then
               Count := @ + Element (C);
            elsif Index (Design, Key (C)) = 1 then
               Count := @ + Element (C) *
                 Combinations (Delete (Design, 1, Key (C)'Length),
                               Cache);
            end if; -- To_String (Design) = Element (P)
         end loop; -- P in Iterate (Patterns)
         Put_Line (Design'Img & Count'Img);
         return Count;
      end Combinations;

      Count : Natural := 0;

   begin -- Count_Combinations
      for D in Iterate (Designs) loop
         Put_Line ("Valid """  & Element (D) & """");
         if Is_Valid (Element (D), Patterns) then
            Put_Line ("Count """ & Element (D) & """");
            Count := @ + Combinations (Element (D), Cache);
            Put (Element (D));
            DJH.Execution_Time.Put_CPU_Time;
         end if; -- Is_Valid (Element (D), Patterns)
      end loop; -- D in Iterate (Designs)
      return Count;
   end Count_Combinations;

   Patterns : Pattern_Lists.List;
   Designs : Towel_Lists.List;
   Cache : Caches.Map;

begin -- December_19
   Read_Input (Patterns, Designs);
   Put_Line ("Part one:" & Count_Valid (Designs, Patterns)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Build_Cache (Patterns, Cache);
   Put_Line (Cache'Img);
   Put_Line ("Part two:" & Count_Combinations (Designs, Patterns, Cache)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_19;
