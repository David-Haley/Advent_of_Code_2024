with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_11 is

   subtype Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   subtype Stones is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Stone_Lines is new Ada.Containers.Doubly_Linked_Lists (Stones);
   use Stone_Lines;

   subtype Blinks is Positive range 1 ..25;

   procedure Read_Input (Stone_Line : out Stone_Lines.List) is

      package Stone_IO is new Ada.Text_IO.Integer_IO (Stones);
      use Stone_IO;

      Input_File : File_Type;
      Stone : Stones;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Stone_Line);
      while not End_Of_Line (Input_File) loop
         Get (Input_File, Stone);
         Append (Stone_Line, Stone);
      end loop; -- not End_Of_Line (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Blink (Stone_Line : in out Stone_Lines.List) is

      Sc : Stone_Lines.Cursor := First (Stone_Line);

   begin -- Blink
      while Sc /= Stone_Lines.No_Element loop
         if Element (Sc) = 0 then
            Stone_Line (Sc) := 1;
         elsif Trim (Element (Sc)'Img, Left)'Length mod 2 = 0 then
            declare -- Split stone
               Stone_String : String := Trim (Element (Sc)'Img, Left);
               Left, Right : Stones;
            begin
               Left :=
                 Stones'Value (Stone_String (Stone_String'First ..
                               (Stone_String'First + Stone_String'Last - 1) / 2
                              ));
               Right :=
                 Stones'Value (Stone_String ((Stone_String'First +
                                 Stone_String'Last + 1) / 2 .. Stone_String'Last
                              ));
               Stone_Line (Sc) := Right;
               Insert (Stone_Line, Sc, Left);
            end; -- Split stone
         else
            Stone_line (Sc) := Element (Sc) * 2024;
         end if; -- Element (Sc) = 0
         Next (Sc);
      end loop; -- Sc /= Stone_Lines.No_Element
   end Blink;

   function Blink_75 (Stone_Line : in Stone_Lines.List)
                      return Long_Natural is

      package Histograms is new
        Ada.Containers.Ordered_Maps (Long_Natural, Long_Natural);
      use Histograms;

      package Caches is new
        Ada.Containers.Ordered_Maps (Stones, Histograms.Map);
      use Caches;

      procedure Blink_25 (Stone : in Stones;
                          Cache : in out Caches.Map) is

         -- Assumes that Stone does not already exist in Cache, will raise an
         -- exception if it already exists

         One_Stone_Line : Stone_Lines.List := Stone_Lines.Empty_List;

      begin -- Blink_25
         Append (One_Stone_Line, Stone);
         for B in Blinks loop
            Blink (One_Stone_Line);
         end loop; -- B in Blinks
         Insert (Cache, Stone, Histograms.Empty_Map);
         for S in Iterate (One_Stone_Line) loop
            if Contains (Cache (Stone), Element (S)) then
               Cache (Stone) (Element (S)) := Cache (Stone) (Element (S)) + 1;
            else
               insert (Cache (Stone), Element (S), 1);
            end if; -- Contains (Cache (Stone), Element (S))
         end loop; -- S in Iterate (One_Stone_Line)
         Clear (One_Stone_Line);
      end Blink_25;

      function Count_Stones (Stone : in Stones;
                             Cache : in out Caches.Map;
                             Level : in Natural := 2) return Long_Natural is

         Result : Long_Natural := 0;

      begin -- Count_Stones
         if not Contains (Cache, Stone) then
            Blink_25 (Stone, Cache);
         end if; -- not Contains (Cache, Stone)
         if Level = 0 then
            for S in Iterate (Cache (Stone)) loop
               Result := @ + Element (S);
            end loop; -- S in Iterate (Cache (Stone))
         else
            for S in Iterate (Cache (Stone)) loop
               Result := @ +
                 Count_Stones (Key (S), Cache, Level - 1) * Element (S);
            end loop; -- S in Iterate (Cache (Stone))
         end if; -- Level = 0
         return Result;
      end Count_Stones;

      Result : Long_Natural := 0;
      Cache : Caches.Map := Caches.Empty_Map;

   begin -- Blink_75
      for S in Iterate (Stone_Line) loop
         Result := @ + Count_Stones (Element (S), Cache);
      end loop; -- S in Iterate (Stone_Line)
      Put_Line ("Cache Keys:" & Length (Cache)'Img);
      return Result;
   end Blink_75;

   Stone_Line :Stone_Lines.List;

begin -- December_11
   Read_Input (Stone_Line);
   for B in Blinks loop
      Blink (Stone_Line);
   end loop; -- B in Blinks
   Put_Line ("Part one:" & Length (Stone_Line)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Stone_Line);
   Put_Line ("Part two:" & Blink_75 (Stone_Line)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_11;
