with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Generic_Constrained_Array_Sort;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   subtype Hosts is String (1 .. 2);

   type Connections is record
      Host_1, Host_2 : Hosts;
   end record; -- Connections

   package Connection_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Connections);
   use Connection_Lists;

   package Host_Sets is new Ada.Containers.Ordered_Sets (Hosts);
   use Host_Sets;

   package Host_Maps is new Ada.Containers.Ordered_Maps (Hosts, Host_Sets.Set);
   use Host_Maps;

   subtype Triple_Indices is Positive range 1 .. 3;
   type Triples is array (Triple_Indices) of Hosts;

   procedure Triple_Sort is new Ada.Containers.Generic_Constrained_Array_Sort
     (Index_Type => Triple_Indices,
      Element_Type => Hosts,
      Array_Type => Triples);

   function "<" (Left, Right : Triples) return Boolean is

      L : Unbounded_String :=
        To_Unbounded_String (Left (1) & Left (2) & Left (3));
      R : Unbounded_String :=
        To_Unbounded_String (Right (1) & Right (2) & Right (3));

   begin -- "<"
      return L < R;
   end "<";

   function "=" (Left, Right : Triples) return Boolean is

      L : Unbounded_String :=
        To_Unbounded_String (Left (1) & Left (2) & Left (3));
      R : Unbounded_String :=
        To_Unbounded_String (Right (1) & Right (2) & Right (3));

   begin -- "="
      return L = R;
   end "=";

   package Triple_Sets is new Ada.Containers.Ordered_Sets (Triples);
   use Triple_Sets;

   procedure Read_Input (Connection_List : out Connection_Lists.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Delimiters : constant Character_Set := To_Set ("-");
      Connection : Connections;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_23.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Connection_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Connection.Host_1 := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Connection.Host_2 := Slice (Text, First, Last);
         Append (Connection_List, Connection);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Build_Host_Map (Connection_List : in Connection_Lists.List;
                             Host_Map : out Host_Maps.Map) is

   begin -- Build_Host_Map
      Clear (Host_Map);
      for C In Iterate (Connection_List) loop
         if not Contains (Host_Map, Element (C).Host_1) then
            Insert (Host_Map, Element (C).Host_1, Host_Sets.Empty_Set);
         end if; -- not Contains (Host_Map, Element (C).Host_1)
         if not Contains (Host_Map, Element (C).Host_2) then
            Insert (Host_Map, Element (C).Host_2, Host_Sets.Empty_Set);
         end if; -- not Contains (Host_Map, Element (C).Host_2)
         Include (Host_Map (Element (C).Host_1), Element (C).Host_2);
         Include (Host_Map (Element (C).Host_2), Element (C).Host_1);
      end loop; -- C In Iterate (Connection_List)
   end Build_Host_Map;

   function Count_Triples (Host_Map : in Host_Maps.Map) return Count_Type is

      function Contains_t (Triple : in Triples) return Boolean is
        (Triple (1) (1) = 't' or Triple (2) (1) = 't' or Triple (3) (1) = 't');

      H2, H3 : Host_Maps.Cursor;
      Triple : Triples;
      Triple_Set : Triple_Sets.Set;

   begin -- Count_Triples
      Clear (Triple_Set);
      for H1 in Iterate (Host_Map) loop
         H2 := Next (H1);
         while H2 /= Host_Maps.No_Element loop
            H3 := Next (H2);
            while H3 /= Host_Maps.No_Element loop
               Triple := (Key(H1), Key (H2), Key (H3));
               if Contains_t (Triple) and then
                 (Contains (Element (H2), Key (H1)) and
                    Contains (Element (H3), Key (H1)) and
                      Contains (Element (H3), Key (H2))) then
                  Triple_Sort (Triple);
                  Include (Triple_Set, Triple);
               end if; -- Contains_t (Triple) and then ...
               Next (H3);
            end loop; -- H3 /= Host_Maps.No_Element
            Next (H2);
         end loop; -- H2 /= Host_Maps.No_Element
      end loop; -- H1 in Iterate (Host_Maps)
      return Length (Triple_Set);
   end Count_Triples;

   function Find_Password (Host_Map : in Host_Maps.Map) return
     Unbounded_String is

      package Clique_Lists is new
        Ada.Containers.Doubly_Linked_Lists (Host_Sets.Set);
      use Clique_Lists;

      procedure Bron_Kerbosch (R_In, P_In, X_In : in Host_Sets.Set;
                               Clique_List : in out Clique_Lists.List) is

         -- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
         --
         -- algorithm BronKerbosch1(R, P, X) is
         --     if P and X are both empty then
         --         report R as a maximal clique
         --     for each vertex v in P do
         --         BronKerbosch1(R u {v}, P n N(v), X n N(v))
         --         P := P \ {v}
         --         X := X u {v}

         P : Host_Sets.Set := copy (P_In);
         X : Host_Sets.Set := Copy (X_In);
         V : Hosts;

      begin -- Bron_Kerbosch
         If Is_Empty (P) and Is_Empty (X) then
            Append (Clique_List, R_In);
         else
            while not Is_Empty (P) loop
               V := First_Element (P);
               Bron_Kerbosch (Union (R_In, To_Set (V)),
                              Intersection (P, Host_Map (V)),
                              Intersection (X, Host_Map (V)),
                              Clique_List);
               Exclude (P, V);
               Include (X, V);
            end loop; -- not Is_Empty (P)
         end if; -- Is_Empty (P) and Is_Empty (X)
      end Bron_Kerbosch;

      R, P, X : Host_Sets.Set := Host_Sets.Empty_Set;
      Clique_List : Clique_Lists.List := Clique_Lists.Empty_List;
      Largest_Set : Host_Sets.Set := Host_Sets.Empty_Set;
      Result : Unbounded_String := Null_Unbounded_String;

   begin -- Find_Password
      for H in Iterate (Host_Map) loop
         Insert (P, Key (H));
      end loop; -- H in Iterate (Host_Map)
      Bron_Kerbosch (R, P, X, Clique_List);
      for C in Iterate (Clique_List) loop
         if Length (Largest_Set) < Length (Element (C)) then
            Largest_Set := Element (C);
         end if; -- Length (Largest_Set) < Length (Element (C))
      end loop; -- Length (Largest_Set) < Length (Element (C))
      for L in Iterate (Largest_Set) loop
         Result := @ & Element (L);
         if L /= Last (Largest_Set) then
            Result := @ & ",";
         end if; -- L /= Last (Largest_Set)
      end loop; -- L in Iterate (Largest_Set)
      return Result;
   end Find_Password;

   Connection_List : Connection_Lists.List;
   Host_Map : Host_Maps.map;

begin -- December_23
   Read_Input (Connection_List);
   Build_Host_Map (Connection_List, Host_Map);
   Put_Line ("Part one:" & Count_Triples (Host_Map)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two: " & Find_Password (Host_Map));
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
