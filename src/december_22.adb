with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype Secret_Numbers is Unsigned_64;

   package SN_IO is new Ada.Text_IO.Modular_IO (Secret_Numbers);
   use SN_IO;

   Prices_Per_Day : constant Positive := 2000;
   Price_Modulus : constant Secret_Numbers := 10;

   package Number_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Secret_Numbers);
   use Number_Lists;

   subtype Differences is Integer range -9 .. 9;
   subtype Prices is Differences range 0 .. Differences'Last;

   subtype Price_Indices is Positive range 1 .. Prices_Per_Day;

   type Price_Element is record
      Price : Prices;
      Difference : Differences;
   end record; -- Price_Element

   subtype Change_Indices is Positive range 1 .. 4;
   type Run_Arrays is array (Change_Indices) of Differences;

   function "<" (Left, Right : Run_Arrays) return Boolean is
     (Left (1) < Right (1) or else
          (Left (1) = Right (1) and (Left (2) < Right (2) or else
             (Left (2) = Right (2) and (Left (3) < Right (3) or else
                  (Left (3) = Right (3) and Left (4) < Right (4)))))));

   --  function "<" (Left, Right : Run_Arrays) return Boolean is
   --
   --     L, R : Integer := 0;
   --     Add : constant Integer := 0 - Differences'First;
   --     Mul : constant Integer := Differences'Last - Differences'First + 1;
   --
   --  begin -- "<"
   --     L := (((Left (1) + Add) * Mul + Left (2) + Add) * Mul +
   --           Left (3) + Add) * Mul + Left (4) + Add;
   --     R := (((Right (1) + Add) * Mul + Right (2) + Add) * Mul +
   --               Right (3) + Add) * Mul + Right (4) + Add;
   --     return L < R;
   --  end "<";

   procedure Test_Less is

      package Run_Stores is new Doubly_Linked_Lists (Run_Arrays);
      use Run_Stores;

      Run : Run_Arrays;
      Run_Store : Run_Stores.List := Run_Stores.Empty_List;
      Count : Natural := 0;
      Always : Boolean := True;
      Never : Boolean := False;
      R : Run_Stores.Cursor;

   begin -- Test_Less
      for I in Differences loop
         for J in Differences loop
            for K in Differences loop
               for L in Differences loop
                  Run (1) := I;
                  Run (2) := J;
                  Run (3) := K;
                  Run (4) := L;
                  Append (Run_Store, Run);
               end loop;
            end loop;
         end loop;
      end loop;
      for L in Iterate (Run_Store) loop
         R := Next (L);
         while R /= Run_Stores.No_Element loop
            Always := Always and Element (L) < Element (R);
            Never := Never or Element (R) < Element (L);
            Next (R);
         end loop;
      end loop;
      Put_Line ("Always " & Always'Img & " Never " & Never'Img);
   end Test_Less;

   package Run_Maps is new
     Ada.Containers.Ordered_Maps (Run_Arrays, Prices);
   use Run_Maps;

   package Run_Sets is new Ada.Containers.Ordered_Sets (Run_Arrays);
   use Run_Sets;

   type Price_Arrays is array (Price_Indices) of Price_Element;

   type Buyer_Elements is record
      Price_Array : Price_Arrays;
      Run_Map : Run_Maps.Map := Run_Maps.Empty_Map;
   end record; -- Buyer_Elements

   package Buyer_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Buyer_Elements);
   use Buyer_Lists;

   package Change_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Run_Arrays);
   use Change_Lists;

   procedure Read_Input (Number_List : out Number_Lists.List) is

      Input_File : File_Type;
      Number : Secret_Numbers;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_22.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Number_List);
      while not End_Of_File (Input_File) loop
         Get (Input_File, Number);
         Append (Number_List, Number);
         Skip_Line (Input_File);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Next (Number : in Secret_Numbers) return Secret_Numbers is

      Result : Secret_Numbers := Number;
      Prune : constant Secret_Numbers := 16777216;

   begin -- Next
      Result := (Result xor (Result * 64)) mod Prune; -- first step
      Result := (Result xor (Result / 32))  mod Prune; -- second step
      Result := (Result xor (Result * 2048)) mod Prune; -- End of third step
      return Result;
   end Next;

   pragma Inline_Always (Next);

   function Last_Number (Seed : in Secret_Numbers) return Secret_Numbers is

      Result : Secret_Numbers := Seed;

   begin -- Last_Number
      for I in Positive range 1 .. Prices_Per_Day loop
         Result := Next (Result);
      end loop; -- I in Positive range 1 .. 2000
      return Result;
   end Last_Number;

   procedure Build_Buyer_List (Number_List : in Number_Lists.List;
                               Buyer_List : out Buyer_Lists.List) is

      Buyer_Element : Buyer_Elements;
      Current, Previous : Secret_Numbers;
      Run_Array : Run_Arrays;

   begin -- Build_Buyer_List
      Clear (Buyer_List);
      for B in Iterate (Number_List) loop
         Previous := Element (B);
         for P in Price_Indices loop
            Current := Next (Previous);
            Buyer_Element.Price_Array (P).Price :=
              Prices (Current mod Price_Modulus);
            Buyer_Element.Price_Array (P).Difference :=
              Differences (Current mod Price_Modulus -
                             Previous mod Price_Modulus);
            Previous := Current;
         end loop; -- P in Price_Indices
         Clear (Buyer_Element.Run_Map);
         for P in Price_Indices range 4 .. Prices_Per_Day loop
            Run_Array (1) := Buyer_Element.Price_Array (P - 3).Difference;
            Run_Array (2) := Buyer_Element.Price_Array (P - 2).Difference;
            Run_Array (3) := Buyer_Element.Price_Array (P - 1).Difference;
            Run_Array (4) := Buyer_Element.Price_Array (P).Difference;
            if not Contains (Buyer_Element.Run_Map, Run_Array) and
              Buyer_Element.Price_Array (P).Price > 0 then
               Insert (Buyer_Element.Run_Map, Run_Array,
                       Buyer_Element.Price_Array (P).Price);
            end if; -- not Contains (Buyer_Element.Run_Map, Run_Array) and ...
         end loop; -- P in Price_Indices range 4 .. Prices_Per_Day
         Append (Buyer_List, Buyer_Element);
      end loop; -- B in Iterate (Number_List)
   end Build_Buyer_List;

   procedure Find_All_Runs (Buyer_List : in Buyer_Lists.List;
                            All_Runs : out Run_Sets.Set) is

   begin -- Find_All_Runs
      Put_Line ("Buyers" & Length (Buyer_List)'Img);
      Clear (All_Runs);
      for B in Iterate (Buyer_List) loop
         Put_Line ("Run" & Length (Element (B).Run_Map)'Img);
         Put_Line (Element (B).Run_Map'Img);
         for R in Iterate (Element (B).Run_Map) loop
            Put_Line (Element (R)'Img);
            Include (All_Runs, Key (R));
         end loop; -- R in Iterate (Element (B).Run_Map)
      end loop; -- B in Iterate (Buyer_List)
      Put_Line ("All_Runs" & Length (All_Runs)'Img);
   end Find_All_Runs;

   function Largest_Sum (Buyer_List : in Buyer_Lists.List;
                         All_Runs : in Run_Sets.Set) return Natural is

      Result : Natural := 0;
      Sum : Natural;

   begin -- Largest_Sum
      for R in Iterate (All_Runs) loop
         Sum := 0;
         for B in Iterate (Buyer_List) loop
            if Contains (Element (B).Run_Map, Element (R)) then
               Sum := @ + Element (B).Run_Map (Element (R));
            end if; -- Contains (Element (B).Run_Map, Element (R))
         end loop; -- B in Iterate (Buyer_List)
         if Sum > Result then
            Result := Sum;
         end if; -- Sum > Result
      end loop; -- R in Iterate (All_Runs)
      return Result;
   end Largest_Sum;

   Number_List : Number_Lists.List;
   Sum : Secret_Numbers := 0;
   Buyer_List : Buyer_Lists .List;
   All_Runs : Run_Sets.Set;

begin -- December_22
   --  Read_Input (Number_List);
   --  for S in Iterate (Number_List) loop
   --     Sum := @ + Last_Number (Element (S));
   --  end loop; -- S in Iterate (Nunber_List)
   --  Put_Line ("Part one:" & Sum'Img);
   --  DJH.Execution_Time.Put_CPU_Time;
   --  Build_Buyer_List (Number_List, Buyer_List);
   --  Find_All_Runs (Buyer_List, All_Runs);
   --  Put_Line (Length (All_Runs)'Img);
   --  Put_Line ("Part two:" & Largest_Sum (Buyer_List, All_Runs)'Img);
   Test_Less;
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
