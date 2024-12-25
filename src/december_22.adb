with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces;use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype Secret_Numbers is Unsigned_64;

   package SN_IO is new Ada.Text_IO.Modular_IO (Secret_Numbers);
   use SN_IO;

   Day_Numbers : constant Positive := 2000;
   Price_Modulus : constant Secret_Numbers := 10;

   package Number_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Secret_Numbers);
   use Number_Lists;

   subtype Differences is Integer range -9 .. 9;
   subtype Prices is Differences range 0 .. Differences'Last;

   subtype Price_Indices is Positive range 1 .. Day_Numbers;

   type Price_Element is record
      Price : Prices;
      Difference : Differences;
   end record; -- Price_Element

   type Price_Arrays is array (Price_Indices) of Price_Element;

   package Buyer_Lists is new Ada.Containers.Doubly_Linked_Lists (Price_Arrays);
   use Buyer_Lists;

   subtype Change_Indices is Positive range 1 .. 4;
   type Change_Arrays is array (Change_Indices) of Differences;

   package Change_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Change_Arrays);
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
      for I in Positive range 1 .. Day_Numbers loop
         Result := Next (Result);
      end loop; -- I in Positive range 1 .. 2000
      return Result;
   end Last_Number;

   procedure Build_Buyer_List (Number_List : in Number_Lists.List;
                               Buyer_List : out Buyer_Lists.List) is

      Price_Array : Price_Arrays;
      Current, Previous : Secret_Numbers;

   begin -- Build_Buyer_List
      Clear (Buyer_List);
      for B in Iterate (Number_List) loop
         Previous := Element (B);
         for P in Price_Indices loop
            Current := Next (Previous);
            Price_Array (P).Price := Prices (Current mod Price_Modulus);
            Price_Array (P).Difference :=
              Differences (Current mod Price_Modulus -
                             Previous mod Price_Modulus);
            Previous := Current;
         end loop; -- P in Price_Indices
         Append (Buyer_List, Price_Array);
      end loop; -- B in Iterate (Number_List)
   end Build_Buyer_List;

   procedure Build_Change_List (Change_List : out Change_Lists.List) is

      Price : Integer;
      Change_Array : Change_Arrays;

   begin -- Build_Change_List
      Clear (Change_List);
      for P in Prices loop
         for D1 in Differences loop
            Change_Array (1) := D1;
            Price := P + D1;
            if Price in Prices then
               for D2 in Differences loop
                  Change_Array (2) := D2;
                  Price := P + D1 + D2;
                  if Price in Prices then
                     for D3 in Differences loop
                        Change_Array (3) := D3;
                        Price := P + D1 + D2 + D3;
                        if Price in Prices then
                           for D4 in Differences loop
                              Change_Array (4) := D4;
                              Price := P + D1 + D2 + D3 + D4;
                              if 5 <= Price and Price <= Prices'Last then
                                 Append (Change_List, Change_Array);
                              end if; -- 5 <= Price and Price <= Prices'Last
                           end loop; -- D4 in Differences
                        end if; -- Price in Prices
                     end loop; -- D3 in Differences
                  end if; -- Price in Prices
               end loop; -- D2 in Differences
            end if; -- Price in Prices
         end loop; -- D1 in Differences
      end loop; -- P in Prices
   end Build_Change_List;

   function Sum_For_Run (Buyer_List : in Buyer_Lists.List;
                         Change_Array : in Change_Arrays) return Natural is

      function Found (Price_Array : in Price_Arrays;
                      P : Price_Indices;
                      Change_Array : in Change_Arrays) return Boolean is
        (Price_Array (P).Difference = Change_Array (1) and then
         Price_Array (P + 1).Difference = Change_Array (2) and then
         Price_Array (P + 2).Difference = Change_Array (3) and then
         Price_Array (P + 3).Difference = Change_Array (4));

      pragma Inline_Always (Found);


      Sum : Natural := 0;
      P : Price_Indices;

   begin -- Sum_For_Run
      for B in Iterate (Buyer_List) loop
         P := Price_Indices'First;
         while not Found (Element (B), P, Change_Array) and
           P < Price_Indices'Last - 3 loop
            P := @ + 1;
         end loop; -- not Found (Price_Arrays, P, Change_Array)
         if Found (Element (B), P, Change_Array) then
            Sum := @ + Element (B) (P + 3).Price;
         end if; -- Found (Element (B), P, Change_Array)
      end loop; -- B in Iterate (Buyer_List)
      return Sum;
   end Sum_For_Run;

   Number_List : Number_Lists.List;
   Sum : Secret_Numbers := 0;
   Largest_Sum : Natural := 0;
   Sum_2 : Natural;
   Buyer_List : Buyer_Lists .List;
   Change_List : Change_Lists.List;

begin -- December_22
   Read_Input (Number_List);
   for S in Iterate (Number_List) loop
      Sum := @ + Last_Number (Element (S));
   end loop; -- S in Iterate (Nunber_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Build_Buyer_List (Number_List, Buyer_List);
   Build_Change_List (Change_List);
   for C in Iterate (Change_List) loop
   Sum_2 := Sum_For_Run (Buyer_List, Element (C));
      if Sum_2 > Largest_Sum then
         Largest_Sum := Sum_2;
         Put_Line (Sum_2'Img);
      end if; -- Sum > Largest_Sum
   end loop; -- C in Iterate (Change_List)
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
