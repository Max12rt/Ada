-- Autorzy: Ruslan Rabadanov (196634), Vitalii Shapovalov (196633)

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   Product_Name: constant array (Product_Type) of String(1 .. 14)
     := ("Potato        ", "Chicken breast", "Broccoli      ", "Tomato        ", "Lemon         ");
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 37)
     := ("Chicken and Potato with Broccoli     ", "Tomato Salad with Lemon              ", "Baked Chicken and Potatoes with Lemon");
   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);
   type My_Str is new String(1 ..256);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Consumer is
      -- Give the Consumer an identity
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   -- In the Buffer, products are assemblied into an assembly
   task type Buffer is
      -- Accept a product to the storage provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer; IsTaken: out Boolean);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer; HasTaken: out Boolean);
   end Buffer;

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;
   task body Producer is
      subtype Production_Time_Range is Integer range 6 .. 10;
      package Random_Production is new
        Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;  --  generator liczb losowych
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
      IsTaken: Boolean := True;

   begin
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production.Reset(G);  --  start random number generator
         Product_Number := 1;
         Product_Type_Number := Product;
         Production := Production_Time;
      end Start;
      Put_Line("[PRODUCER] Started producer of " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(G)); --  symuluj produkcje
         if IsTaken then
            Put_Line("[PRODUCER] Produced product [" & Integer'Image(Product_Number) & " ] " & Product_Name(Product_Type_Number));
         end if;
         -- Accept for storage
         select
            B.Take(Product_Type_Number, Product_Number, IsTaken);
         else
            delay 5.0;
            IsTaken := False;
            Put_Line("[STORAGE] We can't take it now, try later ;( [" & Integer'Image(Product_Number) & " ] "
                     & Product_Name(Product_Type_Number));
         end select;
         if IsTaken then
            Product_Number := Product_Number + 1;
         end if;
      end loop;
   end Producer;


   task body Consumer is
      subtype Consumption_Time_Range is Integer range 8 .. 12;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;  --  random number generator (time)
      G2: Random_Assembly.Generator;  --  also (assemblies)
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer := 1;
      Consumption: Integer;
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 7)
        := ("Bebra  ", "Vitalii");
      HasTaken: Boolean := True;
      Waiting_Counter: Integer := 0;
   begin
      accept Start(Consumer_Number: in Consumer_Type;
                   Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);
         Random_Assembly.Reset(G2);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line("Started consumer " & Consumer_Name(Consumer_Nb));
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Waiting_Counter := Waiting_Counter + 1;
         if Waiting_Counter = 3 then
            Waiting_Counter := 0;
            HasTaken := True;
            Put_Line("[CONSUMER] " & Consumer_Name(Consumer_Nb) & ": has been waiting for too long time and left.");
         else
            if HasTaken then
               Assembly_Type := Random_Assembly.Random(G2);
               -- take an assembly for consumption
               Waiting_Counter := 0;
               Put_Line("[CONSUMER] " & Consumer_Name(Consumer_Nb) & ": ordered dish " &
                          Assembly_Name(Assembly_Type));
            end if;

            HasTaken := False;
            select
               B.Deliver(Assembly_Type, Assembly_Number, HasTaken);
               if HasTaken then
                  Put_Line("[CONSUMER] " & Consumer_Name(Consumer_Nb) & ": taken dish [" & Integer'Image(Assembly_Number) & " ] " &
                             Assembly_Name(Assembly_Type));
               else
                  Put_Line("[CONSUMER] " & Consumer_Name(Consumer_Nb) & ": Please, wait. Lacking products for dish ["
                           & Integer'Image(Assembly_Number) & " ] " & Assembly_Name(Assembly_Type));
               end if;
            else
               Put_Line("[CONSUMER] " & Consumer_Name(Consumer_Nb) & ": Now we can't deliver your ["
                        & Integer'Image(Assembly_Number) & " ] " & Assembly_Name(Assembly_Type));
            end select;

         end if;

      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((2, 2, 1, 0, 0),
            (0, 0, 1, 2, 1),
            (2, 2, 0, 0, 1));
      -- 4, 4, 2, 2, 2 => 14
      -- 1, 2: 30/14 * 4 = 8.57 -> 9
      -- 3,4,5: 30/14 * 2 = 4.28 -> 4
      -- sum(9, 9, 4, 4, 4) = capacity = 30
      Max_Assembly_Content: array(Product_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;
      Max_Capacity_For_Product: array(Product_Type) of Integer
        := (0, 0, 0, 0, 0);

      procedure Setup_Variables is
         Product_Proportions_Sum: Integer := 0;
         Product_Proportions: array(Product_Type) of Integer := (0, 0, 0, 0, 0);
         Coefficient: Float := 0.0;
      begin

         for W in Product_Type loop
            Max_Assembly_Content(W) := 0;

            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
               Product_Proportions(W) := Product_Proportions(W) + Assembly_Content(Z, W);

            end loop;

            Product_Proportions_Sum := Product_Proportions_Sum + Product_Proportions(W);

         end loop;

         Coefficient := Float(Storage_Capacity) / Float (Product_Proportions_Sum);

         for Product_Index in Product_Type loop
            Max_Capacity_For_Product(Product_Index) := Integer (Float'Rounding (Coefficient * Float(Product_Proportions(Product_Index))));
         end loop;


      end Setup_Variables;


      function Can_Accept(Product: Product_Type) return Boolean is
         Free: Integer;    --  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room: Integer;
         MP: Boolean;      --  can accept
      begin
         if Storage(Product) >= Max_Capacity_For_Product(Product) then
            return False;
         end if;
         -- There is free room in the storage
         Free := Storage_Capacity - In_Storage;
         MP := True;
         for W in Product_Type loop
            if Storage(W) < Max_Assembly_Content(W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;    --  storage has products for arbitrary
            --  assembly
         end if;
         if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
            -- exactly this product lacks
            return True;
         end if;
         Lacking_room := 1;      --  insert current product
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Assembly_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         Put_Line("------------------------------------------------------------------------");
         for W in Product_Type loop
            Put_Line("[STORAGE]" & Integer'Image(Storage(W)) & " "
                     & Product_Name(W));
         end loop;
         Put_Line("------------------------------------------------------------------------");
      end Storage_Contents;

   begin
      Put_Line("STORAGE started");
      Setup_Variables;
      loop
         select
            when In_Storage < Storage_Capacity =>
               accept Take(Product: in Product_Type; Number: in Integer; IsTaken: out Boolean) do
                  IsTaken := Can_Accept(Product);
                  if IsTaken then
                     Put_Line("[STORAGE] Accepted product [" & Integer'Image(Number) & " ] " & Product_Name(Product) );
                     Storage(Product) := Storage(Product) + 1;
                     In_Storage := In_Storage + 1;
                     Storage_Contents;
                  else
                     Put_Line("[STORAGE] Storage is full, we can't accept [" & Integer'Image(Number) & " ] " & Product_Name(Product));
                  end if;
               end Take;
         or
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer; HasTaken: out Boolean) do
               Number := Assembly_Number(Assembly);

               if Can_Deliver(Assembly) then
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
                  HasTaken := True;
                  Storage_Contents;
               else
                  HasTaken := False;
               end if;

            end Deliver;

         end select;
      end loop;
   end Buffer;

begin
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I, 10);
   end loop;

   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;
end Simulation;

