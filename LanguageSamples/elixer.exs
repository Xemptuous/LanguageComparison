defmodule Showcase do
  def greet(name), do: IO.puts("Hi, #{name}!")

  def square(n), do: n * n

  def run do
    name = "Alice"
    x = 5
    y = 3.14
    active = true

    greet(name)
    IO.puts("Square of #{x} is #{square(x)}")

    p = %{x: 3, y: 4}
    IO.puts("Point: (#{p.x}, #{p.y})")

    nums = [1, 2, 3]
    Enum.each(nums, fn n -> IO.write("#{n} ") end)
    IO.puts("")

    ages = %{"Alice" => 30, "Bob" => 25}
    IO.puts("Bob is #{ages["Bob"]} years old.")

    loop(0)
  end

  defp loop(count) when count < 3 do
    IO.puts("While loop: #{count}")
    loop(count + 1)
  end

  defp loop(_), do: :ok
end

Showcase.run()
