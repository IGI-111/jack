fun a2(): [int;3] = [1, 2, 3]
fun main(): int = let a1 = [40] in a1[0] + (a2())[1]
