fun int_eq(): bool = 42 == 42
fun bool_eq(): bool = true == true
fun float_eq(): bool = 42.0 == 42.0

fun int_neq(): bool = 42 != 43
fun bool_neq(): bool = true != false
fun float_neq(): bool = 42.0 != 43.0

fun int_mul(): bool = 2 * 2 == 4
fun float_mul(): bool = 2.0 * 2.0 == 4.0

fun int_div(): bool = 4 / 2 == 2
fun float_div(): bool = 4.0 / 2.0 == 2.0

fun int_add(): bool = 2 + 2 == 4
fun float_add(): bool = 2.0 + 2.0 == 4.0

fun int_sub(): bool = 4 - 2 == 2
fun float_sub(): bool = 4.0 - 2.0 == 2.0

fun int_lt(): bool = 2 < 4
fun float_lt(): bool = 2.0 < 4.0

fun int_le(): bool = 4 <= 4
fun float_le(): bool = 4.0 <= 4.0

fun bool_and(): bool = true && true
fun bool_or(): bool = true || false

fun int_gt(): bool = 4 > 2
fun float_gt(): bool = 4.0 > 2.0

fun int_ge(): bool = 4 >= 4
fun float_ge(): bool = 4.0 >= 4.0


fun main(): bool =
    int_eq() &&
    bool_eq() &&
    float_eq() &&
    int_neq() &&
    bool_neq() &&
    float_neq() &&
    bool_and() &&
    bool_or() &&
    int_mul() &&
    float_mul() &&
    int_div() &&
    float_div() &&
    int_add() &&
    float_add() &&
    int_sub() &&
    float_sub() &&
    int_lt() &&
    float_lt() &&
    int_le() &&
    float_le() &&
    int_gt() &&
    float_gt() &&
    int_ge() &&
    float_ge()
