from math import ceil, floor, trunc

import pytest

from ethereum_types.bytes import Bytes64
from ethereum_types.numeric import U256, Uint


def test_uint_new() -> None:
    value = Uint(5)
    assert not isinstance(value, int)
    assert isinstance(value, Uint)


def test_uint_new_negative() -> None:
    with pytest.raises(OverflowError):
        Uint(-5)


def test_uint_new_float() -> None:
    with pytest.raises(TypeError):
        Uint(0.1)  # type: ignore


def test_uint_radd() -> None:
    assert Uint(9) == Uint(4).__radd__(Uint(5))


def test_uint_radd_int() -> None:
    with pytest.raises(TypeError):
        4 + Uint(5)  # type: ignore[operator]


def test_uint_radd_negative() -> None:
    with pytest.raises(TypeError):
        (-4) + Uint(5)  # type: ignore[operator]


def test_uint_radd_float() -> None:
    with pytest.raises(TypeError):
        (1.0) + Uint(5)  # type: ignore[operator]


def test_uint_add() -> None:
    assert Uint(9) == Uint(5) + Uint(4)


def test_uint_add_int() -> None:
    with pytest.raises(TypeError):
        Uint(5) + 4  # type: ignore[operator]


def test_uint_add_negative() -> None:
    with pytest.raises(TypeError):
        Uint(5) + (-4)  # type: ignore[operator]


def test_uint_add_float() -> None:
    with pytest.raises(TypeError):
        Uint(5) + (1.0)  # type: ignore[operator]


def test_uint_iadd() -> None:
    value = Uint(5)
    value += Uint(4)
    assert value == Uint(9)


def test_uint_iadd_int() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value += 4  # type: ignore[arg-type]


def test_uint_iadd_negative() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value += -4  # type: ignore[arg-type]


def test_uint_iadd_float() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value += 1.0  # type: ignore


def test_uint_rsub() -> None:
    assert Uint(1) == Uint(5).__rsub__(Uint(6))


def test_uint_rsub_int() -> None:
    with pytest.raises(TypeError):
        6 - Uint(5)  # type: ignore[operator]


def test_uint_rsub_too_big() -> None:
    with pytest.raises(OverflowError):
        Uint(7).__rsub__(Uint(6))


def test_uint_rsub_negative() -> None:
    with pytest.raises(TypeError):
        (-4) - Uint(5)  # type: ignore[operator]


def test_uint_rsub_float() -> None:
    with pytest.raises(TypeError):
        (6.0) - Uint(5)  # type: ignore[operator]


def test_uint_sub() -> None:
    with pytest.raises(TypeError):
        Uint(5) - 4  # type: ignore[operator]


def test_uint_sub_too_big() -> None:
    with pytest.raises(OverflowError):
        Uint(5) - Uint(6)


def test_uint_sub_negative() -> None:
    with pytest.raises(TypeError):
        Uint(5) - (-4)  # type: ignore[operator]


def test_uint_sub_float() -> None:
    with pytest.raises(TypeError):
        Uint(5) - (1.0)  # type: ignore[operator]


def test_uint_isub() -> None:
    value = Uint(5)
    value -= Uint(4)
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_isub_too_big() -> None:
    value = Uint(5)
    with pytest.raises(OverflowError):
        value -= Uint(6)


def test_uint_isub_negative() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value -= -4  # type: ignore[arg-type]


def test_uint_isub_float() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value -= 1.0  # type: ignore


def test_uint_rmul() -> None:
    actual = Uint(4).__rmul__(Uint(5))
    assert actual == Uint(20)


def test_uint_rmul_int() -> None:
    with pytest.raises(TypeError):
        4 * Uint(5)  # type: ignore[operator]


def test_uint_rmul_negative() -> None:
    with pytest.raises(TypeError):
        (-4) * Uint(5)  # type: ignore[operator]


def test_uint_rmul_float() -> None:
    with pytest.raises(TypeError):
        (1.0) * Uint(5)  # type: ignore[operator]


def test_uint_mul() -> None:
    value = Uint(5) * Uint(4)
    assert isinstance(value, Uint)
    assert value == Uint(20)


def test_uint_mul_int() -> None:
    with pytest.raises(TypeError):
        Uint(5) * 4  # type: ignore[operator]


def test_uint_mul_negative() -> None:
    with pytest.raises(TypeError):
        Uint(5) * (-4)  # type: ignore[operator]


def test_uint_mul_float() -> None:
    with pytest.raises(TypeError):
        Uint(5) * (1.0)  # type: ignore[operator]


def test_uint_imul() -> None:
    value = Uint(5)
    value *= Uint(4)
    assert isinstance(value, Uint)
    assert value == Uint(20)


def test_uint_imul_int() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value *= 4  # type: ignore[arg-type]


def test_uint_imul_negative() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value *= -4  # type: ignore[arg-type]


def test_uint_imul_float() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value *= 1.0  # type: ignore


def test_uint_floordiv() -> None:
    value = Uint(5) // Uint(2)
    assert isinstance(value, Uint)
    assert value == Uint(2)


def test_uint_floordiv_int() -> None:
    with pytest.raises(TypeError):
        Uint(5) // 2  # type: ignore[operator]


def test_uint_floordiv_negative() -> None:
    with pytest.raises(TypeError):
        Uint(5) // -2  # type: ignore[operator]


def test_uint_floordiv_float() -> None:
    with pytest.raises(TypeError):
        Uint(5) // 2.0  # type: ignore[operator]


def test_uint_rfloordiv() -> None:
    value = Uint(2).__rfloordiv__(Uint(5))
    assert isinstance(value, Uint)
    assert value == Uint(2)


def test_uint_rfloordiv_negative() -> None:
    with pytest.raises(TypeError):
        (-2) // Uint(5)  # type: ignore[operator]


def test_uint_rfloordiv_float() -> None:
    with pytest.raises(TypeError):
        5.0 // Uint(2)  # type: ignore[operator]


def test_uint_ifloordiv() -> None:
    value = Uint(5)
    value //= Uint(2)
    assert isinstance(value, Uint)
    assert value == Uint(2)


def test_uint_ifloordiv_negative() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value //= -2  # type: ignore[arg-type]


def test_uint_rmod() -> None:
    value = Uint(2).__rmod__(Uint(5))
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_rmod_negative() -> None:
    with pytest.raises(TypeError):
        (-4) % Uint(5)  # type: ignore[operator]


def test_uint_rmod_float() -> None:
    with pytest.raises(TypeError):
        (6.0) % Uint(5)  # type: ignore[operator]


def test_uint_mod() -> None:
    value = Uint(5) % Uint(4)
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_mod_negative() -> None:
    with pytest.raises(TypeError):
        Uint(5) % (-4)  # type: ignore[operator]


def test_uint_mod_float() -> None:
    with pytest.raises(TypeError):
        Uint(5) % (1.0)  # type: ignore[operator]


def test_uint_imod() -> None:
    value = Uint(5)
    value %= Uint(4)
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_imod_negative() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value %= -4  # type: ignore[arg-type]


def test_uint_imod_float() -> None:
    value = Uint(5)
    with pytest.raises(TypeError):
        value %= 1.0  # type: ignore


def test_uint_divmod() -> None:
    quotient, remainder = divmod(Uint(5), Uint(2))
    assert isinstance(quotient, Uint)
    assert isinstance(remainder, Uint)
    assert quotient == Uint(2)
    assert remainder == Uint(1)


def test_uint_divmod_negative() -> None:
    with pytest.raises(TypeError):
        divmod(Uint(5), -2)  # type: ignore[operator]


def test_uint_divmod_float() -> None:
    with pytest.raises(TypeError):
        divmod(Uint(5), 2.0)  # type: ignore[operator]


def test_uint_rdivmod() -> None:
    quotient, remainder = Uint(2).__rdivmod__(Uint(5))
    assert quotient == Uint(2)
    assert remainder == Uint(1)


def test_uint_rdivmod_int() -> None:
    with pytest.raises(TypeError):
        divmod(5, Uint(2))  # type: ignore[operator]


def test_uint_rdivmod_negative() -> None:
    with pytest.raises(TypeError):
        divmod(-5, Uint(2))  # type: ignore[operator]


def test_uint_rdivmod_float() -> None:
    with pytest.raises(TypeError):
        divmod(5.0, Uint(2))  # type: ignore[operator]


def test_uint_pow() -> None:
    value = Uint(3) ** Uint(2)
    assert isinstance(value, Uint)
    assert value == Uint(9)


def test_uint_pow_negative() -> None:
    with pytest.raises(TypeError):
        Uint(3) ** -2  # type: ignore[operator]


def test_uint_pow_modulo() -> None:
    value = pow(Uint(4), Uint(2), Uint(3))
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_pow_modulo_negative() -> None:
    with pytest.raises(TypeError):
        pow(Uint(4), 2, -3)  # type: ignore[misc]


def test_uint_rpow() -> None:
    value = Uint(2).__rpow__(Uint(3))
    assert isinstance(value, Uint)
    assert value == Uint(9)


def test_uint_rpow_negative() -> None:
    with pytest.raises(TypeError):
        (-3) ** Uint(2)  # type: ignore[operator]


def test_uint_rpow_modulo() -> None:
    value = Uint.__rpow__(Uint(2), Uint(4), Uint(3))
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_rpow_modulo_negative() -> None:
    with pytest.raises(TypeError):
        Uint.__rpow__(Uint(2), 4, -3)  # type: ignore[operator]


def test_uint_ipow() -> None:
    value = Uint(3)
    value **= Uint(2)
    assert isinstance(value, Uint)
    assert value == Uint(9)


def test_uint_ipow_negative() -> None:
    value = Uint(3)
    with pytest.raises(TypeError):
        value **= -2  # type: ignore[arg-type]


def test_uint_ipow_modulo() -> None:
    value = Uint(4).__ipow__(Uint(2), Uint(3))
    assert isinstance(value, Uint)
    assert value == Uint(1)


def test_uint_ipow_modulo_negative() -> None:
    with pytest.raises(TypeError):
        Uint(4).__ipow__(Uint(2), -3)  # type: ignore[arg-type]


def test_uint_to_be_bytes_zero() -> None:
    encoded = Uint(0).to_be_bytes()
    assert encoded == bytes([])


def test_uint_to_be_bytes_one() -> None:
    encoded = Uint(1).to_be_bytes()
    assert encoded == bytes([1])


def test_uint_to_be_bytes_is_big_endian() -> None:
    encoded = Uint(0xABCD).to_be_bytes()
    assert encoded == bytes([0xAB, 0xCD])


def test_uint_to_be_bytes64_zero() -> None:
    actual = Uint(0).to_be_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_be_bytes64_one() -> None:
    actual = Uint(1).to_be_bytes64()
    expected = Bytes64([0] * 63 + [1])
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_be_bytes64_max_value() -> None:
    actual = Uint(2**512 - 1).to_be_bytes64()
    expected = Bytes64([0xFF] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_be_bytes64_too_big() -> None:
    value = Uint(2**512)

    with pytest.raises(OverflowError):
        value.to_be_bytes64()


def test_uint_to_be_bytes32_zero() -> None:
    encoded = Uint(0).to_be_bytes32()
    assert encoded == bytes([0] * 32)


def test_uint_to_be_bytes32_one() -> None:
    encoded = Uint(1).to_be_bytes32()
    assert encoded == bytes([0] * 31 + [1])


def test_uint_to_be_bytes32_max_value() -> None:
    encoded = Uint(2**256 - 1).to_be_bytes32()
    assert encoded == bytes(
        [
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
        ]
    )


def test_uint_from_be_bytes_empty() -> None:
    value = Uint.from_be_bytes(b"")
    assert value == Uint(0)


def test_uint_from_be_bytes_one() -> None:
    value = Uint.from_be_bytes(bytes([1]))
    assert value == Uint(1)


def test_uint_from_be_bytes_is_big_endian() -> None:
    value = Uint.from_be_bytes(bytes([0xAB, 0xCD]))
    assert value == Uint(0xABCD)


def test_uint_from_le_bytes_empty() -> None:
    value = Uint.from_le_bytes(b"")
    assert value == Uint(0)


def test_uint_from_le_bytes_one() -> None:
    value = Uint.from_le_bytes(bytes([1]))
    assert value == Uint(1)


def test_uint_from_le_bytes_is_big_endian() -> None:
    value = Uint.from_le_bytes(bytes([0xAB, 0xCD]))
    assert value == Uint(0xCDAB)


def test_u256_new() -> None:
    value = U256(5)
    assert isinstance(value, int)
    assert isinstance(value, U256)
    assert value == 5


def test_u256_new_negative() -> None:
    with pytest.raises(OverflowError):
        U256(-5)


def test_u256_new_float() -> None:
    with pytest.raises(TypeError):
        U256(0.1)  # type: ignore


def test_u256_new_max_value() -> None:
    value = U256(2**256 - 1)
    assert isinstance(value, U256)
    assert value == 2**256 - 1


def test_u256_new_too_large() -> None:
    with pytest.raises(OverflowError):
        U256(2**256)


def test_u256_radd() -> None:
    value = 4 + U256(5)
    assert isinstance(value, U256)
    assert value == 9


def test_u256_radd_overflow() -> None:
    with pytest.raises(OverflowError):
        (2**256 - 1) + U256(5)


def test_u256_radd_negative() -> None:
    with pytest.raises(OverflowError):
        (-4) + U256(5)


def test_u256_radd_float() -> None:
    value = (1.0) + U256(5)
    assert not isinstance(value, int)
    assert value == 6.0


def test_u256_add() -> None:
    value = U256(5) + 4
    assert isinstance(value, U256)
    assert value == 9


def test_u256_add_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(5) + (2**256 - 1)


def test_u256_add_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5) + (-4)


def test_u256_add_float() -> None:
    value = U256(5) + (1.0)
    assert not isinstance(value, int)
    assert value == 6.0


def test_u256_wrapping_add() -> None:
    value = U256(5).wrapping_add(4)
    assert isinstance(value, U256)
    assert value == 9


def test_u256_wrapping_add_overflow() -> None:
    value = U256(5).wrapping_add(2**256 - 1)
    assert isinstance(value, U256)
    assert value == 4


def test_u256_wrapping_add_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5).wrapping_add(-4)


def test_u256_iadd() -> None:
    value = U256(5)
    value += 4
    assert isinstance(value, U256)
    assert value == 9


def test_u256_iadd_negative() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value += -4


def test_u256_iadd_float() -> None:
    value = U256(5)
    value += 1.0  # type: ignore
    assert not isinstance(value, int)
    assert value == 6.0


def test_u256_iadd_overflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value += 2**256 - 1


def test_u256_rsub() -> None:
    value = 5 - U256(4)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_rsub_underflow() -> None:
    with pytest.raises(OverflowError):
        (0) - U256(1)


def test_u256_rsub_negative() -> None:
    with pytest.raises(OverflowError):
        (-4) - U256(5)


def test_u256_rsub_float() -> None:
    value = (5.0) - U256(1)
    assert not isinstance(value, int)
    assert value == 4.0


def test_u256_sub() -> None:
    value = U256(5) - 4
    assert isinstance(value, U256)
    assert value == 1


def test_u256_sub_underflow() -> None:
    with pytest.raises(OverflowError):
        U256(5) - 6


def test_u256_sub_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5) - (-4)


def test_u256_sub_float() -> None:
    value = U256(5) - (1.0)
    assert not isinstance(value, int)
    assert value == 4.0


def test_u256_wrapping_sub() -> None:
    value = U256(5).wrapping_sub(4)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_wrapping_sub_underflow() -> None:
    value = U256(5).wrapping_sub(6)
    assert isinstance(value, U256)
    assert value == 2**256 - 1


def test_u256_wrapping_sub_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5).wrapping_sub(-4)


def test_u256_isub() -> None:
    value = U256(5)
    value -= 4
    assert isinstance(value, U256)
    assert value == 1


def test_u256_isub_negative() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value -= -4


def test_u256_isub_float() -> None:
    value = U256(5)
    value -= 1.0  # type: ignore
    assert not isinstance(value, int)
    assert value == 4.0


def test_u256_isub_underflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value -= 6


def test_u256_rmul() -> None:
    value = 4 * U256(5)
    assert isinstance(value, U256)
    assert value == 20


def test_u256_rmul_overflow() -> None:
    with pytest.raises(OverflowError):
        (2**256 - 1) * U256(5)


def test_u256_rmul_negative() -> None:
    with pytest.raises(OverflowError):
        (-4) * U256(5)


def test_u256_rmul_float() -> None:
    value = (1.0) * U256(5)
    assert not isinstance(value, int)
    assert value == 5.0


def test_u256_mul() -> None:
    value = U256(5) * 4
    assert isinstance(value, U256)
    assert value == 20


def test_u256_mul_overflow() -> None:
    with pytest.raises(OverflowError):
        U256.MAX_VALUE * 4


def test_u256_mul_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5) * (-4)


def test_u256_mul_float() -> None:
    value = U256(5) * (1.0)
    assert not isinstance(value, int)
    assert value == 5.0


def test_u256_wrapping_mul() -> None:
    value = U256(5).wrapping_mul(4)
    assert isinstance(value, U256)
    assert value == 20


def test_u256_wrapping_mul_overflow() -> None:
    value = U256.MAX_VALUE.wrapping_mul(4)
    assert isinstance(value, U256)
    assert (
        value
        == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC
    )


def test_u256_wrapping_mul_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5).wrapping_mul(-4)


def test_u256_imul() -> None:
    value = U256(5)
    value *= 4
    assert isinstance(value, U256)
    assert value == 20


def test_u256_imul_negative() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value *= -4


def test_u256_imul_arg_overflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value *= 2**256


def test_u256_imul_float() -> None:
    value = U256(5)
    value *= 1.0  # type: ignore
    assert not isinstance(value, int)
    assert value == 5.0


def test_u256_imul_overflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value *= 2**256 - 1


def test_u256_floordiv() -> None:
    value = U256(5) // 2
    assert isinstance(value, U256)
    assert value == 2


def test_u256_floordiv_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(5) // (2**256)


def test_u256_floordiv_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5) // -2


def test_u256_floordiv_float() -> None:
    value = U256(5) // 2.0
    assert not isinstance(value, U256)
    assert value == 2


def test_u256_rfloordiv() -> None:
    value = 5 // U256(2)
    assert isinstance(value, U256)
    assert value == 2


def test_u256_rfloordiv_overflow() -> None:
    with pytest.raises(OverflowError):
        (2**256) // U256(2)


def test_u256_rfloordiv_negative() -> None:
    with pytest.raises(OverflowError):
        (-2) // U256(5)


def test_u256_rfloordiv_float() -> None:
    value = 5.0 // U256(2)
    assert not isinstance(value, U256)
    assert value == 2


def test_u256_ifloordiv() -> None:
    value = U256(5)
    value //= 2
    assert isinstance(value, U256)
    assert value == 2


def test_u256_ifloordiv_negative() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value //= -2


def test_u256_ifloordiv_overflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value //= 2**256


def test_u256_rmod() -> None:
    value = 6 % U256(5)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_rmod_float() -> None:
    value = (6.0) % U256(5)
    assert not isinstance(value, int)
    assert value == 1.0


def test_u256_mod() -> None:
    value = U256(5) % 4
    assert isinstance(value, U256)
    assert value == 1


def test_u256_mod_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(5) % (2**256)


def test_u256_mod_negative() -> None:
    with pytest.raises(OverflowError):
        U256(5) % (-4)


def test_u256_mod_float() -> None:
    value = U256(5) % (1.0)
    assert not isinstance(value, int)
    assert value == 0.0


def test_u256_imod() -> None:
    value = U256(5)
    value %= 4
    assert isinstance(value, U256)
    assert value == 1


def test_u256_imod_overflow() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value %= 2**256


def test_u256_imod_negative() -> None:
    value = U256(5)
    with pytest.raises(OverflowError):
        value %= -4


def test_u256_imod_float() -> None:
    value = U256(5)
    value %= 1.0  # type: ignore
    assert not isinstance(value, int)
    assert value == 0.0


def test_u256_divmod() -> None:
    quotient, remainder = divmod(U256(5), 2)
    assert isinstance(quotient, U256)
    assert isinstance(remainder, U256)
    assert quotient == 2
    assert remainder == 1


def test_u256_divmod_overflow() -> None:
    with pytest.raises(OverflowError):
        divmod(U256(5), 2**256)


def test_u256_divmod_negative() -> None:
    with pytest.raises(OverflowError):
        divmod(U256(5), -2)


def test_u256_divmod_float() -> None:
    quotient, remainder = divmod(U256(5), 2.0)
    assert not isinstance(quotient, U256)
    assert not isinstance(remainder, U256)
    assert quotient == 2
    assert remainder == 1


def test_u256_rdivmod() -> None:
    quotient, remainder = divmod(5, U256(2))
    assert isinstance(quotient, U256)
    assert isinstance(remainder, U256)
    assert quotient == 2
    assert remainder == 1


def test_u256_rdivmod_overflow() -> None:
    with pytest.raises(OverflowError):
        divmod(2**256, U256(2))


def test_u256_rdivmod_negative() -> None:
    with pytest.raises(OverflowError):
        divmod(-5, U256(2))


def test_u256_rdivmod_float() -> None:
    quotient, remainder = divmod(5.0, U256(2))
    assert not isinstance(quotient, U256)
    assert not isinstance(remainder, U256)
    assert quotient == 2
    assert remainder == 1


def test_u256_pow() -> None:
    value = U256(3) ** 2
    assert isinstance(value, U256)
    assert value == 9


def test_u256_pow_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(340282366920938463463374607431768211456) ** 3


def test_u256_pow_negative() -> None:
    with pytest.raises(OverflowError):
        U256(3) ** -2


def test_u256_pow_modulo() -> None:
    value = pow(U256(4), 2, 3)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_pow_modulo_overflow() -> None:
    with pytest.raises(OverflowError):
        pow(U256(4), 2, 2**257)


def test_u256_pow_modulo_negative() -> None:
    with pytest.raises(OverflowError):
        pow(U256(4), 2, -3)


def test_u256_rpow() -> None:
    value = 3 ** U256(2)
    assert isinstance(value, U256)
    assert value == 9


def test_u256_rpow_overflow() -> None:
    with pytest.raises(OverflowError):
        (2**256) ** U256(2)


def test_u256_rpow_negative() -> None:
    with pytest.raises(OverflowError):
        (-3) ** U256(2)


def test_u256_rpow_modulo() -> None:
    value = U256.__rpow__(U256(2), 4, 3)
    assert isinstance(value, int)
    assert value == 1


def test_u256_rpow_modulo_overflow() -> None:
    with pytest.raises(OverflowError):
        U256.__rpow__(U256(2), 4, 2**256 + 1)


def test_u256_rpow_modulo_negative() -> None:
    with pytest.raises(OverflowError):
        U256.__rpow__(U256(2), 4, -3)


def test_u256_ipow() -> None:
    value = U256(3)
    value **= 2
    assert isinstance(value, U256)
    assert value == 9


def test_u256_ipow_overflow() -> None:
    value = U256(340282366920938463463374607431768211456)
    with pytest.raises(OverflowError):
        value **= 3


def test_u256_ipow_negative() -> None:
    value = U256(3)
    with pytest.raises(OverflowError):
        value **= -2


def test_u256_ipow_modulo() -> None:
    value = U256(4).__ipow__(2, 3)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_ipow_modulo_negative() -> None:
    with pytest.raises(OverflowError):
        U256(4).__ipow__(2, -3)


def test_u256_ipow_modulo_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(4).__ipow__(2, 2**256 + 1)


def test_u256_wrapping_pow() -> None:
    value = U256(3).wrapping_pow(2)
    assert isinstance(value, U256)
    assert value == 9


def test_u256_wrapping_pow_overflow() -> None:
    value = U256(340282366920938463463374607431768211455).wrapping_pow(3)
    assert isinstance(value, U256)
    assert value == 0x2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF


def test_u256_wrapping_pow_negative() -> None:
    with pytest.raises(OverflowError):
        U256(3).wrapping_pow(-2)


def test_u256_wrapping_pow_modulo() -> None:
    value = U256(4).wrapping_pow(2, 3)
    assert isinstance(value, U256)
    assert value == 1


def test_u256_wrapping_pow_modulo_overflow() -> None:
    with pytest.raises(OverflowError):
        U256(4).wrapping_pow(2, 2**256 + 1)


def test_u256_wrapping_pow_modulo_negative() -> None:
    with pytest.raises(OverflowError):
        U256(4).wrapping_pow(2, -3)


def test_u256_to_be_bytes_zero() -> None:
    encoded = U256(0).to_be_bytes()
    assert encoded == bytes([])


def test_u256_to_be_bytes_one() -> None:
    encoded = U256(1).to_be_bytes()
    assert encoded == bytes([1])


def test_u256_to_be_bytes_is_big_endian() -> None:
    encoded = U256(0xABCD).to_be_bytes()
    assert encoded == bytes([0xAB, 0xCD])


def test_u256_to_be_bytes32_zero() -> None:
    encoded = U256(0).to_be_bytes32()
    assert encoded == bytes([0] * 32)


def test_u256_to_be_bytes32_one() -> None:
    encoded = U256(1).to_be_bytes32()
    assert encoded == bytes([0] * 31 + [1])


def test_u256_to_be_bytes32_max_value() -> None:
    encoded = U256(2**256 - 1).to_be_bytes32()
    assert encoded == bytes(
        [
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
        ]
    )


def test_u256_from_be_bytes_empty() -> None:
    value = U256.from_be_bytes(b"")
    assert value == 0


def test_u256_from_be_bytes_one() -> None:
    value = U256.from_be_bytes(bytes([1]))
    assert value == 1


def test_u256_from_be_bytes_is_big_endian() -> None:
    value = U256.from_be_bytes(bytes([0xAB, 0xCD]))
    assert value == 0xABCD


def test_u256_from_be_bytes_too_large() -> None:
    with pytest.raises(ValueError):
        U256.from_be_bytes(bytes([0xFF] * 33))


def test_u256_bitwise_and_successful() -> None:
    assert U256(0) & U256(0) == 0
    assert U256(2**256 - 1) & U256(2**256 - 1) == 2**256 - 1
    assert U256(2**256 - 1) & U256(0) == U256(0)


def test_u256_bitwise_and_fails() -> None:
    with pytest.raises(OverflowError):
        U256(0) & (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) & (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) & -10


def test_u256_bitwise_or_successful() -> None:
    assert U256(0) | U256(0) == 0
    assert U256(2**256 - 1) | U256(0) == 2**256 - 1
    assert U256(2**256 - 1) | U256(2**256 - 1) == U256(2**256 - 1)
    assert U256(2**256 - 1) | U256(17) == U256(2**256 - 1)
    assert U256(17) | U256(18) == U256(19)


def test_u256_bitwise_or_failed() -> None:
    with pytest.raises(OverflowError):
        U256(0) | (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) | (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) | -10


def test_u256_bitwise_xor_successful() -> None:
    assert U256(0) ^ U256(0) == 0
    assert U256(2**256 - 1) ^ U256(0) == 2**256 - 1
    assert U256(2**256 - 1) ^ U256(2**256 - 1) == U256(0)
    assert U256(2**256 - 1) ^ U256(17) == U256(2**256 - 1) - U256(17)
    assert U256(17) ^ U256(18) == U256(3)


def test_u256_bitwise_xor_failed() -> None:
    with pytest.raises(OverflowError):
        U256(0) ^ (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) ^ (2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1) ^ -10


def test_u256_bitwise_rxor_successful() -> None:
    assert U256(0).__rxor__(U256(0)) == 0
    assert U256(2**256 - 1).__rxor__(U256(0)) == 2**256 - 1
    assert U256(2**256 - 1).__rxor__(U256(2**256 - 1)) == U256(0)
    assert U256(2**256 - 1).__rxor__(U256(17)) == U256(2**256 - 1) - U256(17)
    assert U256(17).__rxor__(U256(18)) == U256(3)


def test_u256_bitwise_rxor_failed() -> None:
    with pytest.raises(OverflowError):
        U256(0).__rxor__(2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1).__rxor__(2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1).__rxor__(-10)


def test_u256_bitwise_ixor_successful() -> None:
    assert U256(0).__ixor__(U256(0)) == 0
    assert U256(2**256 - 1).__ixor__(U256(0)) == 2**256 - 1
    assert U256(2**256 - 1).__ixor__(U256(2**256 - 1)) == U256(0)
    assert U256(2**256 - 1).__ixor__(U256(17)) == U256(2**256 - 1) - U256(17)
    assert U256(17).__ixor__(U256(18)) == U256(3)


def test_u256_bitwise_ixor_failed() -> None:
    with pytest.raises(OverflowError):
        U256(0).__ixor__(2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1).__ixor__(2**256)
    with pytest.raises(OverflowError):
        U256(2**256 - 1).__ixor__(-10)


def test_u256_invert() -> None:
    assert ~U256(0) == int(U256.MAX_VALUE)
    assert ~U256(10) == int(U256.MAX_VALUE) - 10
    assert ~U256(2**256 - 1) == 0


def test_u256_rshift() -> None:
    assert U256.MAX_VALUE >> 255 == 1
    assert U256.MAX_VALUE >> 256 == 0
    assert U256.MAX_VALUE >> 257 == 0
    assert U256(0) >> 20 == 0


def test_uint_to_le_bytes_zero() -> None:
    encoded = Uint(0).to_le_bytes()
    assert encoded == bytes([])


def test_uint_to_le_bytes_one() -> None:
    encoded = Uint(1).to_le_bytes()
    assert encoded == bytes([1])


def test_uint_to_le_bytes_is_little_endian() -> None:
    encoded = Uint(0xABCD).to_le_bytes()
    assert encoded == bytes([0xCD, 0xAB])


def test_uint_to_le_bytes64_zero() -> None:
    actual = Uint(0).to_le_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_le_bytes64_one() -> None:
    actual = Uint(1).to_le_bytes64()
    expected = Bytes64([1] + [0] * 63)
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_le_bytes64_max_value() -> None:
    actual = Uint(2**512 - 1).to_le_bytes64()
    expected = Bytes64([0xFF] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


def test_uint_to_le_bytes32_zero() -> None:
    encoded = Uint(0).to_le_bytes32()
    assert encoded == bytes([0] * 32)


def test_uint_to_le_bytes32_one() -> None:
    encoded = Uint(1).to_le_bytes32()
    assert encoded == bytes([1] + ([0] * 31))


def test_uint_to_le_bytes32_max_value() -> None:
    encoded = Uint(2**256 - 1).to_le_bytes32()
    assert encoded == bytes(
        [
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
        ]
    )


def test_u256_neg() -> None:
    with pytest.raises(TypeError):
        -Uint(1)


def test_uint_pos() -> None:
    assert Uint(1) == +Uint(1)


def test_uint_invert() -> None:
    with pytest.raises(NotImplementedError):
        ~Uint(1)


def test_uint_int() -> None:
    assert int(Uint(0xF000)) == 0xF000


def test_uint_floor_same() -> None:
    a = Uint(1)
    b = floor(a)
    a += Uint(1)
    assert a == Uint(2)
    assert b == Uint(1)


def test_uint_floor() -> None:
    assert floor(Uint(1)) == Uint(1)


def test_uint_ceil_same() -> None:
    a = Uint(1)
    b = ceil(a)
    a += Uint(1)
    assert a == Uint(2)
    assert b == Uint(1)


def test_uint_ceil() -> None:
    assert ceil(Uint(1)) == Uint(1)


def test_uint_ne() -> None:
    assert Uint(1) != Uint(2)


def test_uint_ne_not() -> None:
    assert not (Uint(1) != Uint(1))


def test_uint_ne_different_types() -> None:
    assert Uint(1) != 1


def test_uint_le() -> None:
    assert Uint(1) <= Uint(1)
    assert Uint(0) <= Uint(1)
    assert not (Uint(2) <= Uint(1))


def test_uint_le_different_types() -> None:
    with pytest.raises(TypeError):
        Uint(1) <= 1  # noqa: B015


def test_uint_ge() -> None:
    assert Uint(1) >= Uint(1)
    assert Uint(2) >= Uint(1)
    assert not (Uint(1) >= Uint(2))


def test_uint_ge_different_types() -> None:
    with pytest.raises(TypeError):
        Uint(1) >= 1  # noqa: B015


def test_uint_lt() -> None:
    assert not (Uint(1) < Uint(1))
    assert Uint(0) < Uint(1)
    assert not (Uint(2) < Uint(1))


def test_uint_lt_different_types() -> None:
    with pytest.raises(TypeError):
        Uint(1) < 1  # noqa: B015


def test_uint_gt() -> None:
    assert not (Uint(1) > Uint(1))
    assert Uint(2) > Uint(1)
    assert not (Uint(1) > Uint(2))


def test_uint_gt_different_types() -> None:
    with pytest.raises(TypeError):
        Uint(1) > 1  # noqa: B015


def test_uint_lshift_int() -> None:
    with pytest.raises(TypeError):
        Uint(3) << 4  # type: ignore[operator]


def test_uint_lshift() -> None:
    expected = Uint(3 << 4)
    actual = Uint(3) << Uint(4)
    assert expected == actual


def test_uint_rlshift_int() -> None:
    assert Uint(3).__rlshift__(4) is NotImplemented  # type: ignore[operator]


def test_uint_rlshift() -> None:
    expected = Uint((3).__rlshift__(4))
    actual = Uint(3).__rlshift__(Uint(4))
    assert expected == actual


def test_uint_rshift() -> None:
    expected = Uint(3 >> 4)
    actual = Uint(3) >> Uint(4)
    assert expected == actual


def test_uint_rshift_int() -> None:
    with pytest.raises(TypeError):
        Uint(3) >> 4  # type: ignore[operator]


def test_uint_rrshift() -> None:
    expected = Uint((3).__rrshift__(4))
    actual = Uint(3).__rrshift__(Uint(4))
    assert expected == actual


def test_uint_rrshift_int() -> None:
    assert Uint(3).__rrshift__(4) is NotImplemented  # type: ignore[operator]


def test_uint_abs() -> None:
    assert abs(Uint(1)) == Uint(1)


def test_uint_abs_same() -> None:
    a = Uint(1)
    b = abs(a)
    a += Uint(1)
    assert a == Uint(2)
    assert b == Uint(1)


def test_uint_trunc() -> None:
    assert trunc(Uint(1)) == Uint(1)


def test_uint_trunc_same() -> None:
    a = Uint(1)
    b = trunc(a)
    a += Uint(1)
    assert a == Uint(2)
    assert b == Uint(1)


def test_uint_round() -> None:
    assert round(Uint(1)) == Uint(1)


def test_uint_round_same() -> None:
    a = Uint(1)
    b = round(a)
    a += Uint(1)
    assert a == Uint(2)
    assert b == Uint(1)


def test_uint_truediv_int() -> None:
    assert Uint(1).__truediv__(2) is NotImplemented  # type: ignore[operator]


def test_uint_rtruediv_int() -> None:
    assert Uint(1).__rtruediv__(2) is NotImplemented  # type: ignore[operator]


def test_uint_truediv() -> None:
    expected = (1).__truediv__(2)
    actual = Uint(1).__truediv__(Uint(2))
    assert expected == actual


def test_uint_rtruediv() -> None:
    expected = (1).__rtruediv__(2)
    actual = Uint(1).__rtruediv__(Uint(2))
    assert expected == actual


def test_uint_eq() -> None:
    assert Uint(1) == Uint(1)


def test_uint_eq_not() -> None:
    assert not (Uint(1) == Uint(2))


def test_uint_eq_different_types() -> None:
    assert not (Uint(1) == 1)


def test_uint_hash() -> None:
    assert hash(Uint(1)) == hash(Uint(1))


def test_uint_bitwise_rand_successful() -> None:
    assert Uint(0).__rand__(Uint(0)) == Uint(0)
    assert Uint(2**256 - 1).__rand__(Uint(2**256 - 1)) == Uint(2**256 - 1)
    assert Uint(2**256 - 1).__rand__(Uint(0)) == Uint(0)


def test_uint_bitwise_rand_fails() -> None:
    with pytest.raises(TypeError):
        (2**256) & Uint(0)  # type: ignore[operator]
    with pytest.raises(TypeError):
        (2**256) & Uint(2**256 - 1)  # type: ignore[operator]
    with pytest.raises(TypeError):
        (-10) & Uint(2**256 - 1)  # type: ignore[operator]


def test_uint_bitwise_and_successful() -> None:
    assert Uint(0) & Uint(0) == Uint(0)
    assert Uint(2**256 - 1) & Uint(2**256 - 1) == Uint(2**256 - 1)
    assert Uint(2**256 - 1) & Uint(0) == Uint(0)


def test_uint_bitwise_and_fails() -> None:
    with pytest.raises(TypeError):
        Uint(0) & (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) & (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) & -10  # type: ignore[operator]


def test_uint_bitwise_ror_successful() -> None:
    assert Uint(0).__ror__(Uint(0)) == Uint(0)
    assert Uint(2**256 - 1).__ror__(Uint(0)) == Uint(2**256 - 1)
    assert Uint(2**256 - 1).__ror__(Uint(2**256 - 1)) == Uint(2**256 - 1)
    assert Uint(2**256 - 1).__ror__(Uint(17)) == Uint(2**256 - 1)
    assert Uint(17).__ror__(Uint(18)) == Uint(19)


def test_uint_bitwise_ror_failed() -> None:
    with pytest.raises(TypeError):
        (2**256) | Uint(0)  # type: ignore[operator]
    with pytest.raises(TypeError):
        (2**256) | Uint(2**256 - 1)  # type: ignore[operator]
    with pytest.raises(TypeError):
        (-10) | Uint(2**256 - 1)  # type: ignore[operator]


def test_uint_bitwise_or_successful() -> None:
    assert Uint(0) | Uint(0) == Uint(0)
    assert Uint(2**256 - 1) | Uint(0) == Uint(2**256 - 1)
    assert Uint(2**256 - 1) | Uint(2**256 - 1) == Uint(2**256 - 1)
    assert Uint(2**256 - 1) | Uint(17) == Uint(2**256 - 1)
    assert Uint(17) | Uint(18) == Uint(19)


def test_uint_bitwise_or_failed() -> None:
    with pytest.raises(TypeError):
        Uint(0) | (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) | (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) | -10  # type: ignore[operator]


def test_uint_bitwise_xor_successful() -> None:
    assert Uint(0) ^ Uint(0) == Uint(0)
    assert Uint(2**256 - 1) ^ Uint(0) == Uint(2**256 - 1)
    assert Uint(2**256 - 1) ^ Uint(2**256 - 1) == Uint(0)
    assert Uint(2**256 - 1) ^ Uint(17) == Uint(2**256 - 1) - Uint(17)
    assert Uint(17) ^ Uint(18) == Uint(3)


def test_uint_bitwise_xor_failed() -> None:
    with pytest.raises(TypeError):
        Uint(0) ^ (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) ^ (2**256)  # type: ignore[operator]
    with pytest.raises(TypeError):
        Uint(2**256 - 1) ^ -10  # type: ignore[operator]


def test_uint_bitwise_rxor_successful() -> None:
    assert Uint(0).__rxor__(Uint(0)) == Uint(0)
    assert Uint(2**256 - 1).__rxor__(Uint(0)) == Uint(2**256 - 1)
    assert Uint(2**256 - 1).__rxor__(Uint(2**256 - 1)) == Uint(0)
    assert Uint(2**256 - 1).__rxor__(Uint(17)) == Uint(2**256 - 1) - Uint(17)
    assert Uint(17).__rxor__(Uint(18)) == Uint(3)


def test_uint_bitwise_rxor_failed() -> None:
    assert Uint(0).__rxor__(2**256) is NotImplemented  # type: ignore[operator]
    assert Uint(2**256 - 1).__rxor__(2**256) is NotImplemented  # type: ignore[operator]
    assert Uint(2**256 - 1).__rxor__(-10) is NotImplemented  # type: ignore[operator]


def test_uint_bitwise_ixor_successful() -> None:
    assert Uint(0).__ixor__(Uint(0)) == Uint(0)
    assert Uint(2**256 - 1).__ixor__(Uint(0)) == Uint(2**256 - 1)
    assert Uint(2**256 - 1).__ixor__(Uint(2**256 - 1)) == Uint(0)
    assert Uint(2**256 - 1).__ixor__(Uint(17)) == Uint(2**256 - 1) - Uint(17)
    assert Uint(17).__ixor__(Uint(18)) == Uint(3)


def test_uint_bitwise_ixor_failed() -> None:
    assert Uint(0).__ixor__(2**256) is NotImplemented  # type: ignore[arg-type]
    assert Uint(2**256 - 1).__ixor__(2**256) is NotImplemented  # type: ignore[arg-type]
    assert Uint(2**256 - 1).__ixor__(-10) is NotImplemented  # type: ignore[arg-type]


def test_uint_repr() -> None:
    assert repr(Uint(1)) == "Uint(1)"


def test_uint_str() -> None:
    assert str(Uint(1)) == "1"
