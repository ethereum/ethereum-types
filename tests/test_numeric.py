from math import ceil, floor, trunc
from typing import Type, TypeAlias, Union

import pytest

from ethereum_types.bytes import Bytes4, Bytes8, Bytes64
from ethereum_types.numeric import (
    U32,
    U64,
    U256,
    FixedUnsigned,
    Uint,
    Unsigned,
    ulen,
)

FIXED_TYPES = (U256, U64, U32)

UNSIGNED_MARKS = (pytest.mark.unsigned,)
FIXED_MARKS = (pytest.mark.fixed,) + UNSIGNED_MARKS
UINT_MARKS = (pytest.mark.arbitrary,) + UNSIGNED_MARKS

FIXED = tuple(pytest.param(x, marks=FIXED_MARKS) for x in FIXED_TYPES)
UNSIGNED = (pytest.param(Uint, marks=UINT_MARKS),) + FIXED

FromBytesType: TypeAlias = Union[Type[Uint], Type[FixedUnsigned]]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_new(Class: Type[Unsigned]) -> None:
    value = Class(5)
    assert not isinstance(value, int)  # type: ignore[unreachable]
    assert isinstance(value, Class)
    assert isinstance(value, Unsigned)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_new_negative(Class: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(-5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_new_float(Class: Type[Unsigned]) -> None:
    assert Class(0.1) == Class(0)
    assert Class(1.0) == Class(1)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_new_max_value(Class: Type[FixedUnsigned]) -> None:
    value = Class(Class.MAX_VALUE._number)
    assert isinstance(value, Class)
    assert isinstance(value, FixedUnsigned)
    assert value == Class.MAX_VALUE


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_new_too_large(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(Class.MAX_VALUE._number + 1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_radd(Class: Type[Unsigned]) -> None:
    assert Class(9) == Class(4).__radd__(Class(5))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_radd_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        4 + Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_radd_overflow(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(Class.MAX_VALUE).__radd__(Class(5))
    with pytest.raises(OverflowError):
        Class(5).__radd__(Class(Class.MAX_VALUE))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_radd_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (1.0) + Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_add(Class: Type[Unsigned]) -> None:
    actual = Class(5) + Class(4)
    assert Class(9) == actual
    assert isinstance(actual, Class)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_add_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) + 4  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_add_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) + (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_add_overflow(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(5) + Class.MAX_VALUE


@pytest.mark.parametrize("Class", UNSIGNED)
def test_iadd(Class: Type[Unsigned]) -> None:
    value = Class(5)
    value2 = value
    value += Class(4)
    assert isinstance(value, Class)
    assert value == Class(9)
    assert value2 == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_iadd_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value += 4  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_iadd_float(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value += 1.0  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_iadd_overflow(Class: Type[FixedUnsigned]) -> None:
    value = Class(5)
    with pytest.raises(OverflowError):
        value += Class.MAX_VALUE
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rsub(Class: Type[Unsigned]) -> None:
    actual = Class(5).__rsub__(Class(6))
    assert isinstance(actual, Class)
    assert Class(1) == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rsub_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        6 - Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rsub_underflow(Class: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(7).__rsub__(Class(6))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rsub_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (6.0) - Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_sub(Class: Type[Unsigned]) -> None:
    actual = Class(5) - Class(4)
    assert Class(1) == actual
    assert isinstance(actual, Class)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_sub_underflow(Class: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(5) - Class(6)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_sub_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) - (-4)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_sub_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) - (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_isub(Class: Type[Unsigned]) -> None:
    value = Class(5)
    value2 = value
    value -= Class(4)
    assert isinstance(value, Class)
    assert value == Class(1)
    assert value2 == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_isub_underflow(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(OverflowError):
        value -= Class(6)
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_isub_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value -= -4  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_isub_float(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value -= 1.0  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmul(Class: Type[Unsigned]) -> None:
    actual = Class(4).__rmul__(Class(5))
    assert actual == Class(20)
    assert isinstance(actual, Class)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmul_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        4 * Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmul_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (1.0) * Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_rmul_overflow(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class.MAX_VALUE.__rmul__(Class(5))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mul(Class: Type[Unsigned]) -> None:
    value = Class(5) * Class(4)
    assert isinstance(value, Class)
    assert value == Class(20)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mul_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value * 4  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mul_float(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value * (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_mul_overflow(Class: Type[FixedUnsigned]) -> None:
    four = Class(4)
    with pytest.raises(OverflowError):
        Class.MAX_VALUE * four


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imul(Class: Type[Unsigned]) -> None:
    value = Class(5)
    value2 = value
    value *= Class(4)
    assert isinstance(value, Class)
    assert value == Class(20)
    assert value2 == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imul_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value *= 4  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imul_float(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value *= 1.0  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_imul_overflow(Class: Type[FixedUnsigned]) -> None:
    value = Class(5)
    with pytest.raises(OverflowError):
        value *= Class.MAX_VALUE
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_floordiv(Class: Type[Unsigned]) -> None:
    value = Class(5) // Class(2)
    assert isinstance(value, Class)
    assert value == Class(2)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_floordiv_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) // 2  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_floordiv_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) // 2.0  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rfloordiv(Class: Type[Unsigned]) -> None:
    value = Class(2).__rfloordiv__(Class(5))
    assert isinstance(value, Class)
    assert value == Class(2)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rfloordiv_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-2) // Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rfloordiv_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        5.0 // Class(2)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ifloordiv(Class: Type[Unsigned]) -> None:
    value = Class(5)
    value2 = value
    value //= Class(2)
    assert isinstance(value, Class)
    assert value == Class(2)
    assert value2 == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ifloordiv_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value //= -2  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmod(Class: Type[Unsigned]) -> None:
    value = Class(2).__rmod__(Class(5))
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmod_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-4) % Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rmod_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (6.0) % Class(5)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mod(Class: Type[Unsigned]) -> None:
    value = Class(5) % Class(4)
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mod_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) % (-4)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_mod_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5) % (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imod(Class: Type[Unsigned]) -> None:
    value = Class(5)
    value2 = value
    value %= Class(4)
    assert isinstance(value, Class)
    assert value == Class(1)
    assert value2 == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imod_int(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value %= -4  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_imod_float(Class: Type[Unsigned]) -> None:
    value = Class(5)
    with pytest.raises(TypeError):
        value %= 1.0  # type: ignore[arg-type]
    assert value == Class(5)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_divmod(Class: Type[Unsigned]) -> None:
    quotient, remainder = divmod(Class(5), Class(2))
    assert isinstance(quotient, Class)
    assert isinstance(remainder, Class)
    assert quotient == Class(2)
    assert remainder == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_divmod_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(Class(5), -2)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_divmod_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(Class(5), 2.0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rdivmod(Class: Type[Unsigned]) -> None:
    quotient, remainder = Class(2).__rdivmod__(Class(5))
    assert quotient == Class(2)
    assert remainder == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rdivmod_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(5, Class(2))  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rdivmod_float(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(5.0, Class(2))  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_pow(Class: Type[Unsigned]) -> None:
    value = Class(3) ** Class(2)
    assert isinstance(value, Class)
    assert value == Class(9)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_pow_overflow(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class(340282366920938463463374607431768211456) ** Class(3)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_pow_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(3) ** -2  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_pow_modulo(Class: Type[Unsigned]) -> None:
    value = pow(Class(4), Class(2), Class(3))
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_pow_modulo_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        pow(Class(4), Class(2), -3)  # type: ignore[misc]

    with pytest.raises(TypeError):
        pow(Class(4), 2, Class(3))  # type: ignore[misc]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rpow(Class: Type[Unsigned]) -> None:
    value = Class(2).__rpow__(Class(3))
    assert isinstance(value, Class)
    assert value == Class(9)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rpow_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-3) ** Class(2)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rpow_modulo(Class: Type[Unsigned]) -> None:
    value = Class.__rpow__(Class(2), Class(4), Class(3))
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_rpow_overflow(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        Class.MAX_VALUE ** Class(2)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rpow_modulo_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class.__rpow__(Class(2), Class(4), -3)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ipow(Class: Type[Unsigned]) -> None:
    value = Class(3)
    value2 = value
    value **= Class(2)
    assert isinstance(value, Class)
    assert value == Class(9)
    assert value2 == Class(3)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ipow_int(Class: Type[Unsigned]) -> None:
    value = Class(3)
    with pytest.raises(TypeError):
        value **= -2  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_ipow_overflow(Class: Type[FixedUnsigned]) -> None:
    value = Class.MAX_VALUE
    with pytest.raises(OverflowError):
        value **= Class(3)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ipow_modulo(Class: Type[Unsigned]) -> None:
    value = Class(4).__ipow__(Class(2), Class(3))
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ipow_modulo_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(4).__ipow__(Class(2), -3)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bit_length(Class: Type[Unsigned]) -> None:
    assert Class(3).bit_length() == Class(2)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_bytes_zero(Class: Type[Unsigned]) -> None:
    encoded = Class(0).to_bytes()
    assert encoded == bytes([0])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_bytes(Class: Type[Unsigned]) -> None:
    encoded = Class(1).to_bytes(length=Uint(5))
    assert encoded == bytes([0, 0, 0, 0, 1])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes_zero(Class: Type[Unsigned]) -> None:
    encoded = Class(0).to_be_bytes()
    assert encoded == bytes([])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes_one(Class: Type[Unsigned]) -> None:
    encoded = Class(1).to_be_bytes()
    assert encoded == bytes([1])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes_is_big_endian(Class: Type[Unsigned]) -> None:
    encoded = Class(0xABCD).to_be_bytes()
    assert encoded == bytes([0xAB, 0xCD])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes64_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_be_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes64_one(Class: Type[Unsigned]) -> None:
    actual = Class(1).to_be_bytes64()
    expected = Bytes64([0] * 63 + [1])
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes64_max_value() -> None:
    actual = Uint(2**512 - 1).to_be_bytes64()
    expected = Bytes64([0xFF] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes64_too_big() -> None:
    value = Uint(2**512)

    with pytest.raises(OverflowError):
        value.to_be_bytes64()


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes32_zero(Class: Type[Unsigned]) -> None:
    encoded = Class(0).to_be_bytes32()
    assert encoded == bytes([0] * 32)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes32_one(Class: Type[Unsigned]) -> None:
    encoded = Class(1).to_be_bytes32()
    assert encoded == bytes([0] * 31 + [1])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes32_max_value() -> None:
    encoded = Uint(2**256 - 1).to_be_bytes32()
    assert encoded == bytes([0xFF] * 32)


@pytest.mark.unsigned
@pytest.mark.fixed
def test_u256_to_be_bytes32_max_value() -> None:
    encoded = U256(2**256 - 1).to_be_bytes32()
    assert encoded == bytes([0xFF] * 32)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_be_bytes_empty(Class: FromBytesType) -> None:
    value = Class.from_be_bytes(b"")
    assert value == Class(0)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_be_bytes_one(Class: FromBytesType) -> None:
    value = Class.from_be_bytes(bytes([1]))
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_be_bytes_is_big_endian(Class: FromBytesType) -> None:
    value = Class.from_be_bytes(bytes([0xAB, 0xCD]))
    assert value == Class(0xABCD)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_le_bytes_empty(Class: FromBytesType) -> None:
    value = Class.from_le_bytes(b"")
    assert value == Class(0)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_le_bytes_one(Class: FromBytesType) -> None:
    value = Class.from_le_bytes(bytes([1]))
    assert value == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_from_le_bytes_is_little_endian(Class: FromBytesType) -> None:
    value = Class.from_le_bytes(bytes([0xAB, 0xCD]))
    assert value == Class(0xCDAB)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_abs(Class: Type[Unsigned]) -> None:
    assert abs(Class(1)) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_abs_same(Class: Type[Unsigned]) -> None:
    a = Class(1)
    b = abs(a)
    a += Class(1)
    assert a == Class(2)
    assert b == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_trunc(Class: Type[Unsigned]) -> None:
    assert trunc(Class(1)) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_trunc_same(Class: Type[Unsigned]) -> None:
    a = Class(1)
    b = trunc(a)
    a += Class(1)
    assert a == Class(2)
    assert b == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_round(Class: Type[Unsigned]) -> None:
    assert round(Class(1)) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_round_same(Class: Type[Unsigned]) -> None:
    a = Class(1)
    b = round(a)
    a += Class(1)
    assert a == Class(2)
    assert b == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_truediv_int(Class: Type[Unsigned]) -> None:
    assert Class(1).__truediv__(2) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rtruediv_int(Class: Type[Unsigned]) -> None:
    assert Class(1).__rtruediv__(2) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_truediv(Class: Type[Unsigned]) -> None:
    expected = (1).__truediv__(2)
    actual = Class(1).__truediv__(Class(2))
    assert expected == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rtruediv(Class: Type[Unsigned]) -> None:
    expected = (1).__rtruediv__(2)
    actual = Class(1).__rtruediv__(Class(2))
    assert expected == actual


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_add(Class: Type[FixedUnsigned]) -> None:
    value = Class(5).wrapping_add(Class(4))
    assert isinstance(value, Class)
    assert value == 9


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_add_overflow(Class: Type[FixedUnsigned]) -> None:
    value = Class(5).wrapping_add(Class.MAX_VALUE)
    assert isinstance(value, Class)
    assert value == 4


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_add_int(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5).wrapping_add(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_sub(Class: Type[FixedUnsigned]) -> None:
    value = Class(5).wrapping_sub(Class(4))
    assert isinstance(value, Class)
    assert value == 1


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_sub_underflow(Class: Type[FixedUnsigned]) -> None:
    value = Class(5).wrapping_sub(Class(6))
    assert isinstance(value, Class)
    assert value == Class.MAX_VALUE


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_sub_int(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5).wrapping_sub(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_mul(Class: Type[FixedUnsigned]) -> None:
    value = Class(5).wrapping_mul(Class(4))
    assert isinstance(value, Class)
    assert value == Class(20)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_mul_overflow(Class: Type[FixedUnsigned]) -> None:
    value = Class.MAX_VALUE.wrapping_mul(Class(4))
    expected = Class.MAX_VALUE - Class(3)
    assert isinstance(value, Class)
    assert value == expected


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_mul_int(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        Class(5).wrapping_mul(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_pow(Class: Type[FixedUnsigned]) -> None:
    value = Class(3).wrapping_pow(Class(2))
    assert isinstance(value, Class)
    assert value == Class(9)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_pow_overflow(Class: Type[FixedUnsigned]) -> None:
    base = (Class.MAX_VALUE // Class(3)) - Class(5)
    actual = base.wrapping_pow(Class(3))
    expected = pow(int(base), 3, int(Class.MAX_VALUE) + 1)
    assert isinstance(actual, Class)
    assert expected == actual


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_pow_int(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        Class(3).wrapping_pow(-2)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_pow_modulo(Class: Type[FixedUnsigned]) -> None:
    value = Class(4).wrapping_pow(Class(2), Class(3))
    assert isinstance(value, Class)
    assert value == Class(1)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_wrapping_pow_modulo_int(Class: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        Class(4).wrapping_pow(Class(2), 2)  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_be_bytes_too_large(Class: Type[FixedUnsigned]) -> None:
    bits = Class.MAX_VALUE._number.bit_length()
    byte_count = (bits + 7) // 8
    byte_count += 1

    with pytest.raises(ValueError):
        Class.from_be_bytes(bytes([0xFF] * byte_count))


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_le_bytes_too_large(Class: Type[FixedUnsigned]) -> None:
    bits = Class.MAX_VALUE._number.bit_length()
    byte_count = (bits + 7) // 8
    byte_count += 1

    with pytest.raises(ValueError):
        Class.from_le_bytes(bytes([0xFF] * byte_count))


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_invert(Class: Type[FixedUnsigned]) -> None:
    assert ~Class(0) == Class.MAX_VALUE
    assert ~Class(10) == Class.MAX_VALUE - Class(10)
    assert ~Class.MAX_VALUE == 0


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_rshift(Class: Type[FixedUnsigned]) -> None:
    bits = Class(Class.MAX_VALUE.bit_length())

    assert Class.MAX_VALUE >> (bits - Class(1)) == Class(1)
    assert Class.MAX_VALUE >> Class(bits) == Class(0)
    assert Class.MAX_VALUE >> Class(bits + Class(1)) == Class(0)
    assert Class(0) >> Class(20) == Class(0)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes_zero(Class: Type[Unsigned]) -> None:
    encoded = Class(0).to_le_bytes()
    assert encoded == bytes([])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes_one(Class: Type[Unsigned]) -> None:
    encoded = Class(1).to_le_bytes()
    assert encoded == bytes([1])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes_is_little_endian(Class: Type[Unsigned]) -> None:
    encoded = Class(0xABCD).to_le_bytes()
    assert encoded == bytes([0xCD, 0xAB])


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes64_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_le_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes64_one(Class: Type[Unsigned]) -> None:
    actual = Class(1).to_le_bytes64()
    expected = Bytes64([1] + [0] * 63)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes64_max_value() -> None:
    actual = Uint(2**512 - 1).to_le_bytes64()
    expected = Bytes64([0xFF] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes64_too_big() -> None:
    value = Uint(2**512)

    with pytest.raises(OverflowError):
        value.to_le_bytes64()


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes32_zero(Class: Type[Unsigned]) -> None:
    encoded = Class(0).to_le_bytes32()
    assert encoded == bytes([0] * 32)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes32_one(Class: Type[Unsigned]) -> None:
    encoded = Class(1).to_le_bytes32()
    assert encoded == bytes([1] + ([0] * 31))


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes32_max_value() -> None:
    encoded = Uint(2**256 - 1).to_le_bytes32()
    assert encoded == bytes([0xFF] * 32)


@pytest.mark.unsigned
@pytest.mark.fixed
def test_u256_to_le_bytes32_max_value() -> None:
    encoded = U256(2**256 - 1).to_le_bytes32()
    assert encoded == bytes([0xFF] * 32)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_neg(Class: Type[Unsigned]) -> None:
    result = -Class(1)
    assert not isinstance(result, Unsigned)  # type: ignore[unreachable]
    assert result == -1


@pytest.mark.parametrize("Class", UNSIGNED)
def test_pos(Class: Type[Unsigned]) -> None:
    assert Class(1) == +Class(1)


@pytest.mark.unsigned
@pytest.mark.fixed
def test_uint_invert() -> None:
    with pytest.raises(NotImplementedError):
        ~Uint(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_int(Class: Type[Unsigned]) -> None:
    assert int(Class(0xF000)) == Class(0xF000)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_floor_same(Class: Type[Unsigned]) -> None:
    a = Class(1)
    b = floor(a)
    a += Class(1)
    assert a == Class(2)
    assert b == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_floor(Class: Type[Unsigned]) -> None:
    assert floor(Class(1)) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ceil_same(Class: Type[Unsigned]) -> None:
    a = Class(1)
    b = ceil(a)
    a += Class(1)
    assert a == Class(2)
    assert b == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ceil(Class: Type[Unsigned]) -> None:
    assert ceil(Class(1)) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ne(Class: Type[Unsigned]) -> None:
    assert Class(1) != Class(2)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ne_not(Class: Type[Unsigned]) -> None:
    assert not (Class(1) != Class(1))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_le(Class: Type[Unsigned]) -> None:
    assert Class(1) <= Class(1)
    assert Class(0) <= Class(1)
    assert not (Class(2) <= Class(1))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_le_different_types(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(1) <= 1  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ge(Class: Type[Unsigned]) -> None:
    assert Class(1) >= Class(1)
    assert Class(2) >= Class(1)
    assert not (Class(1) >= Class(2))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_ge_different_types(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(1) >= 1  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("Class", UNSIGNED)
def test_lt(Class: Type[Unsigned]) -> None:
    assert not (Class(1) < Class(1))
    assert Class(0) < Class(1)
    assert not (Class(2) < Class(1))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_lt_different_types(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(1) < 1  # type: ignore[operator] # noqa: B015
    with pytest.raises(TypeError):
        1 < Class(1)  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("Class", UNSIGNED)
def test_gt(Class: Type[Unsigned]) -> None:
    assert not (Class(1) > Class(1))
    assert Class(2) > Class(1)
    assert not (Class(1) > Class(2))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_gt_different_types(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(1) > 1  # type: ignore[operator] # noqa: B015
    with pytest.raises(TypeError):
        1 > Class(1)  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("Class", UNSIGNED)
def test_lshift_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(3) << 4  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_lshift(Class: Type[Unsigned]) -> None:
    expected = Class(3 << 4)
    actual = Class(3) << Class(4)
    assert expected == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rlshift_int(Class: Type[Unsigned]) -> None:
    assert Class(3).__rlshift__(4) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rlshift(Class: Type[Unsigned]) -> None:
    expected = Class((3).__rlshift__(4))
    actual = Class(3).__rlshift__(Class(4))
    assert expected == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rshift(Class: Type[Unsigned]) -> None:
    expected = Class(3 >> 4)
    actual = Class(3) >> Class(4)
    assert expected == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rshift_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(3) >> 4  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rrshift(Class: Type[Unsigned]) -> None:
    expected = Class((3).__rrshift__(4))
    actual = Class(3).__rrshift__(Class(4))
    assert expected == actual


@pytest.mark.parametrize("Class", UNSIGNED)
def test_rrshift_int(Class: Type[Unsigned]) -> None:
    assert Class(3).__rrshift__(4) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq_str(Class: Type[Unsigned]) -> None:
    assert Class(1).__eq__("hello") is NotImplemented


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq_float(Class: Type[Unsigned]) -> None:
    assert Class(1) == 1.0
    assert 1.0 == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq_int(Class: Type[Unsigned]) -> None:
    assert Class(1) == 1
    assert 1 == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq(Class: Type[Unsigned]) -> None:
    assert Class(1) == Class(1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq_not_float(Class: Type[Unsigned]) -> None:
    assert not (Class(1) == 1.1)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_eq_not(Class: Type[Unsigned]) -> None:
    assert not (Class(1) == Class(2))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_hash(Class: Type[Unsigned]) -> None:
    assert hash(Class(1)) == hash(Class(1))
    assert hash(Class(1)) != hash(Class(2))


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_rand(Class: Type[Unsigned]) -> None:
    assert Class(0).__rand__(Class(0)) == Class(0)
    assert Class(2**32 - 1).__rand__(Class(2**32 - 1)) == Class(2**32 - 1)
    assert Class(2**32 - 1).__rand__(Class(0)) == Class(0)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_rand(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE.__rand__(Class.MAX_VALUE) == Class(Class.MAX_VALUE)
    assert Class.MAX_VALUE.__rand__(Class(0)) == Class(0)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_rand_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (2**256) & Class(0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_and(Class: Type[Unsigned]) -> None:
    assert Class(0) & Class(0) == Class(0)
    assert Class(2**32 - 1) & Class(2**32 - 1) == Class(2**32 - 1)
    assert Class(2**32 - 1) & Class(0) == Class(0)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_and(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE & Class.MAX_VALUE == Class.MAX_VALUE
    assert Class.MAX_VALUE.__rand__(Class(0)) == Class(0)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_and_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(0) & (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_ror(Class: Type[Unsigned]) -> None:
    assert Class(0).__ror__(Class(0)) == Class(0)
    assert Class(2**32 - 1).__ror__(Class(0)) == Class(2**32 - 1)
    assert Class(2**32 - 1).__ror__(Class(2**32 - 1)) == Class(2**32 - 1)
    assert Class(2**32 - 1).__ror__(Class(17)) == Class(2**32 - 1)
    assert Class(17).__ror__(Class(18)) == Class(19)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_ror(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE.__ror__(Class(0)) == Class.MAX_VALUE
    assert Class.MAX_VALUE.__ror__(Class.MAX_VALUE) == Class.MAX_VALUE
    assert Class.MAX_VALUE.__ror__(Class(17)) == Class.MAX_VALUE


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_ror_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (2**256) | Class(0)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_or(Class: Type[Unsigned]) -> None:
    assert Class(0) | Class(0) == Class(0)
    assert Class(2**32 - 1) | Class(0) == Class(2**32 - 1)
    assert Class(2**32 - 1) | Class(2**32 - 1) == Class(2**32 - 1)
    assert Class(2**32 - 1) | Class(17) == Class(2**32 - 1)
    assert Class(17) | Class(18) == Class(19)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_or(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE | Class(0) == Class.MAX_VALUE
    assert Class.MAX_VALUE | Class.MAX_VALUE == Class.MAX_VALUE
    assert Class.MAX_VALUE | Class(17) == Class.MAX_VALUE


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_or_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(0) | (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_xor(Class: Type[Unsigned]) -> None:
    assert Class(0) ^ Class(0) == Class(0)
    assert Class(2**32 - 1) ^ Class(0) == Class(2**32 - 1)
    assert Class(2**32 - 1) ^ Class(2**32 - 1) == Class(0)
    assert Class(2**32 - 1) ^ Class(17) == Class(2**32 - 1) - Class(17)
    assert Class(17) ^ Class(18) == Class(3)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_xor(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE ^ Class(0) == Class.MAX_VALUE
    assert Class.MAX_VALUE ^ Class.MAX_VALUE == Class(0)
    assert Class.MAX_VALUE ^ Class(17) == Class.MAX_VALUE - Class(17)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_xor_int(Class: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        Class(0) ^ (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_rxor(Class: Type[Unsigned]) -> None:
    assert Class(0).__rxor__(Class(0)) == Class(0)
    assert Class(2**32 - 1).__rxor__(Class(0)) == Class(2**32 - 1)
    assert Class(2**32 - 1).__rxor__(Class(2**32 - 1)) == Class(0)
    assert Class(2**32 - 1).__rxor__(Class(17)) == Class(2**32 - 1) - Class(17)
    assert Class(17).__rxor__(Class(18)) == Class(3)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_rxor(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE.__rxor__(Class(0)) == Class.MAX_VALUE
    assert Class.MAX_VALUE.__rxor__(Class.MAX_VALUE) == Class(0)
    assert Class.MAX_VALUE.__rxor__(Class(17)) == Class.MAX_VALUE - Class(17)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_rxor_int(Class: Type[Unsigned]) -> None:
    assert Uint(0).__rxor__(2**256) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_ixor(Class: Type[Unsigned]) -> None:
    value = Class(1)
    value2 = value
    value ^= Class(1)
    assert value == Class(0)
    assert value2 == Class(1)

    assert Class(0).__ixor__(Class(0)) == Class(0)
    assert Class(2**32 - 1).__ixor__(Class(0)) == Class(2**32 - 1)
    assert Class(2**32 - 1).__ixor__(Class(2**32 - 1)) == Class(0)
    assert Class(2**32 - 1).__ixor__(Class(17)) == Class(2**32 - 1) - Class(17)
    assert Class(17).__ixor__(Class(18)) == Class(3)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_bitwise_ixor(Class: Type[FixedUnsigned]) -> None:
    assert Class.MAX_VALUE.__ixor__(Class(0)) == Class.MAX_VALUE
    assert Class.MAX_VALUE.__ixor__(Class.MAX_VALUE) == Class(0)
    assert Class.MAX_VALUE.__ixor__(Class(17)) == Class.MAX_VALUE - Class(17)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_bitwise_ixor_int(Class: Type[Unsigned]) -> None:
    assert Class(0).__ixor__(2**256) is NotImplemented  # type: ignore[arg-type]


@pytest.mark.parametrize("Class", UNSIGNED)
def test_repr(Class: Type[Unsigned]) -> None:
    assert repr(Class(1)) == f"{Class.__name__}(1)"


@pytest.mark.parametrize("Class", UNSIGNED)
def test_str(Class: Type[Unsigned]) -> None:
    assert str(Class(1)) == "1"


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes4_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_le_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0] * 4)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes4(Class: Type[Unsigned]) -> None:
    actual = Class(0x89ABCDEF).to_le_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes4_overflow() -> None:
    value = Uint(0x0189ABCDEF)
    with pytest.raises(OverflowError):
        value.to_le_bytes4()


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes4_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_be_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0] * 4)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes4(Class: Type[Unsigned]) -> None:
    actual = Class(0xEFCDAB89).to_be_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes4_overflow() -> None:
    value = Uint(0x0189ABCDEF)
    with pytest.raises(OverflowError):
        value.to_be_bytes4()


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes8_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_le_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0] * 8)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_le_bytes8(Class: Type[Unsigned]) -> None:
    actual = Class(0x0000000089ABCDEF).to_le_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89, 0x00, 0x00, 0x00, 0x00])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes8() -> None:
    actual = Uint(0x0123456789ABCDEF).to_le_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes8_overflow() -> None:
    value = Uint(0x010123456789ABCDEF)
    with pytest.raises(OverflowError):
        value.to_le_bytes8()


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes8_zero(Class: Type[Unsigned]) -> None:
    actual = Class(0).to_be_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0] * 8)


@pytest.mark.parametrize("Class", UNSIGNED)
def test_to_be_bytes8(Class: Type[Unsigned]) -> None:
    actual = Class(0x00000000EFCDAB89).to_be_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0x00, 0x00, 0x00, 0x00, 0xEF, 0xCD, 0xAB, 0x89])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes8() -> None:
    actual = Uint(0xEFCDAB8967452301).to_be_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes8_overflow() -> None:
    value = Uint(0x010123456789ABCDEF)
    with pytest.raises(OverflowError):
        value.to_be_bytes8()


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_to_signed(Class: Type[FixedUnsigned]) -> None:
    value = Class(1234567890).to_signed()
    assert isinstance(value, int)
    assert value == 1234567890


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_to_signed_max_value(Class: Type[FixedUnsigned]) -> None:
    value = Class.MAX_VALUE.to_signed()
    assert isinstance(value, int)
    assert value == -1


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_signed_to_signed(Class: Type[FixedUnsigned]) -> None:
    unsigned = Class.from_signed(-123456)
    assert isinstance(unsigned, Class)

    signed = unsigned.to_signed()
    assert isinstance(signed, int)
    assert signed == -123456


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_signed(Class: Type[FixedUnsigned]) -> None:
    unsigned = Class.from_signed(1)
    assert isinstance(unsigned, Class)
    assert unsigned == Class(1)


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_signed_negative(Class: Type[FixedUnsigned]) -> None:
    unsigned = Class.from_signed(-1)
    assert isinstance(unsigned, Class)
    assert unsigned == Class.MAX_VALUE


@pytest.mark.parametrize("Class", FIXED)
def test_fixed_from_signed_overflow(Class: Type[FixedUnsigned]) -> None:
    too_positive = int(Class.MAX_VALUE) // 2 + 1
    with pytest.raises(OverflowError):
        Class.from_signed(too_positive)

    too_negative = (-Class.MAX_VALUE // 2) - 1
    with pytest.raises(OverflowError):
        Class.from_signed(too_negative)


def test_unsigned_in_range() -> None:
    number = Uint(0)
    with pytest.raises(NotImplementedError):
        Unsigned._in_range(number, 0)


def test_ulen() -> None:
    actual = ulen([1, 2, 3])
    assert isinstance(actual, Uint)
    assert actual == Uint(3)
