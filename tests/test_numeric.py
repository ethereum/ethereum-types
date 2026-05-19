from math import ceil, floor, trunc
from typing import Type, TypeAlias, Union

import pytest

from ethereum_types.bytes import Bytes1, Bytes4, Bytes8, Bytes64
from ethereum_types.numeric import (
    U32,
    U64,
    U256,
    FixedUnsigned,
    Uint,
    Unsigned,
    ulen,
)

FIXED_TYPES = (U256, U64, U32)  # Just assume U8 works...

UNSIGNED_MARKS = (pytest.mark.unsigned,)
FIXED_MARKS = (pytest.mark.fixed, *UNSIGNED_MARKS)
UINT_MARKS = (pytest.mark.arbitrary, *UNSIGNED_MARKS)

FIXED = tuple(pytest.param(x, marks=FIXED_MARKS) for x in FIXED_TYPES)
UNSIGNED = (pytest.param(Uint, marks=UINT_MARKS), *FIXED)

FromBytesType: TypeAlias = Union[Type[Uint], Type[FixedUnsigned]]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_new(class_: Type[Unsigned]) -> None:
    value = class_(5)
    assert not isinstance(value, int)  # type: ignore[unreachable]
    assert isinstance(value, class_)
    assert isinstance(value, Unsigned)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_new_negative(class_: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(-5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_new_float(class_: Type[Unsigned]) -> None:
    assert class_(0.1) == class_(0)
    assert class_(1.0) == class_(1)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_new_max_value(class_: Type[FixedUnsigned]) -> None:
    value = class_(class_.MAX_VALUE._number)
    assert isinstance(value, class_)
    assert isinstance(value, FixedUnsigned)
    assert value == class_.MAX_VALUE


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_new_too_large(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(class_.MAX_VALUE._number + 1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_radd(class_: Type[Unsigned]) -> None:
    assert class_(9) == class_(4).__radd__(class_(5))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_radd_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        4 + class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_radd_overflow(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(class_.MAX_VALUE).__radd__(class_(5))
    with pytest.raises(OverflowError):
        class_(5).__radd__(class_(class_.MAX_VALUE))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_radd_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (1.0) + class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_add(class_: Type[Unsigned]) -> None:
    actual = class_(5) + class_(4)
    assert class_(9) == actual
    assert isinstance(actual, class_)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_add_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) + 4  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_add_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) + (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_add_overflow(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(5) + class_.MAX_VALUE


@pytest.mark.parametrize("class_", UNSIGNED)
def test_iadd(class_: Type[Unsigned]) -> None:
    value = class_(5)
    value2 = value
    value += class_(4)
    assert isinstance(value, class_)
    assert value == class_(9)
    assert value2 == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_iadd_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value += 4  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_iadd_float(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value += 1.0  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_iadd_overflow(class_: Type[FixedUnsigned]) -> None:
    value = class_(5)
    with pytest.raises(OverflowError):
        value += class_.MAX_VALUE
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rsub(class_: Type[Unsigned]) -> None:
    actual = class_(5).__rsub__(class_(6))
    assert isinstance(actual, class_)
    assert class_(1) == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rsub_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        6 - class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rsub_underflow(class_: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(7).__rsub__(class_(6))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rsub_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (6.0) - class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_sub(class_: Type[Unsigned]) -> None:
    actual = class_(5) - class_(4)
    assert class_(1) == actual
    assert isinstance(actual, class_)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_sub_underflow(class_: Type[Unsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(5) - class_(6)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_sub_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) - (-4)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_sub_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) - (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_isub(class_: Type[Unsigned]) -> None:
    value = class_(5)
    value2 = value
    value -= class_(4)
    assert isinstance(value, class_)
    assert value == class_(1)
    assert value2 == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_isub_underflow(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(OverflowError):
        value -= class_(6)
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_isub_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value -= -4  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_isub_float(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value -= 1.0  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmul(class_: Type[Unsigned]) -> None:
    actual = class_(4).__rmul__(class_(5))
    assert actual == class_(20)
    assert isinstance(actual, class_)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmul_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        4 * class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmul_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (1.0) * class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_rmul_overflow(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_.MAX_VALUE.__rmul__(class_(5))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mul(class_: Type[Unsigned]) -> None:
    value = class_(5) * class_(4)
    assert isinstance(value, class_)
    assert value == class_(20)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mul_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value * 4  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mul_float(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value * (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_mul_overflow(class_: Type[FixedUnsigned]) -> None:
    four = class_(4)
    with pytest.raises(OverflowError):
        class_.MAX_VALUE * four


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imul(class_: Type[Unsigned]) -> None:
    value = class_(5)
    value2 = value
    value *= class_(4)
    assert isinstance(value, class_)
    assert value == class_(20)
    assert value2 == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imul_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value *= 4  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imul_float(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value *= 1.0  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_imul_overflow(class_: Type[FixedUnsigned]) -> None:
    value = class_(5)
    with pytest.raises(OverflowError):
        value *= class_.MAX_VALUE
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_floordiv(class_: Type[Unsigned]) -> None:
    value = class_(5) // class_(2)
    assert isinstance(value, class_)
    assert value == class_(2)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_floordiv_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) // 2  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_floordiv_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) // 2.0  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rfloordiv(class_: Type[Unsigned]) -> None:
    value = class_(2).__rfloordiv__(class_(5))
    assert isinstance(value, class_)
    assert value == class_(2)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rfloordiv_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-2) // class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rfloordiv_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        5.0 // class_(2)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ifloordiv(class_: Type[Unsigned]) -> None:
    value = class_(5)
    value2 = value
    value //= class_(2)
    assert isinstance(value, class_)
    assert value == class_(2)
    assert value2 == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ifloordiv_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value //= -2  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmod(class_: Type[Unsigned]) -> None:
    value = class_(2).__rmod__(class_(5))
    assert isinstance(value, class_)
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmod_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-4) % class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rmod_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (6.0) % class_(5)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mod(class_: Type[Unsigned]) -> None:
    value = class_(5) % class_(4)
    assert isinstance(value, class_)
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mod_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) % (-4)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_mod_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5) % (1.0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imod(class_: Type[Unsigned]) -> None:
    value = class_(5)
    value2 = value
    value %= class_(4)
    assert isinstance(value, class_)
    assert value == class_(1)
    assert value2 == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imod_int(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value %= -4  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_imod_float(class_: Type[Unsigned]) -> None:
    value = class_(5)
    with pytest.raises(TypeError):
        value %= 1.0  # type: ignore[arg-type]
    assert value == class_(5)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_divmod(class_: Type[Unsigned]) -> None:
    quotient, remainder = divmod(class_(5), class_(2))
    assert isinstance(quotient, class_)
    assert isinstance(remainder, class_)
    assert quotient == class_(2)
    assert remainder == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_divmod_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(class_(5), -2)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_divmod_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(class_(5), 2.0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rdivmod(class_: Type[Unsigned]) -> None:
    quotient, remainder = class_(2).__rdivmod__(class_(5))
    assert quotient == class_(2)
    assert remainder == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rdivmod_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(5, class_(2))  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rdivmod_float(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        divmod(5.0, class_(2))  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_pow(class_: Type[Unsigned]) -> None:
    value = class_(3) ** class_(2)
    assert isinstance(value, class_)
    assert value == class_(9)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_pow_overflow(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_(340282366920938463463374607431768211456) ** class_(3)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_pow_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(3) ** -2  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_pow_modulo(class_: Type[Unsigned]) -> None:
    value = pow(class_(4), class_(2), class_(3))
    assert isinstance(value, class_)
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_pow_modulo_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        pow(class_(4), class_(2), -3)  # type: ignore[misc]

    with pytest.raises(TypeError):
        pow(class_(4), 2, class_(3))  # type: ignore[misc]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rpow(class_: Type[Unsigned]) -> None:
    value = class_(2).__rpow__(class_(3))
    assert isinstance(value, class_)
    assert value == class_(9)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rpow_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (-3) ** class_(2)  # type: ignore[operator]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_rpow_overflow(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(OverflowError):
        class_.MAX_VALUE ** class_(2)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ipow(class_: Type[Unsigned]) -> None:
    value = class_(3)
    value2 = value
    value **= class_(2)
    assert isinstance(value, class_)
    assert value == class_(9)
    assert value2 == class_(3)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ipow_int(class_: Type[Unsigned]) -> None:
    value = class_(3)
    with pytest.raises(TypeError):
        value **= -2  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_ipow_overflow(class_: Type[FixedUnsigned]) -> None:
    value = class_.MAX_VALUE
    with pytest.raises(OverflowError):
        value **= class_(3)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ipow_modulo(class_: Type[Unsigned]) -> None:
    value = class_(4).__ipow__(class_(2), class_(3))
    assert isinstance(value, class_)
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ipow_modulo_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(4).__ipow__(class_(2), -3)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bit_length(class_: Type[Unsigned]) -> None:
    assert class_(3).bit_length() == class_(2)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_bytes_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_bytes()
    assert encoded == bytes([0])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_bytes(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_bytes(length=Uint(5))
    assert encoded == bytes([0, 0, 0, 0, 1])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_bytes1_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_bytes1()
    assert encoded == bytes([0])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_bytes1_one(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_bytes1()
    assert encoded == bytes([1])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_bytes1_max_value() -> None:
    actual = Uint(2**8 - 1).to_bytes1()
    expected = Bytes1([0xFF])
    assert isinstance(actual, Bytes1)
    assert actual == expected


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_bytes1_too_big() -> None:
    value = Uint(2**8)

    with pytest.raises(OverflowError):
        value.to_bytes1()


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_be_bytes()
    assert encoded == bytes([])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes_one(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_be_bytes()
    assert encoded == bytes([1])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes_is_big_endian(class_: Type[Unsigned]) -> None:
    encoded = class_(0xABCD).to_be_bytes()
    assert encoded == bytes([0xAB, 0xCD])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes64_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_be_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes64_one(class_: Type[Unsigned]) -> None:
    actual = class_(1).to_be_bytes64()
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


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes32_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_be_bytes32()
    assert encoded == bytes([0] * 32)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes32_one(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_be_bytes32()
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


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_be_bytes_empty(class_: FromBytesType) -> None:
    value = class_.from_be_bytes(b"")
    assert value == class_(0)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_be_bytes_one(class_: FromBytesType) -> None:
    value = class_.from_be_bytes(bytes([1]))
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_be_bytes_is_big_endian(class_: FromBytesType) -> None:
    value = class_.from_be_bytes(bytes([0xAB, 0xCD]))
    assert value == class_(0xABCD)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_le_bytes_empty(class_: FromBytesType) -> None:
    value = class_.from_le_bytes(b"")
    assert value == class_(0)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_le_bytes_one(class_: FromBytesType) -> None:
    value = class_.from_le_bytes(bytes([1]))
    assert value == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_from_le_bytes_is_little_endian(class_: FromBytesType) -> None:
    value = class_.from_le_bytes(bytes([0xAB, 0xCD]))
    assert value == class_(0xCDAB)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_abs(class_: Type[Unsigned]) -> None:
    assert abs(class_(1)) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_abs_same(class_: Type[Unsigned]) -> None:
    a = class_(1)
    b = abs(a)
    a += class_(1)
    assert a == class_(2)
    assert b == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_trunc(class_: Type[Unsigned]) -> None:
    assert trunc(class_(1)) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_trunc_same(class_: Type[Unsigned]) -> None:
    a = class_(1)
    b = trunc(a)
    a += class_(1)
    assert a == class_(2)
    assert b == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_round(class_: Type[Unsigned]) -> None:
    assert round(class_(1)) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_round_same(class_: Type[Unsigned]) -> None:
    a = class_(1)
    b = round(a)
    a += class_(1)
    assert a == class_(2)
    assert b == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_truediv_int(class_: Type[Unsigned]) -> None:
    assert class_(1).__truediv__(2) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rtruediv_int(class_: Type[Unsigned]) -> None:
    assert class_(1).__rtruediv__(2) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_truediv(class_: Type[Unsigned]) -> None:
    expected = (1).__truediv__(2)
    actual = class_(1).__truediv__(class_(2))
    assert expected == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rtruediv(class_: Type[Unsigned]) -> None:
    expected = (1).__rtruediv__(2)
    actual = class_(1).__rtruediv__(class_(2))
    assert expected == actual


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_add(class_: Type[FixedUnsigned]) -> None:
    value = class_(5).wrapping_add(class_(4))
    assert isinstance(value, class_)
    assert value == 9


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_add_overflow(class_: Type[FixedUnsigned]) -> None:
    value = class_(5).wrapping_add(class_.MAX_VALUE)
    assert isinstance(value, class_)
    assert value == 4


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_add_int(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5).wrapping_add(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_sub(class_: Type[FixedUnsigned]) -> None:
    value = class_(5).wrapping_sub(class_(4))
    assert isinstance(value, class_)
    assert value == 1


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_sub_underflow(class_: Type[FixedUnsigned]) -> None:
    value = class_(5).wrapping_sub(class_(6))
    assert isinstance(value, class_)
    assert value == class_.MAX_VALUE


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_sub_int(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5).wrapping_sub(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_mul(class_: Type[FixedUnsigned]) -> None:
    value = class_(5).wrapping_mul(class_(4))
    assert isinstance(value, class_)
    assert value == class_(20)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_mul_overflow(class_: Type[FixedUnsigned]) -> None:
    value = class_.MAX_VALUE.wrapping_mul(class_(4))
    expected = class_.MAX_VALUE - class_(3)
    assert isinstance(value, class_)
    assert value == expected


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_mul_int(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        class_(5).wrapping_mul(-4)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_pow(class_: Type[FixedUnsigned]) -> None:
    value = class_(3).wrapping_pow(class_(2))
    assert isinstance(value, class_)
    assert value == class_(9)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_pow_overflow(class_: Type[FixedUnsigned]) -> None:
    base = (class_.MAX_VALUE // class_(3)) - class_(5)
    actual = base.wrapping_pow(class_(3))
    expected = pow(int(base), 3, int(class_.MAX_VALUE) + 1)
    assert isinstance(actual, class_)
    assert expected == actual


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_pow_int(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        class_(3).wrapping_pow(-2)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_pow_modulo(class_: Type[FixedUnsigned]) -> None:
    value = class_(4).wrapping_pow(class_(2), class_(3))
    assert isinstance(value, class_)
    assert value == class_(1)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_wrapping_pow_modulo_int(class_: Type[FixedUnsigned]) -> None:
    with pytest.raises(TypeError):
        class_(4).wrapping_pow(class_(2), 2)  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_be_bytes_too_large(class_: Type[FixedUnsigned]) -> None:
    bits = class_.MAX_VALUE._number.bit_length()
    byte_count = (bits + 7) // 8
    byte_count += 1

    match = f"expected at most {byte_count - 1} but got {byte_count}"
    with pytest.raises(ValueError, match=match):
        class_.from_be_bytes(bytes([0xFF] * byte_count))


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_le_bytes_too_large(class_: Type[FixedUnsigned]) -> None:
    bits = class_.MAX_VALUE._number.bit_length()
    byte_count = (bits + 7) // 8
    byte_count += 1

    match = f"expected at most {byte_count - 1} but got {byte_count}"
    with pytest.raises(ValueError, match=match):
        class_.from_le_bytes(bytes([0xFF] * byte_count))


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_invert(class_: Type[FixedUnsigned]) -> None:
    assert ~class_(0) == class_.MAX_VALUE
    assert ~class_(10) == class_.MAX_VALUE - class_(10)
    assert ~class_.MAX_VALUE == 0


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_rshift(class_: Type[FixedUnsigned]) -> None:
    bits = class_(class_.MAX_VALUE.bit_length())

    assert class_.MAX_VALUE >> (bits - class_(1)) == class_(1)
    assert class_.MAX_VALUE >> class_(bits) == class_(0)
    assert class_.MAX_VALUE >> class_(bits + class_(1)) == class_(0)
    assert class_(0) >> class_(20) == class_(0)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_le_bytes()
    assert encoded == bytes([])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes_one(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_le_bytes()
    assert encoded == bytes([1])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes_is_little_endian(class_: Type[Unsigned]) -> None:
    encoded = class_(0xABCD).to_le_bytes()
    assert encoded == bytes([0xCD, 0xAB])


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes64_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_le_bytes64()
    expected = Bytes64([0] * 64)
    assert isinstance(actual, Bytes64)
    assert actual == expected


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes64_one(class_: Type[Unsigned]) -> None:
    actual = class_(1).to_le_bytes64()
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


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes32_zero(class_: Type[Unsigned]) -> None:
    encoded = class_(0).to_le_bytes32()
    assert encoded == bytes([0] * 32)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes32_one(class_: Type[Unsigned]) -> None:
    encoded = class_(1).to_le_bytes32()
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


@pytest.mark.parametrize("class_", UNSIGNED)
def test_neg(class_: Type[Unsigned]) -> None:
    result = -class_(1)
    assert not isinstance(result, Unsigned)  # type: ignore[unreachable]
    assert result == -1


@pytest.mark.parametrize("class_", UNSIGNED)
def test_pos(class_: Type[Unsigned]) -> None:
    assert class_(1) == +class_(1)


@pytest.mark.unsigned
@pytest.mark.fixed
def test_uint_invert() -> None:
    with pytest.raises(NotImplementedError):
        ~Uint(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_int(class_: Type[Unsigned]) -> None:
    assert int(class_(0xF000)) == class_(0xF000)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_floor_same(class_: Type[Unsigned]) -> None:
    a = class_(1)
    b = floor(a)
    a += class_(1)
    assert a == class_(2)
    assert b == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_floor(class_: Type[Unsigned]) -> None:
    assert floor(class_(1)) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ceil_same(class_: Type[Unsigned]) -> None:
    a = class_(1)
    b = ceil(a)
    a += class_(1)
    assert a == class_(2)
    assert b == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ceil(class_: Type[Unsigned]) -> None:
    assert ceil(class_(1)) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ne(class_: Type[Unsigned]) -> None:
    assert class_(1) != class_(2)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ne_not(class_: Type[Unsigned]) -> None:
    assert not (class_(1) != class_(1))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_le(class_: Type[Unsigned]) -> None:
    assert class_(1) <= class_(1)
    assert class_(0) <= class_(1)
    assert not (class_(2) <= class_(1))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_le_different_types(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(1) <= 1  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ge(class_: Type[Unsigned]) -> None:
    assert class_(1) >= class_(1)
    assert class_(2) >= class_(1)
    assert not (class_(1) >= class_(2))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_ge_different_types(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(1) >= 1  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("class_", UNSIGNED)
def test_lt(class_: Type[Unsigned]) -> None:
    assert not (class_(1) < class_(1))
    assert class_(0) < class_(1)
    assert not (class_(2) < class_(1))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_lt_different_types(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(1) < 1  # type: ignore[operator] # noqa: B015
    with pytest.raises(TypeError):
        1 < class_(1)  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("class_", UNSIGNED)
def test_gt(class_: Type[Unsigned]) -> None:
    assert not (class_(1) > class_(1))
    assert class_(2) > class_(1)
    assert not (class_(1) > class_(2))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_gt_different_types(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(1) > 1  # type: ignore[operator] # noqa: B015
    with pytest.raises(TypeError):
        1 > class_(1)  # type: ignore[operator] # noqa: B015


@pytest.mark.parametrize("class_", UNSIGNED)
def test_lshift_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(3) << 4  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_lshift(class_: Type[Unsigned]) -> None:
    expected = class_(3 << 4)
    actual = class_(3) << class_(4)
    assert expected == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rlshift_int(class_: Type[Unsigned]) -> None:
    assert class_(3).__rlshift__(4) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rlshift(class_: Type[Unsigned]) -> None:
    expected = class_((3).__rlshift__(4))
    actual = class_(3).__rlshift__(class_(4))
    assert expected == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rshift(class_: Type[Unsigned]) -> None:
    expected = class_(3 >> 4)
    actual = class_(3) >> class_(4)
    assert expected == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rshift_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(3) >> 4  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rrshift(class_: Type[Unsigned]) -> None:
    expected = class_((3).__rrshift__(4))
    actual = class_(3).__rrshift__(class_(4))
    assert expected == actual


@pytest.mark.parametrize("class_", UNSIGNED)
def test_rrshift_int(class_: Type[Unsigned]) -> None:
    assert class_(3).__rrshift__(4) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq_str(class_: Type[Unsigned]) -> None:
    assert class_(1).__eq__("hello") is NotImplemented


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq_float(class_: Type[Unsigned]) -> None:
    assert class_(1) == 1.0
    assert 1.0 == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq_int(class_: Type[Unsigned]) -> None:
    assert class_(1) == 1
    assert 1 == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq(class_: Type[Unsigned]) -> None:
    assert class_(1) == class_(1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq_not_float(class_: Type[Unsigned]) -> None:
    assert not (class_(1) == 1.1)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_eq_not(class_: Type[Unsigned]) -> None:
    assert not (class_(1) == class_(2))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_hash(class_: Type[Unsigned]) -> None:
    assert hash(class_(1)) == hash(class_(1))
    assert hash(class_(1)) != hash(class_(2))


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_rand(class_: Type[Unsigned]) -> None:
    assert class_(0).__rand__(class_(0)) == class_(0)
    assert class_(2**32 - 1).__rand__(class_(2**32 - 1)) == class_(2**32 - 1)
    assert class_(2**32 - 1).__rand__(class_(0)) == class_(0)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_rand(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE.__rand__(class_.MAX_VALUE) == class_(
        class_.MAX_VALUE
    )
    assert class_.MAX_VALUE.__rand__(class_(0)) == class_(0)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_rand_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (2**256) & class_(0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_and(class_: Type[Unsigned]) -> None:
    assert class_(0) & class_(0) == class_(0)
    assert class_(2**32 - 1) & class_(2**32 - 1) == class_(2**32 - 1)
    assert class_(2**32 - 1) & class_(0) == class_(0)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_and(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE & class_.MAX_VALUE == class_.MAX_VALUE
    assert class_.MAX_VALUE.__rand__(class_(0)) == class_(0)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_and_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(0) & (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_ror(class_: Type[Unsigned]) -> None:
    assert class_(0).__ror__(class_(0)) == class_(0)
    assert class_(2**32 - 1).__ror__(class_(0)) == class_(2**32 - 1)
    assert class_(2**32 - 1).__ror__(class_(2**32 - 1)) == class_(2**32 - 1)
    assert class_(2**32 - 1).__ror__(class_(17)) == class_(2**32 - 1)
    assert class_(17).__ror__(class_(18)) == class_(19)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_ror(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE.__ror__(class_(0)) == class_.MAX_VALUE
    assert class_.MAX_VALUE.__ror__(class_.MAX_VALUE) == class_.MAX_VALUE
    assert class_.MAX_VALUE.__ror__(class_(17)) == class_.MAX_VALUE


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_ror_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        (2**256) | class_(0)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_or(class_: Type[Unsigned]) -> None:
    assert class_(0) | class_(0) == class_(0)
    assert class_(2**32 - 1) | class_(0) == class_(2**32 - 1)
    assert class_(2**32 - 1) | class_(2**32 - 1) == class_(2**32 - 1)
    assert class_(2**32 - 1) | class_(17) == class_(2**32 - 1)
    assert class_(17) | class_(18) == class_(19)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_or(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE | class_(0) == class_.MAX_VALUE
    assert class_.MAX_VALUE | class_.MAX_VALUE == class_.MAX_VALUE
    assert class_.MAX_VALUE | class_(17) == class_.MAX_VALUE


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_or_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(0) | (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_xor(class_: Type[Unsigned]) -> None:
    assert class_(0) ^ class_(0) == class_(0)
    assert class_(2**32 - 1) ^ class_(0) == class_(2**32 - 1)
    assert class_(2**32 - 1) ^ class_(2**32 - 1) == class_(0)
    assert class_(2**32 - 1) ^ class_(17) == class_(2**32 - 1) - class_(17)
    assert class_(17) ^ class_(18) == class_(3)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_xor(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE ^ class_(0) == class_.MAX_VALUE
    assert class_.MAX_VALUE ^ class_.MAX_VALUE == class_(0)
    assert class_.MAX_VALUE ^ class_(17) == class_.MAX_VALUE - class_(17)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_xor_int(class_: Type[Unsigned]) -> None:
    with pytest.raises(TypeError):
        class_(0) ^ (2**256)  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_rxor(class_: Type[Unsigned]) -> None:
    assert class_(0).__rxor__(class_(0)) == class_(0)
    assert class_(2**32 - 1).__rxor__(class_(0)) == class_(2**32 - 1)
    assert class_(2**32 - 1).__rxor__(class_(2**32 - 1)) == class_(0)
    assert class_(2**32 - 1).__rxor__(class_(17)) == class_(
        2**32 - 1
    ) - class_(17)
    assert class_(17).__rxor__(class_(18)) == class_(3)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_rxor(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE.__rxor__(class_(0)) == class_.MAX_VALUE
    assert class_.MAX_VALUE.__rxor__(class_.MAX_VALUE) == class_(0)
    assert class_.MAX_VALUE.__rxor__(class_(17)) == class_.MAX_VALUE - class_(
        17
    )


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_rxor_int(class_: Type[Unsigned]) -> None:
    assert class_(0).__rxor__(2**256) is NotImplemented  # type: ignore[operator]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_ixor(class_: Type[Unsigned]) -> None:
    value = class_(1)
    value2 = value
    value ^= class_(1)
    assert value == class_(0)
    assert value2 == class_(1)

    assert class_(0).__ixor__(class_(0)) == class_(0)
    assert class_(2**32 - 1).__ixor__(class_(0)) == class_(2**32 - 1)
    assert class_(2**32 - 1).__ixor__(class_(2**32 - 1)) == class_(0)
    assert class_(2**32 - 1).__ixor__(class_(17)) == class_(
        2**32 - 1
    ) - class_(17)
    assert class_(17).__ixor__(class_(18)) == class_(3)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_bitwise_ixor(class_: Type[FixedUnsigned]) -> None:
    assert class_.MAX_VALUE.__ixor__(class_(0)) == class_.MAX_VALUE
    assert class_.MAX_VALUE.__ixor__(class_.MAX_VALUE) == class_(0)
    assert class_.MAX_VALUE.__ixor__(class_(17)) == class_.MAX_VALUE - class_(
        17
    )


@pytest.mark.parametrize("class_", UNSIGNED)
def test_bitwise_ixor_int(class_: Type[Unsigned]) -> None:
    assert class_(0).__ixor__(2**256) is NotImplemented  # type: ignore[arg-type]


@pytest.mark.parametrize("class_", UNSIGNED)
def test_repr(class_: Type[Unsigned]) -> None:
    assert repr(class_(1)) == f"{class_.__name__}(1)"


@pytest.mark.parametrize("class_", UNSIGNED)
def test_str(class_: Type[Unsigned]) -> None:
    assert str(class_(1)) == "1"


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes4_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_le_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0] * 4)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes4(class_: Type[Unsigned]) -> None:
    actual = class_(0x89ABCDEF).to_le_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_le_bytes4_overflow() -> None:
    value = Uint(0x0189ABCDEF)
    with pytest.raises(OverflowError):
        value.to_le_bytes4()


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes4_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_be_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0] * 4)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes4(class_: Type[Unsigned]) -> None:
    actual = class_(0xEFCDAB89).to_be_bytes4()
    assert isinstance(actual, Bytes4)
    assert actual == bytes([0xEF, 0xCD, 0xAB, 0x89])


@pytest.mark.unsigned
@pytest.mark.arbitrary
def test_uint_to_be_bytes4_overflow() -> None:
    value = Uint(0x0189ABCDEF)
    with pytest.raises(OverflowError):
        value.to_be_bytes4()


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes8_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_le_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0] * 8)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_le_bytes8(class_: Type[Unsigned]) -> None:
    actual = class_(0x0000000089ABCDEF).to_le_bytes8()
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


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes8_zero(class_: Type[Unsigned]) -> None:
    actual = class_(0).to_be_bytes8()
    assert isinstance(actual, Bytes8)
    assert actual == bytes([0] * 8)


@pytest.mark.parametrize("class_", UNSIGNED)
def test_to_be_bytes8(class_: Type[Unsigned]) -> None:
    actual = class_(0x00000000EFCDAB89).to_be_bytes8()
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


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_to_signed(class_: Type[FixedUnsigned]) -> None:
    value = class_(1234567890).to_signed()
    assert isinstance(value, int)
    assert value == 1234567890


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_to_signed_max_value(class_: Type[FixedUnsigned]) -> None:
    value = class_.MAX_VALUE.to_signed()
    assert isinstance(value, int)
    assert value == -1


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_signed_to_signed(class_: Type[FixedUnsigned]) -> None:
    unsigned = class_.from_signed(-123456)
    assert isinstance(unsigned, class_)

    signed = unsigned.to_signed()
    assert isinstance(signed, int)
    assert signed == -123456


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_signed(class_: Type[FixedUnsigned]) -> None:
    unsigned = class_.from_signed(1)
    assert isinstance(unsigned, class_)
    assert unsigned == class_(1)


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_signed_negative(class_: Type[FixedUnsigned]) -> None:
    unsigned = class_.from_signed(-1)
    assert isinstance(unsigned, class_)
    assert unsigned == class_.MAX_VALUE


@pytest.mark.parametrize("class_", FIXED)
def test_fixed_from_signed_overflow(class_: Type[FixedUnsigned]) -> None:
    too_positive = int(class_.MAX_VALUE) // 2 + 1
    with pytest.raises(OverflowError):
        class_.from_signed(too_positive)

    too_negative = (-class_.MAX_VALUE // 2) - 1
    with pytest.raises(OverflowError):
        class_.from_signed(too_negative)


def test_unsigned_in_range() -> None:
    number = Uint(0)
    with pytest.raises(NotImplementedError):
        Unsigned._in_range(number, 0)


def test_ulen() -> None:
    actual = ulen([1, 2, 3])
    assert isinstance(actual, Uint)
    assert actual == Uint(3)
