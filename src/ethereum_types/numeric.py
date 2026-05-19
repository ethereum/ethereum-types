"""
Numeric types (mostly integers.)
"""

from abc import abstractmethod
from numbers import Integral
from types import NotImplementedType
from typing import (
    ClassVar,
    Final,
    Literal,
    Optional,
    Sized,
    SupportsInt,
    Tuple,
    Type,
    Union,
    cast,
)

from mypy_extensions import mypyc_attr
from typing_extensions import Self, TypeAlias, override

from .bytes import Bytes, Bytes1, Bytes4, Bytes8, Bytes32, Bytes64

_BytesLike: TypeAlias = Union[bytes, bytearray, memoryview]


def _max_value(bits: int) -> int:
    assert bits >= 0
    value = (2**bits) - 1
    return cast("int", value)  # 2**-1 == 0.5


@mypyc_attr(acyclic=True)
class Unsigned:
    """
    Base of integer types.
    """

    __slots__ = ("_number",)
    _number: Final[int]

    def __init__(self, value: SupportsInt) -> None:
        int_value = int(value)
        if not self._in_range(int_value):
            raise OverflowError
        self._number = int_value

    @abstractmethod
    def _in_range(self, value: int) -> bool:
        raise NotImplementedError

    def __abs__(self) -> Self:
        return type(self)(self)

    def __radd__(self, left: Self) -> Self:
        return self.__add__(left)

    def __add__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number + right._number)

    def __iadd__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number + right._number)

    def __sub__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        if self._number < right._number:
            raise OverflowError

        return class_(self._number - right._number)

    def __rsub__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        if self._number > left._number:
            raise OverflowError

        return class_(left._number - self._number)

    def __isub__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        if right._number > self._number:
            raise OverflowError
        return class_(self._number - right._number)

    def __mul__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number * right._number)

    def __rmul__(self, left: Self) -> Self:
        return self.__mul__(left)

    def __imul__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number * right._number)

    def __truediv__(self, other: Self) -> float:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented
        return self._number.__truediv__(other._number)

    def __rtruediv__(self, other: Self) -> float:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented
        return self._number.__rtruediv__(other._number)

    def __floordiv__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number.__floordiv__(right._number))

    def __rfloordiv__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rfloordiv__(left._number))

    def __ifloordiv__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number // right._number)

    def __mod__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number % right._number)

    def __rmod__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rmod__(left._number))

    def __imod__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented
        return class_(self._number % right._number)

    def __divmod__(self, right: Self) -> Tuple[Self, Self]:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        result = self._number.__divmod__(right._number)
        return (
            class_(result[0]),
            class_(result[1]),
        )

    def __rdivmod__(
        self, left: Self
    ) -> Union[Tuple[Self, Self], NotImplementedType]:
        class_ = type(self)
        if not isinstance(left, class_):
            # No idea why mypy is assuming this is an Any...
            return NotImplemented  # type: ignore[no-any-return]

        result = self._number.__rdivmod__(left._number)
        return (
            class_(result[0]),
            class_(result[1]),
        )

    def __pow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        class_ = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, class_):
                return NotImplemented
            modulo_int = modulo._number

        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number.__pow__(right._number, modulo_int))

    def __rpow__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rpow__(left._number))

    def __ipow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        class_ = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, class_):
                raise TypeError
            modulo_int = modulo._number

        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number.__pow__(right._number, modulo_int))

    def __xor__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number.__xor__(right._number))

    def __rxor__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rxor__(left._number))

    def __ixor__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number.__xor__(right._number))

    def __and__(self, other: Self) -> Self:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return class_(self._number.__and__(other._number))

    def __rand__(self, other: Self) -> Self:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return class_(self._number.__rand__(other._number))

    def __or__(self, other: Self) -> Self:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return class_(self._number.__or__(other._number))

    def __ror__(self, other: Self) -> Self:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return class_(self._number.__ror__(other._number))

    def __neg__(self) -> int:
        return -self._number

    def __pos__(self) -> Self:
        return type(self)(self._number)

    def __invert__(self) -> Self:
        # TODO: How should this behave?
        raise NotImplementedError

    def __floor__(self) -> Self:
        return type(self)(self)

    def __ceil__(self) -> Self:
        return type(self)(self)

    def __int__(self) -> int:
        return self._number

    @override
    def __eq__(self, other: object) -> bool:
        # Unlike the other comparison dunder methods (eg. `__lt__`, `__ge__`,
        # etc.), `__eq__` is expected to work with any object, so mypy doesn't
        # detect comparisons between `Uint` and `int` as errors. Instead of
        # throwing a `TypeError` at runtime, we try to behave sanely and
        # soundly by converting `other` to an integer if possible, then
        # comparing.
        if isinstance(other, Unsigned):
            return self._number == other._number
        elif isinstance(other, SupportsInt):
            other_int = int(other)
            if other != other_int:
                # If `other` doesn't equal `int(other)`, `self` definitely
                # doesn't equal `other` since `self` has to be an integer.
                return False
            return self._number == other_int
        return NotImplemented

    def __le__(self, other: Self) -> bool:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return self._number <= other._number

    def __ge__(self, other: Self) -> bool:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return self._number >= other._number

    def __lt__(self, other: Self) -> bool:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return self._number < other._number

    def __gt__(self, other: Self) -> bool:
        class_ = type(self)
        if not isinstance(other, class_):
            return NotImplemented

        return self._number > other._number

    def __round__(self, ndigits: Optional[int] = None) -> Self:
        return type(self)(self)

    def __trunc__(self) -> Self:
        return type(self)(self)

    def __rshift__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number >> right._number)

    def __rrshift__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rrshift__(left._number))

    def __lshift__(self, right: Self) -> Self:
        class_ = type(self)
        if not isinstance(right, class_):
            return NotImplemented

        return class_(self._number << right._number)

    def __rlshift__(self, left: Self) -> Self:
        class_ = type(self)
        if not isinstance(left, class_):
            return NotImplemented

        return class_(self._number.__rlshift__(left._number))

    # Well, mypy is straight up incorrect on this.
    @override
    def __hash__(self) -> int:
        return hash((type(self), self._number))

    @override
    def __repr__(self) -> str:
        return f"{type(self).__name__}({self._number})"

    @override
    def __str__(self) -> str:
        return str(self._number)

    def to_be_bytes64(self) -> Bytes64:
        """
        Converts this unsigned integer into its big endian representation with
        exactly 64 bytes.
        """
        return Bytes64(self._number.to_bytes(64, "big"))

    def to_be_bytes32(self) -> Bytes32:
        """
        Converts this unsigned integer into its big endian representation
        with exactly 32 bytes.
        """
        return Bytes32(self._number.to_bytes(32, "big"))

    def to_bytes1(self) -> Bytes1:
        """
        Converts this unsigned integer into a byte sequence with exactly 1
        bytes.
        """
        return Bytes1(self._number.to_bytes(1, "little"))

    def to_le_bytes4(self) -> "Bytes4":
        """
        Converts this unsigned integer into its little endian representation,
        with exactly 4 bytes.
        """
        return Bytes4(self._number.to_bytes(4, "little"))

    def to_be_bytes4(self) -> "Bytes4":
        """
        Converts this unsigned integer into its big endian representation, with
        exactly 4 bytes.
        """
        return Bytes4(self._number.to_bytes(4, "big"))

    def to_le_bytes8(self) -> "Bytes8":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, with exactly 8 bytes.
        """
        return Bytes8(self._number.to_bytes(8, "little"))

    def to_be_bytes8(self) -> "Bytes8":
        """
        Converts this unsigned integer into its big endian representation, with
        exactly 8 bytes.
        """
        return Bytes8(self._number.to_bytes(8, "big"))

    def to_bytes(
        self,
        length: Optional[Self] = None,
        byteorder: Literal["big", "little"] = "big",
    ) -> Bytes:
        """
        Return an array of bytes representing an integer.
        """
        length_int = 1 if length is None else int(length)
        return self._number.to_bytes(length=length_int, byteorder=byteorder)

    def to_be_bytes(self) -> "Bytes":
        """
        Converts this unsigned integer into its big endian representation,
        without padding.
        """
        bit_length = self._number.bit_length()
        byte_length = (bit_length + 7) // 8
        return self._number.to_bytes(byte_length, "big")

    def to_le_bytes(self) -> "Bytes":
        """
        Converts this unsigned integer into its little endian representation,
        without padding.
        """
        bit_length = self._number.bit_length()
        number_bytes = (bit_length + 7) // 8
        return self._number.to_bytes(number_bytes, "little")

    def to_le_bytes32(self) -> Bytes32:
        """
        Converts this unsigned integer into its little endian representation
        with exactly 32 bytes.
        """
        return Bytes32(self._number.to_bytes(32, "little"))

    def to_le_bytes64(self) -> Bytes64:
        """
        Converts this unsigned integer into its little endian representation
        with exactly 64 bytes.
        """
        return Bytes64(self._number.to_bytes(64, "little"))

    def bit_length(self) -> "Uint":
        """
        Minimum number of bits required to represent this number in binary.
        """
        return Uint(self._number.bit_length())


@mypyc_attr(acyclic=True)
class Uint(Unsigned):
    """
    Unsigned integer of arbitrary size.
    """

    @classmethod
    def from_be_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its big endian representation.
        """
        return cls(int.from_bytes(buffer, "big"))

    @classmethod
    def from_le_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its little endian representation.
        """
        return cls(int.from_bytes(buffer, "little"))

    @override
    def _in_range(self, value: int) -> bool:
        return value >= 0


Integral.register(Uint)


def ulen(sized: Sized, /) -> Uint:
    """
    Return the number of items in a container, as a `Uint`.
    """
    return Uint(len(sized))


@mypyc_attr(acyclic=True)
class FixedUnsigned(Unsigned):
    """
    Superclass for fixed size unsigned integers. Not intended to be used
    directly, but rather to be subclassed.
    """

    MAX_VALUE: ClassVar[Self]
    """
    Largest value that can be represented by this integer type.
    """

    @classmethod
    def from_be_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into a fixed sized unsigned integer
        from its big endian representation.
        """
        bits = cls.MAX_VALUE._number.bit_length()
        byte_count = (bits + 7) // 8
        if len(buffer) > byte_count:
            message = f"expected at most {byte_count} but got {len(buffer)}"
            raise ValueError(message)

        return cls(int.from_bytes(buffer, "big"))

    @classmethod
    def from_le_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into a fixed sized unsigned integer
        from its little endian representation.
        """
        bits = cls.MAX_VALUE._number.bit_length()
        byte_count = (bits + 7) // 8
        if len(buffer) > byte_count:
            message = f"expected at most {byte_count} but got {len(buffer)}"
            raise ValueError(message)

        return cls(int.from_bytes(buffer, "little"))

    @classmethod
    def from_signed(cls: Type[Self], value: int) -> Self:
        """
        Creates an unsigned integer representing `value` using two's
        complement.
        """
        if value >= (cls.MAX_VALUE._number // 2 + 1):
            raise OverflowError

        if value >= 0:
            return cls(value)

        if value < (-cls.MAX_VALUE // 2):
            raise OverflowError

        return cls(value & cls.MAX_VALUE._number)

    @override
    def _in_range(self, value: int) -> bool:
        return value >= 0 and value <= self.MAX_VALUE._number

    def wrapping_add(self, right: Self) -> Self:
        """
        Return a new instance containing `self + right (mod N)`.
        """
        class_ = type(self)
        if not isinstance(right, class_):
            raise TypeError

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return class_((self._number + right._number) & self.MAX_VALUE._number)

    def wrapping_sub(self, right: Self) -> Self:
        """
        Return a new instance containing `self - right (mod N)`.
        """
        class_ = type(self)
        if not isinstance(right, class_):
            raise TypeError

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return class_((self._number - right._number) & self.MAX_VALUE._number)

    def wrapping_mul(self, right: Self) -> Self:
        """
        Return a new instance containing `self * right (mod N)`.
        """
        class_ = type(self)
        if not isinstance(right, class_):
            raise TypeError

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return class_((self._number * right._number) & self.MAX_VALUE._number)

    def wrapping_pow(self, right: Self, modulo: Optional[Self] = None) -> Self:
        """
        Return a new instance containing `self ** right (mod modulo)`.

        If omitted, `modulo` defaults to `Uint(self.MAX_VALUE) + 1`.
        """
        class_ = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, class_):
                raise TypeError
            modulo_int = modulo._number

        if not isinstance(right, class_):
            raise TypeError

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return class_(
            self._number.__pow__(right._number, modulo_int)
            & self.MAX_VALUE._number
        )

    @override
    def __invert__(self: Self) -> Self:
        return type(self)(
            int.__invert__(self._number) & self.MAX_VALUE._number
        )

    def to_signed(self) -> int:
        """
        Decodes a signed integer from its two's complement representation.
        """
        bits = self.MAX_VALUE._number.bit_length()
        bits = 8 * ((bits + 7) // 8)
        if self._number.bit_length() < bits:
            # This means that the sign bit is 0
            return int(self)

        # -1 * (2's complement of value)
        return int(self) - (self.MAX_VALUE._number + 1)


@mypyc_attr(acyclic=True)
class U256(FixedUnsigned):
    """
    Unsigned integer, which can represent `0` to `2 ** 256 - 1`, inclusive.
    """

    MAX_VALUE: ClassVar["U256"]
    """
    Largest value that can be represented by this integer type.
    """


Integral.register(U256)


@mypyc_attr(acyclic=True)
class _U256(U256):
    @override
    def _in_range(self, value: int) -> bool:
        return True


U256.MAX_VALUE = _U256(_max_value(256))
U256.MAX_VALUE = U256(_max_value(256))


@mypyc_attr(acyclic=True)
class U8(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 8 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U8"]
    """
    Largest value that can be represented by this integer type.
    """


Integral.register(U8)


@mypyc_attr(acyclic=True)
class _U8(U8):
    @override
    def _in_range(self, value: int) -> bool:
        return True


U8.MAX_VALUE = _U8(_max_value(8))
U8.MAX_VALUE = U8(_max_value(8))


@mypyc_attr(acyclic=True)
class U16(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 16 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U16"]
    """
    Largest value that can be represented by this integer type.
    """


Integral.register(U16)


@mypyc_attr(acyclic=True)
class _U16(U16):
    @override
    def _in_range(self, value: int) -> bool:
        return True


U16.MAX_VALUE = _U16(_max_value(16))
U16.MAX_VALUE = U16(_max_value(16))


@mypyc_attr(acyclic=True)
class U32(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 32 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U32"]
    """
    Largest value that can be represented by this integer type.
    """


Integral.register(U32)


@mypyc_attr(acyclic=True)
class _U32(U32):
    @override
    def _in_range(self, value: int) -> bool:
        return True


U32.MAX_VALUE = _U32(_max_value(32))
U32.MAX_VALUE = U32(_max_value(32))


@mypyc_attr(acyclic=True)
class U64(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 64 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U64"]
    """
    Largest value that can be represented by this integer type.
    """


Integral.register(U64)


@mypyc_attr(acyclic=True)
class _U64(U64):
    @override
    def _in_range(self, value: int) -> bool:
        return True


U64.MAX_VALUE = _U64(_max_value(64))
U64.MAX_VALUE = U64(_max_value(64))
