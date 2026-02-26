"""
Numeric types (mostly integers.)
"""

from typing import (
    ClassVar,
    Literal,
    Optional,
    Sized,
    SupportsInt,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from typing_extensions import Self

from .bytes import Bytes, Bytes1, Bytes4, Bytes8, Bytes32, Bytes64

_BytesLike = Union[bytes, bytearray, memoryview]


class Unsigned(int):
    """
    Base of integer types.
    """

    __slots__ = ()

    def __new__(cls, value: SupportsInt) -> Self:
        """Create a new unsigned integer in range."""
        int_value = int(value)
        instance = int.__new__(cls, int_value)
        if not instance._in_range(int_value):
            raise OverflowError()
        return instance

    def _in_range(self, value: int) -> bool:
        raise NotImplementedError

    def __abs__(self) -> Self:
        return self

    def __radd__(self, left: Self) -> Self:
        return self.__add__(left)

    def __add__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__add__(self, right))

    def __iadd__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__add__(self, right))

    def __sub__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__sub__(self, right)
        if result < 0:
            raise OverflowError()
        return int.__new__(Class, result)

    def __rsub__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        result = int.__sub__(left, self)
        if result < 0:
            raise OverflowError()
        return int.__new__(Class, result)

    def __isub__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__sub__(self, right)
        if result < 0:
            raise OverflowError()
        return int.__new__(Class, result)

    def __mul__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__mul__(self, right))

    def __rmul__(self, left: Self) -> Self:
        return self.__mul__(left)

    def __imul__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__mul__(self, right))

    def __truediv__(self, other: Self) -> float:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__truediv__(self, other)

    def __rtruediv__(self, other: Self) -> float:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__truediv__(other, self)

    def __floordiv__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__floordiv__(self, right))

    def __rfloordiv__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__floordiv__(left, self))

    def __ifloordiv__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__floordiv__(self, right))

    def __mod__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__mod__(self, right))

    def __rmod__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__mod__(left, self))

    def __imod__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__mod__(self, right))

    def __divmod__(self, right: Self) -> Tuple[Self, Self]:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__divmod__(self, right)
        return (
            int.__new__(Class, result[0]),
            int.__new__(Class, result[1]),
        )

    def __rdivmod__(self, left: Self) -> Tuple[Self, Self]:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        result = int.__divmod__(left, self)
        return (
            int.__new__(Class, result[0]),
            int.__new__(Class, result[1]),
        )

    def __pow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                return NotImplemented
            modulo_int = modulo
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__pow__(self, right, modulo_int))

    def __rpow__(self, left: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                raise TypeError()
            modulo_int = modulo
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__pow__(left, self, modulo_int))

    def __ipow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                raise TypeError()
            modulo_int = modulo
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__pow__(self, right, modulo_int))

    def __xor__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__xor__(self, right))

    def __rxor__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__xor__(left, self))

    def __ixor__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__xor__(self, right))

    def __and__(self, other: Self) -> Self:
        Class = type(self)
        if not isinstance(other, Class):
            return NotImplemented
        return int.__new__(Class, int.__and__(self, other))

    def __rand__(self, other: Self) -> Self:
        Class = type(self)
        if not isinstance(other, Class):
            return NotImplemented
        return int.__new__(Class, int.__and__(other, self))

    def __or__(self, other: Self) -> Self:
        Class = type(self)
        if not isinstance(other, Class):
            return NotImplemented
        return int.__new__(Class, int.__or__(self, other))

    def __ror__(self, other: Self) -> Self:
        Class = type(self)
        if not isinstance(other, Class):
            return NotImplemented
        return int.__new__(Class, int.__or__(other, self))

    def __neg__(self) -> int:
        return int.__neg__(self)

    def __pos__(self) -> Self:
        return self

    def __invert__(self) -> Self:
        # TODO: How should this behave?
        raise NotImplementedError()

    def __floor__(self) -> Self:
        return self

    def __ceil__(self) -> Self:
        return self

    def __eq__(self, other: object) -> bool:
        # Unlike the other comparison dunder methods, `__eq__` is expected to
        # work with any object, so mypy doesn't detect comparisons between
        # `Uint` and `int` as errors. Instead of throwing a `TypeError` at
        # runtime, we try to behave sanely and soundly by converting `other`
        # to an integer if possible, then comparing.
        if isinstance(other, Unsigned):
            return int.__eq__(self, other)
        elif isinstance(other, SupportsInt):
            other_int = int(other)
            if other != other_int:
                return False
            return int.__eq__(self, other_int)
        return NotImplemented

    def __le__(self, other: Self) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__le__(self, other)

    def __ge__(self, other: Self) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__ge__(self, other)

    def __lt__(self, other: Self) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__lt__(self, other)

    def __gt__(self, other: Self) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        return int.__gt__(self, other)

    def __round__(self, ndigits: Optional[int] = None) -> Self:
        return self

    def __trunc__(self) -> Self:
        return self

    def __rshift__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__rshift__(self, right))

    def __rrshift__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__rshift__(left, self))

    def __lshift__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        return int.__new__(Class, int.__lshift__(self, right))

    def __rlshift__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        return int.__new__(Class, int.__lshift__(left, self))

    def __hash__(self) -> int:
        return hash((type(self), int.__int__(self)))

    def __repr__(self) -> str:
        return "{}({})".format(type(self).__name__, int.__repr__(self))

    def __str__(self) -> str:
        return int.__repr__(self)

    def to_be_bytes64(self) -> Bytes64:
        """
        Converts this unsigned integer into its big endian representation with
        exactly 64 bytes.
        """
        return Bytes64(int.to_bytes(self, 64, "big"))

    def to_be_bytes32(self) -> Bytes32:
        """
        Converts this unsigned integer into its big endian representation
        with exactly 32 bytes.
        """
        return Bytes32(int.to_bytes(self, 32, "big"))

    def to_bytes1(self) -> Bytes1:
        """
        Converts this unsigned integer into a byte sequence with exactly 1
        bytes.
        """
        return Bytes1(int.to_bytes(self, 1, "little"))

    def to_le_bytes4(self) -> "Bytes4":
        """
        Converts this unsigned integer into its little endian representation,
        with exactly 4 bytes.
        """
        return Bytes4(int.to_bytes(self, 4, "little"))

    def to_be_bytes4(self) -> "Bytes4":
        """
        Converts this unsigned integer into its big endian representation, with
        exactly 4 bytes.
        """
        return Bytes4(int.to_bytes(self, 4, "big"))

    def to_le_bytes8(self) -> "Bytes8":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, with exactly 8 bytes.
        """
        return Bytes8(int.to_bytes(self, 8, "little"))

    def to_be_bytes8(self) -> "Bytes8":
        """
        Converts this unsigned integer into its big endian representation, with
        exactly 8 bytes.
        """
        return Bytes8(int.to_bytes(self, 8, "big"))

    def to_bytes(
        self,
        length: Optional[Self] = None,
        byteorder: Literal["big", "little"] = "big",
    ) -> Bytes:
        """
        Return an array of bytes representing an integer.
        """
        if length is None:
            length_int = 1
        else:
            length_int = length
        return int.to_bytes(self, length=length_int, byteorder=byteorder)

    def to_be_bytes(self) -> "Bytes":
        """
        Converts this unsigned integer into its big endian representation,
        without padding.
        """
        bit_length = int.bit_length(self)
        byte_length = (bit_length + 7) // 8
        return int.to_bytes(self, byte_length, "big")

    def to_le_bytes(self) -> "Bytes":
        """
        Converts this unsigned integer into its little endian representation,
        without padding.
        """
        bit_length = int.bit_length(self)
        number_bytes = (bit_length + 7) // 8
        return int.to_bytes(self, number_bytes, "little")

    def to_le_bytes32(self) -> Bytes32:
        """
        Converts this unsigned integer into its little endian representation
        with exactly 32 bytes.
        """
        return Bytes32(int.to_bytes(self, 32, "little"))

    def to_le_bytes64(self) -> Bytes64:
        """
        Converts this unsigned integer into its little endian representation
        with exactly 64 bytes.
        """
        return Bytes64(int.to_bytes(self, 64, "little"))

    def bit_length(self) -> "Uint":
        """
        Minimum number of bits required to represent this number in binary.
        """
        return int.__new__(Uint, int.bit_length(self))


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

    def _in_range(self, value: int) -> bool:
        return value >= 0


def ulen(input: Sized, /) -> Uint:
    """
    Return the number of items in a container, as a `Uint`.
    """
    return Uint(len(input))


class FixedUnsigned(Unsigned):
    """
    Superclass for fixed size unsigned integers. Not intended to be used
    directly, but rather to be subclassed.
    """

    MAX_VALUE: ClassVar[Self]
    """
    Largest value that can be represented by this integer type.
    """

    _MAX_INT: ClassVar[int]
    """
    TODO: note we could have just used int(MAX_VALUE) but I imagine
    TODO: performance wise this would be bad. Haven't benchmarked though.
    Plain Python int mask `(2**bits) - 1`. Set by `_max_value()` at class
    creation time.
    """

    @classmethod
    def from_be_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into a fixed sized unsigned integer
        from its big endian representation.
        """
        bits = cls._MAX_INT.bit_length()
        byte_count = (bits + 7) // 8
        if len(buffer) > byte_count:
            raise ValueError()
        return cls(int.from_bytes(buffer, "big"))

    @classmethod
    def from_le_bytes(cls: Type[Self], buffer: _BytesLike) -> Self:
        """
        Converts a sequence of bytes into a fixed sized unsigned integer
        from its little endian representation.
        """
        bits = cls._MAX_INT.bit_length()
        byte_count = (bits + 7) // 8
        if len(buffer) > byte_count:
            raise ValueError()
        return cls(int.from_bytes(buffer, "little"))

    @classmethod
    def from_signed(cls: Type[Self], value: int) -> Self:
        """
        Creates an unsigned integer representing `value` using two's
        complement.
        """
        if value >= (cls._MAX_INT // 2 + 1):
            raise OverflowError
        if value >= 0:
            return cls(value)
        if value < (-(cls._MAX_INT // 2) - 1):
            raise OverflowError
        return cls(value & cls._MAX_INT)

    def _in_range(self, value: int) -> bool:
        return 0 <= value <= type(self)._MAX_INT

    def __add__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__add__(self, right)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __iadd__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__add__(self, right)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __mul__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__mul__(self, right)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __imul__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__mul__(self, right)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __pow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                return NotImplemented
            modulo_int = modulo
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__pow__(self, right, modulo_int)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __rpow__(self, left: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                raise TypeError()
            modulo_int = modulo
        if not isinstance(left, Class):
            return NotImplemented
        result = int.__pow__(left, self, modulo_int)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __ipow__(self, right: Self, modulo: Optional[Self] = None) -> Self:
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                raise TypeError()
            modulo_int = modulo
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__pow__(self, right, modulo_int)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __lshift__(self, right: Self) -> Self:
        Class = type(self)
        if not isinstance(right, Class):
            return NotImplemented
        result = int.__lshift__(self, right)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def __rlshift__(self, left: Self) -> Self:
        Class = type(self)
        if not isinstance(left, Class):
            return NotImplemented
        result = int.__lshift__(left, self)
        if result > Class._MAX_INT:
            raise OverflowError()
        return int.__new__(Class, result)

    def wrapping_add(self, right: Self) -> Self:
        """
        Return a new instance containing `self + right (mod N)`.
        """
        Class = type(self)
        if not isinstance(right, Class):
            raise TypeError()
        return int.__new__(Class, int.__add__(self, right) & self._MAX_INT)

    def wrapping_sub(self, right: Self) -> Self:
        """
        Return a new instance containing `self - right (mod N)`.
        """
        Class = type(self)
        if not isinstance(right, Class):
            raise TypeError()
        return int.__new__(Class, int.__sub__(self, right) & self._MAX_INT)

    def wrapping_mul(self, right: Self) -> Self:
        """
        Return a new instance containing `self * right (mod N)`.
        """
        Class = type(self)
        if not isinstance(right, Class):
            raise TypeError

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return int.__new__(Class, int.__mul__(self, right) & self._MAX_INT)

    def wrapping_pow(self, right: Self, modulo: Optional[Self] = None) -> Self:
        """
        Return a new instance containing `self ** right (mod modulo)`.

        If omitted, `modulo` defaults to `Uint(self.MAX_VALUE) + 1`.
        """
        Class = type(self)
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Class):
                raise TypeError()
            modulo_int = modulo
        if not isinstance(right, Class):
            raise TypeError()
        return int.__new__(
            Class,
            int.__pow__(self, right, modulo_int) & self._MAX_INT,
        )

    def __invert__(self: Self) -> Self:
        return int.__new__(
            type(self),
            int.__invert__(self) & self._MAX_INT,
        )

    def to_signed(self) -> int:
        """
        Decodes a signed integer from its two's complement representation.
        """
        n = int.__int__(self)
        bits = self._MAX_INT.bit_length()
        bits = 8 * ((bits + 7) // 8)
        if n.bit_length() < bits:
            # This means that the sign bit is 0
            return n
        # -1 * (2's complement of value)
        return n - (self._MAX_INT + 1)


_V = TypeVar("_V", bound=FixedUnsigned)


def _max_value(class_: Type[_V], bits: int) -> _V:
    max_int = (2**bits) - 1
    class_._MAX_INT = max_int
    return int.__new__(class_, max_int)


class U256(FixedUnsigned):
    """
    Unsigned integer, which can represent `0` to `2 ** 256 - 1`, inclusive.
    """

    MAX_VALUE: ClassVar["U256"]
    """
    Largest value that can be represented by this integer type.
    """


U256.MAX_VALUE = _max_value(U256, 256)


class U8(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 8 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U8"]
    """
    Largest value that can be represented by this integer type.
    """


U8.MAX_VALUE = _max_value(U8, 8)


class U16(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 16 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U16"]
    """
    Largest value that can be represented by this integer type.
    """


U16.MAX_VALUE = _max_value(U16, 16)


class U32(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 32 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U32"]
    """
    Largest value that can be represented by this integer type.
    """


U32.MAX_VALUE = _max_value(U32, 32)


class U64(FixedUnsigned):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 64 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U64"]
    """
    Largest value that can be represented by this integer type.
    """


U64.MAX_VALUE = _max_value(U64, 64)
