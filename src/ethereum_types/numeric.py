"""
Numeric types (mostly integers.)
"""

from numbers import Integral
from typing import (
    ClassVar,
    Final,
    Literal,
    NoReturn,
    Optional,
    Sized,
    SupportsInt,
    Tuple,
    Type,
    TypeVar,
    Union,
)

from .bytes import Bytes, Bytes4, Bytes8, Bytes32, Bytes64

U = TypeVar("U", bound="Uint")


class Uint(Integral):
    """
    Unsigned integer of arbitrary size.
    """

    __slots__ = ("_number",)
    _number: Final[int]

    @classmethod
    def from_be_bytes(cls: Type[U], buffer: "Bytes") -> U:
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its big endian representation.
        """
        return cls(int.from_bytes(buffer, "big"))

    @classmethod
    def from_le_bytes(cls: Type[U], buffer: "Bytes") -> U:
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its little endian representation.
        """
        return cls(int.from_bytes(buffer, "little"))

    def __init__(self, value: Union[int, "Uint"]) -> None:
        my_value: int
        if isinstance(value, int):
            if value < 0:
                raise OverflowError()
            my_value = value
        elif isinstance(value, Uint):
            my_value = value._number
        else:
            raise TypeError()

        if type(my_value) is not int:
            my_value = int(
                my_value
            )  # TODO: Workaround for partial U256 support
        self._number = my_value

    def __abs__(self) -> "Uint":
        return Uint(self)

    def __radd__(self, left: "Uint") -> "Uint":
        return self.__add__(left)

    def __add__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        return Uint(self._number + right._number)

    def __iadd__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        return Uint(self._number + right._number)

    def __sub__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        if self._number < right._number:
            raise OverflowError()

        return Uint(self._number - right._number)

    def __rsub__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        if self._number > left._number:
            raise OverflowError()

        return Uint(left._number - self._number)

    def __isub__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        if right._number > self._number:
            raise OverflowError()
        return Uint(self._number - right._number)

    def __mul__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number * right._number)

    def __rmul__(self, left: "Uint") -> "Uint":
        return self.__mul__(left)

    def __imul__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        return Uint(self._number * right._number)

    def __truediv__(self, other: "Uint") -> float:
        if not isinstance(other, Uint):
            return NotImplemented
        return self._number.__truediv__(other._number)

    def __rtruediv__(self, other: "Uint") -> float:
        if not isinstance(other, Uint):
            return NotImplemented
        return self._number.__rtruediv__(other._number)

    def __floordiv__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number.__floordiv__(right._number))

    def __rfloordiv__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rfloordiv__(left._number))

    def __ifloordiv__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        return Uint(self._number // right._number)

    def __mod__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number % right._number)

    def __rmod__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rmod__(left._number))

    def __imod__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented
        return Uint(self._number % right._number)

    def __divmod__(self, right: "Uint") -> Tuple["Uint", "Uint"]:
        if not isinstance(right, Uint):
            return NotImplemented

        result = self._number.__divmod__(right._number)
        return (
            Uint(result[0]),
            Uint(result[1]),
        )

    def __rdivmod__(self, left: "Uint") -> Tuple["Uint", "Uint"]:
        if not isinstance(left, Uint):
            return NotImplemented

        result = self._number.__rdivmod__(left._number)
        return (
            Uint(result[0]),
            Uint(result[1]),
        )

    def __pow__(
        self, right: "Uint", modulo: Optional["Uint"] = None
    ) -> "Uint":
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Uint):
                return NotImplemented
            modulo_int = modulo._number

        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number.__pow__(right._number, modulo_int))

    def __rpow__(
        self, left: "Uint", modulo: Optional["Uint"] = None
    ) -> "Uint":
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Uint):
                raise TypeError()
            modulo_int = modulo._number

        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rpow__(left._number, modulo_int))

    def __ipow__(
        self, right: "Uint", modulo: Optional["Uint"] = None
    ) -> "Uint":
        modulo_int = None
        if modulo is not None:
            if not isinstance(modulo, Uint):
                raise TypeError()
            modulo_int = modulo._number

        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number.__pow__(right._number, modulo_int))

    def __xor__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number.__xor__(right._number))

    def __rxor__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rxor__(left._number))

    def __ixor__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number.__xor__(right._number))

    def __and__(self, other: "Uint") -> "Uint":
        if not isinstance(other, Uint):
            return NotImplemented

        return Uint(self._number.__and__(other._number))

    def __floor__(self) -> "Uint":
        return Uint(self)

    def __ceil__(self) -> "Uint":
        return Uint(self)

    def __int__(self) -> int:
        return self._number

    def __rand__(self, other: "Uint") -> "Uint":
        if not isinstance(other, Uint):
            return NotImplemented

        return Uint(self._number.__rand__(other._number))

    def __or__(self, other: "Uint") -> "Uint":
        if not isinstance(other, Uint):
            return NotImplemented

        return Uint(self._number.__or__(other._number))

    def __ror__(self, other: "Uint") -> "Uint":
        if not isinstance(other, Uint):
            return NotImplemented

        return Uint(self._number.__ror__(other._number))

    def __neg__(self) -> int:
        return -self._number

    def __pos__(self) -> "Uint":
        return Uint(self._number)

    def __invert__(self) -> NoReturn:
        # TODO: How should this behave?
        raise NotImplementedError()

    def __eq__(self, other: object) -> bool:
        # Unlike the other comparison dunder methods (eg. `__lt__`, `__ge__`,
        # etc.), `__eq__` is expected to work with any object, so mypy doesn't
        # detect comparisons between `Uint` and `int` as errors. Instead of
        # throwing a `TypeError` at runtime, we try to behave sanely and
        # soundly by converting `other` to an integer if possible, then
        # comparing.
        if isinstance(other, Uint):
            return self._number == other._number
        elif isinstance(other, SupportsInt):
            other_int = int(other)
            if other != other_int:
                # If `other` doesn't equal `int(other)`, `self` definitely
                # doesn't equal `other` since `self` has to be an integer.
                return False
            return self._number == other_int
        return NotImplemented

    def __le__(self, other: "Uint") -> bool:
        if not isinstance(other, Uint):
            return NotImplemented

        return self._number <= other._number

    def __ge__(self, other: "Uint") -> bool:
        if not isinstance(other, Uint):
            return NotImplemented

        return self._number >= other._number

    def __lt__(self, other: "Uint") -> bool:
        if not isinstance(other, Uint):
            return NotImplemented

        return self._number < other._number

    def __gt__(self, other: "Uint") -> bool:
        if not isinstance(other, Uint):
            return NotImplemented

        return self._number > other._number

    def __round__(self, ndigits: Optional[int] = None) -> "Uint":
        return Uint(self)

    def __trunc__(self) -> "Uint":
        return Uint(self)

    def __rshift__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number >> right._number)

    def __rrshift__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rrshift__(left._number))

    def __lshift__(self, right: "Uint") -> "Uint":
        if not isinstance(right, Uint):
            return NotImplemented

        return Uint(self._number << right._number)

    def __rlshift__(self, left: "Uint") -> "Uint":
        if not isinstance(left, Uint):
            return NotImplemented

        return Uint(self._number.__rlshift__(left._number))

    def __hash__(self) -> int:
        return hash((Uint, self._number))

    def __repr__(self) -> str:
        return "{}({})".format(type(self).__name__, self._number)

    def __str__(self) -> str:
        return str(self._number)

    def to_be_bytes64(self) -> Bytes64:
        """
        Converts this arbitrarily sized unsigned integer into its big endian
        representation with exactly 64 bytes.
        """
        return Bytes64(self._number.to_bytes(64, "big"))

    def to_be_bytes32(self) -> Bytes32:
        """
        Converts this arbitrarily sized unsigned integer into its big endian
        representation with exactly 32 bytes.
        """
        return Bytes32(self._number.to_bytes(32, "big"))

    def to_bytes(
        self,
        length: Optional["Uint"] = None,
        byteorder: Literal["big", "little"] = "big",
    ) -> "Bytes":
        """
        Return an array of bytes representing an integer.
        """
        if length is None:
            length_int = 1
        else:
            length_int = int(length)
        return self._number.to_bytes(length=length_int, byteorder=byteorder)

    def to_be_bytes(self) -> "Bytes":
        """
        Converts this arbitrarily sized unsigned integer into its big endian
        representation, without padding.
        """
        bit_length = self._number.bit_length()
        byte_length = (bit_length + 7) // 8
        return self._number.to_bytes(byte_length, "big")

    def to_le_bytes(self) -> "Bytes":
        """
        Converts this arbitrarily sized unsigned integer into its little endian
        representation, without padding.
        """
        bit_length = self._number.bit_length()
        number_bytes = (bit_length + 7) // 8
        return self._number.to_bytes(number_bytes, "little")

    def to_le_bytes32(self) -> Bytes32:
        """
        Converts this arbitrarily sized unsigned integer into its little endian
        representation with exactly 32 bytes.
        """
        return Bytes32(self._number.to_bytes(32, "little"))

    def to_le_bytes64(self) -> Bytes64:
        """
        Converts this arbitrarily sized unsigned integer into its little endian
        representation with exactly 64 bytes.
        """
        return Bytes64(self._number.to_bytes(64, "little"))

    def bit_length(self) -> "Uint":
        """
        Minimum number of bits required to represent this number in binary.
        """
        return Uint(self._number.bit_length())


def ulen(input: Sized, /) -> Uint:
    """
    Return the number of items in a container, as a `Uint`.
    """
    return Uint(len(input))


T = TypeVar("T", bound="FixedUint")


class FixedUint(int):
    """
    Superclass for fixed size unsigned integers. Not intended to be used
    directly, but rather to be subclassed.
    """

    MAX_VALUE: ClassVar["FixedUint"]
    """
    Largest value that can be represented by this integer type.
    """

    __slots__ = ()
    # _number: Final[int]

    def __init__(self: T, value: SupportsInt) -> None:
        value_int = int(value)

        if value_int < 0 or value_int > self.MAX_VALUE:
            raise OverflowError()

        self._number = value_int

    def __radd__(self: T, left: int) -> T:
        return self.__add__(left)

    def __add__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        result = int.__add__(self, right)

        if right < 0 or result > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, result)

    def wrapping_add(self: T, right: int) -> T:
        """
        Return a new instance containing `self + right (mod N)`.

        Passing a `right` value greater than [`MAX_VALUE`] or less than zero
        will raise a `ValueError`, even if the result would fit in this integer
        type.

        [`MAX_VALUE`]: ref:ethereum.base_types.FixedUint.MAX_VALUE
        """
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return int.__new__(
            self.__class__, int.__add__(self, right) & self.MAX_VALUE
        )

    def __iadd__(self: T, right: int) -> T:
        return self.__add__(right)

    def __sub__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE or self < right:
            raise OverflowError()

        return int.__new__(self.__class__, int.__sub__(self, right))

    def wrapping_sub(self: T, right: int) -> T:
        """
        Return a new instance containing `self - right (mod N)`.

        Passing a `right` value greater than [`MAX_VALUE`] or less than zero
        will raise a `ValueError`, even if the result would fit in this integer
        type.

        [`MAX_VALUE`]: ref:ethereum.base_types.FixedUint.MAX_VALUE
        """
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return int.__new__(
            self.__class__, int.__sub__(self, right) & self.MAX_VALUE
        )

    def __rsub__(self: T, left: int) -> T:
        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE or self > left:
            raise OverflowError()

        return int.__new__(self.__class__, int.__rsub__(self, left))

    def __isub__(self: T, right: int) -> T:
        return self.__sub__(right)

    def __mul__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        result = int.__mul__(self, right)

        if right < 0 or result > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, result)

    def wrapping_mul(self: T, right: int) -> T:
        """
        Return a new instance containing `self * right (mod N)`.

        Passing a `right` value greater than [`MAX_VALUE`] or less than zero
        will raise a `ValueError`, even if the result would fit in this integer
        type.

        [`MAX_VALUE`]: ref:ethereum.base_types.FixedUint.MAX_VALUE
        """
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return int.__new__(
            self.__class__, int.__mul__(self, right) & self.MAX_VALUE
        )

    def __rmul__(self: T, left: int) -> T:
        return self.__mul__(left)

    def __imul__(self: T, right: int) -> T:
        return self.__mul__(right)

    # Explicitly don't override __truediv__, __rtruediv__, and __itruediv__
    # since they return floats anyway.

    def __floordiv__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__floordiv__(self, right))

    def __rfloordiv__(self: T, left: int) -> T:
        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__rfloordiv__(self, left))

    def __ifloordiv__(self: T, right: int) -> T:
        return self.__floordiv__(right)

    def __mod__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__mod__(self, right))

    def __rmod__(self: T, left: int) -> T:
        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__rmod__(self, left))

    def __imod__(self: T, right: int) -> T:
        return self.__mod__(right)

    def __divmod__(self: T, right: int) -> Tuple[T, T]:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        result = super(FixedUint, self).__divmod__(right)
        return (
            int.__new__(self.__class__, result[0]),
            int.__new__(self.__class__, result[1]),
        )

    def __rdivmod__(self: T, left: int) -> Tuple[T, T]:
        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE:
            raise OverflowError()

        result = super(FixedUint, self).__rdivmod__(left)
        return (
            int.__new__(self.__class__, result[0]),
            int.__new__(self.__class__, result[1]),
        )

    def __pow__(  # type: ignore[override]
        self: T, right: int, modulo: Optional[int] = None
    ) -> T:
        if modulo is not None:
            if not isinstance(modulo, int):
                return NotImplemented

            if modulo < 0 or modulo > self.MAX_VALUE:
                raise OverflowError()

        if not isinstance(right, int):
            return NotImplemented

        result = int.__pow__(self, right, modulo)

        if right < 0 or right > self.MAX_VALUE or result > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, result)

    def wrapping_pow(self: T, right: int, modulo: Optional[int] = None) -> T:
        """
        Return a new instance containing `self ** right (mod modulo)`.

        If omitted, `modulo` defaults to `Uint(self.MAX_VALUE) + 1`.

        Passing a `right` or `modulo` value greater than [`MAX_VALUE`] or
        less than zero will raise a `ValueError`, even if the result would fit
        in this integer type.

        [`MAX_VALUE`]: ref:ethereum.base_types.FixedUint.MAX_VALUE
        """
        if modulo is not None:
            if not isinstance(modulo, int):
                return NotImplemented

            if modulo < 0 or modulo > self.MAX_VALUE:
                raise OverflowError()

        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        # This is a fast way of ensuring that the result is < (2 ** 256)
        return int.__new__(
            self.__class__, int.__pow__(self, right, modulo) & self.MAX_VALUE
        )

    def __rpow__(  # type: ignore[misc]
        self: T, left: int, modulo: Optional[int] = None
    ) -> T:
        if modulo is not None:
            if not isinstance(modulo, int):
                return NotImplemented

            if modulo < 0 or modulo > self.MAX_VALUE:
                raise OverflowError()

        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__rpow__(self, left, modulo))

    def __ipow__(  # type: ignore[override]
        self: T, right: int, modulo: Optional[int] = None
    ) -> T:
        return self.__pow__(right, modulo)

    def __and__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__and__(self, right))

    def __or__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__or__(self, right))

    def __xor__(self: T, right: int) -> T:
        if not isinstance(right, int):
            return NotImplemented

        if right < 0 or right > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__xor__(self, right))

    def __rxor__(self: T, left: int) -> T:
        if not isinstance(left, int):
            return NotImplemented

        if left < 0 or left > self.MAX_VALUE:
            raise OverflowError()

        return int.__new__(self.__class__, int.__rxor__(self, left))

    def __ixor__(self: T, right: int) -> T:
        return self.__xor__(right)

    def __invert__(self: T) -> T:
        return int.__new__(
            self.__class__, int.__invert__(self) & self.MAX_VALUE
        )

    def __rshift__(self: T, shift_by: int) -> T:
        if not isinstance(shift_by, int):
            return NotImplemented
        return int.__new__(self.__class__, int.__rshift__(self, shift_by))

    def to_be_bytes(self) -> "Bytes":
        """
        Converts this unsigned integer into its big endian representation,
        omitting leading zero bytes.
        """
        bit_length = self.bit_length()
        byte_length = (bit_length + 7) // 8
        return self.to_bytes(byte_length, "big")

    # TODO: Implement neg, pos, abs ...


class U256(FixedUint):
    """
    Unsigned integer, which can represent `0` to `2 ** 256 - 1`, inclusive.
    """

    MAX_VALUE: ClassVar["U256"]
    """
    Largest value that can be represented by this integer type.
    """

    __slots__ = ()

    @classmethod
    def from_be_bytes(cls: Type, buffer: "Bytes") -> "U256":
        """
        Converts a sequence of bytes into a fixed sized unsigned integer
        from its big endian representation.
        """
        if len(buffer) > 32:
            raise ValueError()

        return cls(int.from_bytes(buffer, "big"))

    @classmethod
    def from_signed(cls: Type, value: int) -> "U256":
        """
        Creates an unsigned integer representing `value` using two's
        complement.
        """
        if value >= 0:
            return cls(value)

        return cls(value & cls.MAX_VALUE)

    def to_be_bytes32(self) -> Bytes32:
        """
        Converts this 256-bit unsigned integer into its big endian
        representation with exactly 32 bytes.
        """
        return Bytes32(self.to_bytes(32, "big"))

    def to_signed(self) -> int:
        """
        Decodes a signed integer from its two's complement representation.
        """
        if self.bit_length() < 256:
            # This means that the sign bit is 0
            return int(self)

        # -1 * (2's complement of U256 value)
        return int(self) - int(U256_CEILING)


U256.MAX_VALUE = int.__new__(U256, (2**256) - 1)


class U32(FixedUint):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 32 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U32"]
    """
    Largest value that can be represented by this integer type.
    """

    __slots__ = ()

    @classmethod
    def from_le_bytes(cls: Type, buffer: "Bytes") -> "U32":
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its little endian representation.
        """
        if len(buffer) > 4:
            raise ValueError()

        return cls(int.from_bytes(buffer, "little"))

    def to_le_bytes4(self) -> "Bytes4":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, with exactly 4 bytes.
        """
        return Bytes4(self.to_bytes(4, "little"))

    def to_le_bytes(self) -> "Bytes":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, in the fewest bytes possible.
        """
        bit_length = self.bit_length()
        byte_length = (bit_length + 7) // 8
        return self.to_bytes(byte_length, "little")


U32.MAX_VALUE = int.__new__(U32, (2**32) - 1)


class U64(FixedUint):
    """
    Unsigned positive integer, which can represent `0` to `2 ** 64 - 1`,
    inclusive.
    """

    MAX_VALUE: ClassVar["U64"]
    """
    Largest value that can be represented by this integer type.
    """

    __slots__ = ()

    @classmethod
    def from_le_bytes(cls: Type, buffer: "Bytes") -> "U64":
        """
        Converts a sequence of bytes into an arbitrarily sized unsigned integer
        from its little endian representation.
        """
        if len(buffer) > 8:
            raise ValueError()

        return cls(int.from_bytes(buffer, "little"))

    def to_le_bytes8(self) -> "Bytes8":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, with exactly 8 bytes.
        """
        return Bytes8(self.to_bytes(8, "little"))

    def to_le_bytes(self) -> "Bytes":
        """
        Converts this fixed sized unsigned integer into its little endian
        representation, in the fewest bytes possible.
        """
        bit_length = self.bit_length()
        byte_length = (bit_length + 7) // 8
        return self.to_bytes(byte_length, "little")

    @classmethod
    def from_be_bytes(cls: Type, buffer: "Bytes") -> "U64":
        """
        Converts a sequence of bytes into an unsigned 64 bit integer from its
        big endian representation.
        """
        if len(buffer) > 8:
            raise ValueError()

        return cls(int.from_bytes(buffer, "big"))


U64.MAX_VALUE = int.__new__(U64, (2**64) - 1)


U255_CEILING = Uint(2**255)
"""
Smallest value that requires 256 bits to represent. Mostly used in signed
arithmetic operations.
"""

U256_CEILING = Uint(2**256)
"""
Smallest value that requires 257 bits to represent. Used when converting a
[`U256`] in two's complement format to a regular `int` in [`U256.to_signed`].

[`U256`]: ref:ethereum_types.numeric.U256
[`U256.to_signed`]: ref:ethereum_types.numeric.U256.to_signed
"""
