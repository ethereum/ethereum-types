"""
Sequences of 256-bit values.
"""

from typing import TYPE_CHECKING, Any, ClassVar, Type, TypeVar

from mypy_extensions import mypyc_attr

if TYPE_CHECKING:
    from .numeric import Uint

B = TypeVar("B", bound="FixedBytes")


@mypyc_attr(native_class=False)
class FixedBytes(bytes):
    """
    Superclass for fixed sized byte arrays. Not intended to be used directly,
    but should be subclassed.
    """

    LENGTH: ClassVar[int]
    """
    Number of bytes in each instance of this class.
    """

    __slots__ = ()

    def __new__(cls: Type[B], *args: Any, **kwargs: Any) -> B:
        """
        Create a new instance, ensuring the result has the correct length.
        """
        result = bytes.__new__(cls, *args, **kwargs)
        if len(result) != cls.LENGTH:
            raise ValueError(
                f"expected {cls.LENGTH} bytes but got {len(result)}"
            )
        return result

    def zero_bytes(self) -> "Uint":
        """
        Count and return the number of zero bytes in the byte array.
        """
        from .numeric import Uint

        return Uint(self.count(b"\0"))


@mypyc_attr(native_class=False)
class Bytes0(FixedBytes):
    """
    Byte array of exactly zero elements.
    """

    LENGTH = 0
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes1(FixedBytes):
    """
    Byte array of exactly one elements.
    """

    LENGTH = 1
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes4(FixedBytes):
    """
    Byte array of exactly four elements.
    """

    LENGTH = 4
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes8(FixedBytes):
    """
    Byte array of exactly eight elements.
    """

    LENGTH = 8
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes20(FixedBytes):
    """
    Byte array of exactly 20 elements.
    """

    LENGTH = 20
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes32(FixedBytes):
    """
    Byte array of exactly 32 elements.
    """

    LENGTH = 32
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes48(FixedBytes):
    """
    Byte array of exactly 48 elements.
    """

    LENGTH = 48


@mypyc_attr(native_class=False)
class Bytes64(FixedBytes):
    """
    Byte array of exactly 64 elements.
    """

    LENGTH = 64
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes96(FixedBytes):
    """
    Byte array of exactly 96 elements.
    """

    LENGTH = 96
    """
    Number of bytes in each instance of this class.
    """


@mypyc_attr(native_class=False)
class Bytes256(FixedBytes):
    """
    Byte array of exactly 256 elements.
    """

    LENGTH = 256
    """
    Number of bytes in each instance of this class.
    """


Bytes = bytes
"""
Sequence of bytes (octets) of arbitrary length.
"""
