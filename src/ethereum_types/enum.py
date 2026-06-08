"""
Support for enumerations.
"""

from enum import KEEP, Enum, EnumType, Flag, FlagBoundary
from typing import Any, Callable, SupportsInt, TypeVar, Union

from typing_extensions import assert_never, override

from .numeric import Uint

_T = TypeVar("_T")


def _copy_type(f: _T) -> Callable[[Any], _T]:
    # See: https://github.com/python/typing/issues/769#issuecomment-903760354
    del f
    return lambda x: x


class _UintEnumType(EnumType, type):
    @_copy_type(EnumType.__call__)
    @override
    def __call__(cls, value, *args, **kwargs):  # type: ignore[no-untyped-def]
        # Allow syntax like `MyUintEnum(3)` instead of `MyUintEnum(Uint(3))`.
        # Required because we implement the dunder methods in `Unsigned` as
        # `Self(int + int)`.
        return super().__call__(Uint(value), *args, **kwargs)


class UintEnum(Uint, Enum, metaclass=_UintEnumType):
    """
    Base class for creating enumerated constants that are also subclasses of
    [`Uint`].

    `UintEnum` is similar to [`IntEnum`], but its members are subclasses of
    [`Uint`] instead of `int`.

    **Note:** Unlike [`IntEnum`], `UintEnum` integer operations that would
    result in a numeric value without a corresponding enumeration member raise
    a `ValueError` instead of silently converting to a [`Uint`].

    ```python
    >>> from ethereum_types.numeric import Uint
    >>> from ethereum_types.enum import UintEnum
    >>> class Color(UintEnum):
    ...     RED = Uint(1)
    ...     BLUE = Uint(2)
    ...
    >>> Color.RED
    <Color.RED: 1>
    ```

    [`Uint`]: ref:ethereum_types.numeric.Uint
    [`IntEnum`]: https://docs.python.org/3/library/enum.html#enum.IntEnum
    """

    __repr__ = Enum.__repr__


class _UintFlagType(_UintEnumType):
    _boundary_: FlagBoundary

    @_copy_type(EnumType.__call__)
    @override
    def __call__(cls, value, *args, **kwargs):  # type: ignore[no-untyped-def]
        result = super().__call__(value, *args, **kwargs)
        if isinstance(result, int):
            assert cls._boundary_ is not KEEP
            value = result
        else:
            if cls._boundary_ is KEEP or result._name_ in cls.__members__:
                return result
            value = result._value_

        if cls._boundary_ is FlagBoundary.EJECT:
            return Uint(value)

        if value == 0:
            member: UintFlag = Uint.__new__(cls)  # type: ignore[arg-type]
            Uint.__init__(member, 0)
            member._value_ = 0
            member._name_ = "None"
            return member

        return result

        assert_never(cls._boundary_)  # pragma: nocover


class UintFlag(Uint, Flag, metaclass=_UintFlagType, boundary=KEEP):
    def __new__(cls, value: SupportsInt) -> "UintFlag":
        member = Uint.__new__(cls)
        Uint.__init__(member, value)
        member._value_ = int(value)
        return member

    @classmethod
    @override
    def _missing_(cls, value: object) -> Union[None, "Uint", int]:
        if not isinstance(value, SupportsInt):
            return None

        int_value = int(value)
        missed = super()._missing_(int_value)
        if isinstance(missed, UintFlag):
            UintFlag.__init__(missed, int_value)
            return missed

        assert isinstance(missed, (Uint, int))
        return missed

    @override
    def __repr__(self) -> str:
        value = Uint(self._value_)
        return f"<{self.__class__.__name__}.{self._name_}: {value!r}>"
