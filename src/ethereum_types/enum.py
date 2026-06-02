"""
Support for enumerations.
"""

from enum import Enum, EnumType
from typing import Any, Callable, TypeVar

from typing_extensions import override

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

    @override
    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}.{self._name_}: {self._value_!r}>"
