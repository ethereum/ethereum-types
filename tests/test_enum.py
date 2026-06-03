from enum import auto

import pytest

from ethereum_types.enum import UintEnum
from ethereum_types.numeric import Uint


def test_subclass() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)
        B = Uint(2)

    assert isinstance(_MyEnum.A, Uint)
    assert _MyEnum.A == Uint(1)

    assert isinstance(_MyEnum.B, Uint)
    assert _MyEnum.B == Uint(2)

    assert len(_MyEnum) == 2
    assert _MyEnum.__members__ == {"A": Uint(1), "B": Uint(2)}

    actual = _MyEnum(Uint(1))
    assert actual is _MyEnum.A


@pytest.mark.xfail(strict=True, reason="see ethereum/ethereum-types#21")
def test_subclass_cast() -> None:
    class _MyEnum(UintEnum):
        A = 1
        B = 2

    assert isinstance(_MyEnum.A, Uint)
    assert _MyEnum.A == Uint(1)

    assert isinstance(_MyEnum.B, Uint)
    assert _MyEnum.B == Uint(2)

    actual = _MyEnum(Uint(1))
    assert actual is _MyEnum.A


def test_auto() -> None:
    class _MyEnum(UintEnum):
        A = auto()
        B = auto()

    assert isinstance(_MyEnum.A, Uint)
    assert _MyEnum.A == Uint(1)

    assert isinstance(_MyEnum.B, Uint)
    assert _MyEnum.B == Uint(2)


def test_add() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)
        B = Uint(2)

    actual = _MyEnum.A + _MyEnum.A
    assert actual is _MyEnum.B


def test_add_not_member() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)
        B = Uint(2)

    with pytest.raises(ValueError, match="is not a valid"):
        _MyEnum.A + _MyEnum.B


def test_not_member() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)

    with pytest.raises(ValueError, match="is not a valid"):
        _MyEnum(3)


def test_repr() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)

    assert repr(_MyEnum.A) == "<_MyEnum.A: Uint(1)>"
