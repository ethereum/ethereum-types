from enum import Enum, FlagBoundary, auto

import pytest
from typing_extensions import assert_never

from ethereum_types.enum import UintEnum, UintFlag
from ethereum_types.numeric import Uint


def test_enum_subclass() -> None:
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


def test_flag_subclass() -> None:
    class _MyFlag(UintFlag):
        A = Uint(1)
        B = Uint(2)

    assert isinstance(_MyFlag.A, Uint)
    assert _MyFlag.A == Uint(1)

    assert isinstance(_MyFlag.B, Uint)
    assert _MyFlag.B == Uint(2)

    assert len(_MyFlag) == 2
    assert _MyFlag.__members__ == {"A": Uint(1), "B": Uint(2)}

    actual = _MyFlag(Uint(1))
    assert actual is _MyFlag.A


def test_enum_subclass_cast() -> None:
    class _MyEnum(UintEnum):
        A = 1
        B = 2

    assert isinstance(_MyEnum.A, Uint)
    assert _MyEnum.A == Uint(1)

    assert isinstance(_MyEnum.B, Uint)
    assert _MyEnum.B == Uint(2)

    actual = _MyEnum(Uint(1))
    assert actual is _MyEnum.A


def test_flag_subclass_cast() -> None:
    class _MyFlag(UintFlag):
        A = 1
        B = 2

    assert isinstance(_MyFlag.A, Uint)
    assert _MyFlag.A == Uint(1)

    assert isinstance(_MyFlag.B, Uint)
    assert _MyFlag.B == Uint(2)

    actual = _MyFlag(Uint(1))
    assert actual is _MyFlag.A


def test_enum_auto() -> None:
    class _MyEnum(UintEnum):
        A = auto()
        B = auto()

    assert isinstance(_MyEnum.A, Uint)
    assert _MyEnum.A == Uint(1)

    assert isinstance(_MyEnum.B, Uint)
    assert _MyEnum.B == Uint(2)


def test_flag_auto() -> None:
    class _MyFlag(UintFlag):
        A = auto()
        B = auto()
        C = auto()

    assert isinstance(_MyFlag.A, Uint)
    assert _MyFlag.A == Uint(1)

    assert isinstance(_MyFlag.B, Uint)
    assert _MyFlag.B == Uint(2)

    assert isinstance(_MyFlag.C, Uint)
    assert _MyFlag.C == Uint(4)


def test_enum_add() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)
        B = Uint(2)

    actual = _MyEnum.A + _MyEnum.A
    assert actual is _MyEnum.B


def test_flag_add() -> None:
    class _MyFlag(UintFlag):
        A = Uint(1)
        B = Uint(2)

    actual = _MyFlag.A + _MyFlag.A
    assert actual is _MyFlag.B


def test_enum_add_not_member() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)
        B = Uint(2)

    with pytest.raises(ValueError, match="is not a valid"):
        _MyEnum.A + _MyEnum.B


class Result(Enum):
    FLAG = auto()
    NUMBER = auto()


OPS = [
    "__add__",
    "__radd__",
    "__or__",
    "__ror__",
    "__xor__",
    "__rxor__",
    "__mul__",
]


@pytest.mark.parametrize("op", OPS)
@pytest.mark.parametrize(
    ("boundary", "result_type"),
    [
        (FlagBoundary.KEEP, Result.FLAG),
        (FlagBoundary.EJECT, Result.NUMBER),
        (FlagBoundary.CONFORM, Result.FLAG),
    ],
)
def test_flag_op_not_member_boundary(
    op: str, boundary: FlagBoundary, result_type: Result
) -> None:
    class _MyUintFlag(UintFlag, boundary=boundary):
        A = Uint(2)
        B = Uint(4)

    actual = getattr(_MyUintFlag, op)(_MyUintFlag.A, _MyUintFlag.B)

    assert isinstance(actual, Uint)
    if result_type is Result.FLAG:
        assert isinstance(actual, UintFlag)
    elif result_type is Result.NUMBER:
        assert not isinstance(actual, UintFlag)
    else:
        assert_never(result_type)


@pytest.mark.parametrize("op", OPS)
def test_flag_op_member_boundary_strict(op: str) -> None:
    class _MyUintFlag(UintFlag, boundary=FlagBoundary.STRICT):
        A = Uint(1)
        B = Uint(4)

    getattr(_MyUintFlag, op)(_MyUintFlag.A, _MyUintFlag.B)


def test_flag_op_not_member_boundary_strict() -> None:
    class _MyUintFlag(UintFlag, boundary=FlagBoundary.STRICT):
        A = Uint(1)

    with pytest.raises(ValueError, match="invalid value"):
        _MyUintFlag.A + _MyUintFlag.A


def test_flag_conform_not_member() -> None:
    class _MyUintFlag(UintFlag, boundary=FlagBoundary.CONFORM):
        A = Uint(1)
        B = Uint(2)

    actual = _MyUintFlag(4)

    assert isinstance(actual, _MyUintFlag)
    assert actual == Uint(0)


def test_enum_not_member() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)

    with pytest.raises(ValueError, match="is not a valid"):
        _MyEnum(3)


def test_flag_not_member_boundary_keep() -> None:
    class _MyFlag(UintFlag):
        A = Uint(1)

    actual = _MyFlag(3)
    assert isinstance(actual, _MyFlag)
    assert actual == Uint(3)


def test_flag_not_member_boundary_conform() -> None:
    class _MyFlag(UintFlag, boundary=FlagBoundary.CONFORM):
        A = Uint(1)

    actual = _MyFlag(3)
    assert isinstance(actual, _MyFlag)
    assert actual is _MyFlag(1)


def test_flag_not_member_boundary_eject() -> None:
    class _MyFlag(UintFlag, boundary=FlagBoundary.EJECT):
        A = Uint(1)

    actual = _MyFlag(3)
    assert isinstance(actual, Uint)
    assert not isinstance(actual, UintFlag)


def test_flag_not_member_boundary_strict() -> None:
    class _MyFlag(UintFlag, boundary=FlagBoundary.STRICT):
        A = Uint(1)

    with pytest.raises(ValueError, match="invalid value 3"):
        _MyFlag(3)


def test_enum_repr() -> None:
    class _MyEnum(UintEnum):
        A = Uint(1)

    assert repr(_MyEnum.A) == "<_MyEnum.A: Uint(1)>"


def test_flag_repr() -> None:
    class _MyFlag(UintFlag):
        A = Uint(1)

    assert repr(_MyFlag.A) == "<_MyFlag.A: Uint(1)>"


def test_flag_missing_not_uint() -> None:
    assert UintFlag._missing_(None) is None
