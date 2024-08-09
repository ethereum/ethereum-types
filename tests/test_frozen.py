from dataclasses import dataclass

import pytest

from ethereum_types.frozen import modify, slotted_freezable


@slotted_freezable
@dataclass
class Mock:
    a: int
    b: str


def test_slotted_freezable_init() -> None:
    positional = Mock(1, "2")
    assert positional.a == 1
    assert positional.b == "2"

    keyword = Mock(b="2", a=1)
    assert keyword.a == 1
    assert keyword.b == "2"


def test_slotted_freezable_frozen() -> None:
    value = Mock(1, "2")
    with pytest.raises(AttributeError):
        value.a = 2

    with pytest.raises(AttributeError):
        value.b = "2"

    with pytest.raises(AttributeError):
        del value.a


def test_slotted_freezable_init_thawed_frozen() -> None:
    value = Mock(1, "2", _frozen=False)  # type: ignore[call-arg]
    value.a = 2
    value.b = "2"
    del value.a


def test_slotted_freezable_modify() -> None:
    before = Mock(1, "2")

    def f(x: Mock) -> None:
        x.a = 3

    after = modify(before, f)

    assert before.a == 1
    assert after.a == 3
    assert before.b == after.b
    assert after is not before
