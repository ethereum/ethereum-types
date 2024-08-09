import pytest

from ethereum_types.bytes import FixedBytes


def test_fixed_bytes_init_too_short() -> None:
    class TestBytes(FixedBytes):
        LENGTH = 5

    with pytest.raises(ValueError):
        TestBytes(b"\0")


def test_fixed_bytes_init_too_long() -> None:
    class TestBytes(FixedBytes):
        LENGTH = 5

    with pytest.raises(ValueError):
        TestBytes(b"\0" * 6)


def test_fixed_bytes_init() -> None:
    class TestBytes(FixedBytes):
        LENGTH = 5

    tb = TestBytes(b"\0" * 5)
    assert tb == b"\0\0\0\0\0"


def test_fixed_bytes_init_bytearray() -> None:
    class TestBytes(FixedBytes):
        LENGTH = 5

    tb = TestBytes(bytearray([0, 0, 0, 0, 0]))
    assert tb == b"\0\0\0\0\0"
    assert isinstance(tb, bytes)
    assert not isinstance(tb, bytearray)  # type: ignore[unreachable]


def test_fixed_bytes_concat() -> None:
    class TestBytes(FixedBytes):
        LENGTH = 5

    tb0 = TestBytes(b"\0" * 5)
    tb1 = TestBytes(b"1" * 5)

    tb = tb0 + tb1

    assert tb == b"\x00\x00\x00\x00\x0011111"
    assert isinstance(tb, bytes)
    assert not isinstance(tb, TestBytes)
