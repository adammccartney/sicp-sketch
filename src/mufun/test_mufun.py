import pytest

from sequencers import inf_sequencer

@pytest.fixture
def notes():
    return "cdfe"

@pytest.fixture
def rhythm():
    return ["4.", "8", "4"]

def test_inf_sequencer(notes, rhythm):
    "Test that inf sequencer generates a reliable sequence"
    expected = "c4. d8 f4 e4. c8 d4 f4. e8 c4 d4."
    inf_seq = inf_sequencer(notes, rhythm)
    melody = []
    for _ in range(10):
        melody.append(next(inf_seq))
    result = " ".join(melody)
    assert expected == result 

