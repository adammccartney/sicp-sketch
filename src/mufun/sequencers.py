#!/usr/bin/env python3

import itertools

__doc__ = """Sketch to play with a few generator functions that generate musical
sequences in the lilypond syntax"""


def inf_sequencer(notes, rhythm):
    "Generator that creates musical loop"
    inf_notes = itertools.cycle(notes)
    inf_rhythm = itertools.cycle(rhythm)
    return map(lambda a, b: f"{a}{b}", inf_notes, inf_rhythm) 



