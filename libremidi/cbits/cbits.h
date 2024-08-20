#pragma once
#include "libremidi/libremidi-c.h"

#if __cplusplus
extern "C" {
#endif

void cbits_in_port_free(libremidi_midi_in_port*);

void cbits_out_port_free(libremidi_midi_out_port*);

void cbits_observer_free(libremidi_midi_observer_handle*);

void cbits_in_free(libremidi_midi_in_handle*);

void cbits_out_free(libremidi_midi_out_handle*);

#if __cplusplus
}
#endif
