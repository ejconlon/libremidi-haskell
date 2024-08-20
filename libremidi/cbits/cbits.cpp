#include "cbits.h"
#include "libremidi/libremidi-c.h"

void cbits_in_port_free(libremidi_midi_in_port* ptr) {
  libremidi_midi_in_port_free(ptr);
}

void cbits_out_port_free(libremidi_midi_out_port* ptr) {
  libremidi_midi_out_port_free(ptr);
}

void cbits_observer_free(libremidi_midi_observer_handle* ptr) {
  libremidi_midi_observer_free(ptr);
}

void cbits_in_free(libremidi_midi_in_handle* ptr) {
  libremidi_midi_in_free(ptr);
}

void cbits_out_free(libremidi_midi_out_handle* ptr) {
  libremidi_midi_out_free(ptr);
}
