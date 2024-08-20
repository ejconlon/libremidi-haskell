{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Libremidi.Foreign where

import Foreign.C (CInt (..))

#include "libremidi/libremidi-c.h"

type CTimestampMode = CInt

#{enum CTimestampMode
 , CInt
 , ctmNoTimestamp = NoTimestamp
 , ctmRelative = Relative
 , ctmAbsolute = Absolute
 , ctmSystemMonotonic = SystemMonotonic
 , ctmAudioFrame = AudioFrame
 , ctmCustom = Custom
}

type CApi = CInt

#{enum CApi
 , CInt
 , caUnspecified = UNSPECIFIED
 , caCoremidi = COREMIDI
 , caAlsaSeq = ALSA_SEQ
 , caAlsaRaw = ALSA_RAW
 , caJackMidi = JACK_MIDI
 , caWindowsMm = WINDOWS_MM
 , caWindowsUwp = WINDOWS_UWP
 , caWebmidi = WEBMIDI
 , caPipewire = PIPEWIRE
 , caAlsaRawUmp = ALSA_RAW_UMP
 , caAlsaSeqUmp = ALSA_SEQ_UMP
 , caCoremidiUmp = COREMIDI_UMP
 , caWindowsMidiServices = WINDOWS_MIDI_SERVICES
 , caDummy = DUMMY
}

type CConfigType = CInt

#{enum CConfigType
 , CInt
 , cctObserver = Observer
 , cctInput = Input
 , cctOutput = Output
}

type CVersion = CInt

#{enum CVersion
 , CInt
 , cvMidi1 = MIDI1
 , cvMidi2 = MIDI2
}

-- Example
-- foreign import ccall "libremidi/libremidi_c.h "
--   rtmidi_api_display_name :: ApiInternal -> IO CString

-- LIBREMIDI_EXPORT
-- int libremidi_midi_api_configuration_init(libremidi_api_configuration*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_observer_configuration_init(libremidi_observer_configuration*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_configuration_init(libremidi_midi_configuration*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_port_clone(const libremidi_midi_in_port* port, libremidi_midi_in_port** dst);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_port_free(libremidi_midi_in_port* port);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_port_name(
--     const libremidi_midi_in_port* port, const char** name, size_t* len);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_port_clone(
--     const libremidi_midi_out_port* port, libremidi_midi_out_port** dst);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_port_free(libremidi_midi_out_port* port);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_port_name(
--     const libremidi_midi_out_port* port, const char** name, size_t* len);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_observer_new(
--     const libremidi_observer_configuration*, libremidi_api_configuration*,
--     libremidi_midi_observer_handle**);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_observer_enumerate_input_ports(
--     libremidi_midi_observer_handle*, void* context,
--     void (*)(void* ctx, const libremidi_midi_in_port*));
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_observer_enumerate_output_ports(
--     libremidi_midi_observer_handle*, void* context,
--     void (*)(void* ctx, const libremidi_midi_out_port*));
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_observer_free(libremidi_midi_observer_handle*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_new(
--     const libremidi_midi_configuration*, const libremidi_api_configuration*,
--     libremidi_midi_in_handle**);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_is_connected(const libremidi_midi_in_handle*);
--
-- LIBREMIDI_EXPORT
-- libremidi_timestamp libremidi_midi_in_absolute_timestamp(libremidi_midi_in_handle*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_in_free(libremidi_midi_in_handle*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_new(
--     const libremidi_midi_configuration*, const libremidi_api_configuration*,
--     libremidi_midi_out_handle**);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_is_connected(const libremidi_midi_out_handle*);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_send_message(libremidi_midi_out_handle*, const midi1_symbol*, size_t);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_send_ump(libremidi_midi_out_handle*, const midi2_symbol*, size_t);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_schedule_message(
--     libremidi_midi_out_handle*, int64_t ts, const midi1_symbol*, size_t);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_schedule_ump(
--     libremidi_midi_out_handle*, int64_t ts, const midi2_symbol*, size_t);
--
-- LIBREMIDI_EXPORT
-- int libremidi_midi_out_free(libremidi_midi_out_handle*);
