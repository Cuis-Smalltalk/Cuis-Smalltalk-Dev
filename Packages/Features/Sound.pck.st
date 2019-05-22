'From Cuis 5.0 of 7 November 2016 [latest update: #3695] on 11 April 2019 at 3:36:06 pm'!
'Description '!
!provides: 'Sound' 1 23!
!requires: 'Collections-CompactArrays' 1 2 nil!
!requires: 'Compression' 1 nil nil!
SystemOrganization addCategory: #'Sound-Synthesis'!
SystemOrganization addCategory: #'Sound-Scores'!
SystemOrganization addCategory: #'Sound-System-Support'!


!classDefinition: #SoundBuffer category: #'Sound-Synthesis'!
ArrayedCollection variableWordSubclass: #SoundBuffer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SoundBuffer class' category: #'Sound-Synthesis'!
SoundBuffer class
	instanceVariableNames: ''!

!classDefinition: #Sonogram category: #'Sound-Synthesis'!
ImageMorph subclass: #Sonogram
	instanceVariableNames: 'lastX scrollDelta columnForm minVal maxVal pixValMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'Sonogram class' category: #'Sound-Synthesis'!
Sonogram class
	instanceVariableNames: ''!

!classDefinition: #AIFFFileReader category: #'Sound-Synthesis'!
Object subclass: #AIFFFileReader
	instanceVariableNames: 'in fileType channelCount frameCount bitsPerSample samplingRate channelData channelDataOffset markers pitch gain isLooped skipDataChunk mergeIfStereo'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'AIFFFileReader class' category: #'Sound-Synthesis'!
AIFFFileReader class
	instanceVariableNames: ''!

!classDefinition: #AbstractSound category: #'Sound-Synthesis'!
Object subclass: #AbstractSound
	instanceVariableNames: 'envelopes mSecsSinceStart samplesUntilNextControl scaledVol scaledVolIncr scaledVolLimit'
	classVariableNames: 'FloatScaleFactor MaxScaledValue PitchesForBottomOctave ScaleFactor Sounds TopOfBottomOctave UnloadedSnd'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'AbstractSound class' category: #'Sound-Synthesis'!
AbstractSound class
	instanceVariableNames: ''!

!classDefinition: #FMSound category: #'Sound-Synthesis'!
AbstractSound subclass: #FMSound
	instanceVariableNames: 'initialCount count waveTable scaledWaveTableSize scaledIndex scaledIndexIncr modulation multiplier normalizedModulation scaledOffsetIndex scaledOffsetIndexIncr'
	classVariableNames: 'SineTable'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'FMSound class' category: #'Sound-Synthesis'!
FMSound class
	instanceVariableNames: ''!

!classDefinition: #FMBassoonSound category: #'Sound-Synthesis'!
FMSound subclass: #FMBassoonSound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'FMBassoonSound class' category: #'Sound-Synthesis'!
FMBassoonSound class
	instanceVariableNames: ''!

!classDefinition: #FMClarinetSound category: #'Sound-Synthesis'!
FMSound subclass: #FMClarinetSound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'FMClarinetSound class' category: #'Sound-Synthesis'!
FMClarinetSound class
	instanceVariableNames: ''!

!classDefinition: #UnloadedSound category: #'Sound-Synthesis'!
FMSound subclass: #UnloadedSound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'UnloadedSound class' category: #'Sound-Synthesis'!
UnloadedSound class
	instanceVariableNames: ''!

!classDefinition: #LoopedSampledSound category: #'Sound-Synthesis'!
AbstractSound subclass: #LoopedSampledSound
	instanceVariableNames: 'initialCount count releaseCount sampleCountForRelease leftSamples rightSamples originalSamplingRate perceivedPitch gain firstSample lastSample loopEnd scaledLoopLength scaledIndex scaledIndexIncr'
	classVariableNames: 'FloatLoopIndexScaleFactor LoopIndexFractionMask LoopIndexScaleFactor'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'LoopedSampledSound class' category: #'Sound-Synthesis'!
LoopedSampledSound class
	instanceVariableNames: ''!

!classDefinition: #MixedSound category: #'Sound-Synthesis'!
AbstractSound subclass: #MixedSound
	instanceVariableNames: 'sounds leftVols rightVols soundDone'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'MixedSound class' category: #'Sound-Synthesis'!
MixedSound class
	instanceVariableNames: ''!

!classDefinition: #PluckedSound category: #'Sound-Synthesis'!
AbstractSound subclass: #PluckedSound
	instanceVariableNames: 'initialCount count ring scaledIndex scaledIndexIncr scaledIndexLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'PluckedSound class' category: #'Sound-Synthesis'!
PluckedSound class
	instanceVariableNames: ''!

!classDefinition: #QueueSound category: #'Sound-Synthesis'!
AbstractSound subclass: #QueueSound
	instanceVariableNames: 'startTime sounds currentSound done'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'QueueSound class' category: #'Sound-Synthesis'!
QueueSound class
	instanceVariableNames: ''!

!classDefinition: #RepeatingSound category: #'Sound-Synthesis'!
AbstractSound subclass: #RepeatingSound
	instanceVariableNames: 'sound iterationCount iteration samplesPerIteration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'RepeatingSound class' category: #'Sound-Synthesis'!
RepeatingSound class
	instanceVariableNames: ''!

!classDefinition: #RestSound category: #'Sound-Synthesis'!
AbstractSound subclass: #RestSound
	instanceVariableNames: 'initialCount count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'RestSound class' category: #'Sound-Synthesis'!
RestSound class
	instanceVariableNames: ''!

!classDefinition: #ReverbSound category: #'Sound-Synthesis'!
AbstractSound subclass: #ReverbSound
	instanceVariableNames: 'sound tapDelays tapGains tapCount bufferSize bufferIndex leftBuffer rightBuffer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'ReverbSound class' category: #'Sound-Synthesis'!
ReverbSound class
	instanceVariableNames: ''!

!classDefinition: #SampledSound category: #'Sound-Synthesis'!
AbstractSound subclass: #SampledSound
	instanceVariableNames: 'initialCount count samples originalSamplingRate samplesSize scaledIndex indexHighBits scaledIncrement'
	classVariableNames: 'CoffeeCupClink DefaultSampleTable IncrementFractionBits IncrementScaleFactor NominalSamplePitch ScaledIndexOverflow SoundLibrary'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SampledSound class' category: #'Sound-Synthesis'!
SampledSound class
	instanceVariableNames: ''!

!classDefinition: #SequentialSound category: #'Sound-Synthesis'!
AbstractSound subclass: #SequentialSound
	instanceVariableNames: 'sounds currentIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SequentialSound class' category: #'Sound-Synthesis'!
SequentialSound class
	instanceVariableNames: ''!

!classDefinition: #StreamingMonoSound category: #'Sound-Synthesis'!
AbstractSound subclass: #StreamingMonoSound
	instanceVariableNames: 'stream volume repeat headerStart audioDataStart streamSamplingRate totalSamples codec mixer leftoverSamples lastBufferMSecs mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'StreamingMonoSound class' category: #'Sound-Synthesis'!
StreamingMonoSound class
	instanceVariableNames: ''!

!classDefinition: #ScorePlayer category: #'Sound-Scores'!
AbstractSound subclass: #ScorePlayer
	instanceVariableNames: 'score instruments overallVolume leftVols rightVols muted rate tempo secsPerTick done repeat ticksSinceStart ticksClockIncr trackEventIndex tempoMapIndex activeSounds activeMIDINotes midiPort midiPlayerProcess durationInTicks'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'ScorePlayer class' category: #'Sound-Scores'!
ScorePlayer class
	instanceVariableNames: ''!

!classDefinition: #CompressedSoundData category: #'Sound-Synthesis'!
Object subclass: #CompressedSoundData
	instanceVariableNames: 'channels soundClassName codecName loopEnd loopLength perceivedPitch samplingRate gain firstSample cachedSound'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'CompressedSoundData class' category: #'Sound-Synthesis'!
CompressedSoundData class
	instanceVariableNames: ''!

!classDefinition: #Envelope category: #'Sound-Synthesis'!
Object subclass: #Envelope
	instanceVariableNames: 'points loopStartIndex loopEndIndex loopStartMSecs loopMSecs target updateSelector loopEndMSecs endMSecs scale decayScale lastValue currValue valueIncr nextRecomputeTime noChangesDuringLoop'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'Envelope class' category: #'Sound-Synthesis'!
Envelope class
	instanceVariableNames: ''!

!classDefinition: #PitchEnvelope category: #'Sound-Synthesis'!
Envelope subclass: #PitchEnvelope
	instanceVariableNames: 'centerPitch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'PitchEnvelope class' category: #'Sound-Synthesis'!
PitchEnvelope class
	instanceVariableNames: ''!

!classDefinition: #RandomEnvelope category: #'Sound-Synthesis'!
Envelope subclass: #RandomEnvelope
	instanceVariableNames: 'rand lowLimit highLimit delta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'RandomEnvelope class' category: #'Sound-Synthesis'!
RandomEnvelope class
	instanceVariableNames: ''!

!classDefinition: #VolumeEnvelope category: #'Sound-Synthesis'!
Envelope subclass: #VolumeEnvelope
	instanceVariableNames: 'targetVol mSecsForChange'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'VolumeEnvelope class' category: #'Sound-Synthesis'!
VolumeEnvelope class
	instanceVariableNames: ''!

!classDefinition: #SampledInstrument category: #'Sound-Synthesis'!
Object subclass: #SampledInstrument
	instanceVariableNames: 'sustainedSoft sustainedLoud staccatoSoft staccatoLoud sustainedThreshold loudThreshold'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SampledInstrument class' category: #'Sound-Synthesis'!
SampledInstrument class
	instanceVariableNames: ''!

!classDefinition: #SoundCodec category: #'Sound-Synthesis'!
Object subclass: #SoundCodec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SoundCodec class' category: #'Sound-Synthesis'!
SoundCodec class
	instanceVariableNames: ''!

!classDefinition: #ADPCMCodec category: #'Sound-Synthesis'!
SoundCodec subclass: #ADPCMCodec
	instanceVariableNames: 'predicted index deltaSignMask deltaValueMask deltaValueHighBit frameSizeMask currentByte bitPosition byteIndex encodedBytes samples rightSamples sampleIndex bitsPerSample stepSizeTable indexTable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'ADPCMCodec class' category: #'Sound-Synthesis'!
ADPCMCodec class
	instanceVariableNames: ''!

!classDefinition: #GSMCodec category: #'Sound-Synthesis'!
SoundCodec subclass: #GSMCodec
	instanceVariableNames: 'encodeState decodeState'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'GSMCodec class' category: #'Sound-Synthesis'!
GSMCodec class
	instanceVariableNames: ''!

!classDefinition: #MuLawCodec category: #'Sound-Synthesis'!
SoundCodec subclass: #MuLawCodec
	instanceVariableNames: ''
	classVariableNames: 'DecodingTable'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'MuLawCodec class' category: #'Sound-Synthesis'!
MuLawCodec class
	instanceVariableNames: ''!

!classDefinition: #SoundPlayer category: #'Sound-Synthesis'!
Object subclass: #SoundPlayer
	instanceVariableNames: ''
	classVariableNames: 'ActiveSounds Buffer BufferIndex BufferMSecs LastBuffer PlayerProcess PlayerSemaphore ReadyForBuffer ReverbState SamplingRate SoundJustStarted SoundSupported Stereo UseReadySemaphore UseReverb'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SoundPlayer class' category: #'Sound-Synthesis'!
SoundPlayer class
	instanceVariableNames: ''!

!classDefinition: #SoundRecorder category: #'Sound-Synthesis'!
Object subclass: #SoundRecorder
	instanceVariableNames: 'stereo samplingRate recordLevel recordedBuffers recordedSound recordProcess bufferAvailableSema paused meteringBuffer meterLevel soundPlaying currentBuffer nextIndex codec desiredSampleRate'
	classVariableNames: 'CanRecordWhilePlaying RecorderActive'
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SoundRecorder class' category: #'Sound-Synthesis'!
SoundRecorder class
	instanceVariableNames: ''!

!classDefinition: #SoundInputStream category: #'Sound-Synthesis'!
SoundRecorder subclass: #SoundInputStream
	instanceVariableNames: 'bufferSize mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Synthesis'!
!classDefinition: 'SoundInputStream class' category: #'Sound-Synthesis'!
SoundInputStream class
	instanceVariableNames: ''!

!classDefinition: #AbstractScoreEvent category: #'Sound-Scores'!
Object subclass: #AbstractScoreEvent
	instanceVariableNames: 'time'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'AbstractScoreEvent class' category: #'Sound-Scores'!
AbstractScoreEvent class
	instanceVariableNames: ''!

!classDefinition: #ControlChangeEvent category: #'Sound-Scores'!
AbstractScoreEvent subclass: #ControlChangeEvent
	instanceVariableNames: 'control value channel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'ControlChangeEvent class' category: #'Sound-Scores'!
ControlChangeEvent class
	instanceVariableNames: ''!

!classDefinition: #NoteEvent category: #'Sound-Scores'!
AbstractScoreEvent subclass: #NoteEvent
	instanceVariableNames: 'duration midiKey velocity channel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'NoteEvent class' category: #'Sound-Scores'!
NoteEvent class
	instanceVariableNames: ''!

!classDefinition: #PitchBendEvent category: #'Sound-Scores'!
AbstractScoreEvent subclass: #PitchBendEvent
	instanceVariableNames: 'bend channel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'PitchBendEvent class' category: #'Sound-Scores'!
PitchBendEvent class
	instanceVariableNames: ''!

!classDefinition: #ProgramChangeEvent category: #'Sound-Scores'!
AbstractScoreEvent subclass: #ProgramChangeEvent
	instanceVariableNames: 'program channel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'ProgramChangeEvent class' category: #'Sound-Scores'!
ProgramChangeEvent class
	instanceVariableNames: ''!

!classDefinition: #TempoEvent category: #'Sound-Scores'!
AbstractScoreEvent subclass: #TempoEvent
	instanceVariableNames: 'tempo'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'TempoEvent class' category: #'Sound-Scores'!
TempoEvent class
	instanceVariableNames: ''!

!classDefinition: #MIDIFileReader category: #'Sound-Scores'!
Object subclass: #MIDIFileReader
	instanceVariableNames: 'stream fileType trackCount ticksPerQuarter tracks trackInfo tempoMap strings track trackStream activeEvents maxNoteTicks'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'MIDIFileReader class' category: #'Sound-Scores'!
MIDIFileReader class
	instanceVariableNames: ''!

!classDefinition: #MIDIInputParser category: #'Sound-Scores'!
Object subclass: #MIDIInputParser
	instanceVariableNames: 'cmdActionTable midiPort received rawDataBuffer sysExBuffer ignoreSysEx startTime timeNow state lastSelector lastCmdByte argByte1 argByte2'
	classVariableNames: 'DefaultMidiTable'
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'MIDIInputParser class' category: #'Sound-Scores'!
MIDIInputParser class
	instanceVariableNames: ''!

!classDefinition: #MIDIScore category: #'Sound-Scores'!
Object subclass: #MIDIScore
	instanceVariableNames: 'tracks trackInfo tempoMap ticksPerQuarterNote'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'MIDIScore class' category: #'Sound-Scores'!
MIDIScore class
	instanceVariableNames: ''!

!classDefinition: #MIDISynth category: #'Sound-Scores'!
Object subclass: #MIDISynth
	instanceVariableNames: 'midiParser channels process'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'MIDISynth class' category: #'Sound-Scores'!
MIDISynth class
	instanceVariableNames: ''!

!classDefinition: #MIDISynthChannel category: #'Sound-Scores'!
Object subclass: #MIDISynthChannel
	instanceVariableNames: 'instrument muted masterVolume channelVolume pan pitchBend activeSounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'MIDISynthChannel class' category: #'Sound-Scores'!
MIDISynthChannel class
	instanceVariableNames: ''!

!classDefinition: #SimpleMIDIPort category: #'Sound-Scores'!
Object subclass: #SimpleMIDIPort
	instanceVariableNames: 'portNumber accessSema lastCommandByteOut'
	classVariableNames: 'DefaultPortNumber InterfaceClockRate'
	poolDictionaries: ''
	category: 'Sound-Scores'!
!classDefinition: 'SimpleMIDIPort class' category: #'Sound-Scores'!
SimpleMIDIPort class
	instanceVariableNames: ''!

!classDefinition: #Beeper category: #'Sound-System-Support'!
Object subclass: #Beeper
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Sound-System-Support'!
!classDefinition: 'Beeper class' category: #'Sound-System-Support'!
Beeper class
	instanceVariableNames: 'default'!

!classDefinition: #SoundSystem category: #'Sound-System-Support'!
Object subclass: #SoundSystem
	instanceVariableNames: ''
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Sound-System-Support'!
!classDefinition: 'SoundSystem class' category: #'Sound-System-Support'!
SoundSystem class
	instanceVariableNames: ''!


!SoundBuffer commentStamp: '<historical>' prior: 0!
SoundBuffers store 16 bit unsigned quantities.

Array size must be even, as two vales are stored in each word.!

!Sonogram commentStamp: '<historical>' prior: 0!
Sonograms are imageMorphs that will repeatedly plot arrays of values as black on white columns moving to the right in time and scrolling left as necessary.!

!AIFFFileReader commentStamp: '<historical>' prior: 0!
I am a parser for AIFF (audio interchange file format) files. I can read uncompressed 8-bit and 16-bit mono, stereo, or multichannel AIFF files. I read the marker information used by the TransferStation utility to mark the loop points in sounds extracted from commercial sampled-sound CD-ROMs.
!

!UnloadedSound commentStamp: '<historical>' prior: 0!
Instances of me, which are really just FMSounds, are used placeholders for sounds that have been unloaded from this image but which may be re-loaded later.!

!LoopedSampledSound commentStamp: '<historical>' prior: 0!
I respresent a sequence of sound samples, often used to record a single note played by a real instrument. I can be pitch-shifted up or down, and can include a looped portion to allow a sound to be sustained indefinitely.
!

!PluckedSound commentStamp: '<historical>' prior: 0!
The Karplus-Strong plucked string algorithm: start with a buffer full of random noise and repeatedly play the contents of that buffer while averaging adjacent samples. High harmonics damp out more quickly, transfering their energy to lower ones. The length of the buffer corresponds to the length of the string. Fractional indexing is used to allow precise tuning; without this, the pitch would be rounded to the pitch corresponding to the nearest buffer size.
!

!QueueSound commentStamp: 'efc 1/2/2003 00:30' prior: 0!
I am a queue for sound - give me a bunch of sounds to play and I will play them one at a time in the order that they are received.

Example:
"Here is a simple example which plays two sounds three times."
| clink warble queue |
clink _ SampledSound soundNamed: 'clink'.
warble _ SampledSound soundNamed: 'warble'.
queue _ QueueSound new.
3 timesRepeat:[
	queue add: clink; add: warble
].
queue play.

Structure:
 startTime 		Integer -- if present, start playing when startTime <= Time millisecondClockValue
							(schedule the sound to play later)
 sounds			SharedQueue -- the synchronized list of sounds.
 currentSound	AbstractSound -- the currently active sound
 done			Boolean -- am I done playing ?

Other:
You may want to keep track of the queue's position so that you can feed it at an appropriate rate. To do this in an event driven way, modify or subclass nextSound to notify you when appropriate. You could also poll by checking currentSound, but this is not recommended for most applications.

!

!StreamingMonoSound commentStamp: '<historical>' prior: 0!
I implement a streaming player for monophonic Sun (.au) and AIFF (.aif) audio files.
Example of use:
	(StreamingMonoSound onFileNamed: 'song.aif') play.
!

!ScorePlayer commentStamp: '<historical>' prior: 0!
This is a real-time player for MIDI scores (i.e., scores read from MIDI files). Score can be played using either the internal sound synthesis or an external MIDI synthesizer on platforms that support MIDI output.
!

!CompressedSoundData commentStamp: '<historical>' prior: 0!
Instances of this class hold the data resulting from compressing a sound.  Each carries a reference to the codec class that created it, so that it can reconstruct a sound similar to the original in response to the message asSound.

In order to facilitate integration with existing sounds, a CompressedSoundData instance can masquerade as a sound by caching a copy of its original sound and delegating the essential sound-playing protocol to that cached copy.  It should probably be made a subclass of AbstractSound to complete the illusion.!

!Envelope commentStamp: '<historical>' prior: 0!
An envelope models a three-stage progression for a musical note: attack, sustain, decay. Envelopes can either return the envelope value at a given time or can update some target object using a client-specified message selector.

The points instance variable holds an array of (time, value) points, where the times are in milliseconds. The points array must contain at least two points. The time coordinate of the first point must be zero and the time coordinates of subsequent points must be in ascending order, although the spacing between them is arbitrary. Envelope values between points are computed by linear interpolation.

The scale slot is initially set so that the peak of envelope matches some note attribute, such as its loudness. When entering the decay phase, the scale is adjusted so that the decay begins from the envelope's current value. This avoids a potential sharp transient when entering the decay phase.

The loopStartIndex and loopEndIndex slots contain the indices of points in the points array; if they are equal, then the envelope holds a constant value for the sustain phase of the note. Otherwise, envelope values are computed by repeatedly looping between these two points.

The loopEndMSecs slot can be set in advance (as when playing a score) or dynamically (as when responding to interactive inputs from a MIDI keyboard). In the latter case, the value of scale is adjusted to start the decay phase with the current envelope value. Thus, if a note ends before its attack is complete, the decay phase is started immediately (i.e., the attack phase is never completed).

For best results, amplitude envelopes should start and end with zero values. Otherwise, the sharp transient at the beginning or end of the note may cause audible clicks or static. For envelopes on other parameters, this may not be necessary.
!

!SampledInstrument commentStamp: '<historical>' prior: 0!
I represent a collection of individual notes at different pitches, volumes, and articulations. On request, I can select the best note to use for a given pitch, duration, and volume. I currently only support two volumes, loud and soft, and two articulations, normal and staccato, but I can easily be extended to include more. The main barrier to keeping more variations is simply the memory space (assuming my component notes are sampled sounds).
!

!SoundCodec commentStamp: '<historical>' prior: 0!
I am an abstract class that describes the protocol for sound codecs. Each codec (the name stems from "COder/DECoder") describes a particular algorithm for compressing and decompressing sound data. Most sound codecs are called 'lossy' because they lose information; the decompressed sound data is not exactly the same as the original data.
!

!ADPCMCodec commentStamp: '<historical>' prior: 0!
This is a simple ADPCM (adapative delta pulse code modulation) codec. This is a general audio codec that compresses speech, music, or sound effects equally well, and works at any sampling rate (i.e., it contains no frequency-sensitive filters). It compresses 16-bit sample data down to 5, 4, 3, or 2 bits per sample, with lower fidelity and increased noise at the lowest bit rates. Although it does not deliver state-of-the-art compressions, the algorithm is small, simple, and extremely fast, since the encode/decode primitives have been translated into C primitives.

This codec will also encode and decode all Flash .swf file compressed sound formats, both mono and stereo. (Note: stereo Flash compression is not yet implemented, but stereo decompression works.)
!

!MuLawCodec commentStamp: '<historical>' prior: 0!
I represent a mu-law (u-law) codec. I compress sound data by a factor of 2:1 by encoding the most significant 12 bits of each 16-bit sample as a signed, exponentially encoded byte. The idea is to use more resolution for smaller lower sample values. This encoding was developed for the North American phone system and a variant of it, a-law, is a European phone standard. It is a popular sound encoding on Unix platforms (.au files).
!

!SoundInputStream commentStamp: '<historical>' prior: 0!
This subclass of SoundRecorder supports real-time processing of incoming sound data. The sound input process queues raw sound buffers, allowing them to be read and processed by the client as they become available. A semaphore is used to synchronize between the record process and the client process. Since sound data is buffered, the client process may lag behind the input process without losing data.
!

!AbstractScoreEvent commentStamp: '<historical>' prior: 0!
Abstract class for timed events in a MIDI score.
!

!NoteEvent commentStamp: '<historical>' prior: 0!
Represents a note on or off event in a MIDI score.
!

!TempoEvent commentStamp: '<historical>' prior: 0!
Represents a tempo change in a MIDI score.
!

!MIDIFileReader commentStamp: '<historical>' prior: 0!
A reader for Standard 1.0 format MIDI files.
MIDI File Types:
	type 0 -- one multi-channel track
	type 1 -- one or more simultaneous tracks
	type 2 -- a number on independent single-track patterns

Instance variables:
	stream			source of MIDI data
	fileType			MIDI file type
	trackCount		number of tracks in file
	ticksPerQuarter	number of ticks per quarter note for all tracks in this file
	tracks			collects track data for non-empty tracks
	strings			collects all strings in the MIDI file
	tempoMap		nil or a MIDITrack consisting only of tempo change events
	trackStream		stream on buffer containing track chunk
	track			track being read
	activeEvents 	notes that have been turned on but not off
!

!MIDIInputParser commentStamp: '<historical>' prior: 0!
I am a parser for a MIDI data stream. I support:

	real-time MIDI recording,
	overdubbing (recording while playing),
	monitoring incoming MIDI, and
	interactive MIDI performances.

Note: MIDI controllers such as pitch benders and breath controllers generate large volumes of data which consume processor time. In cases where this information is not of interest to the program using it, it is best to filter it out as soon as possible. I support various options for doing this filtering, including filtering by MIDI channel and/or by command type.
!

!MIDIScore commentStamp: '<historical>' prior: 0!
A MIDIScore is a container for a number of MIDI tracks as well as an ambient track for such things as sounds, book page triggers and other related events.!

!MIDISynth commentStamp: '<historical>' prior: 0!
I implement a simple real-time MIDI synthesizer on platforms that support MIDI input. I work best on platforms that allow the sound buffer to be made very short--under 50 milliseconds is good and under 20 milliseconds is preferred (see below). The buffer size is changed by modifying the class initialization method of SoundPlayer and executing the do-it there to re-start the sound player.

Each instance of me takes input from a single MIDI input port. Multiple instances of me can be used to handle multiple MIDI input ports. I distribute incoming commands among my sixteen MIDISynthChannel objects. Most of the interpretation of the MIDI commands is done by these channel objects. 

Buffer size notes: At the moment, most fast PowerPC Macintosh computers can probably work with buffer sizes down to 50 milliseconds, and the Powerbook G3 works down to about 15 milliseconds. You will need to experiment to discover the minimum buffer size that does not result in clicking during sound output. (Hint: Be sure to turn off power cycling on your Powerbook. Other applications and extensions can steal cycles from Squeak, causing intermittent clicking. Experimentation may be necessary to find a configuration that works for you.)
!

!MIDISynthChannel commentStamp: '<historical>' prior: 0!
I implement one polyphonic channel of a 16-channel MIDI synthesizer. Many MIDI commands effect all the notes played on a particular channel, so I record the state for a single channel, including a list of notes currently playing.

This initial implementation is extremely spartan, having just enough functionality to play notes. Things that are not implemented include:

  1. program changes
  2. sustain pedal
  3. aftertouch (either kind)
  4. most controllers
  5. portamento
  6. mono-mode
!

!SimpleMIDIPort commentStamp: '<historical>' prior: 0!
This is a first cut at a simple MIDI output port.
!

!Beeper commentStamp: 'gk 2/26/2004 22:44' prior: 0!
Beeper provides simple audio (or in some other way) feedback to the user.

The recommended use is "Beeper beep" to give the user the equivalence of a beep. If you want to force the beep to use the primitive in the VM for beeping, then use "Beeper beepPrimitive". In either case, if sounds are disabled there will be no beep.

The actual beeping, when you use "Beeper beep", is done by sending a #play message to a registered playable object. You can register your own playable object by invoking the class side method #setDefault: passing in an object that responds to the #play message.

The default playable object is an instance of Beeper itself which implements #play on the instance side. That implementation delegates the playing of the beep to the default SoundService.

Note that #play is introduced as a common interface between AbstractSound and Beeper.
This way we can register instances of AbstractSound as playable entities, for example:

	Beeper setDefault: (SampledSound new
						setSamples: self coffeeCupClink
						samplingRate: 12000).

Then "Beeper beep" will play the coffeeCup sound.!

!SoundSystem commentStamp: '<historical>' prior: 0!
This is the sound system. A sound system offers a small protocol for playing sounds and making beeps and works like a facade towards the rest of Squeak. A sound system is registered in the application registry SoundService and can be accessed by "SoundService default" like for example:

SoundService default playSoundNamed: 'croak'

The idea is that as much sound playing as possible should go through this facade. This way we decouple the sound system from the rest of Squeak and make it pluggable. It also is a perfect spot to check for the Preference class>>soundsEnabled.

Two basic subclasses exist at the time of this writing, the BaseSoundSystem which represents the standard Squeak sound system, and the DummySoundSystem which is a dummy implementation that can be used when there is no sound card available, or when the base sound system isn't in the image, or when you simply don't want to use the available sound card.!

!SoundBuffer class methodsFor: 'class initialization' stamp: 'jmv 2/25/2016 10:33'!
startUp
	"Check if the word order has changed from the last save."

	| la |
	la _ ShortIntegerArray classPool at: #LastSaveOrder.
	((la at: 2) = 42 and: [(la at: 1) = 13]) 
		ifTrue: [
			self allInstancesDo: [ :each | 
				BitBlt swapHalvesIn32BitWords: each ]]
			"Reverse the two 16-bit halves."
			"Another reversal happened automatically which reversed the bytes."! !

!AbstractSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:55'!
postCopy
	"A sound should copy all of the state needed to play itself, allowing two copies of a sound to play at the same time. These semantics require a recursive copy but only down to the level of immutable data. For example, a SampledSound need not copy its sample buffer. Subclasses overriding this method should include a resend to super."

	self copyEnvelopes! !

!FMSound methodsFor: 'storing' stamp: 'jmv 3/13/2012 12:34'!
storeOn: strm
	| env |
	strm nextPutAll: '(((FMSound';
		nextPutAll: ' pitch: '; print: self pitch;
		nextPutAll: ' dur: '; print: self duration;
		nextPutAll: ' loudness: '; print: self loudness; nextPutAll: ')';
		nextPutAll: ' modulation: '; print: self modulation;
		nextPutAll: ' ratio: '; print: self ratio; nextPutAll: ')'.
	1 to: envelopes size do:
		[:i | env _ envelopes at: i.
		strm newLine; nextPutAll: '    addEnvelope: '. env storeOn: strm.
		i < envelopes size ifTrue: [strm nextPutAll: ';']].
	strm  nextPutAll: ')'.
! !

!LoopedSampledSound methodsFor: 'disk i/o' stamp: 'tk 9/25/2000 12:06'!
objectForDataStream: refStrm
    "Answer an object to store on a data stream, a copy of myself whose SampleBuffers have been converted into ByteArrays."

	refStrm replace: leftSamples with: leftSamples asByteArray.
	refStrm replace: rightSamples with: rightSamples asByteArray.
	"substitution will be made in DataStream nextPut:"
	^ self
! !

!MixedSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:57'!
postCopy
	"Copy my component sounds."

	super postCopy.
	self copySounds! !

!PluckedSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:56'!
postCopy

	super postCopy.
	self copyRing
! !

!RepeatingSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:58'!
postCopy
	"Copy my component sound."

	super postCopy.
	self copySound
! !

!ReverbSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:58'!
postCopy
	"Copy my component sound."

	super postCopy.
	self copySound
! !

!SequentialSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:58'!
postCopy
	"Copy my component sounds."

	super postCopy.
	self copySounds
! !

!ScorePlayer methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:58'!
postCopy
	"Copy my component sounds."

	super postCopy.
	self copySounds
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:03'!
name

	^ self updateSelector allButLast
! !

!Envelope methodsFor: 'storing' stamp: 'di 2/1/98 15:45'!
storeOn: strm
	strm nextPutAll: '((' , self class name;
		nextPutAll: ' points: '; store: (points collect: [:p | p x @ (p y roundTo: 0.00001)]);
		nextPutAll: ' loopStart: '; print: loopStartIndex;
		nextPutAll: ' loopEnd: '; print: loopEndIndex; nextPutAll: ')';
		nextPutAll: ' updateSelector: '; store: self updateSelector; nextPutAll: ';';
		nextPutAll: ' scale: '; print: scale; nextPutAll: ')'.
! !

!RandomEnvelope methodsFor: 'envelope compatibility' stamp: 'jm 1/14/1999 13:17'!
name

	^ 'random ', updateSelector
! !

!SoundPlayer class methodsFor: 'snapshotting' stamp: 'jm 5/8/1998 18:48'!
shutDown
	"Stop player process, for example before snapshotting."

	self stopPlayerProcess.
	ReverbState _ nil.
! !

!SoundPlayer class methodsFor: 'snapshotting' stamp: 'jm 7/11/97 12:17'!
startUp
	"Start up the player process."

	SoundPlayer initialize.
	SoundPlayer
		startPlayerProcessBufferSize: (BufferMSecs * SamplingRate) // 1000
		rate: SamplingRate
		stereo: Stereo.
! !

!ControlChangeEvent methodsFor: 'printing' stamp: 'sma 6/1/2000 09:34'!
printOn: aStream
	aStream
		nextPut: $(;
		print: time;
		nextPutAll: ': ctrl[';
		print: control;
		nextPutAll: ']=';
		print: value;
		nextPut: $)! !

!NoteEvent methodsFor: 'printing' stamp: 'jm 1/3/98 08:59'!
printOn: aStream

	aStream nextPut: $(.
	time printOn: aStream.
	aStream nextPutAll: ': '.
	aStream nextPutAll: self keyName.
	aStream space.
	duration printOn: aStream.
	aStream nextPut: $).
! !

!PitchBendEvent methodsFor: 'printing' stamp: 'jm 9/10/1998 09:42'!
printOn: aStream

	aStream nextPut: $(.
	time printOn: aStream.
	aStream nextPutAll: ': bend '.
	bend printOn: aStream.
	aStream nextPut: $).
! !

!ProgramChangeEvent methodsFor: 'printing' stamp: 'jm 9/10/1998 08:28'!
printOn: aStream

	aStream nextPut: $(.
	time printOn: aStream.
	aStream nextPutAll: ': prog '.
	program printOn: aStream.
	aStream nextPut: $).
! !

!TempoEvent methodsFor: 'as yet unclassified' stamp: 'jm 9/10/1998 08:37'!
printOn: aStream

	aStream nextPut: $(.
	time printOn: aStream.
	aStream nextPutAll: ': tempo '.
	((120.0 * (500000.0 / tempo)) roundTo: 0.01) printOn: aStream.
	aStream nextPut: $).
! !

!SoundSystem class methodsFor: 'as yet unclassified' stamp: 'jmv 6/7/2013 22:05'!
shutDown
	SoundPlayer shutDown! !

!SoundSystem methodsFor: 'misc' stamp: 'jmv 6/7/2013 21:58'!
shutDown
	SoundPlayer shutDown
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 8/15/1998 13:03'!
asByteArray
	"Answer a ByteArray containing my sample data serialized in most-significant byte first order."

	| sampleCount bytes dst s |
	sampleCount _ self monoSampleCount.
	bytes _ ByteArray new: 2 * sampleCount.
	dst _ 0.
	1 to: sampleCount do: [:src |
		s _ self at: src.
		bytes at: (dst _ dst + 1) put: ((s bitShift: -8) bitAnd: 255).
		bytes at: (dst _ dst + 1) put: (s bitAnd: 255)].
	^ bytes

	! !

!SoundBuffer methodsFor: 'utilities' stamp: 'RAA 12/30/2000 18:26'!
averageEvery: nSamples from: anotherBuffer upTo: inCount

	| fromIndex sum |

	fromIndex _ 1.
	1 to: inCount // nSamples do: [ :i |
		sum _ 0.
		nSamples timesRepeat: [
			sum _ sum + (anotherBuffer at: fromIndex).
			fromIndex _ fromIndex + 1.
		].
		self at: i put: sum // nSamples.
	].
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 8/18/1998 06:49'!
downSampledLowPassFiltering: doFiltering
	"Answer a new SoundBuffer half the size of the receiver consisting of every other sample. If doFiltering is true, a simple low-pass filter is applied to avoid aliasing of high frequencies. Assume that receiver is monophonic."
	"Details: The simple low-pass filter in the current implementation could be improved, at some additional cost."

	| n resultBuf j |
	n _ self monoSampleCount.
	resultBuf _ SoundBuffer newMonoSampleCount: n // 2.
	j _ 0.
	doFiltering
		ifTrue: [
			1 to: n by: 2 do: [:i |
				resultBuf at: (j _ j + 1) put:
					(((self at: i) + (self at: i + 1)) bitShift: -1)]]
		ifFalse: [
			1 to: n by: 2 do: [:i |
				resultBuf at: (j _ j + 1) put: (self at: i)]].

	^ resultBuf! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 8/18/1998 06:52'!
extractLeftChannel
	"Answer a new SoundBuffer half the size of the receiver consisting of only the left channel of the receiver, which is assumed to contain stereo sound data."

	| n resultBuf j |
	n _ self monoSampleCount.
	resultBuf _ SoundBuffer newMonoSampleCount: n // 2.
	j _ 0.
	1 to: n by: 2 do: [:i | resultBuf at: (j _ j + 1) put: (self at: i)].
	^ resultBuf! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 8/18/1998 06:53'!
extractRightChannel
	"Answer a new SoundBuffer half the size of the receiver consisting of only the right channel of the receiver, which is assumed to contain stereo sound data."

	| n resultBuf j |
	n _ self monoSampleCount.
	resultBuf _ SoundBuffer newMonoSampleCount: n // 2.
	j _ 0.
	2 to: n by: 2 do: [:i | resultBuf at: (j _ j + 1) put: (self at: i)].
	^ resultBuf! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jhm 10/15/97 15:13'!
indexOfFirstSampleOver: threshold
	"Return the index of the first sample whose absolute value is over the given threshold value. Return an index one greater than my size if no sample is over the threshold."

	1 to: self size do: [:i |
		(self at: i) abs > threshold ifTrue: [^ i]].
	^ self size + 1! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jhm 10/15/97 15:13'!
indexOfLastSampleOver: threshold
	"Return the index of the last sample whose absolute value is over the given threshold value. Return zero if no sample is over the threshold."

	self size to: 1 by: -1 do: [:i |
		(self at: i) abs > threshold ifTrue: [^ i]].
	^ 0
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jmv 5/14/2015 09:54'!
lowPassFiltered
	"Answer a simple low-pass filtered copy of this buffer. Assume it is monophonic."

	| sz out last this |
	sz _ self monoSampleCount.
	out _ self copy.
	last _ self at: 1.
	2 to: sz do: [:i |
		this _ self at: i.
		out at: i put: (this + last) // 2.
		last _ this].
	^ out
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 11/15/2001 18:26'!
mergeStereo
	"Answer a new SoundBuffer half the size of the receiver that mixes the left and right stereo channels of the receiver, which is assumed to contain stereo sound data."

	| n resultBuf j |
	n _ self monoSampleCount.
	resultBuf _ SoundBuffer newMonoSampleCount: n // 2.
	j _ 0.
	1 to: n by: 2 do: [:i | resultBuf at: (j _ j + 1) put: (((self at: i) + (self at: i + 1)) // 2)].
	^ resultBuf
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jhm 10/15/97 15:13'!
normalized: percentOfFullVolume
	"Increase my amplitudes so that the highest peak is the given percent of full volume. For example 's normalized: 50' would normalize to half of full volume."

	| peak s mult |
	peak _ 0.
	1 to: self size do: [:i |
		s _ (self at: i) abs.
		s > peak ifTrue: [peak _ s]].
	mult _ (32767.0 * percentOfFullVolume) / (100.0 * peak).
	1 to: self size do: [:i | self at: i put: (mult * (self at: i)) asInteger].
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jmv 2/26/2016 17:09'!
saveAsAIFFFileSamplingRate: rate on: aBinaryStream
	"Store this mono sound buffer in AIFF file format with the given sampling rate on the given stream."

	| sampleCount s swapBytes |
	sampleCount _ self monoSampleCount.
	aBinaryStream nextPutAll: 'FORM' asByteArray.
	aBinaryStream nextSignedInt32Put: (2 * sampleCount) + ((7 * 4) + 18) bigEndian: true.
	aBinaryStream nextPutAll: 'AIFF' asByteArray.
	aBinaryStream nextPutAll: 'COMM' asByteArray.
	aBinaryStream nextSignedInt32Put: 18 bigEndian: true.
	aBinaryStream nextUnsignedInt16Put: 1 bigEndian: true.  "channels"
	aBinaryStream nextSignedInt32Put: sampleCount bigEndian: true.
	aBinaryStream nextUnsignedInt16Put: 16 bigEndian: true.  "bits/sample"
	self storeExtendedFloat: rate on: aBinaryStream.
	aBinaryStream nextPutAll: 'SSND' asByteArray.
	aBinaryStream nextSignedInt32Put: (2 * sampleCount) + 8 bigEndian: true.
	aBinaryStream nextSignedInt32Put: 0 bigEndian: true.
	aBinaryStream nextSignedInt32Put: 0 bigEndian: true.

	(aBinaryStream isKindOf: StandardFileStream) ifTrue: [
		"optimization: write sound buffer directly to file"
		swapBytes _ Smalltalk isLittleEndian.
		swapBytes ifTrue: [self reverseEndianness].  "make big endian"
		aBinaryStream next: (self size // 2) putAll: self startingAt: 1.  "size in words"
		swapBytes ifTrue: [self reverseEndianness].  "revert to little endian"
		^ self].

	1 to: sampleCount do: [:i |
		s _ self at: i.
		aBinaryStream nextPut: ((s bitShift: -8) bitAnd: 16rFF).
		aBinaryStream nextPut: (s bitAnd: 16rFF)].
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jm 3/28/1999 07:23'!
splitStereo
	"Answer an array of two SoundBuffers half the size of the receiver consisting of the left and right channels of the receiver (which is assumed to contain stereo sound data)."

	| n leftBuf rightBuf leftIndex rightIndex |
	n _ self monoSampleCount.
	leftBuf _ SoundBuffer newMonoSampleCount: n // 2.
	rightBuf _ SoundBuffer newMonoSampleCount: n // 2.
	leftIndex _ rightIndex _ 0.
	1 to: n by: 2 do: [:i |
		leftBuf at: (leftIndex _ leftIndex + 1) put: (self at: i).
		rightBuf at: (rightIndex _ rightIndex + 1) put: (self at: i + 1)].
	^ Array with: leftBuf with: rightBuf
! !

!SoundBuffer methodsFor: 'utilities' stamp: 'jhm 10/15/97 15:13'!
trimmedThreshold: threshold

	| start end |
	start _ self indexOfFirstSampleOver: threshold.
	end _  self indexOfLastSampleOver: threshold.
	start > end ifTrue: [^ SoundBuffer new].
	start _ (start - 200) max: 1.
	end _ (end + 200) min: self size.
	^ self copyFrom: start to: end
! !

!SoundBuffer methodsFor: 'primitives' stamp: 'jm 9/17/97 13:03'!
at: index
	"Return the 16-bit integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.
! !

!SoundBuffer methodsFor: 'primitives' stamp: 'jm 9/17/97 13:03'!
at: index put: value
	"Store the given 16-bit integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.
! !

!SoundBuffer methodsFor: 'primitives' stamp: 'jm 9/2/97 16:07'!
primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."
	"Note: Since 16-bit word arrays are not built into the virtual machine, this primitive fills by 32-bit words."

	<primitive: 145>
	self errorImproperStore.
! !

!SoundBuffer methodsFor: 'accessing' stamp: 'tk 3/13/2000 14:46'!
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 2! !

!SoundBuffer methodsFor: 'accessing' stamp: 'jm 9/17/97 13:00'!
monoSampleCount
	"Return the number of monaural 16-bit samples that fit into this SoundBuffer."

	^ super size * 2
! !

!SoundBuffer methodsFor: 'accessing' stamp: 'jm 9/17/97 13:28'!
size
	"Return the number of 16-bit sound samples that fit in this sound buffer. To avoid confusion, it is better to get the size of SoundBuffer using monoSampleCount or stereoSampleCount."

	^ self monoSampleCount
! !

!SoundBuffer methodsFor: 'accessing' stamp: 'jm 9/17/97 13:01'!
stereoSampleCount
	"Return the number of stereo slices that fit into this SoundBuffer. A stereo 'slice' consists of two 16-bit samples, one for each channel."

	^ super size
! !

!SoundBuffer methodsFor: 'objects from disk' stamp: 'jmv 2/15/2008 00:49'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Swap each pair of bytes (16-bit word), if the current machine is Little Endian.
	Why is this the right thing to do?  We are using memory as a byteStream.  High and low bytes are reversed in each 16-bit word, but the stream of words ascends through memory.  Different from a Bitmap."

	| hack blt |
	Smalltalk isLittleEndian ifTrue: [
		"The implementation is a hack, but fast for large ranges"
		hack _ Form new hackBits: self.
		blt _ (BitBlt toForm: hack) sourceForm: hack.
		blt combinationRule: Form reverse.  "XOR"
		blt sourceY: 0; destY: 0; height: self size; width: 1.
		blt sourceX: 0; destX: 1; copyBits.  "Exchange bytes 0 and 1"
		blt sourceX: 1; destX: 0; copyBits.
		blt sourceX: 0; destX: 1; copyBits.
		blt sourceX: 2; destX: 3; copyBits.  "Exchange bytes 2 and 3"
		blt sourceX: 3; destX: 2; copyBits.
		blt sourceX: 2; destX: 3; copyBits].

! !

!SoundBuffer methodsFor: 'objects from disk' stamp: 'jm 10/29/2001 19:53'!
reverseEndianness
	"Swap the bytes of each 16-bit word, using a fast BitBlt hack."

	| hack blt |
	hack _ Form new hackBits: self.
	blt _ (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: 0; destY: 0; height: self size; width: 1.
	blt sourceX: 0; destX: 1; copyBits.  "Exchange bytes 0 and 1"
	blt sourceX: 1; destX: 0; copyBits.
	blt sourceX: 0; destX: 1; copyBits.
	blt sourceX: 2; destX: 3; copyBits.  "Exchange bytes 2 and 3"
	blt sourceX: 3; destX: 2; copyBits.
	blt sourceX: 2; destX: 3; copyBits.
! !

!SoundBuffer methodsFor: 'as yet unclassified' stamp: 'RAA 7/11/2000 11:31'!
writeOnGZIPByteStream: aStream 
	
	aStream nextPutAllWordArray: self! !

!SoundBuffer class methodsFor: 'class initialization' stamp: 'jmv 12/15/2014 21:51'!
initialize
	"SoundBuffer initialize"
	Smalltalk addToStartUpList: self! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'RAA 12/30/2000 18:20'!
averageEvery: nSamples from: anotherBuffer upTo: inCount

	^(self newMonoSampleCount: inCount // nSamples)
		averageEvery: nSamples 
		from: anotherBuffer 
		upTo: inCount! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 9/17/97 12:39'!
fromArray: anArray
	"Return a new SoundBuffer whose contents are copied from the given Array or ByteArray."

	| new |
	new _ SoundBuffer newMonoSampleCount: anArray size.
	1 to: anArray size do: [:i | new at: i put: (anArray at: i)].
	^ new
! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 8/15/1998 14:35'!
fromByteArray: aByteArray
	"Convert the given ByteArray (stored with the most significant byte first) into 16-bit sample buffer."

	| n buf src w |
	n _ aByteArray size // 2.
	buf _ SoundBuffer newMonoSampleCount: n.
	src _ 1.
	1 to: n do: [:i |
		w _ ((aByteArray at: src) bitShift: 8) + (aByteArray at: src + 1).
		w > 32767 ifTrue: [w _ w - 65536].
		buf at: i put: w.
		src _ src + 2].
	^ buf
! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 9/17/97 13:25'!
new: anInteger
	"See the comment in newMonoSampleCount:. To avoid confusion, it is best to create new instances using newMonoSampleCount: or newStereoSampleCount:."

	^ self newMonoSampleCount: anInteger
! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 9/17/97 12:44'!
newMonoSampleCount: anInteger
	"Return a SoundBuffer large enough to hold the given number of monaural samples (i.e., 16-bit words)."
	"Details: The size is rounded up to an even number, since the underlying representation is in terms of 32-bit words."

	^ self basicNew: (anInteger + 1) // 2
! !

!SoundBuffer class methodsFor: 'instance creation' stamp: 'jm 9/17/97 12:52'!
newStereoSampleCount: anInteger
	"Return a SoundBuffer large enough to hold the given number of stereo slices. A stereo 'slice' consists of two 16-bit samples, one for each channel."

	^ self basicNew: anInteger
! !

!Sonogram methodsFor: 'all' stamp: 'jmv 12/16/2011 10:26'!
extent: aPoint minVal: min maxVal: max scrollDelta: d
	minVal _ min.
	maxVal _ max.
	scrollDelta _ d.
	self morphExtent: aPoint.

" try following with scrolldelta = 1, 20, 200
	| s data |
	s _ Sonogram new extent: 200@50
				minVal: 0.0 maxVal: 1.0 scrollDelta: 20.
	World addMorph: s.
	data _ (1 to: 133) collect: [:i | 0.0].
	1 to: 300 do:
		[:i | data at: (i\\133)+1 put: 1.0.
		s plotColumn: data.
		data at: (i\\133)+1 put: 0.0.
		World doOneCycleNow].
	s delete	
"! !

!Sonogram methodsFor: 'all' stamp: 'jmv 1/21/2015 23:56'!
plotColumn: dataArray

	| chm1 i normVal r |
	chm1 _ columnForm height - 1.
	0 to: chm1 do: [ :y | 
		i _ y*(dataArray size-1)//chm1 + 1.
		normVal _ ((dataArray at: i) - minVal) / (maxVal - minVal).
		normVal < 0.0 ifTrue: [normVal _ 0.0].
		normVal > 1.0 ifTrue: [normVal _ 1.0].
		columnForm bits at: chm1-y+1 put: (pixValMap at: (normVal * 255.0) truncated + 1)].
	(lastX _ lastX + 1) > (image width - 1) ifTrue:
		[self scroll].
	image copy: (r _ (lastX@0 extent: 1@image height))
			from: (32//image depth-1)@0
			in: columnForm rule: Form over.
	"self changed."
	self invalidateLocalRect: r! !

!Sonogram methodsFor: 'all' stamp: 'jmv 3/14/2011 09:26'!
scroll
	image copy: (scrollDelta@0 extent: (image width-scrollDelta)@image height)
			from: image to: 0@0 rule: Form over.
	lastX _ lastX - scrollDelta.
	self redrawNeeded! !

!Sonogram methodsFor: 'geometry' stamp: 'jmv 12/16/2011 10:22'!
morphExtent: newExtent
	super image: (Form extent: newExtent depth: Display depth).
	lastX _ -1.
	columnForm _ Form extent: (32//image depth)@(image height) depth: image depth.
	pixValMap _ ((1 to: 256) collect:
			[:i | columnForm pixelValueFor: (Color gray: (256-i)/255.0)])
		as: Bitmap.
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:25'!
bitsPerSample

	^ bitsPerSample
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:24'!
channelCount

	^ channelCount
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:25'!
channelData

	^ channelData
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 10/20/2001 15:07'!
channelDataOffset

	^ channelDataOffset
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:24'!
frameCount

	^ frameCount
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:25'!
gain

	^ gain
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 01:40'!
isLooped

	^ isLooped
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 20:02'!
isStereo

	^ channelData size = 2
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 18:26'!
leftSamples

	^ channelData at: 1
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 18:30'!
loopEnd

	^ markers last last
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 18:30'!
loopLength

	^ markers last last - markers first last
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 21:25'!
markers

	^ markers
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 01:48'!
pitch

	^ pitch
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 8/2/1998 19:34'!
rightSamples

	^ channelData at: 2
! !

!AIFFFileReader methodsFor: 'accessing' stamp: 'jm 7/12/1998 18:25'!
samplingRate

	^ samplingRate
! !

!AIFFFileReader methodsFor: 'other' stamp: 'jm 7/12/1998 01:44'!
pitchForKey: midiKey
	"Convert my MIDI key number to a pitch and return it."

	| indexInOctave octave p |
	indexInOctave _ (midiKey \\ 12) + 1.
	octave _ (midiKey // 12) + 1.
	"Table generator: (0 to: 11) collect: [:i | 16.3516 * (2.0 raisedTo: i asFloat / 12.0)]"
	p _ #(16.3516 17.32391 18.35405 19.44544 20.60173 21.82677
		  23.12466 24.49972 25.95655 27.50000 29.13524 30.86771) at: indexInOctave.
	^ p * (#(0.5 1.0 2.0 4.0 8.0 16.0 32.0 64.0 128.0 256.0 512.0) at: octave)
! !

!AIFFFileReader methodsFor: 'other' stamp: 'jm 1/14/1999 10:11'!
sound
	"Answer the sound represented by this AIFFFileReader. This method should be called only after readFrom: has been done."

	| snd rightSnd |
	snd _ SampledSound
		samples: (channelData at: 1)
		samplingRate: samplingRate.
	self isStereo ifTrue: [
		rightSnd _ SampledSound
			samples: (channelData at: 2)
			samplingRate: samplingRate.
		snd _ MixedSound new
			add: snd pan: 0;
			add: rightSnd pan: 1.0].
	^ snd
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jm 6/29/1998 07:33'!
readChunk: chunkType size: chunkSize
	"Read a AIFF chunk of the given type. Skip unrecognized chunks. Leave the input stream positioned chunkSize bytes past its position when this method is called."

	chunkType = 'COMM' ifTrue: [^ self readCommonChunk: chunkSize].
	chunkType = 'SSND' ifTrue: [^ self readSamplesChunk: chunkSize].
	chunkType = 'INST' ifTrue: [^ self readInstrumentChunk: chunkSize].
	chunkType = 'MARK' ifTrue: [^ self readMarkerChunk: chunkSize].
	in skip: chunkSize.  "skip unknown chunks"
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 2/26/2016 16:59'!
readCommonChunk: chunkSize
	"Read a COMM chunk. All AIFF files have exactly one chunk of this type."

	| compressionType |
	channelCount _ in nextUnsignedInt16BigEndian: true.
	frameCount _ in nextUnsignedInt32BigEndian: true.
	bitsPerSample _ in nextUnsignedInt16BigEndian: true.
	samplingRate _ self readExtendedFloat.
	chunkSize > 18 ifTrue: [
		fileType = 'AIFF'
			ifTrue: [self error: 'unexpectedly long COMM chunk size for AIFF file'].
		compressionType _ (in next: 4) asString.
		compressionType = 'NONE' ifFalse: [self error: 'cannot read compressed AIFF files'].
		in skip: (chunkSize - 22)].  "skip the reminder of AIFF-C style chunk"
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 4/11/2019 14:01:46'!
readExtendedFloat
	"Read and answer an Apple extended-precision 80-bit floating point number from the input stream."
	"Details: I could not find the specification for this format, so constants were determined empirically based on assumption of 1-bit sign, 15-bit exponent, 64-bit mantissa. This format does not seem to have an implicit one before the mantissa as some float formats do."

	| signAndExp mantissa sign exp |
	signAndExp _ in nextUnsignedInt16BigEndian: true.
	"scaled by (2 raisedTo: -64) below"
	mantissa _ (in nextUnsignedInt32BigEndian: true) << 32 + (in nextUnsignedInt32BigEndian: true).
	(signAndExp bitAnd: 16r8000) = 0
		ifTrue: [sign _ 1.0]
		ifFalse: [sign _ -1.0].
	exp _ (signAndExp bitAnd: 16r7FFF) - 16r4000 + 2.  "not sure why +2 is needed..."
	^ (sign * mantissa asFloat * (2.0 raisedTo: exp - 64)) roundTo: 0.00000001
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 2/26/2016 17:00'!
readFrom: aBinaryStream
	"Read AIFF data from the given binary stream."
	"Details: An AIFF file consists of a header (FORM chunk) followed by a sequence of tagged data chunks. Each chunk starts with a header consisting of a four-byte tag (a string) and a four byte size. These eight bytes of chunk header are not included in the chunk size. For each chunk, the readChunk:size: method consumes chunkSize bytes of the input stream, parsing recognized chunks or skipping unrecognized ones. If chunkSize is odd, it will be followed by a padding byte. Chunks may occur in any order."

	| sz end chunkType chunkSize p |
	in _ aBinaryStream.

	"read FORM chunk"
	(in next: 4) asString = 'FORM' ifFalse: [^ self error: 'not an AIFF file'].
	sz _ in nextUnsignedInt32BigEndian: true.
	end _ in position + sz.
	fileType _ (in next: 4) asString.

	[in atEnd not and: [in position < end]] whileTrue: [
		chunkType _ (in next: 4) asString.
		chunkSize _ in nextUnsignedInt32BigEndian: true.
		p _ in position.
		self readChunk: chunkType size: chunkSize.
		(in position = (p + chunkSize))
			ifFalse: [self error: 'chunk size mismatch; bad AIFF file?'].
		chunkSize odd ifTrue: [in skip: 1]].  "skip padding byte"
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 2/26/2016 16:57'!
readInstrumentChunk: chunkSize

	| midiKey detune lowNote highNote lowVelocity highVelocity
	  sustainMode sustainStartID sustainEndID
	  releaseMode releaseStartID releaseEndID |

	midiKey _ in next.
	detune _ in next.
	lowNote _ in next.
	highNote _ in next.
	lowVelocity _ in next.
	highVelocity _ in next.
	gain _ in nextUnsignedInt16BigEndian: true.
	sustainMode _ in nextUnsignedInt16BigEndian: true.
	sustainStartID _ in nextUnsignedInt16BigEndian: true.
	sustainEndID _ in nextUnsignedInt16BigEndian: true.
	releaseMode _ in nextUnsignedInt16BigEndian: true.
	releaseStartID _ in nextUnsignedInt16BigEndian: true.
	releaseEndID _ in nextUnsignedInt16BigEndian: true.
	isLooped _ sustainMode = 1.
	(isLooped and: [markers notNil]) ifTrue: [
		((markers first last > frameCount) or:
		 [markers last last > frameCount]) ifTrue: [
			"bad loop data; some sample CD files claim to be looped but aren't"
			isLooped _ false]].
	pitch _ self pitchForKey: midiKey.
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 2/26/2016 17:00'!
readMarkerChunk: chunkSize

	| markerCount id position labelBytes label |
	markerCount _ in nextUnsignedInt16BigEndian: true.
	markers _ Array new: markerCount.
	1 to: markerCount do: [:i |
		id _ in nextUnsignedInt16BigEndian: true.
		position _ in nextUnsignedInt32BigEndian: true.
		labelBytes _ in next.
		label _ (in next: labelBytes) asString.
		labelBytes even ifTrue: [in skip: 1].
		markers at: i put: (Array with: id with: label with: position)].

! !

!AIFFFileReader methodsFor: 'private' stamp: 'jm 8/2/1998 18:58'!
readMergedStereoChannelDataFrom: s
	"Read stereophonic channel data from the given stream, mixing the two channels to create a single monophonic channel. Each frame contains two samples."

	| buf w1 w2 |
	buf _ channelData at: 1.
	bitsPerSample = 8
		ifTrue: [
			1 to: frameCount do: [:i |
				w1 _ s next.
				w1 > 127 ifTrue: [w1 _ w1 - 256].
				w2 _ s next.
				w2 > 127 ifTrue: [w2 _ w2 - 256].
				buf at: i put: ((w1 + w2) bitShift: 7)]]
		ifFalse: [
			1 to: frameCount do: [:i |
				w1 _ (s next bitShift: 8) + s next.
				w1 > 32767 ifTrue: [w1 _ w1 - 65536].
				w2 _ (s next bitShift: 8) + s next.
				w2 > 32767 ifTrue: [w2 _ w2 - 65536].
				buf at: i put: ((w1 + w2) bitShift: -1)]].
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jm 8/2/1998 18:53'!
readMonoChannelDataFrom: s
	"Read monophonic channel data from the given stream. Each frame contains a single sample."

	| buf w |
	buf _ channelData at: 1.  "the only buffer"
	bitsPerSample = 8
		ifTrue: [
			1 to: frameCount do: [:i |
				w _ s next.
				w > 127 ifTrue: [w _ w - 256].
				buf at: i put: (w bitShift: 8)]]
		ifFalse: [
			1 to: frameCount do: [:i |
				w _ (s next bitShift: 8) + s next.
				w > 32767 ifTrue: [w _ w - 65536].
				buf at: i put: w]].
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jm 8/2/1998 18:55'!
readMultiChannelDataFrom: s
	"Read multi-channel data from the given stream. Each frame contains channelCount samples."

	| w |
	bitsPerSample = 8
		ifTrue: [
			1 to: frameCount do: [:i |
				1 to: channelCount do: [:ch |
					w _ s next.
					w > 127 ifTrue: [w _ w - 256].
					(channelData at: ch) at: i put: (w bitShift: 8)]]]
		ifFalse: [
			1 to: frameCount do: [:i |
				1 to: channelCount do: [:ch |
					w _ (s next bitShift: 8) + s next.
					w > 32767 ifTrue: [w _ w - 65536].
					(channelData at: ch) at: i put: w]]].
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jmv 2/26/2016 17:00'!
readSamplesChunk: chunkSize
	"Read a SSND chunk. All AIFF files with a non-zero frameCount contain exactly one chunk of this type."

	| offset blockSize bytesOfSamples s |
	offset _ in nextUnsignedInt32BigEndian: true.
	blockSize _ in nextUnsignedInt32BigEndian: true.
	((offset ~= 0) or: [blockSize ~= 0])
		ifTrue: [^ self error: 'this AIFF reader cannot handle blocked sample chunks'].
	bytesOfSamples _ chunkSize - 8.
	bytesOfSamples = (channelCount * frameCount * (bitsPerSample // 8))
		ifFalse: [self error: 'actual sample count does not match COMM chunk'].

	channelDataOffset _ in position.  "record stream position for start of data"
	skipDataChunk ifTrue: [in skip: (chunkSize - 8). ^ self].  "if skipDataChunk, skip sample data"

	(mergeIfStereo and: [channelCount = 2])
		ifTrue: [
			channelData _ Array with: (SoundBuffer newMonoSampleCount: frameCount)]
		ifFalse: [
			channelData _
				(1 to: channelCount) collect: [:i | SoundBuffer newMonoSampleCount: frameCount]].

	(bytesOfSamples < (Smalltalk garbageCollectMost - 300000))
		ifTrue: [s _ ReadStream on: (in next: bytesOfSamples)]  "bulk-read, then process"
		ifFalse: [s _ in].  "not enough space to buffer; read directly from file"

	"mono and stereo are special-cased for better performance"
	channelCount = 1 ifTrue: [^ self readMonoChannelDataFrom: s].
	channelCount = 2 ifTrue: [
		mergeIfStereo
			ifTrue: [channelCount _ 1. ^ self readMergedStereoChannelDataFrom: s]
			ifFalse: [^ self readStereoChannelDataFrom: s]].
	self readMultiChannelDataFrom: s.
! !

!AIFFFileReader methodsFor: 'private' stamp: 'jm 8/2/1998 18:56'!
readStereoChannelDataFrom: s
	"Read stereophonic channel data from the given stream. Each frame contains two samples."

	| left right w |
	left _ channelData at: 1.
	right _ channelData at: 2.
	bitsPerSample = 8
		ifTrue: [
			1 to: frameCount do: [:i |
				w _ s next.
				w > 127 ifTrue: [w _ w - 256].
				left at: i put: (w bitShift: 8).
				w _ s next.
				w > 127 ifTrue: [w _ w - 256].
				right at: i put: (w bitShift: 8)]]
		ifFalse: [
			1 to: frameCount do: [:i |
				w _ (s next bitShift: 8) + s next.
				w > 32767 ifTrue: [w _ w - 65536].
				left at: i put: w.
				w _ (s next bitShift: 8) + s next.
				w > 32767 ifTrue: [w _ w - 65536].
				right at: i put: w]].
! !

!AIFFFileReader methodsFor: 'reading' stamp: 'jm 8/2/1998 16:27'!
readFromFile: fileName
	"Read the AIFF file of the given name."
	"AIFFFileReader new readFromFile: 'test.aiff'"

	self readFromFile: fileName
		mergeIfStereo: false
		skipDataChunk: false.
! !

!AIFFFileReader methodsFor: 'reading' stamp: 'pb 5/25/2016 01:30'!
readFromFile: fileName mergeIfStereo: mergeFlag skipDataChunk: skipDataFlag
	"Read the AIFF file of the given name. See comment in readFromStream:mergeIfStereo:skipDataChunk:."
	"AIFFFileReader new readFromFile: 'test.aiff' mergeIfStereo: false skipDataChunk: true"

	fileName asFileEntry readStreamDo: [ :strm |
		strm binary.
		self readFromStream: strm mergeIfStereo: mergeFlag skipDataChunk: skipDataFlag ]! !

!AIFFFileReader methodsFor: 'reading'!
readFromStream: aBinaryStream mergeIfStereo: mergeFlag skipDataChunk: skipDataFlag
	"Read an AIFF file from the given binary stream. If mergeFlag is true and the file contains stereo data, then the left and right channels will be mixed together as the samples are read in. If skipDataFlag is true, then the data chunk to be skipped; this allows the other chunks of a file to be processed in order to extract format information quickly without reading the data."

	mergeIfStereo _ mergeFlag.
	skipDataChunk _ skipDataFlag.
	isLooped _ false.
	gain _ 1.0.
	self readFrom: aBinaryStream.
! !

!AbstractSound methodsFor: 'copying' stamp: 'jm 12/17/97 22:22'!
copyEnvelopes
	"Private!! Support for copying. Copy my envelopes."

	envelopes _ envelopes collect: [:e | e copy target: self].
! !

!AbstractSound methodsFor: 'copying' stamp: 'di 3/4/1999 21:29'!
sounds
	"Allows simple sounds to behave as, eg, sequential sounds"

	^ Array with: self! !

!AbstractSound methodsFor: 'composition'!
+ aSound
	"Return the mix of the receiver and the argument sound."

	^ MixedSound new
		add: self;
		add: aSound
! !

!AbstractSound methodsFor: 'composition'!
, aSound
	"Return the concatenation of the receiver and the argument sound."

	^ SequentialSound new
		add: self;
		add: aSound
! !

!AbstractSound methodsFor: 'composition' stamp: 'jm 2/2/1999 15:53'!
asSound

	^ self
! !

!AbstractSound methodsFor: 'composition' stamp: 'jm 12/17/97 18:00'!
delayedBy: seconds
	"Return a composite sound consisting of a rest for the given amount of time followed by the receiver."

	^ (RestSound dur: seconds), self
! !

!AbstractSound methodsFor: 'envelopes' stamp: 'jm 12/17/97 22:23'!
addEnvelope: anEnvelope
	"Add the given envelope to my envelopes list."

	anEnvelope target: self.
	envelopes _ envelopes copyWith: anEnvelope.
! !

!AbstractSound methodsFor: 'envelopes' stamp: 'jm 12/15/97 17:02'!
envelopes
	"Return my collection of envelopes."

	^ envelopes
! !

!AbstractSound methodsFor: 'envelopes' stamp: 'jm 8/18/1998 09:57'!
removeAllEnvelopes
	"Remove all envelopes from my envelopes list."

	envelopes _ #().
! !

!AbstractSound methodsFor: 'envelopes' stamp: 'jm 12/15/97 17:02'!
removeEnvelope: anEnvelope
	"Remove the given envelope from my envelopes list."

	envelopes _ envelopes copyWithout: anEnvelope.
! !

!AbstractSound methodsFor: 'volume' stamp: 'RAA 8/11/2000 11:51'!
adjustVolumeTo: vol overMSecs: mSecs
	"Adjust the volume of this sound to the given volume, a number in the range [0.0..1.0], over the given number of milliseconds. The volume will be changed a little bit on each sample until the desired volume is reached."

	| newScaledVol |

	self flag: #bob.		"I removed the upper limit to allow making sounds louder. hmm..."

	newScaledVol _ (32768.0 * vol) truncated.
	newScaledVol = scaledVol ifTrue: [^ self].
	scaledVolLimit _ newScaledVol.
	"scaledVolLimit > ScaleFactor ifTrue: [scaledVolLimit _ ScaleFactor]."
	scaledVolLimit < 0 ifTrue: [scaledVolLimit _ 0].
	mSecs = 0
		ifTrue: [  "change immediately"
			scaledVol _ scaledVolLimit.
			scaledVolIncr _ 0]
		ifFalse: [
			scaledVolIncr _
				((scaledVolLimit - scaledVol) * 1000) // (self samplingRate * mSecs)].
! !

!AbstractSound methodsFor: 'volume' stamp: 'jm 12/17/97 17:39'!
initialVolume: vol
	"Set the initial volume of this sound to the given volume, a number in the range [0.0..1.0]."

	scaledVol _ (((vol asFloat min: 1.0) max: 0.0) * ScaleFactor) rounded.
	scaledVolLimit _ scaledVol.
	scaledVolIncr _ 0.
! !

!AbstractSound methodsFor: 'volume' stamp: 'jm 8/13/1998 16:37'!
loudness
	"Answer the current volume setting for this sound."

	^ scaledVol asFloat / ScaleFactor asFloat! !

!AbstractSound methodsFor: 'volume' stamp: 'jm 8/13/1998 16:28'!
volumeEnvelopeScaledTo: scalePoint
	"Return a collection of values representing my volume envelope scaled by the given point. The scale point's x component is pixels/second and its y component is the number of pixels for full volume."

	self error: 'not yet implemented'.
! !

!AbstractSound methodsFor: 'conversion' stamp: 'jm 12/16/2001 13:26'!
asSampledSound
	"Answer a SampledSound containing my samples. If the receiver is some kind of sampled sound, the resulting SampledSound will have the same original sampling rate as the receiver."

	^ SampledSound samples: self samples samplingRate: self originalSamplingRate
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 1/26/98 22:05'!
computeSamplesForSeconds: seconds
	"Compute the samples of this sound without outputting them, and return the resulting buffer of samples."

	| buf |
	self reset.
	buf _ SoundBuffer newStereoSampleCount: (self samplingRate * seconds) asInteger.
	self playSampleCount: buf stereoSampleCount into: buf startingAt: 1.
	^ buf
! !

!AbstractSound methodsFor: 'playing' stamp: 'ar 12/5/1998 22:20'!
isPlaying
	"Return true if the receiver is currently playing"
	^ SoundPlayer isPlaying: self! !

!AbstractSound methodsFor: 'playing' stamp: 'di 5/30/1999 12:46'!
millisecondsSinceStart

	^ mSecsSinceStart! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 8/24/97 20:48'!
pause
	"Pause this sound. It can be resumed from this point, or reset and resumed to start from the beginning."

	SoundPlayer pauseSound: self.! !

!AbstractSound methodsFor: 'playing' stamp: 'gk 2/24/2004 22:23'!
play
	"Play this sound to the sound output port in real time."

	SoundPlayer playSound: self.! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 8/13/1998 15:09'!
playAndWaitUntilDone
	"Play this sound to the sound ouput port and wait until it has finished playing before returning."

	SoundPlayer playSound: self.
	[self samplesRemaining > 0] whileTrue.
	(Delay forMilliseconds: 2 * SoundPlayer bufferMSecs) wait.  "ensure last buffer has been output"
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 8/18/1998 10:52'!
playChromaticRunFrom: startPitch to: endPitch
	"Play a fast chromatic run between the given pitches. Useful for auditioning a sound."

	(AbstractSound chromaticRunFrom: startPitch to: endPitch on: self) play.
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 8/13/1998 16:17'!
playSampleCount: n into: aSoundBuffer startingAt: startIndex
	"Mix the next n samples of this sound into the given buffer starting at the given index. Update the receiver's control parameters periodically."

	| fullVol samplesBetweenControlUpdates pastEnd i remainingSamples count |
	fullVol _ AbstractSound scaleFactor.
	samplesBetweenControlUpdates _ self samplingRate // self controlRate.
	pastEnd _ startIndex + n.  "index just after the last sample"
	i _ startIndex.
	[i < pastEnd] whileTrue: [
		remainingSamples _ self samplesRemaining.
		remainingSamples <= 0 ifTrue: [^ self].
		count _ pastEnd - i.
		samplesUntilNextControl < count ifTrue: [count _ samplesUntilNextControl].
		remainingSamples < count ifTrue: [count _ remainingSamples].
		self mixSampleCount: count into: aSoundBuffer startingAt: i leftVol: fullVol rightVol: fullVol.
		samplesUntilNextControl _ samplesUntilNextControl - count.
		samplesUntilNextControl <= 0 ifTrue: [
			self doControl.
			samplesUntilNextControl _ samplesBetweenControlUpdates].
		i _ i + count].
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 7/5/1998 17:53'!
playSilently
	"Compute the samples of this sound without outputting them. Used for performance analysis."

	| bufSize buf |
	self reset.
	bufSize _ self samplingRate // 10.
	buf _ SoundBuffer newStereoSampleCount: bufSize.
	[self samplesRemaining > 0] whileTrue: [
		buf primFill: 0.
		self playSampleCount: bufSize into: buf startingAt: 1].
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 1/26/98 22:06'!
playSilentlyUntil: startTime
	"Compute the samples of this sound without outputting them. Used to fast foward to a particular starting time. The start time is given in seconds."

	| buf startSample nextSample samplesRemaining n |
	self reset.
	buf _ SoundBuffer newStereoSampleCount: (self samplingRate // 10).
	startSample _ (startTime * self samplingRate) asInteger.
	nextSample _ 1.
	[self samplesRemaining > 0] whileTrue: [
		nextSample >= startSample ifTrue: [^ self].
		samplesRemaining _ startSample - nextSample.
		samplesRemaining > buf stereoSampleCount
			ifTrue: [n _ buf stereoSampleCount]
			ifFalse: [n _ samplesRemaining].
		self playSampleCount: n into: buf startingAt: 1.
		nextSample _ nextSample + n].
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 3/4/98 13:16'!
resumePlaying
	"Resume playing this sound from where it last stopped."

	SoundPlayer resumePlaying: self.
! !

!AbstractSound methodsFor: 'playing' stamp: 'jm 12/16/2001 13:22'!
samples
	"Answer a monophonic sample buffer containing my samples. The left and write channels are merged."
	"Warning: This may require a lot of memory!!"

	^ (self computeSamplesForSeconds: self duration) mergeStereo
! !

!AbstractSound methodsFor: 'playing' stamp: 'jmv 4/1/2009 21:36'!
viewSamples
	"Open a WaveEditor on my samples."

"	WaveEditor openOn: self samples"! !

!AbstractSound methodsFor: 'sampling rates' stamp: 'jm 12/15/97 14:15'!
controlRate
	"Answer the number of control changes per second."

	^ 100
! !

!AbstractSound methodsFor: 'sampling rates' stamp: 'jm 12/16/2001 13:14'!
originalSamplingRate
	"For sampled sounds, answer the sampling rate used to record the stored samples. For other sounds, this is the same as the playback sampling rate."

	^ SoundPlayer samplingRate
! !

!AbstractSound methodsFor: 'sampling rates' stamp: 'jm 12/17/97 18:00'!
samplingRate
	"Answer the sampling rate in samples per second."

	^ SoundPlayer samplingRate
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 8/17/1998 13:34'!
doControl
	"Update the control parameters of this sound using its envelopes, if any."
	"Note: This is only called at a small fraction of the sampling rate."

	| pitchModOrRatioChange |
	envelopes size > 0 ifTrue: [
		pitchModOrRatioChange _ false.
		1 to: envelopes size do: [:i |
			((envelopes at: i) updateTargetAt: mSecsSinceStart)
				ifTrue: [pitchModOrRatioChange _ true]].
		pitchModOrRatioChange ifTrue: [self internalizeModulationAndRatio]].
	mSecsSinceStart _ mSecsSinceStart + (1000 // self controlRate).
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 2/4/98 08:56'!
internalizeModulationAndRatio
	"Overridden by FMSound. This default implementation does nothing."
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 7/6/1998 06:40'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Mix the given number of samples with the samples already in the given buffer starting at the given index. Assume that the buffer size is at least (index + count) - 1. The leftVol and rightVol parameters determine the volume of the sound in each channel, where 0 is silence and ScaleFactor is full volume."

	self subclassResponsibility.
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 8/17/1998 13:45'!
reset
	"Reset my internal state for a replay. Methods that override this method should do super reset."

	mSecsSinceStart _ 0.
	samplesUntilNextControl _ 0.
	envelopes size > 0 ifTrue: [
		1 to: envelopes size do: [:i | (envelopes at: i) reset]].
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 12/17/97 17:57'!
samplesRemaining
	"Answer the number of samples remaining until the end of this sound. A sound with an indefinite ending time should answer some large integer such as 1000000."

	^ 1000000
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 9/9/1998 21:56'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds. This default implementation does nothing."
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 9/9/1998 21:54'!
stopGracefully
	"End this note with a graceful decay. If the note has envelopes, determine the decay time from its envelopes."

	| decayInMs env |
	envelopes isEmpty
		ifTrue: [
			self adjustVolumeTo: 0 overMSecs: 10.
			decayInMs _ 10]
		ifFalse: [
			env _ envelopes first.
			decayInMs _ env attackTime + env decayTime].
	self duration: (mSecsSinceStart + decayInMs) / 1000.0.
	self stopAfterMSecs: decayInMs.
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 1/5/98 14:21'!
storeSample: sample in: aSoundBuffer at: sliceIndex leftVol: leftVol rightVol: rightVol
	"This method is provided for documentation. To gain 10% more speed when running sound generation in Smalltalk, this method is hand-inlined into all sound generation methods that use it."

	| i s |
		leftVol > 0 ifTrue: [
			i _ (2 * sliceIndex) - 1.
			s _ (aSoundBuffer at: i) + ((sample * leftVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		rightVol > 0 ifTrue: [
			i _ 2 * sliceIndex.
			s _ (aSoundBuffer at: i) + ((sample * rightVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
! !

!AbstractSound methodsFor: 'sound generation' stamp: 'jm 12/17/97 17:57'!
updateVolume
	"Increment the volume envelope of this sound. To avoid clicks, the volume envelope must be interpolated at the sampling rate, rather than just at the control rate like other envelopes. At the control rate, the volume envelope computes the slope and next target volume volume for the current segment of the envelope (i.e., it sets the rate of change for the volume parameter). When that target volume is reached, incrementing is stopped until a new increment is set."
	"This method is provided for documentation. To gain 10% more speed when running sound generation in Smalltalk, it is hand-inlined into all sound generation methods that use it."

		scaledVolIncr ~= 0 ifTrue: [
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0]].
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 12/9/97 11:31'!
duration: seconds
	"Scale my envelopes to the given duration. Subclasses overriding this method should include a resend to super."

	envelopes do: [:e | e duration: seconds].
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 2/4/98 09:54'!
initialize

	envelopes _ #().
	mSecsSinceStart _ 0.
	samplesUntilNextControl _ 0.
	scaledVol _ (1.0 * ScaleFactor) rounded.
	scaledVolIncr _ 0.
	scaledVolLimit _ scaledVol.
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 3/24/1999 12:03'!
loudness: aNumber
	"Initialize my volume envelopes and initial volume. Subclasses overriding this method should include a resend to super."

	| vol |
	vol _ (aNumber asFloat max: 0.0) min: 1.0.
	envelopes do: [:e |
		(e isKindOf: VolumeEnvelope) ifTrue: [e scale: vol]].
	self initialVolume: vol.
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 7/6/1998 17:04'!
nameOrNumberToPitch: aStringOrNumber
	"Answer the pitch in cycles/second for the given pitch specification. The specification can be either a numeric pitch or pitch name such as 'c4'."

	aStringOrNumber isNumber
		ifTrue: [^ aStringOrNumber asFloat]
		ifFalse: [^ AbstractSound pitchForName: aStringOrNumber]
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 8/19/1998 08:45'!
setPitch: pitchNameOrNumber dur: d loudness: l
	"Initialize my envelopes for the given parameters. Subclasses overriding this method should include a resend to super."

	| p |
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	envelopes do: [:e |
		e volume: l.
		e centerPitch: p].
	self initialVolume: l.
	self duration: d.
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 8/3/1998 17:11'!
soundForMidiKey: midiKey dur: d loudness: l
	"Answer an initialized sound object (a copy of the receiver) that generates a note for the given MIDI key (in the range 0..127), duration (in seconds), and loudness (in the range 0.0 to 1.0)."

	^ self copy
		setPitch: (AbstractSound pitchForMIDIKey: midiKey)
		dur: d
		loudness: l
! !

!AbstractSound methodsFor: 'initialization' stamp: 'jm 8/3/1998 16:58'!
soundForPitch: pitchNameOrNumber dur: d loudness: l
	"Answer an initialized sound object (a copy of the receiver) that generates a note of the given pitch, duration, and loudness. Pitch may be a numeric pitch or a string pitch name such as 'c4'. Duration is in seconds and loudness is in the range 0.0 to 1.0."

	^ self copy setPitch: pitchNameOrNumber dur: d loudness: l
! !

!AbstractSound methodsFor: 'accessing' stamp: 'jm 12/16/2001 22:34'!
isStereo
	"Answer true if this sound has distinct left and right channels. (Every sound plays into a stereo sample buffer, but most sounds, which produce exactly the same samples on both channels, are not stereo.)"

	^ false
! !

!AbstractSound methodsFor: 'file i/o' stamp: 'pb 5/25/2016 01:35'!
storeAIFFOnFileNamed: fileName
	"Store this sound as a AIFF file of the given name."

	fileName asFileEntry writeStreamDo: [ :strm |
		strm binary.
		self storeAIFFSamplesOn: strm ]! !

!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 2/26/2016 17:03'!
storeAIFFSamplesOn: aBinaryStream
	"Store this sound as a 16-bit AIFF file at the current SoundPlayer sampling rate. Store both channels if self isStereo is true; otherwise, store the left channel only as a mono sound."

	| samplesToStore channelCount dataByteCount |
	samplesToStore _ (self duration * self samplingRate) ceiling.
	channelCount _ self isStereo ifTrue: [2] ifFalse: [1].
	dataByteCount _ samplesToStore * channelCount * 2.

	"write AIFF file header:"
	aBinaryStream nextPutAll: 'FORM' asByteArray.
	aBinaryStream nextSignedInt32Put: ((7 * 4) + 18) + dataByteCount bigEndian: true.
	aBinaryStream nextPutAll: 'AIFF' asByteArray.
	aBinaryStream nextPutAll: 'COMM' asByteArray.
	aBinaryStream nextSignedInt32Put: 18 bigEndian: true.
	aBinaryStream nextUnsignedInt16Put: channelCount bigEndian: true.
	aBinaryStream nextSignedInt32Put: samplesToStore bigEndian: true.
	aBinaryStream nextUnsignedInt16Put: 16 bigEndian: true.  "bits/sample"
	self storeExtendedFloat: self samplingRate on: aBinaryStream.
	aBinaryStream nextPutAll: 'SSND' asByteArray.
	aBinaryStream nextSignedInt32Put: dataByteCount + 8 bigEndian: true.
	aBinaryStream nextSignedInt32Put: 0 bigEndian: true.
	aBinaryStream nextSignedInt32Put: 0 bigEndian: true.

	"write data:"
	self storeSampleCount: samplesToStore bigEndian: true on: aBinaryStream.
! !

!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 4/11/2019 15:33:07'!
storeExtendedFloat: aNumber on: aBinaryStream
	"Store an Apple extended-precision 80-bit floating point number on the given stream."
	"Details: I could not find the specification for this format, so constants were determined empirically based on assumption of 1-bit sign, 15-bit exponent, 64-bit mantissa. This format does not seem to have an implicit one before the mantissa as some float formats do."

	| n isNeg exp mantissa |
	n _ aNumber asFloat.
	isNeg _ false.
	n < 0.0 ifTrue: [
		n _ 0.0 - n.
		isNeg _ true].
	exp _ (n log: 2.0) ceiling.
	mantissa _ (n * (2 raisedTo: 64 - exp)) truncated.
	exp _ exp + 16r4000 - 2.  "not sure why the -2 is needed..."
	isNeg ifTrue: [exp _ exp bitOr: 16r8000].  "set sign bit"
	aBinaryStream nextPut: ((exp bitShift: -8) bitAnd: 16rFF).
	aBinaryStream nextPut: (exp bitAnd: 16rFF).
	aBinaryStream nextUnsignedInt32Put: mantissa >> 32 bigEndian: true.
	aBinaryStream nextUnsignedInt32Put: (mantissa bitAnd: 16rFFFFFFFF) bigEndian: true.! !

!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 7/17/2017 15:39:47'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files). If self isStereo is true, both channels are stored, creating a stereo file. Otherwise, only the left channel is stored, creating a mono file."

	| bufSize stereoBuffer  |
	self reset.
	bufSize _ (2 * self samplingRate rounded) min: samplesToStore.  "two second buffer"
	stereoBuffer _ SoundBuffer newStereoSampleCount: bufSize.

	'Storing audio...' displayProgressAt: Sensor mousePoint
		from: 0 to: samplesToStore during: [:barBlock | | remaining out |
			remaining _ samplesToStore.
			[remaining > 0] whileTrue: [
				barBlock value: samplesToStore - remaining.
				stereoBuffer primFill: 0.  "clear the buffer"
				self playSampleCount: (bufSize min: remaining) into: stereoBuffer startingAt: 1.
				self isStereo
					ifTrue: [out _ stereoBuffer]
					ifFalse: [out _ stereoBuffer extractLeftChannel].
				(aBinaryStream isKindOf: StandardFileStream)
					ifTrue: [  "optimization for files: write sound buffer directly to file"
						aBinaryStream next: (out size // 2) putAll: out startingAt: 1]  "size in words"
					ifFalse: [  "for non-file streams:"
						1 to: out monoSampleCount do: [:i | aBinaryStream nextSignedInt16Put: (out at: i) bigEndian: bigEndianFlag ]].
				remaining _ remaining - bufSize]]! !

!AbstractSound methodsFor: 'file i/o' stamp: 'pb 5/25/2016 01:35'!
storeWAVOnFileNamed: fileName
	"Store this sound as a 16-bit Windows WAV file of the given name."

	fileName asFileEntry writeStreamDo: [ :strm |
		strm binary.
		self storeWAVSamplesOn: strm ]! !

!AbstractSound methodsFor: 'file i/o' stamp: 'jmv 2/26/2016 16:33'!
storeWAVSamplesOn: aBinaryStream
	"Store this sound as a 16-bit Windows WAV file at the current SoundPlayer sampling rate. Store both channels if self isStereo is true; otherwise, store the left channel only as a mono sound."

	| samplesToStore channelCount dataByteCount samplesPerSec bytesPerSec |
	samplesToStore _ (self duration * self samplingRate) ceiling.
	channelCount _ self isStereo ifTrue: [2] ifFalse: [1].
	dataByteCount _ samplesToStore * channelCount * 2.
	samplesPerSec _ self samplingRate rounded.
	bytesPerSec _ samplesPerSec * channelCount * 2.

	"file header"
	aBinaryStream
		nextPutAll: 'RIFF' asByteArray;
		nextUnsignedInt32Put: dataByteCount + 36 bigEndian: false;	"total length of all chunks"
		nextPutAll: 'WAVE' asByteArray.

	"format chunk"
	aBinaryStream
		nextPutAll: 'fmt ' asByteArray;
		nextUnsignedInt32Put: 16 bigEndian: false;	"length of this chunk"
		nextUnsignedInt16Put: 1 bigEndian: false;	"format tag"
		nextUnsignedInt16Put: channelCount bigEndian: false;
		nextUnsignedInt32Put: samplesPerSec bigEndian: false;
		nextUnsignedInt32Put: bytesPerSec bigEndian: false;
		nextUnsignedInt16Put: 4 bigEndian: false;	"alignment"
		nextUnsignedInt16Put: 16 bigEndian: false.	"bits per sample"

	"data chunk"
	aBinaryStream
		nextPutAll: 'data' asByteArray;
		nextUnsignedInt32Put: dataByteCount bigEndian: false.  "length of this chunk"

	self storeSampleCount: samplesToStore bigEndian: false on: aBinaryStream.
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 1/5/98 17:38'!
bachFugue
	"Play a fugue by J. S. Bach using and instance of me as the sound for all four voices."
	"PluckedSound bachFugue play"

	^ self bachFugueOn: self default
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 1/5/98 18:27'!
bachFugueOn: aSound
	"Play a fugue by J. S. Bach using the given sound as the sound for all four voices."
	"PluckedSound bachFugue play"

	^ MixedSound new
		add: (self bachFugueVoice1On: aSound) pan: 1.0;
		add: (self bachFugueVoice2On: aSound) pan: 0.0;
		add: (self bachFugueVoice3On: aSound) pan: 1.0;
		add: (self bachFugueVoice4On: aSound) pan: 0.0.
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 12/17/97 16:51'!
bachFugueVoice1On: aSound
	"Voice one of a fugue by J. S. Bach."

	^ self noteSequenceOn: aSound from: #(
		(1047 0.15 268)
		(988  0.15 268)
		(1047 0.30 268)
		(784  0.30 268)
		(831  0.30 268)
		(1047 0.15 268)
		(988  0.15 268)
		(1047 0.30 268)
		(1175 0.30 268)
		(784  0.30 268)
		(1047 0.15 268)
		(988  0.15 268)
		(1047 0.30 268)
		(1175 0.30 268)
		(698  0.15 268)
		(784  0.15 268)
		(831  0.60 268)
		(784  0.15 268)
		(698  0.15 268)
		(622  0.15 268)
		(1047 0.15 268)
		(988  0.15 268)
		(880  0.15 268)
		(784  0.15 268)
		(698  0.15 268)
		(622  0.15 268)
		(587  0.15 268)
		(523  0.30 268)
		(1245 0.30 268)
		(1175 0.30 268)
		(1047 0.30 268)
		(932  0.30 268)
		(880  0.30 268)
		(932  0.30 268)
		(1047 0.30 268)
		(740  0.30 268)
		(784  0.30 268)
		(880  0.30 268)
		(740  0.30 268)
		(784  0.60 268)
		(rest 0.15)
		(523  0.15 268)
		(587  0.15 268)
		(622  0.15 268)
		(698  0.15 268)
		(784  0.15 268)
		(831  0.45 268)
		(587  0.15 268)
		(622  0.15 268)
		(698  0.15 268)
		(784  0.15 268)
		(880  0.15 268)
		(932  0.45 268)
		(622  0.15 268)
		(698  0.15 268)
		(784  0.15 268)
		(831  0.15 268)
		(784  0.15 268)
		(698  0.15 268)
		(622  0.15 268)
		(587  0.30 268)
		(1047 0.15 268)
		(988  0.15 268)
		(1047 0.60 268)
		(rest 0.9)
		(1397 0.30 268)
		(1245 0.30 268)
		(1175 0.30 268)
		(rest 0.3)
		(831  0.30 268)
		(784  0.30 268)
		(698  0.30 268)
		(784  0.30 268)
		(698  0.15 268)
		(622  0.15 268)
		(698  0.30 268)
		(587  0.30 268)
		(784  0.60 268)
		(rest 0.3)
		(988  0.30 268)
		(1047 0.30 268)
		(1047 0.15 268)
		(988  0.15 268)
		(1047 0.30 268)
		(784  0.30 268)
		(831  0.60 268)
		(rest 0.3)
		(880  0.30 268)
		(932  0.30 268)
		(932  0.15 268)
		(880  0.15 268)
		(932  0.30 268)
		(698  0.30 268)
		(784  0.60 268)
		(rest 0.3)
		(784  0.30 268)
		(831  0.30 268)
		(831  0.30 268)
		(784  0.30 268)
		(698  0.30 268)
		(rest 0.3)
		(415  0.30 268)
		(466  0.30 268)
		(523  0.30 268)
		(rest 0.3)
		(415  0.15 268)
		(392  0.15 268)
		(415  0.30 268)
		(349  0.30 268)
		(466  0.30 268)
		(523  0.30 268)
		(466  0.30 268)
		(415  0.30 268)
		(466  0.30 268)
		(392  0.30 268)
		(349  0.30 268)
		(311  0.30 268)
		(349  0.30 268)
		(554  0.30 268)
		(523  0.30 268)
		(466  0.30 268)
		(523  0.30 268)
		(415  0.30 268)
		(392  0.30 268)
		(349  0.30 268)
		(392  0.30 268)
		(784  0.15 268)
		(740  0.15 268)
		(784  0.30 268)
		(523  0.30 268)
		(622  0.30 268)
		(784  0.15 268)
		(740  0.15 268)
		(784  0.30 268)
		(880  0.30 268)
		(587  0.30 268)
		(784  0.15 268)
		(740  0.15 268)
		(784  0.30 268)
		(880  0.30 268)
		(523  0.15 268)
		(587  0.15 268)
		(622  0.60 268)
		(587  0.15 268)
		(523  0.15 268)
		(466  0.30 346)
		(rest 0.45)
		(587  0.15 346)
		(659  0.15 346)
		(740  0.15 346)
		(784  0.15 346)
		(880  0.15 346)
		(932  0.45 346)
		(659  0.15 346)
		(698  0.15 346)
		(784  0.15 346)
		(880  0.15 346)
		(932  0.15 346)
		(1047 0.45 346)
		(740  0.15 346)
		(784  0.15 346)
		(880  0.15 346)
		(932  0.30 346)
		(622  0.15 346)
		(587  0.15 346)
		(622  0.30 346)
		(392  0.30 346)
		(415  0.30 346)
		(698  0.15 346)
		(622  0.15 346)
		(698  0.30 346)
		(440  0.30 346)
		(466  0.30 346)
		(784  0.15 346)
		(698  0.15 346)
		(784  0.30 346)
		(494  0.30 346)
		(523  0.15 346)
		(698  0.15 346)
		(622  0.15 346)
		(587  0.15 346)
		(523  0.15 346)
		(466  0.15 346)
		(440  0.15 346)
		(392  0.15 346)
		(349  0.30 346)
		(831  0.30 346)
		(784  0.30 346)
		(698  0.30 346)
		(622  0.30 346)
		(587  0.30 346)
		(622  0.30 346)
		(698  0.30 346)
		(494  0.30 346)
		(523  0.30 346)
		(587  0.30 346)
		(494  0.30 346)
		(523  0.60 346)
		(rest 0.3)
		(659  0.30 346)
		(698  0.30 346)
		(698  0.15 346)
		(659  0.15 346)
		(698  0.30 346)
		(523  0.30 346)
		(587  0.60 346)
		(rest 0.3)
		(587  0.30 346)
		(622  0.30 346)
		(622  0.15 346)
		(587  0.15 346)
		(622  0.30 346)
		(466  0.30 346)
		(523  1.20 346)
		(523  0.30 346)
		(587  0.15 346)
		(622  0.15 346)
		(698  0.15 346)
		(622  0.15 346)
		(698  0.15 346)
		(587  0.15 346)
		(494  0.30 457)
		(rest 0.6)
		(494  0.30 457)
		(523  0.30 457)
		(rest 0.6)
		(622  0.30 457)
		(587  0.30 457)
		(rest 0.6)
		(698  0.60 457)
		(rest 0.6)
		(698  0.30 457)
		(622  0.30 457)
		(831  0.30 457)
		(784  0.30 457)
		(698  0.30 457)
		(622  0.30 457)
		(587  0.30 457)
		(622  0.30 457)
		(698  0.30 457)
		(494  0.30 457)
		(523  0.30 457)
		(587  0.30 457)
		(494  0.30 457)
		(494  0.30 457)
		(523  0.30 457)
		(rest 0.3)
		(523  0.30 457)
		(698  0.15 457)
		(587  0.15 457)
		(622  0.15 457)
		(523  0.45 457)
		(494  0.30 457)
		(523  0.60 457)
		(rest 0.3)
		(659  0.30 268)
		(698  0.60 268)
		(rest 0.3)
		(698  0.30 268)
		(698  0.30 268)
		(622  0.15 268)
		(587  0.15 268)
		(622  0.30 268)
		(698  0.30 268)
		(587  0.40 268)
		(rest 0.4)
		(587  0.40 268)
		(rest 0.4)
		(523  1.60 268)).! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 12/17/97 16:52'!
bachFugueVoice2On: aSound
	"Voice two of a fugue by J. S. Bach."

	^ self noteSequenceOn: aSound from: #(
		(rest 4.8)
		(1568 0.15 346)
		(1480 0.15 346)
		(1568 0.30 346)
		(1047 0.30 346)
		(1245 0.30 346)
		(1568 0.15 346)
		(1480 0.15 346)
		(1568 0.30 346)
		(1760 0.30 346)
		(1175 0.30 346)
		(1568 0.15 346)
		(1480 0.15 346)
		(1568 0.30 346)
		(1760 0.30 346)
		(1047 0.15 346)
		(1175 0.15 346)
		(1245 0.60 346)
		(1175 0.15 346)
		(1047 0.15 346)
		(932  0.30 346)
		(1245 0.15 346)
		(1175 0.15 346)
		(1245 0.30 346)
		(784  0.30 346)
		(831  0.30 346)
		(1397 0.15 346)
		(1245 0.15 346)
		(1397 0.30 346)
		(880  0.30 346)
		(932  0.30 346)
		(1568 0.15 346)
		(1397 0.15 346)
		(1568 0.30 346)
		(988  0.30 346)
		(1047 0.30 346)
		(1175 0.15 346)
		(1245 0.15 346)
		(1397 0.90 346)
		(1245 0.15 346)
		(1175 0.15 346)
		(1047 0.15 346)
		(932  0.15 346)
		(831  0.15 346)
		(784  0.15 346)
		(698  0.30 346)
		(1661 0.30 346)
		(1568 0.30 346)
		(1397 0.30 346)
		(1245 0.30 346)
		(1175 0.30 346)
		(1245 0.30 346)
		(1397 0.30 346)
		(988  0.30 346)
		(1047 0.30 346)
		(1175 0.30 346)
		(988  0.30 346)
		(1047 0.30 457)
		(1568 0.15 457)
		(1480 0.15 457)
		(1568 0.30 457)
		(1175 0.30 457)
		(1245 0.60 457)
		(rest 0.3)
		(1319 0.30 457)
		(1397 0.30 457)
		(1397 0.15 457)
		(1319 0.15 457)
		(1397 0.30 457)
		(1047 0.30 457)
		(1175 0.60 457)
		(rest 0.3)
		(1175 0.30 457)
		(1245 0.30 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1245 0.30 457)
		(932  0.30 457)
		(1047 0.30 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1245 0.30 457)
		(1397 0.30 457)
		(932  0.30 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1245 0.30 457)
		(1397 0.30 457)
		(831  0.15 457)
		(932  0.15 457)
		(1047 0.60 457)
		(932  0.15 457)
		(831  0.15 457)
		(784  0.15 457)
		(622  0.15 457)
		(698  0.15 457)
		(784  0.15 457)
		(831  0.15 457)
		(932  0.15 457)
		(1047 0.15 457)
		(1175 0.15 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1047 0.15 457)
		(1175 0.15 457)
		(1245 0.15 457)
		(1397 0.15 457)
		(1568 0.15 457)
		(1760 0.15 457)
		(1865 0.15 457)
		(698  0.15 457)
		(784  0.15 457)
		(831  0.15 457)
		(932  0.15 457)
		(1047 0.15 457)
		(1175 0.15 457)
		(1319 0.15 457)
		(1397 0.15 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1245 0.15 457)
		(1397 0.15 457)
		(1568 0.15 457)
		(1760 0.15 457)
		(1976 0.15 457)
		(2093 0.30 457)
		(1976 0.15 457)
		(1760 0.15 457)
		(1568 0.15 457)
		(1397 0.15 457)
		(1245 0.15 457)
		(1175 0.15 457)
		(1047 0.30 457)
		(1245 0.30 457)
		(1175 0.30 457)
		(1047 0.30 457)
		(932  0.30 457)
		(880  0.30 457)
		(932  0.30 457)
		(1047 0.30 457)
		(740  0.30 457)
		(784  0.30 457)
		(880  0.30 457)
		(740  0.30 457)
		(784  0.30 457)
		(1175 0.15 457)
		(1047 0.15 457)
		(1175 0.30 457)
		(rest 0.6)
		(1319 0.15 457)
		(1175 0.15 457)
		(1319 0.30 457)
		(rest 0.6)
		(1480 0.15 457)
		(1319 0.15 457)
		(1480 0.30 457)
		(rest 0.6)
		(784  0.15 457)
		(698  0.15 457)
		(784  0.30 457)
		(rest 0.6)
		(880  0.15 457)
		(784  0.15 457)
		(880  0.30 457)
		(rest 0.6)
		(988  0.15 457)
		(880  0.15 457)
		(988  0.30 457)
		(rest 0.6)
		(1047 0.15 457)
		(988  0.15 457)
		(1047 0.30 457)
		(784  0.30 457)
		(831  0.30 457)
		(1047 0.15 457)
		(988  0.15 457)
		(1047 0.30 457)
		(1175 0.30 457)
		(784  0.30 457)
		(1047 0.15 457)
		(988  0.15 457)
		(1047 0.30 457)
		(1175 0.30 457)
		(698  0.15 457)
		(784  0.15 457)
		(831  0.60 457)
		(784  0.15 457)
		(698  0.15 457)
		(622  0.30 457)
		(1047 0.15 457)
		(988  0.15 457)
		(1047 0.30 457)
		(784  0.30 457)
		(831  0.60 457)
		(rest 0.3)
		(880  0.30 457)
		(932  0.30 457)
		(932  0.15 457)
		(880  0.15 457)
		(932  0.30 457)
		(698  0.30 457)
		(784  0.60 457)
		(rest 0.3)
		(784  0.60 457)
		(831  0.15 457)
		(932  0.15 457)
		(1047 0.15 457)
		(988  0.15 457)
		(1047 0.15 457)
		(831  0.15 457)
		(698  1.20 457)
		(698  0.30 591)
		(1175 0.15 591)
		(1047 0.15 591)
		(1175 0.30 591)
		(698  0.30 591)
		(622  0.30 591)
		(1245 0.15 591)
		(1175 0.15 591)
		(1245 0.30 591)
		(784  0.30 591)
		(698  0.30 591)
		(1397 0.15 591)
		(1245 0.15 591)
		(1397 0.30 591)
		(831  0.30 591)
		(784  0.15 591)
		(1397 0.15 591)
		(1245 0.15 591)
		(1175 0.15 591)
		(1047 0.15 591)
		(988  0.15 591)
		(880  0.15 591)
		(784  0.15 591)
		(1047 0.30 591)
		(1397 0.30 591)
		(1245 0.30 591)
		(1175 0.30 591)
		(rest 0.3)
		(831  0.30 591)
		(784  0.30 591)
		(698  0.30 591)
		(784  0.30 591)
		(698  0.15 591)
		(622  0.15 591)
		(698  0.30 591)
		(587  0.30 591)
		(831  0.30 591)
		(784  0.30 591)
		(rest 0.3)
		(880  0.30 591)
		(988  0.30 591)
		(1047 0.30 591)
		(698  0.15 591)
		(622  0.15 591)
		(587  0.15 591)
		(523  0.15 591)
		(523  0.30 591)
		(1047 0.15 346)
		(988  0.15 346)
		(1047 0.30 346)
		(784  0.30 346)
		(831  0.30 346)
		(1047 0.15 346)
		(988  0.15 346)
		(1047 0.30 346)
		(1175 0.30 346)
		(784  0.30 346)
		(1047 0.15 346)
		(988  0.15 346)
		(1047 0.30 346)
		(1175 0.30 346)
		(698  0.20 346)
		(784  0.20 346)
		(831  0.80 346)
		(784  0.20 346)
		(698  0.20 346)
		(659  1.60 346)).
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 12/17/97 16:52'!
bachFugueVoice3On: aSound
	"Voice three of a fugue by J. S. Bach."

	^ self noteSequenceOn: aSound from: #(
		(rest 14.4)
		(523  0.15 457)
		(494  0.15 457)
		(523  0.30 457)
		(392  0.30 457)
		(415  0.30 457)
		(523  0.15 457)
		(494  0.15 457)
		(523  0.30 457)
		(587  0.30 457)
		(392  0.30 457)
		(523  0.15 457)
		(494  0.15 457)
		(523  0.30 457)
		(587  0.30 457)
		(349  0.15 457)
		(392  0.15 457)
		(415  0.60 457)
		(392  0.15 457)
		(349  0.15 457)
		(311  0.15 457)
		(523  0.15 457)
		(494  0.15 457)
		(440  0.15 457)
		(392  0.15 457)
		(349  0.15 457)
		(311  0.15 457)
		(294  0.15 457)
		(262  0.15 457)
		(294  0.15 457)
		(311  0.15 457)
		(294  0.15 457)
		(262  0.15 457)
		(233  0.15 457)
		(208  0.15 457)
		(196  0.15 457)
		(175  0.15 457)
		(466  0.15 457)
		(415  0.15 457)
		(392  0.15 457)
		(349  0.15 457)
		(311  0.15 457)
		(294  0.15 457)
		(262  0.15 457)
		(233  0.15 457)
		(262  0.15 457)
		(294  0.15 457)
		(262  0.15 457)
		(233  0.15 457)
		(208  0.15 457)
		(196  0.15 457)
		(175  0.15 457)
		(156  0.15 457)
		(415  0.15 457)
		(392  0.15 457)
		(349  0.15 457)
		(311  0.15 457)
		(277  0.15 457)
		(262  0.15 457)
		(233  0.15 457)
		(208  0.30 457)
		(523  0.30 457)
		(466  0.30 457)
		(415  0.30 457)
		(392  0.30 457)
		(349  0.30 457)
		(392  0.30 457)
		(415  0.30 457)
		(294  0.30 457)
		(311  0.30 457)
		(349  0.30 457)
		(294  0.30 457)
		(311  0.30 457)
		(415  0.30 457)
		(392  0.30 457)
		(349  0.30 457)
		(392  0.30 457)
		(311  0.30 457)
		(294  0.30 457)
		(262  0.30 457)
		(294  0.30 457)
		(466  0.30 457)
		(415  0.30 457)
		(392  0.30 457)
		(415  0.30 457)
		(349  0.30 457)
		(311  0.30 457)
		(294  0.30 457)
		(311  0.30 457)
		(rest 1.2)
		(262  0.30 457)
		(233  0.30 457)
		(220  0.30 457)
		(rest 0.3)
		(311  0.30 457)
		(294  0.30 457)
		(262  0.30 457)
		(294  0.30 457)
		(262  0.15 457)
		(233  0.15 457)
		(262  0.30 457)
		(294  0.30 457)
		(196  0.30 591)
		(466  0.15 591)
		(440  0.15 591)
		(466  0.30 591)
		(294  0.30 591)
		(311  0.30 591)
		(523  0.15 591)
		(466  0.15 591)
		(523  0.30 591)
		(330  0.30 591)
		(349  0.30 591)
		(587  0.15 591)
		(523  0.15 591)
		(587  0.30 591)
		(370  0.30 591)
		(392  0.60 591)
		(rest 0.15)
		(196  0.15 591)
		(220  0.15 591)
		(247  0.15 591)
		(262  0.15 591)
		(294  0.15 591)
		(311  0.45 591)
		(220  0.15 591)
		(233  0.15 591)
		(262  0.15 591)
		(294  0.15 591)
		(311  0.15 591)
		(349  0.45 591)
		(247  0.15 591)
		(262  0.15 591)
		(294  0.15 591)
		(311  0.30 591)
		(rest 0.6)
		(330  0.30 591)
		(349  0.30 591)
		(175  0.30 591)
		(156  0.30 591)
		(147  0.30 591)
		(rest 0.3)
		(208  0.30 591)
		(196  0.30 591)
		(175  0.30 591)
		(196  0.30 591)
		(175  0.15 591)
		(156  0.15 591)
		(175  0.30 591)
		(196  0.30 591)
		(262  0.15 591)
		(294  0.15 591)
		(311  0.15 591)
		(294  0.15 591)
		(262  0.15 591)
		(233  0.15 591)
		(208  0.15 591)
		(196  0.15 591)
		(175  0.15 591)
		(466  0.15 591)
		(415  0.15 591)
		(392  0.15 591)
		(349  0.15 591)
		(311  0.15 591)
		(294  0.15 591)
		(262  0.15 591)
		(233  0.15 591)
		(262  0.15 591)
		(294  0.15 591)
		(262  0.15 591)
		(233  0.15 591)
		(208  0.15 591)
		(196  0.15 591)
		(175  0.15 591)
		(156  0.15 591)
		(415  0.15 591)
		(392  0.15 591)
		(349  0.15 591)
		(311  0.15 591)
		(294  0.15 591)
		(262  0.15 591)
		(233  0.15 591)
		(208  0.15 591)
		(233  0.15 591)
		(262  0.15 591)
		(233  0.15 591)
		(208  0.15 591)
		(196  0.15 591)
		(175  0.15 591)
		(156  0.15 591)
		(147  0.15 591)
		(392  0.15 591)
		(349  0.15 591)
		(311  0.15 591)
		(294  0.15 591)
		(262  0.15 591)
		(247  0.15 591)
		(220  0.15 591)
		(196  0.60 772)
		(196  0.60 772)
		(rest 0.15)
		(196  0.15 772)
		(220  0.15 772)
		(247  0.15 772)
		(262  0.15 772)
		(294  0.15 772)
		(311  0.15 772)
		(349  0.15 772)
		(392  0.15 772)
		(349  0.15 772)
		(415  0.15 772)
		(392  0.15 772)
		(349  0.15 772)
		(311  0.15 772)
		(294  0.15 772)
		(262  0.15 772)
		(247  0.30 772)
		(262  0.15 772)
		(494  0.15 772)
		(262  0.30 772)
		(196  0.30 772)
		(208  0.30 772)
		(262  0.15 772)
		(247  0.15 772)
		(262  0.30 772)
		(294  0.30 772)
		(196  0.30 772)
		(262  0.15 772)
		(247  0.15 772)
		(262  0.30 772)
		(294  0.30 772)
		(175  0.15 772)
		(196  0.15 772)
		(208  0.60 772)
		(196  0.15 772)
		(175  0.15 772)
		(156  0.60 772)
		(rest 0.3)
		(311  0.30 772)
		(294  0.30 772)
		(262  0.30 772)
		(392  0.30 772)
		(196  0.30 772)
		(262  3.60 268)
		(494  0.40 268)
		(rest 0.4)
		(494  0.40 268)
		(rest 0.4)
		(392  1.60 268)).
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 12/17/97 16:52'!
bachFugueVoice4On: aSound
	"Voice four of a fugue by J. S. Bach."

	^ self noteSequenceOn: aSound from: #(
		(rest 61.2)
		(131  0.15 500)
		(123  0.15 500)
		(131  0.30 500)
		(98   0.30 500)
		(104  0.30 500)
		(131  0.15 500)
		(123  0.15 500)
		(131  0.30 500)
		(147  0.30 500)
		(98   0.30 500)
		(131  0.15 500)
		(123  0.15 500)
		(131  0.30 500)
		(147  0.30 500)
		(87   0.15 500)
		(98   0.15 500)
		(104  0.60 500)
		(98   0.15 500)
		(87   0.15 500)
		(78   0.60 500)
		(rest 0.3)
		(156  0.30 500)
		(147  0.30 500)
		(131  0.30 500)
		(196  0.30 500)
		(98   0.30 500)
		(131  3.60 268)
		(131  3.20 205)).
! !

!AbstractSound class methodsFor: 'examples-bach fugue' stamp: 'jm 1/5/98 17:45'!
stereoBachFugue
	"Play fugue by J. S. Bach in stereo using different timbres."
	"AbstractSound stereoBachFugue play"

	"(AbstractSound bachFugueVoice1On: FMSound flute1) play"
	"(AbstractSound bachFugueVoice1On: PluckedSound default) play"

	^ MixedSound new
		add: (self bachFugueVoice1On: FMSound oboe1) pan: 0.2;
		add: (self bachFugueVoice2On: FMSound organ1) pan: 0.8;
		add: (self bachFugueVoice3On: PluckedSound default) pan: 0.4;
		add: (self bachFugueVoice4On: FMSound brass1) pan: 0.6.
! !

!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:50'!
busySignal: count
	"AbstractSound busySignal: 3"
	| m s |
	s _ SequentialSound new.
	m _ MixedSound new.
	m	add: (FMSound new setPitch: 480 dur: 0.5 loudness: 0.5);
		add: (FMSound new setPitch: 620 dur: 0.5 loudness: 0.5).
	s add: m.
	s add: (FMSound new setPitch: 1 dur: 0.5 loudness: 0).
	^ (RepeatingSound repeat: s count: count) play.

! !

!AbstractSound class methodsFor: 'utilities' stamp: 'eem 6/11/2008 12:40'!
dial: aString
	| s |
	"AbstractSound dial: '867-5309'" "ask for Jenny"

	s := SequentialSound new.
	aString do: [ :c | | index lo hi m |
		c = $,
			ifTrue: [ s add: (FMSound new setPitch: 1 dur: 1 loudness: 0) ]
			ifFalse: [
				(index := ('123A456B789C*0#D' indexOf: c)) > 0
					ifTrue: [
						lo := #(697 770 852 941) at: (index - 1 // 4 + 1).
						hi := #(1209 1336 1477 1633) at: (index - 1 \\ 4 + 1).
						m := MixedSound new.
						m add: (FMSound new setPitch: lo dur: 0.15 loudness: 0.5).
						m add: (FMSound new setPitch: hi dur: 0.15 loudness: 0.5).
						s add: m.
						s add: (FMSound new setPitch: 1 dur: 0.05 loudness: 0)]]].
	^ s play.

! !

!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:49'!
dialTone: duration
	"AbstractSound dialTone: 2"
	| m |
	m _ MixedSound new.
	m add: (FMSound new setPitch: 350 dur: duration loudness: 0.5).
	m add: (FMSound new setPitch: 440 dur: duration loudness: 0.5).
	m play.
	^ m! !

!AbstractSound class methodsFor: 'utilities' stamp: 'DSM 9/5/2000 13:50'!
hangUpWarning: count
	"AbstractSound hangUpWarning: 20"
	| m s |
	s _ SequentialSound new.
	m _ MixedSound new.
	m	add: (FMSound new setPitch: 1400 dur: 0.1 loudness: 0.5);
		add: (FMSound new setPitch: 2060 dur: 0.1 loudness: 0.5).
	s add: m; add: (FMSound new setPitch: 1 dur: 0.1 loudness: 0).
	^ (RepeatingSound repeat: s count: count) play

! !

!AbstractSound class methodsFor: 'utilities' stamp: 'jm 8/3/1998 16:16'!
indexOfBottomOctavePitch: p
	"Answer the index of the first pitch in the bottom octave equal to or higher than the given pitch. Assume that the given pitch is below the top pitch of the bottom octave."

	1 to: PitchesForBottomOctave size do: [:i |
		(PitchesForBottomOctave at: i) >= p ifTrue: [^ i]].
	self error: 'implementation error: argument pitch should be below or within the bottom octave'.
! !

!AbstractSound class methodsFor: 'utilities' stamp: 'jm 8/3/1998 16:16'!
midiKeyForPitch: pitchNameOrNumber
	"Answer the midiKey closest to the given pitch. Pitch may be a numeric pitch or a pitch name string such as 'c4'."
	"AbstractSound midiKeyForPitch: 440.0"

	| p octave i midiKey |
	pitchNameOrNumber isNumber
		ifTrue: [p _ pitchNameOrNumber asFloat]
		ifFalse: [p _ AbstractSound pitchForName: pitchNameOrNumber].
	octave _ -1.
	[p >= TopOfBottomOctave] whileTrue: [
		octave _ octave + 1.
		p _ p / 2.0].

	i _ self indexOfBottomOctavePitch: p.
	(i > 1) ifTrue: [
		(p - (PitchesForBottomOctave at: i - 1)) < ((PitchesForBottomOctave at: i) - p)
			ifTrue: [i _ i - 1]].

	midiKey _ ((octave * 12) + 11 + i).
	midiKey > 127 ifTrue: [midiKey _ 127].
	^ midiKey
! !

!AbstractSound class methodsFor: 'utilities' stamp: 'jm 8/3/1998 16:43'!
pitchForMIDIKey: midiKey
	"Answer the pitch for the given MIDI key."
	"(1 to: 127) collect: [:i | AbstractSound pitchForMIDIKey: i]"

	| indexInOctave octave |
	indexInOctave _ (midiKey \\ 12) + 1.
	octave _ (midiKey // 12) + 1.
	^ (PitchesForBottomOctave at: indexInOctave) *
		(#(1.0 2.0 4.0 8.0 16.0 32.0 64.0 128.0 256.0 512.0 1024.0) at: octave)
! !

!AbstractSound class methodsFor: 'utilities' stamp: 'jmv 2/6/2010 20:32'!
pitchForName: aString
	"AbstractSound pitchForName: 'c2'"
	"#(c 'c#' d eb e f fs g 'g#' a bf b) collect: [ :s | AbstractSound pitchForName: s, '4']"

	| s modifier octave i j noteName p |
	s _ ReadStream on: aString.
	modifier _ $n.
	noteName _ s next.
	(s atEnd not and: [ s peek isDigit ]) ifFalse: [ modifier _ s next ].
	octave _ s atEnd
		ifTrue: [ 4 ]
		ifFalse: [ Integer readFrom: s ].
	octave < 0 ifTrue: [ self error: 'cannot use negative octave number' ].
	i _ 'cdefgab' indexOf: noteName.
	i = 0 ifTrue: [ self error: 'bad note name: ', noteName asString ].
	i _ #(2 4 6 7 9 11 13) at: i.
	j _ 's#fb' indexOf: modifier.
	j = 0 ifFalse: [ i _ i + (#(1 1 -1 -1) at: j) ].  "i is now in range: [1..14]"
	"Table generator: (1 to: 14) collect: [ :i | 16.3516 * (2.0 raisedTo: (i - 2) asFloat / 12.0)]"
	p _ #(15.4339 16.3516 17.3239 18.354 19.4454 20.6017 21.8268 23.1247 24.4997 25.9565 27.5 29.1352 30.8677 32.7032) at: i.
	p _ p * (2.0 raisedToInteger: octave).
	^ p
! !

!AbstractSound class methodsFor: 'utilities' stamp: 'jmv 3/13/2012 12:13'!
pitchTable
	"AbstractSound pitchTable"

	| out i |
	out := WriteStream on: (String new: 1000).
	i := 12.
	0 to: 8 do: [:octave |
		#(c 'c#' d eb e f fs g 'g#' a bf b) do: [:noteName | | note |
			note := noteName, octave printString.
			out nextPutAll: note; tab.
			out nextPutAll: i printString; tab.
			out nextPutAll: (AbstractSound pitchForName: note) printString; newLine.
			i := i + 1]].
	^ out contents
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 6/30/1998 18:40'!
chromaticPitchesFrom: aPitch

	| halfStep pitch |
	halfStep _ 2.0 raisedTo: (1.0 / 12.0).
	pitch _ aPitch isNumber
			ifTrue: [aPitch]
			ifFalse: [self pitchForName: aPitch].
	pitch _ pitch / halfStep.
	^ (0 to: 14) collect: [:i | pitch _ pitch * halfStep]
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 8/18/1998 11:32'!
chromaticRunFrom: startPitch to: endPitch on: aSound
	"Answer a composite sound consisting of a rapid chromatic run between the given pitches on the given sound."
	"(AbstractSound chromaticRunFrom: 'c3' to: 'c#5' on: FMSound oboe1) play"

	| scale halfStep pEnd p |
	scale _ SequentialSound new.
	halfStep _ 2.0 raisedTo: (1.0 / 12.0).
	endPitch isNumber
		ifTrue: [pEnd _ endPitch asFloat]
		ifFalse: [pEnd _ AbstractSound pitchForName: endPitch].
	startPitch isNumber
		ifTrue: [p _ startPitch asFloat]
		ifFalse: [p _ AbstractSound pitchForName: startPitch].
	[p <= pEnd] whileTrue: [
		scale add: (aSound soundForPitch: p dur: 0.2 loudness: 0.5).
		p _ p * halfStep].
	^ scale
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/5/98 17:35'!
chromaticScale
	"PluckedSound chromaticScale play"

	^ self chromaticScaleOn: self default
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/31/98 16:14'!
chromaticScaleOn: aSound
	"PluckedSound chromaticScale play"

	^ self noteSequenceOn: aSound
		from: (((self chromaticPitchesFrom: #c4) copyFrom: 1 to: 13)
			 collect: [:pitch | Array with: pitch with: 0.5 with: 300])
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/5/98 17:36'!
hiMajorScale
	"FMSound hiMajorScale play"

	^ self hiMajorScaleOn: self default
! !

!AbstractSound class methodsFor: 'examples' stamp: 'di 1/30/98 16:00'!
hiMajorScaleOn: aSound
	"FMSound hiMajorScale play"

	^ self majorScaleOn: aSound from: #c6! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/5/98 17:36'!
lowMajorScale
	"PluckedSound lowMajorScale play"

	^ self lowMajorScaleOn: self default
! !

!AbstractSound class methodsFor: 'examples' stamp: 'di 1/30/98 16:01'!
lowMajorScaleOn: aSound
	"PluckedSound lowMajorScale play"

	^ self majorScaleOn: aSound from: #c3! !

!AbstractSound class methodsFor: 'examples' stamp: 'di 1/30/98 16:04'!
majorChord
	"FMSound majorChord play"
	^ self majorChordOn: self default from: #c4! !

!AbstractSound class methodsFor: 'examples' stamp: 'eem 6/11/2008 12:41'!
majorChordOn: aSound from: aPitch
	"FMSound majorChord play"

	| score majorScale leadingRest pan |
	majorScale := self majorPitchesFrom: aPitch.
	score := MixedSound new.
	leadingRest := pan := 0.
	#(1 3 5 8) do: [:noteIndex | | note |
		note := aSound
					soundForPitch: (majorScale at: noteIndex)
					dur: 2.0 - leadingRest
					loudness: 0.3.
		score add: (RestSound dur: leadingRest), note pan: pan.
		leadingRest := leadingRest + 0.2.
		pan := pan + 0.3].
	^ score
! !

!AbstractSound class methodsFor: 'examples' stamp: 'di 1/30/98 14:45'!
majorPitchesFrom: aPitch
	| chromatic |
	chromatic _ self chromaticPitchesFrom: aPitch.
	^ #(1 3 5 6 8 10 12 13 15 13 12 10 8 6 5 3 1) collect: [:i | chromatic at: i].
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/5/98 17:34'!
majorScale
	"FMSound majorScale play"

	^ self majorScaleOn: self default
! !

!AbstractSound class methodsFor: 'examples' stamp: 'di 1/30/98 16:00'!
majorScaleOn: aSound
	"FMSound majorScale play"

	^ self majorScaleOn: aSound from: #c5! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 7/13/1998 13:09'!
majorScaleOn: aSound from: aPitch
	"FMSound majorScale play"

	^ self noteSequenceOn: aSound
		from: ((self majorPitchesFrom: aPitch)
			 collect: [:pitch | Array with: pitch with: 0.5 with: 300])
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jmv 3/1/2010 14:41'!
majorScaleOn: aSound from: aPitch octaves: octaveCount
	"(AbstractSound majorScaleOn: FMSound oboe1 from: #c2 octaves: 5) play"

	| startingPitch pitches |
	startingPitch _ aPitch isNumber
		ifTrue: [aPitch]
		ifFalse: [self pitchForName: aPitch].
	pitches _ OrderedCollection new.
	0 to: octaveCount - 1 do: [ :i | | chromatic |
		chromatic _ self chromaticPitchesFrom: startingPitch * (2 raisedTo: i).
		#(1 3 5 6 8 10 12) do: [ :j | pitches addLast: (chromatic at: j)]].
	pitches addLast: startingPitch * (2 raisedTo: octaveCount).
	^ self noteSequenceOn: aSound
		from: (pitches collect: [ :pitch | Array with: pitch with: 0.5 with: 300])
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jm 1/5/98 17:32'!
scaleTest
	"AbstractSound scaleTest play"

	^ MixedSound new
		add: FMSound majorScale pan: 0;
		add: (PluckedSound lowMajorScale delayedBy: 0.5) pan: 1.0.
! !

!AbstractSound class methodsFor: 'examples' stamp: 'jmv 1/14/2013 21:11'!
testFMInteractively
	"Experiment with different settings of the FM modulation and multiplier settings interactively by moving the mouse. The top-left corner of the screen is 0 for both parameters. Stop when the mouse is pressed."
	"AbstractSound testFMInteractively"

	| s mousePt lastVal status mod ratio |
	SoundPlayer startPlayerProcessBufferSize: 1100 rate: 11025 stereo: false.
	s _ FMSound pitch: 440.0 dur: 200.0 loudness: 0.2.

	SoundPlayer playSound: s.
	lastVal _ nil.
	[Sensor isAnyButtonPressed] whileFalse: [
		mousePt _ Sensor mousePoint.
		mousePt ~= lastVal ifTrue: [
			mod _ mousePt x asFloat / 20.0.
			ratio _ mousePt y asFloat / 20.0.
			s modulation: mod ratio: ratio.
			lastVal _ mousePt.
			status _
'mod: ', mod printString, '
ratio: ', ratio printString.
			status displayOn: Display at: 10@10]].

	SoundPlayer shutDown.
! !

!AbstractSound class methodsFor: 'instance creation' stamp: 'jm 1/5/98 17:40'!
default
	"Return a default sound prototype for this class, with envelopes if appropriate. (This is in contrast to new, which returns a raw instance without envelopes.)"

	^ self new
! !

!AbstractSound class methodsFor: 'instance creation' stamp: 'jm 12/17/97 17:26'!
dur: d
	"Return a rest of the given duration."

	^ self basicNew setDur: d
! !

!AbstractSound class methodsFor: 'instance creation' stamp: 'eem 6/11/2008 12:41'!
noteSequenceOn: aSound from: anArray
	"Build a note sequence (i.e., a SequentialSound) from the given array using the given sound as the instrument. Elements are either (pitch, duration, loudness) triples or (#rest duration) pairs.  Pitches can be given as names or as numbers."
	| score |
	score := SequentialSound new.
	anArray do: [:el | | pitch |
		el size = 3
			ifTrue: [
				pitch := el at: 1.
				pitch isNumber ifFalse: [pitch := self pitchForName: pitch].
				score add: (
					aSound
						soundForPitch: pitch
						dur: (el at: 2)
						loudness: (el at: 3) / 1000.0)]
			ifFalse: [
				score add: (RestSound dur: (el at: 2))]].
	^ score
! !

!AbstractSound class methodsFor: 'instance creation' stamp: 'jm 12/17/97 17:27'!
pitch: p dur: d loudness: l
	"Return a new sound object for a note with the given parameters."

	^ self new setPitch: p dur: d loudness: l
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jm 8/19/1998 12:42'!
fileOutSoundLibrary
	"File out the current sound library."
	"AbstractSound fileOutSoundLibrary"

	self fileOutSoundLibrary: Sounds.
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'pb 5/25/2016 01:35'!
fileOutSoundLibrary: aDictionary
	"File out the given dictionary, which is assumed to contain sound and instrument objects keyed by their names."
	"Note: This method is separated out so that one can file out edited sound libraries, as well as the system sound library. To make such a collection, you can inspect AbstractSound sounds and remove the items you don't want. Then do: 'AbstractSound fileOutSoundLibrary: self' from the Dictionary inspector."

	| fileName refStream |
	(aDictionary isKindOf: Dictionary)
		ifFalse: [self error: 'arg should be a dictionary of sounds'].
	fileName _ FillInTheBlankMorph request: 'Sound library file name?'.
	fileName isEmptyOrNil ifTrue: [^ self].
	(fileName, '.sounds') asFileEntry writeStreamDo: [ :file |
		refStream _ SmartRefStream on: file.
		[ refStream nextPut: aDictionary ]]! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jm 9/12/1998 21:35'!
storeFiledInSound: snd named: sndName
	"Store the given sound in the sound library. Use the given name if it isn't in use, otherwise ask the user what to do."

	| menu choice i |
	(Sounds includesKey: sndName) ifFalse: [  "no name clash"
		Sounds at: sndName put: snd.
		^ self].

	(Sounds at: sndName) == UnloadedSnd ifTrue: [
		"re-loading a sound that was unloaded to save space"
		Sounds at: sndName put: snd.
		^ self].

	"the given sound name is already used"
	menu _ SelectionMenu selections:
		#('replace the existing sound' 'rename the new sound' 'skip it').
	choice _ menu startUpWithCaption:
		'"', sndName, '" has the same name as an existing sound'.
	(choice beginsWith: 'replace') ifTrue: [
		Sounds at: sndName put: snd.
		^ self].
	(choice beginsWith: 'rename') ifTrue: [
		i _ 2.
		[Sounds includesKey: (sndName, ' v', i printString)] whileTrue: [i _ i + 1].
		Sounds at: (sndName, ' v', i printString) put: snd].
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jmv 1/16/2011 23:57'!
unloadSampledTimbres
	"This can be done to unload those bulky sampled timbres to shrink the image. The unloaded sounds are replaced by a well-known 'unloaded sound' object to enable the unloaded sounds to be detected when the process is reversed."
	"AbstractSound unloadSampledTimbres"

	Sounds keys do: [:soundName |
		(((Sounds at: soundName) isKindOf: SampledInstrument) or:
		 [(Sounds at: soundName) isKindOf: LoopedSampledSound]) ifTrue: [
			Sounds at: soundName put: self unloadedSound]].
	self updateScorePlayers.
	Smalltalk garbageCollect! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jm 9/11/1998 16:47'!
unloadSoundNamed: soundName

	(Sounds includesKey: soundName) ifTrue: [
		Sounds at: soundName put: self unloadedSound].
	self updateScorePlayers.
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jm 9/12/1998 21:48'!
unloadedSound
	"Answer a sound to be used as the place-holder for sounds that have been unloaded."

	UnloadedSnd ifNil: [UnloadedSnd _ UnloadedSound default copy].
	^ UnloadedSnd
! !

!AbstractSound class methodsFor: 'sound library-file in/out' stamp: 'jmv 4/1/2009 21:39'!
updateScorePlayers
	"Force all ScorePlayers to update their instrument list from the sound library. This may done after loading, unloading, or replacing a sound to make all ScorePlayers feel the change."

	| soundsBeingEdited |
	ScorePlayer allSubInstancesDo: [:p | p pause].
	SoundPlayer shutDown.
"	soundsBeingEdited := EnvelopeEditorMorph allSubInstances 
				collect: [:ed | ed soundBeingEdited].
	ScorePlayerMorph 
		allSubInstancesDo: [:p | p updateInstrumentsFromLibraryExcept: soundsBeingEdited]"! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jm 8/14/1998 13:27'!
initSounds
	"AbstractSound initSounds"

	Sounds _ Dictionary new.
	(FMSound class organization listAtCategoryNamed: #instruments)
		do: [:sel | Sounds at: sel asString put: (FMSound perform: sel)].
! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jm 8/14/1998 13:25'!
soundNamed: soundName

	^ Sounds at: soundName
! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jm 3/4/98 10:29'!
soundNamed: soundName ifAbsent: aBlock

	^ Sounds at: soundName ifAbsent: aBlock
! !

!AbstractSound class methodsFor: 'sound library' stamp: 'di 11/7/2000 12:12'!
soundNamed: soundName put: aSound

	Sounds at: soundName put: aSound.
	AbstractSound updateScorePlayers.
! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jmv 1/16/2011 23:53'!
soundNames

	^ Sounds keys sort! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jm 8/4/1998 18:26'!
sounds

	^ Sounds
! !

!AbstractSound class methodsFor: 'sound library' stamp: 'jmv 3/2/2010 09:00'!
updateFMSounds
	"AbstractSound updateFMSounds"

	Sounds keys do: [:k |
		((Sounds at: k) isKindOf: FMSound) ifTrue: [
			Sounds removeKey: k ifAbsent: nil]].

	(FMSound class organization listAtCategoryNamed: #instruments) do:
		[:sel | Sounds at: sel asString put: (FMSound perform: sel)].
! !

!AbstractSound class methodsFor: 'class initialization' stamp: 'jm 8/3/1998 16:13'!
initialize
	"AbstractSound initialize"
 
	| bottomC |
	ScaleFactor _ 2 raisedTo: 15.
	FloatScaleFactor _ ScaleFactor asFloat.
	MaxScaledValue _ ((2 raisedTo: 31) // ScaleFactor) - 1.  "magnitude of largest scaled value in 32-bits"

	"generate pitches for c-1 through c0"
	bottomC _ (440.0 / 32) * (2.0 raisedTo: -9.0 / 12.0).
	PitchesForBottomOctave _ (0 to: 12) collect: [:i | bottomC * (2.0 raisedTo: i asFloat / 12.0)].
	TopOfBottomOctave _ PitchesForBottomOctave last.
! !

!AbstractSound class methodsFor: 'class initialization' stamp: 'jm 1/5/98 13:51'!
scaleFactor

	^ ScaleFactor
! !

!AbstractSound class methodsFor: 'primitive generation' stamp: 'ar 2/3/2001 15:30'!
translatedPrimitives
	^#(
		(FMSound mixSampleCount:into:startingAt:leftVol:rightVol:)
		(PluckedSound mixSampleCount:into:startingAt:leftVol:rightVol:)
		(LoopedSampledSound mixSampleCount:into:startingAt:leftVol:rightVol:)
		(SampledSound mixSampleCount:into:startingAt:leftVol:rightVol:)
		(ReverbSound applyReverbTo:startingAt:count:)
	).
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 3/26/98 10:45'!
duration

	^ initialCount asFloat / self samplingRate asFloat
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 9/9/1998 07:49'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:44'!
internalizeModulationAndRatio
	"Recompute the internal state for the modulation index and frequency ratio relative to the current pitch."

	modulation < 0.0 ifTrue: [modulation _ modulation negated].
	multiplier < 0.0 ifTrue: [multiplier _ multiplier negated].
	normalizedModulation _
		((modulation * scaledIndexIncr)  / ScaleFactor) asInteger.
	scaledOffsetIndexIncr _ (multiplier * scaledIndexIncr) asInteger.

	"clip to maximum values if necessary"
	normalizedModulation > MaxScaledValue ifTrue: [
		normalizedModulation _ MaxScaledValue.
		modulation _ (normalizedModulation * ScaleFactor) asFloat / scaledIndexIncr].
	scaledOffsetIndexIncr > (scaledWaveTableSize // 2) ifTrue: [
		scaledOffsetIndexIncr _ scaledWaveTableSize // 2.
		multiplier _ scaledOffsetIndexIncr asFloat / scaledIndexIncr].
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:15'!
modulation
	"Return the FM modulation index."

	^ modulation
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:22'!
modulation: mod
	"Set the FM modulation index. Typical values range from 0 (no modulation) to 5, although values up to about 10 are sometimes useful."
	"Warning: This method is intended primarily for use by envelopes. For efficiency during envelope processing, this change will not take effect until internalizeModulationAndRatio is called."

	modulation _ mod asFloat.
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:39'!
modulation: mod multiplier: freqRatio
	"For backward compatibility. Needed to read old .fmp files."

	self modulation: mod ratio: freqRatio.
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:41'!
modulation: mod ratio: freqRatio
	"Set the modulation index and carrier to modulation frequency ratio for this sound, and compute the internal state that depends on these parameters."

	modulation _ mod asFloat.
	multiplier _ freqRatio asFloat.
	self internalizeModulationAndRatio.
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 12/17/97 18:05'!
multiplier

	^ multiplier
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 8/7/1998 15:45'!
pitch

	^ (self samplingRate asFloat * scaledIndexIncr / ScaleFactor) asFloat / waveTable size
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:38'!
pitch: p
	"Warning: Since the modulation and ratio are relative to the current pitch, some internal state must be recomputed when the pitch is changed. However, for efficiency during envelope processing, this compuation will not be done until internalizeModulationAndRatio is called."

	scaledIndexIncr _
		((p asFloat * waveTable size asFloat * ScaleFactor asFloat) / self samplingRate asFloat) asInteger
			min: (waveTable size // 2) * ScaleFactor.
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:08'!
ratio
	"Return the FM modulation to carrier frequency ratio."

	^ multiplier
! !

!FMSound methodsFor: 'accessing' stamp: 'jm 2/4/98 07:22'!
ratio: freqRatio
	"Set the FM modulation to carrier frequency ratio."
	"Warning: This method is intended primarily for use by envelopes. For efficiency during envelope processing, this change will not take effect until internalizeModulationAndRatio is called."

	multiplier _ freqRatio asFloat.
! !

!FMSound methodsFor: 'initialization' stamp: 'jm 7/5/1998 11:44'!
initialize

	super initialize.
	waveTable _ SineTable.
	scaledWaveTableSize _ waveTable size * ScaleFactor.
	self setPitch: 440.0 dur: 1.0 loudness: 0.2.
! !

!FMSound methodsFor: 'initialization' stamp: 'jm 7/6/1998 17:08'!
setPitch: pitchNameOrNumber dur: d loudness: vol
	"(FMSound pitch: 'a4' dur: 2.5 loudness: 0.4) play"

	super setPitch: pitchNameOrNumber dur: d loudness: vol.
	modulation ifNil: [modulation _ 0.0].
	multiplier ifNil: [multiplier _ 0.0].
	self pitch: (self nameOrNumberToPitch: pitchNameOrNumber).
	self reset.
! !

!FMSound methodsFor: 'initialization' stamp: 'jm 9/20/1998 10:10'!
setWavetable: anArray
	"(AbstractSound lowMajorScaleOn: (FMSound new setWavetable: AA)) play"

	| samples p dur vol |
	"copy the array into a SoundBuffer if necessary"
	anArray class isPointers
		ifTrue: [samples _ SoundBuffer fromArray: anArray]
		ifFalse: [samples _ anArray].

	p _ self pitch.
	dur _ self duration.
	vol _ self loudness.
	waveTable _ samples.
	scaledWaveTableSize _ waveTable size * ScaleFactor.
	self setPitch: p dur: dur loudness: vol.
! !

!FMSound methodsFor: 'sound generation' stamp: 'ar 2/3/2001 15:22'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play samples from a wave table by stepping a fixed amount through the table on every sample. The table index and increment are scaled to allow fractional increments for greater pitch accuracy."
	"(FMSound pitch: 440.0 dur: 1.0 loudness: 0.5) play"

	| doingFM lastIndex sample offset i s |
	<primitive:'primitiveMixFMSound' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #waveTable declareC: 'short int *waveTable'.

	doingFM _ (normalizedModulation ~= 0) and: [scaledOffsetIndexIncr ~= 0].
	lastIndex _ (startIndex + n) - 1.
	startIndex to: lastIndex do: [:sliceIndex |
		sample _ (scaledVol * (waveTable at: (scaledIndex // ScaleFactor) + 1)) // ScaleFactor.
		doingFM
			ifTrue: [
				offset _ normalizedModulation * (waveTable at: (scaledOffsetIndex // ScaleFactor) + 1).
				scaledOffsetIndex _ (scaledOffsetIndex + scaledOffsetIndexIncr) \\ scaledWaveTableSize.
				scaledOffsetIndex < 0
					ifTrue: [scaledOffsetIndex _ scaledOffsetIndex + scaledWaveTableSize].
				scaledIndex _ (scaledIndex + scaledIndexIncr + offset) \\ scaledWaveTableSize.
				scaledIndex < 0
					ifTrue: [scaledIndex _ scaledIndex + scaledWaveTableSize]]
			ifFalse: [
				scaledIndex _ (scaledIndex + scaledIndexIncr) \\ scaledWaveTableSize].

		leftVol > 0 ifTrue: [
			i _ (2 * sliceIndex) - 1.
			s _ (aSoundBuffer at: i) + ((sample * leftVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		rightVol > 0 ifTrue: [
			i _ 2 * sliceIndex.
			s _ (aSoundBuffer at: i) + ((sample * rightVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].

		scaledVolIncr ~= 0 ifTrue: [
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0]]].

	count _ count - n.
! !

!FMSound methodsFor: 'sound generation' stamp: 'jm 2/4/98 20:12'!
reset

	self internalizeModulationAndRatio.
	super reset.
	count _ initialCount.
	scaledIndex _ 0.
	scaledOffsetIndex _ 0.
! !

!FMSound methodsFor: 'sound generation' stamp: 'jm 12/8/97 19:34'!
samplesRemaining

	^ count
! !

!FMSound methodsFor: 'sound generation' stamp: 'jm 9/9/1998 21:55'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds."

	count _ (mSecs * self samplingRate) // 1000.
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
bass1
	"FMSound bass1 play"
	"(FMSound lowMajorScaleOn: FMSound bass1) play"

	| snd |
	snd _ FMSound new modulation: 0 ratio: 0.
	snd addEnvelope: (VolumeEnvelope exponentialDecay: 0.95).
	^ snd setPitch: 220 dur: 1.0 loudness: 0.3
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 5/30/1999 20:37'!
bassoon1
	"FMSound bassoon1 play"
	"(FMSound lowMajorScaleOn: FMSound bassoon1) play"

	| snd p env |
	snd _ FMBassoonSound new ratio: 1.

	p _ OrderedCollection new.
	p add: 0@0.0; add: 40@0.45; add: 90@1.0; add: 180@0.9; add: 270@1.0; add: 320@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 3 loopEnd: 5).

	p _ OrderedCollection new.
	p add: 0@0.2; add: 40@0.9; add: 90@0.6; add: 270@0.6; add: 320@0.5.
	env _ Envelope points: p loopStart: 3 loopEnd: 4.
	env updateSelector: #modulation:; scale: 5.05.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
brass1
	"FMSound brass1 play"
	"(FMSound lowMajorScaleOn: FMSound brass1) play"

	| snd p env |
	snd _ FMSound new modulation: 0 ratio: 1.
	p _ OrderedCollection new.
	p add: 0@0.0; add: 30@0.8; add: 90@1.0; add: 120@0.9; add: 220@0.7; add: 320@0.9; add: 360@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 4 loopEnd: 6).

	p _ OrderedCollection new.
	p add: 0@0.5; add: 60@1.0; add: 120@0.8; add: 220@0.65; add: 320@0.8; add: 360@0.0.
	env _ Envelope points: p loopStart: 3 loopEnd: 5.
	env target: snd; updateSelector: #modulation:; scale: 5.0.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
brass2
	"FMSound brass2 play"
	"(FMSound lowMajorScaleOn: FMSound brass2) play"

	| snd p env |
	snd _ FMSound new modulation: 1 ratio: 1.

	p _ OrderedCollection new.
	p add: 0@0.0; add: 20@1.0; add: 40@0.9; add: 100@0.7; add: 160@0.9; add: 200@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 3 loopEnd: 5).

	p _ OrderedCollection new.
	p add: 0@0.5; add: 30@1.0; add: 40@0.8; add: 100@0.7; add: 160@0.8; add: 200@0.0.
	env _ Envelope points: p loopStart: 3 loopEnd: 5.
	env updateSelector: #modulation:; scale: 5.0.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:10'!
clarinet
	"FMSound clarinet play"
	"(FMSound lowMajorScaleOn: FMSound clarinet) play"

	| snd p env |
	snd _ FMSound new modulation: 0 ratio: 2.

	p _ OrderedCollection new.
	p add: 0@0.0; add: 60@1.0; add: 310@1.0; add: 350@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).

	p _ OrderedCollection new.
	p add: 0@0.0167; add: 60@0.106; add: 310@0.106; add: 350@0.0.
	env _ Envelope points: p loopStart: 2 loopEnd: 3.
	env updateSelector: #modulation:; scale: 10.0.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 5/30/1999 10:20'!
clarinet2
	"FMSound clarinet2 play"
	"(FMSound lowMajorScaleOn: FMSound clarinet2) play"

	| snd p env |
	snd _ FMClarinetSound new modulation: 0 ratio: 2.

	p _ OrderedCollection new.
	p add: 0@0.0; add: 60@1.0; add: 310@1.0; add: 350@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).

	p _ OrderedCollection new.
	p add: 0@0.0167; add: 60@0.106; add: 310@0.106; add: 350@0.0.
	env _ Envelope points: p loopStart: 2 loopEnd: 3.
	env updateSelector: #modulation:; scale: 10.0.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5

! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/5/98 17:35'!
default

	^ self oboe1
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 8/14/1998 13:02'!
flute1
	"FMSound flute1 play"
	"(FMSound majorScaleOn: FMSound flute1) play"

	| snd p |
	snd _ FMSound new.
	p _ OrderedCollection new.
	p add: 0@0; add: 20@1.0; add: 100@1.0; add: 120@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 8/14/1998 13:02'!
flute2
	"FMSound flute2 play"
	"(FMSound majorScaleOn: FMSound flute2) play"

	| snd p |
	snd _ FMSound new.
	p _ OrderedCollection new.
	p add: 0@0; add: 20@1.0; add: 100@1.0; add: 120@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).
	snd addEnvelope: (RandomEnvelope for: #pitch:).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 9/2/1999 13:32'!
marimba
	"FMSound marimba play"
	"(FMSound majorScaleOn: FMSound marimba) play"

	| snd p env |
	snd _ FMSound new modulation: 1 ratio: 0.98.

	p _ OrderedCollection new.
	p add: 0@1.0; add: 10@0.3; add: 40@0.1; add: 80@0.02; add: 120@0.1; add: 160@0.02; add: 220@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 4 loopEnd: 6).

	p _ OrderedCollection new.
	p add: 0@1.2; add: 80@0.85; add: 120@1.0; add: 160@0.85; add: 220@0.0.
	env _ Envelope points: p loopStart: 2 loopEnd: 4.
	env updateSelector: #modulation:.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
mellowBrass
	"FMSound mellowBrass play"
	"(FMSound lowMajorScaleOn: FMSound mellowBrass) play"

	| snd p env |
	snd _ FMSound new modulation: 0 ratio: 1.

	p _ OrderedCollection new.
	p add: 0@0.0; add: 70@0.325; add: 120@0.194; add: 200@0.194; add: 320@0.194; add: 380@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 3 loopEnd: 5).

	p _ OrderedCollection new.
	p add: 0@0.1; add: 70@0.68; add: 120@0.528; add: 200@0.519; add: 320@0.528; add: 380@0.0.
	env _ Envelope points: p loopStart: 3 loopEnd: 5.
	env updateSelector: #modulation:; scale: 5.0.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
oboe1
	"FMSound oboe1 play"
	"(FMSound majorScaleOn: FMSound oboe1) play"

	| snd p |
	snd _ FMSound new modulation: 1 ratio: 1.
	p _ OrderedCollection new.
	p add: 0@0.0; add: 10@1.0; add: 100@1.0; add: 120@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:00'!
oboe2
	"FMSound oboe2 play"
	"(FMSound majorScaleOn: FMSound oboe2) play"

	| snd p |
	snd _ FMSound new modulation: 1 ratio: 1.
	p _ OrderedCollection new.
	p add: 0@0; add: 20@1.0; add: 100@1.0; add: 120@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).
	snd addEnvelope: (RandomEnvelope for: #pitch:).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 1/14/1999 12:56'!
organ1
	"FMSound organ1 play"
	"(FMSound majorScaleOn: FMSound organ1) play"

	| snd p |
	snd _ FMSound new.
	p _ OrderedCollection new.
	p add: 0@0; add: 60@1.0; add: 110@0.8; add: 200@1.0; add: 250@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 4).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 9/1/1999 17:33'!
pluckedElecBass
	"FMSound pluckedElecBass play"
	"(FMSound lowMajorScaleOn: FMSound pluckedElecBass) play"

	| snd p env |
	snd _ FMSound new modulation: 1 ratio: 3.0.

	p _ OrderedCollection new.
	p add: 0@0.4; add: 20@1.0; add: 30@0.6; add: 100@0.6; add: 130@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 3 loopEnd: 4).

	p _ OrderedCollection new.
	p add: 0@1.0; add: 20@2.0; add: 30@4.5; add: 100@4.5; add: 130@0.0.
	env _ Envelope points: p loopStart: 3 loopEnd: 4.
	env updateSelector: #modulation:.
	snd addEnvelope: env.

	p _ OrderedCollection new.
	p add: 0@6.0; add: 20@4.0; add: 30@3.0; add: 100@3.0; add: 130@3.0.
	env _ Envelope points: p loopStart: 3 loopEnd: 4.
	env updateSelector: #ratio:.
	snd addEnvelope: env.

	^ snd setPitch: 220.0 dur: 1.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 8/14/1998 13:00'!
randomWeird1
	"FMSound randomWeird1 play"

	| snd p |
	snd _ FMSound new.
	snd addEnvelope: (VolumeEnvelope exponentialDecay: 0.96).
	p _ Array with: 0@0 with: 100@1.0 with: 250@0.7 with: 400@1.0 with: 500@0.
	snd addEnvelope: (PitchEnvelope points: p loopStart: 2 loopEnd: 4).
	^ snd setPitch: (150 + 2000 atRandom) dur: 2.0 loudness: 0.5
! !

!FMSound class methodsFor: 'instruments' stamp: 'jm 8/14/1998 12:57'!
randomWeird2
	"FMSound randomWeird2 play"

	| snd |
	snd _ FMSound new.
	snd addEnvelope: (VolumeEnvelope exponentialDecay: 0.96).
	snd addEnvelope: (PitchEnvelope exponentialDecay: 0.98).
	^ snd setPitch: (150 + 2000 atRandom) dur: 2.0 loudness: 0.5
! !

!FMSound class methodsFor: 'class initialization' stamp: 'jm 7/6/1998 10:26'!
initialize
	"Build a sine wave table."
	"FMSound initialize"

	| tableSize radiansPerStep peak |
	tableSize _ 4000.
	SineTable _ SoundBuffer newMonoSampleCount: tableSize.
	radiansPerStep _ (2.0 * Float pi) / tableSize asFloat.
	peak _ ((1 bitShift: 15) - 1) asFloat.  "range is +/- (2^15 - 1)"
	1 to: tableSize do: [:i |
		SineTable at: i put: (peak * (radiansPerStep * (i - 1)) sin) rounded].
! !

!FMSound class methodsFor: 'class initialization' stamp: 'jm 7/5/1998 14:22'!
sineTable
	"Answer a SoundBuffer containing one complete cycle of a sine wave."

	^ SineTable
! !

!FMBassoonSound methodsFor: 'as yet unclassified' stamp: 'jm 5/30/1999 21:17'!
setPitch: pitchNameOrNumber dur: d loudness: l
	"Select a modulation ratio and modulation envelope scale based on my pitch."

	| p modScale |
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	modScale _ 9.4.
	p > 100.0 ifTrue: [modScale _ 8.3].
	p > 150.0 ifTrue: [modScale _ 6.4].
	p > 200.0 ifTrue: [modScale _ 5.2].
	p > 300.0 ifTrue: [modScale _ 3.9].
	p > 400.0 ifTrue: [modScale _ 2.8].
	p > 600.0 ifTrue: [modScale _ 1.7].

	envelopes size > 0 ifTrue: [
		envelopes do: [:e |
			(e updateSelector = #modulation:)
				ifTrue: [e scale: modScale]]].

	super setPitch: p dur: d loudness: l.
! !

!FMClarinetSound methodsFor: 'initialization' stamp: 'jm 5/30/1999 10:10'!
setPitch: pitchNameOrNumber dur: d loudness: l
	"Select a modulation ratio and modulation envelope scale based on my pitch."

	| p modScale |
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	p < 262.0
		ifTrue: [modScale _ 25.0. self ratio: 4]
		ifFalse: [modScale _ 20.0. self ratio: 2].
	p > 524.0 ifTrue: [modScale _ 8.0].

	envelopes size > 0 ifTrue: [
		envelopes do: [:e |
			(e updateSelector = #modulation:)
				ifTrue: [e scale: modScale]]].

	super setPitch: p dur: d loudness: l.
! !

!UnloadedSound class methodsFor: 'as yet unclassified' stamp: 'jm 1/14/1999 12:00'!
default
	"UnloadedSound default play"

	| snd p |
	snd _ super new modulation: 1 ratio: 1.
	p _ OrderedCollection new.
	p add: 0@0.0; add: 10@1.0; add: 100@1.0; add: 120@0.0.
	snd addEnvelope: (VolumeEnvelope points: p loopStart: 2 loopEnd: 3).
	^ snd setPitch: 440.0 dur: 1.0 loudness: 0.5
! !

!LoopedSampledSound methodsFor: 'disk i/o' stamp: 'tk 4/8/1999 12:45'!
comeFullyUpOnReload: smartRefStream
	"Convert my sample buffers from ByteArrays into SampleBuffers after raw loading from a DataStream. Answer myself."

	leftSamples == rightSamples
		ifTrue: [
			leftSamples _ SoundBuffer fromByteArray: self leftSamples.
			rightSamples _ leftSamples]
		ifFalse: [
			leftSamples _ SoundBuffer fromByteArray: self leftSamples.
			rightSamples _ SoundBuffer fromByteArray: self rightSamples].

! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 10/14/1998 16:04'!
addReleaseEnvelope
	"Add a simple release envelope to this sound."

	| p env |
	p _ OrderedCollection new.
	p add: 0@1.0; add: 10@1.0; add: 100@1.0; add: 120@0.0.
	env _ (VolumeEnvelope points: p loopStart: 2 loopEnd: 3) target: self.
	envelopes size > 0 ifTrue: [  "remove any existing volume envelopes"
		envelopes copy do: [:e |
			(e isKindOf: VolumeEnvelope) ifTrue: [self removeEnvelope: e]]].
	self addEnvelope: env.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 8/18/1998 11:48'!
computeSampleCountForRelease
	"Calculate the number of samples before the end of the note after which looping back will be be disabled. The units of this value, sampleCountForRelease, are samples at the original sampling rate. When playing a specific note, this value is converted to releaseCount, which is number of samples to be computed at the current pitch and sampling rate."
	"Details: For short loops, set the sampleCountForRelease to the loop length plus the number of samples between loopEnd and lastSample. Otherwise, set it to 1/10th of a second worth of samples plus the number of samples between loopEnd and lastSample. In this case, the trailing samples will be played only if the last loop-back occurs within 1/10th of a second of the total note duration, and the note may be shortened by up to 1/10th second. For long loops, this is the best we can do."

	(scaledLoopLength > 0 and: [lastSample > loopEnd])
		ifTrue: [
			sampleCountForRelease _ (lastSample - loopEnd) +
				(self loopLength min: (originalSamplingRate / 10.0)) asInteger]
		ifFalse: [sampleCountForRelease _ 0].

	releaseCount _ sampleCountForRelease.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 5/5/1999 20:59'!
fromAIFFFileNamed: fileName mergeIfStereo: mergeFlag
	"Initialize this sound from the data in the given AIFF file. If mergeFlag is true and the file is stereo, its left and right channels are mixed together to produce a mono sampled sound."

	| aiffFileReader |
	aiffFileReader _ AIFFFileReader new.
	aiffFileReader readFromFile: fileName
		mergeIfStereo: mergeFlag
		skipDataChunk: false.
	aiffFileReader isLooped
		ifTrue: [
			self samples: aiffFileReader leftSamples
				loopEnd: aiffFileReader loopEnd
				loopLength: aiffFileReader loopLength
				pitch: aiffFileReader pitch
				samplingRate: aiffFileReader samplingRate]
		ifFalse: [
			self unloopedSamples: aiffFileReader leftSamples
				pitch: aiffFileReader pitch
				samplingRate: aiffFileReader samplingRate].

	"the following must be done second, since the initialization above sets
	 leftSamples and rightSamples to the same sample data"
	aiffFileReader isStereo
		ifTrue: [rightSamples _ aiffFileReader rightSamples].

	initialCount _ (leftSamples size * self samplingRate) // originalSamplingRate.
	self loudness: 1.0.

	self addReleaseEnvelope.
! !

!LoopedSampledSound methodsFor: 'initialization'!
fromAIFFFileReader: aiffFileReader mergeIfStereo: mergeFlag
	"Initialize this sound from the data in the given AIFF file. If mergeFlag is true and the file is stereo, its left and right channels are mixed together to produce a mono sampled sound."

	aiffFileReader isLooped
		ifTrue: [
			self samples: aiffFileReader leftSamples
				loopEnd: aiffFileReader loopEnd
				loopLength: aiffFileReader loopLength
				pitch: aiffFileReader pitch
				samplingRate: aiffFileReader samplingRate]
		ifFalse: [
			self unloopedSamples: aiffFileReader leftSamples
				pitch: aiffFileReader pitch
				samplingRate: aiffFileReader samplingRate].

	"the following must be done second, since the initialization above sets
	 leftSamples and rightSamples to the same sample data"
	aiffFileReader isStereo
		ifTrue: [rightSamples _ aiffFileReader rightSamples].

	initialCount _ (leftSamples size * self samplingRate) // originalSamplingRate.
	self loudness: 1.0.

	self addReleaseEnvelope.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 8/18/1998 07:43'!
initialize
	"This default initialization creates a loop consisting of a single cycle of a sine wave."
	"(LoopedSampledSound pitch: 440.0 dur: 1.0 loudness: 0.4) play"

	| samples |
	super initialize.
	samples _ FMSound sineTable.
	self samples: samples
		loopEnd: samples size
		loopLength: samples size
		pitch: 1.0
		samplingRate: samples size.
	self addReleaseEnvelope.
	self setPitch: 440.0 dur: 1.0 loudness: 0.5.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 7/9/1999 22:28'!
samples: aSoundBuffer loopEnd: loopEndIndex loopLength: loopSampleCount pitch: perceivedPitchInHz samplingRate: samplingRateInHz
	"Make this sound use the given samples array with a loop of the given length starting at the given index. The loop length may have a fractional part; this is necessary to achieve pitch accuracy for short loops."

	| loopStartIndex |
	super initialize.
	loopStartIndex _ (loopEndIndex - loopSampleCount) truncated + 1.
	((1 <= loopStartIndex) and:
	 [loopStartIndex < loopEndIndex and:
	 [loopEndIndex <= aSoundBuffer size]])
		ifFalse: [self error: 'bad loop parameters'].

	leftSamples _ rightSamples _ aSoundBuffer.
	originalSamplingRate _ samplingRateInHz asFloat.
	perceivedPitch _ perceivedPitchInHz asFloat.
	gain _ 1.0.
	firstSample _ 1.
	lastSample _ leftSamples size.
	lastSample >= (SmallInteger maxVal // LoopIndexScaleFactor) ifTrue: [
		self error: 'cannot handle more than ',
			(SmallInteger maxVal // LoopIndexScaleFactor) printString, ' samples'].
	loopEnd _ loopEndIndex.
	scaledLoopLength _ (loopSampleCount * LoopIndexScaleFactor) asInteger.
	scaledIndexIncr _ (samplingRateInHz * LoopIndexScaleFactor) // self samplingRate.
	self computeSampleCountForRelease.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 7/6/1998 17:09'!
setPitch: pitchNameOrNumber dur: d loudness: vol
	"(LoopedSampledSound pitch: 440.0 dur: 2.5 loudness: 0.4) play"

	super setPitch: pitchNameOrNumber dur: d loudness: vol.
	self pitch: (self nameOrNumberToPitch: pitchNameOrNumber).
	self reset.
! !

!LoopedSampledSound methodsFor: 'initialization' stamp: 'jm 7/9/1999 22:28'!
unloopedSamples: aSoundBuffer pitch: perceivedPitchInHz samplingRate: samplingRateInHz
	"Make this sound play the given samples unlooped. The samples have the given perceived pitch when played at the given sampling rate. By convention, unpitched sounds such as percussion sounds should specify a pitch of nil or 100 Hz."

	super initialize.
	leftSamples _ rightSamples _ aSoundBuffer.
	originalSamplingRate _ samplingRateInHz asFloat.
	perceivedPitchInHz
		ifNil: [perceivedPitch _ 100.0]
		ifNotNil: [perceivedPitch _ perceivedPitchInHz asFloat].
	gain _ 1.0.
	firstSample _ 1.
	lastSample _ leftSamples size.
	lastSample >= (SmallInteger maxVal // LoopIndexScaleFactor) ifTrue: [
		self error: 'cannot handle more than ',
			(SmallInteger maxVal // LoopIndexScaleFactor) printString, ' samples'].
	loopEnd _ leftSamples size.
	scaledLoopLength _ 0.  "zero length means unlooped"
	scaledIndexIncr _ (samplingRateInHz * LoopIndexScaleFactor) // self samplingRate.
	self computeSampleCountForRelease.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 5/31/1999 14:09'!
beUnlooped

	scaledLoopLength _ 0.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/18/1998 07:26'!
duration
	"Answer the duration of this sound in seconds."

	^ initialCount asFloat / self samplingRate asFloat
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 9/11/1998 15:36'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/18/1998 07:25'!
firstSample

	^ firstSample
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/18/1998 07:25'!
firstSample: aNumber

	firstSample _ (aNumber asInteger max: 1) min: lastSample.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/3/1998 18:52'!
gain

	^ gain
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/3/1998 18:52'!
gain: aNumber

	gain _ aNumber asFloat.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/18/1998 07:26'!
isLooped

	^ scaledLoopLength ~= 0.  "zero loop length means unlooped"
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/2/1998 10:14'!
isStereo

	^ leftSamples ~~ rightSamples
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 7/13/1998 11:46'!
leftSamples

	^ leftSamples
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 7/13/1998 11:46'!
leftSamples: aSampleBuffer

	leftSamples _ aSampleBuffer.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/17/1998 09:35'!
loopEnd

	^ loopEnd
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/2/1998 10:12'!
loopLength

	^ scaledLoopLength / FloatLoopIndexScaleFactor
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 10/14/1998 16:26'!
originalSamplingRate

	^ originalSamplingRate
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:54'!
perceivedPitch

	^ perceivedPitch
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/17/1998 09:08'!
pitch

	^ (scaledIndexIncr asFloat * perceivedPitch * self samplingRate asFloat) /
	  (originalSamplingRate * FloatLoopIndexScaleFactor)
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 8/18/1998 11:38'!
pitch: p

	scaledIndexIncr _
		((p asFloat * originalSamplingRate * FloatLoopIndexScaleFactor) /
		 (perceivedPitch * self samplingRate asFloat)) asInteger.

	sampleCountForRelease > 0
		ifTrue: [releaseCount _ (sampleCountForRelease * LoopIndexScaleFactor) // scaledIndexIncr]
		ifFalse: [releaseCount _ 0].
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 7/13/1998 11:46'!
rightSamples

	^ rightSamples
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 7/13/1998 11:46'!
rightSamples: aSampleBuffer

	rightSamples _ aSampleBuffer.
! !

!LoopedSampledSound methodsFor: 'accessing' stamp: 'jm 3/15/1999 08:01'!
samples
	"For compatability with SampledSound. Just return my left channel (which is the only channel if I am mono)."

	^ leftSamples
! !

!LoopedSampledSound methodsFor: 'other' stamp: 'jm 5/29/1999 18:56'!
findStartPointAfter: index
	"Answer the index of the last zero crossing sample before the given index."

	| i |
	i _ index min: lastSample.

	"scan backwards to the last zero-crossing"
	(leftSamples at: i) > 0
		ifTrue: [
			[i > 1 and: [(leftSamples at: i) > 0]] whileTrue: [i _ i - 1]]
		ifFalse: [
			[i > 1 and: [(leftSamples at: i) < 0]] whileTrue: [i _ i - 1]].
	^ i
! !

!LoopedSampledSound methodsFor: 'other' stamp: 'jm 8/18/1998 09:29'!
findStartPointForThreshold: threshold
	"Answer the index of the last zero crossing sample before the first sample whose absolute value (in either the right or left channel) exceeds the given threshold."

	| i |
	i _ self indexOfFirstPointOverThreshold: threshold.
	i >= lastSample ifTrue: [^ self error: 'no sample exceeds the given threshold'].

	"scan backwards to the last zero-crossing"
	(leftSamples at: i) > 0
		ifTrue: [
			[i > 1 and: [(leftSamples at: i) > 0]] whileTrue: [i _ i - 1]]
		ifFalse: [
			[i > 1 and: [(leftSamples at: i) < 0]] whileTrue: [i _ i - 1]].
	^ i
! !

!LoopedSampledSound methodsFor: 'other' stamp: 'jm 8/17/1998 09:22'!
indexOfFirstPointOverThreshold: threshold
	"Answer the index of the first sample whose absolute value exceeds the given threshold."

	| s |
	leftSamples == rightSamples
		ifTrue: [
			1 to: lastSample do: [:i |
				s _ leftSamples at: i.
				s < 0 ifTrue: [s _ 0 - s].
				s > threshold ifTrue: [^ i]]]
		ifFalse: [
			1 to: lastSample do: [:i |
				s _ leftSamples at: i.
				s < 0 ifTrue: [s _ 0 - s].
				s > threshold ifTrue: [^ i].
				s _ rightSamples at: i.
				s < 0 ifTrue: [s _ 0 - s].
				s > threshold ifTrue: [^ i]]].
	^ lastSample + 1
! !

!LoopedSampledSound methodsFor: 'sound generation' stamp: 'ar 2/3/2001 15:23'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play samples from a wave table by stepping a fixed amount through the table on every sample. The table index and increment are scaled to allow fractional increments for greater pitch accuracy.  If a loop length is specified, then the index is looped back when the loopEnd index is reached until count drops below releaseCount. This allows a short sampled sound to be sustained indefinitely."
	"(LoopedSampledSound pitch: 440.0 dur: 5.0 loudness: 0.5) play"

	| lastIndex sampleIndex i s compositeLeftVol compositeRightVol nextSampleIndex m isInStereo rightVal leftVal |
	<primitive:'primitiveMixLoopedSampledSound' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #leftSamples declareC: 'short int *leftSamples'.
	self var: #rightSamples declareC: 'short int *rightSamples'.

	isInStereo _ leftSamples ~~ rightSamples.
	compositeLeftVol _ (leftVol * scaledVol) // ScaleFactor.
	compositeRightVol _  (rightVol * scaledVol) // ScaleFactor.

	i _ (2 * startIndex) - 1.
	lastIndex _ (startIndex + n) - 1.
	startIndex to: lastIndex do: [:sliceIndex |
		sampleIndex _ (scaledIndex _ scaledIndex + scaledIndexIncr) // LoopIndexScaleFactor.
		((sampleIndex > loopEnd) and: [count > releaseCount]) ifTrue: [
			"loop back if not within releaseCount of the note end"
			"note: unlooped sounds will have loopEnd = lastSample"
			sampleIndex _ (scaledIndex _ scaledIndex - scaledLoopLength) // LoopIndexScaleFactor].
		(nextSampleIndex _ sampleIndex + 1) > lastSample ifTrue: [
			sampleIndex > lastSample ifTrue: [count _ 0. ^ nil].  "done!!"
			scaledLoopLength = 0
				ifTrue: [nextSampleIndex _ sampleIndex]
				ifFalse: [nextSampleIndex _ ((scaledIndex - scaledLoopLength) // LoopIndexScaleFactor) + 1]].

		m _ scaledIndex bitAnd: LoopIndexFractionMask.
		rightVal _ leftVal _
			(((leftSamples at: sampleIndex) * (LoopIndexScaleFactor - m)) +
			 ((leftSamples at: nextSampleIndex) * m)) // LoopIndexScaleFactor.
		isInStereo ifTrue: [
			rightVal _
				(((rightSamples at: sampleIndex) * (LoopIndexScaleFactor - m)) +
				 ((rightSamples at: nextSampleIndex) * m)) // LoopIndexScaleFactor].

		leftVol > 0 ifTrue: [
			s _ (aSoundBuffer at: i) + ((compositeLeftVol * leftVal) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		i _ i + 1.
		rightVol > 0 ifTrue: [
			s _ (aSoundBuffer at: i) + ((compositeRightVol * rightVal) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		i _ i + 1.

		scaledVolIncr ~= 0 ifTrue: [  "update volume envelope if it is changing"
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0].
			compositeLeftVol _ (leftVol * scaledVol) // ScaleFactor.
			compositeRightVol _  (rightVol * scaledVol) // ScaleFactor]].

	count _ count - n.
! !

!LoopedSampledSound methodsFor: 'sound generation' stamp: 'jm 8/17/1998 09:38'!
reset

	super reset.
	count _ initialCount.
	scaledIndex _ firstSample * LoopIndexScaleFactor.
! !

!LoopedSampledSound methodsFor: 'sound generation' stamp: 'jm 8/18/1998 09:31'!
samplesRemaining
	"Answer the number of samples remaining until the end of this sound."

	^ count
! !

!LoopedSampledSound methodsFor: 'sound generation' stamp: 'jm 9/9/1998 21:57'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds."

	count _ (mSecs * self samplingRate) // 1000.
! !

!LoopedSampledSound methodsFor: 'file i/o' stamp: 'jmv 2/26/2016 16:05'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files)."

	(self isStereo or: [self samplingRate ~= originalSamplingRate]) ifTrue: [
		^ super storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream].

	"optimization: if I'm not stereo and sampling rates match, just store my buffer"
	(aBinaryStream isKindOf: StandardFileStream)
		ifTrue: [  "optimization for files: write sound buffer directly to file"
			aBinaryStream next: (leftSamples size // 2) putAll: leftSamples startingAt: 1]  "size in words"
		ifFalse: [  "for non-file streams:"
			1 to: leftSamples monoSampleCount do: [:i | aBinaryStream nextSignedInt16Put: (leftSamples at: i) bigEndian: bigEndianFlag ]]! !

!LoopedSampledSound class methodsFor: 'instance creation'!
fromAIFFFileNamed: fileName mergeIfStereo: mergeFlag
	"Initialize this sound from the data in the given AIFF file. If mergeFlag is true and the file is stereo, its left and right channels are mixed together to produce a mono sampled sound."

	| aiffFileReader |
	aiffFileReader _ AIFFFileReader new.
	aiffFileReader readFromFile: fileName
		mergeIfStereo: mergeFlag
		skipDataChunk: false.
	self new fromAIFFFileReader: aiffFileReader mergeIfStereo: mergeFlag! !

!LoopedSampledSound class methodsFor: 'instance creation' stamp: 'jm 8/18/1998 07:40'!
samples: aSoundBuffer loopEnd: loopEndIndex loopLength: loopSampleCount pitch: perceivedPitchInHz samplingRate: samplingRateInHz
	"See the comment in the instance method of this name."

	^ self basicNew
		samples: aSoundBuffer
		loopEnd: loopEndIndex
		loopLength: loopSampleCount
		pitch: perceivedPitchInHz
		samplingRate: samplingRateInHz
! !

!LoopedSampledSound class methodsFor: 'instance creation' stamp: 'jm 8/18/1998 07:41'!
unloopedSamples: aSoundBuffer pitch: perceivedPitchInHz samplingRate: samplingRateInHz
	"See the comment in the instance method of this name."

	^ self basicNew
		unloopedSamples: aSoundBuffer
		pitch: perceivedPitchInHz
		samplingRate: samplingRateInHz
! !

!LoopedSampledSound class methodsFor: 'class initialization' stamp: 'jm 8/13/1998 12:54'!
initialize
	"LoopedSampledSound initialize"

	LoopIndexScaleFactor _ 512.
	FloatLoopIndexScaleFactor _ LoopIndexScaleFactor asFloat.
	LoopIndexFractionMask _ LoopIndexScaleFactor - 1.
! !

!MixedSound methodsFor: 'copying' stamp: 'jm 12/15/97 22:33'!
copySounds
	"Private!! Support for copying. Copy my component sounds and settings array."

	sounds _ sounds collect: [:s | s copy].
	leftVols _ leftVols copy.
	rightVols _ rightVols copy.
! !

!MixedSound methodsFor: 'composition'!
+ aSound
	"Return the mix of the receiver and the argument sound."

	^ self add: aSound
! !

!MixedSound methodsFor: 'composition' stamp: 'jm 1/5/98 13:47'!
add: aSound
	"Add the given sound with a pan setting of centered and no attenuation."

	self add: aSound pan: 0.5 volume: 1.0.
! !

!MixedSound methodsFor: 'composition' stamp: 'jm 1/5/98 13:47'!
add: aSound pan: leftRightPan
	"Add the given sound with the given left-right panning and no attenuation."

	self add: aSound pan: leftRightPan volume: 1.0.
! !

!MixedSound methodsFor: 'composition' stamp: 'jm 1/5/98 17:33'!
add: aSound pan: leftRightPan volume: volume
	"Add the given sound with the given left-right pan, where 0.0 is full left, 1.0 is full right, and 0.5 is centered. The loudness of the sound will be scaled by volume, which ranges from 0 to 1.0."

	| pan vol |
	pan _ ((leftRightPan * ScaleFactor) asInteger max: 0) min: ScaleFactor.
	vol _ ((volume * ScaleFactor) asInteger max: 0) min: ScaleFactor.
	sounds _ sounds copyWith: aSound.
	leftVols _ leftVols copyWith: ((ScaleFactor - pan) * vol) // ScaleFactor.
	rightVols _ rightVols copyWith: (pan * vol) // ScaleFactor.
! !

!MixedSound methodsFor: 'sound generation' stamp: 'jm 11/25/97 13:40'!
doControl

	super doControl.
	1 to: sounds size do: [:i | (sounds at: i) doControl].
! !

!MixedSound methodsFor: 'sound generation' stamp: 'jm 1/5/98 13:42'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a number of sounds concurrently. The level of each sound can be set independently for the left and right channels."

	| snd left right |
	1 to: sounds size do: [:i |
		(soundDone at: i) ifFalse: [
			snd _ sounds at: i.
			left _ (leftVol * (leftVols at: i)) // ScaleFactor.
			right _ (rightVol * (rightVols at: i)) // ScaleFactor.
			snd samplesRemaining > 0
				ifTrue: [
					snd mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: left rightVol: right]
				ifFalse: [soundDone at: i put: true]]].
! !

!MixedSound methodsFor: 'sound generation' stamp: 'jm 12/8/97 17:07'!
reset

	super reset.
	sounds do: [:snd | snd reset].
	soundDone _ (Array new: sounds size) atAllPut: false.
! !

!MixedSound methodsFor: 'sound generation' stamp: 'jm 12/8/97 17:08'!
samplesRemaining

	| remaining r |
	remaining _ 0.
	1 to: sounds size do: [:i |
		r _ (sounds at: i) samplesRemaining.
		r > remaining ifTrue: [remaining _ r]].

	^ remaining
! !

!MixedSound methodsFor: 'sound generation' stamp: 'jm 1/10/1999 08:45'!
stopGracefully
	"End this note with a graceful decay. If the note has envelopes, determine the decay time from its envelopes."

	super stopGracefully.
	sounds do: [:s | s stopGracefully].
! !

!MixedSound methodsFor: 'accessing' stamp: 'jm 8/17/1998 14:05'!
duration
	"Answer the duration of this sound in seconds."

	| dur |
	dur _ 0.
	sounds do: [:snd | dur _ dur max: snd duration].
	^ dur
! !

!MixedSound methodsFor: 'accessing' stamp: 'jm 12/16/2001 20:23'!
isStereo

	^ true
! !

!MixedSound methodsFor: 'accessing' stamp: 'jm 2/4/98 13:37'!
sounds

	^ sounds
! !

!MixedSound methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:48'!
initialize

	super initialize.
	sounds _ #().
	leftVols _ #().
	rightVols _ #()! !

!PluckedSound methodsFor: 'copying' stamp: 'jm 11/4/97 08:25'!
copyRing
	"Private!! Support for copying"

	ring _ ring copy.
! !

!PluckedSound methodsFor: 'accessing' stamp: 'jm 8/17/1998 14:07'!
duration
	"Answer the duration of this sound in seconds."

	^ initialCount asFloat / self samplingRate
! !

!PluckedSound methodsFor: 'accessing' stamp: 'jm 9/11/1998 15:40'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!PluckedSound methodsFor: 'sound generation' stamp: 'ar 2/3/2001 15:23'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"The Karplus-Strong plucked string algorithm: start with a buffer full of random noise and repeatedly play the contents of that buffer while averaging adjacent samples. High harmonics damp out more quickly, transfering their energy to lower ones. The length of the buffer corresponds to the length of the string."
	"(PluckedSound pitch: 220.0 dur: 6.0 loudness: 0.8) play"

	| lastIndex scaledThisIndex scaledNextIndex average sample i s |
	<primitive:'primitiveMixPluckedSound' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #ring declareC: 'short int *ring'.

	lastIndex _ (startIndex + n) - 1.
	scaledThisIndex _ scaledNextIndex _ scaledIndex.
	startIndex to: lastIndex do: [:sliceIndex |
		scaledNextIndex _ scaledThisIndex + scaledIndexIncr.
		scaledNextIndex >= scaledIndexLimit
			ifTrue: [scaledNextIndex _ ScaleFactor + (scaledNextIndex - scaledIndexLimit)].
		average _
			((ring at: scaledThisIndex // ScaleFactor) +
			 (ring at: scaledNextIndex // ScaleFactor)) // 2.
		ring at: scaledThisIndex // ScaleFactor put: average.
		sample _ (average * scaledVol) // ScaleFactor.  "scale by volume"
		scaledThisIndex _ scaledNextIndex.

		leftVol > 0 ifTrue: [
			i _ (2 * sliceIndex) - 1.
			s _ (aSoundBuffer at: i) + ((sample * leftVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		rightVol > 0 ifTrue: [
			i _ 2 * sliceIndex.
			s _ (aSoundBuffer at: i) + ((sample * rightVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].

		scaledVolIncr ~= 0 ifTrue: [
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0]]].

	scaledIndex _ scaledNextIndex.
	count _ count - n.
! !

!PluckedSound methodsFor: 'sound generation' stamp: 'jm 12/17/97 21:35'!
reset
	"Fill the ring with random noise."

	| seed n |
	super reset.
	seed _ 17.
	n _ ring monoSampleCount.
	1 to: n do: [:i |
		seed _ ((seed * 1309) + 13849) bitAnd: 65535.
		ring at: i put: seed - 32768].
	count _ initialCount.
	scaledIndex _ ScaleFactor.
! !

!PluckedSound methodsFor: 'sound generation' stamp: 'jm 11/26/97 10:51'!
samplesRemaining

	^ count
! !

!PluckedSound methodsFor: 'sound generation' stamp: 'jm 9/9/1998 21:58'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds."

	count _ (mSecs * self samplingRate) // 1000.
! !

!PluckedSound methodsFor: 'initialization' stamp: 'jm 7/6/1998 17:09'!
setPitch: pitchNameOrNumber dur: d loudness: vol

	| p sz |
	super setPitch: pitchNameOrNumber dur: d loudness: vol.
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	initialCount _ (d * self samplingRate asFloat) asInteger.
	ring _ SoundBuffer newMonoSampleCount:
		(((2.0 * self samplingRate) / p) asInteger max: 2).
	sz _ ring monoSampleCount.
	scaledIndexLimit _ (sz + 1) * ScaleFactor.
	scaledIndexIncr _ (p * sz * ScaleFactor) // (2.0 * self samplingRate).
	self reset.
! !

!PluckedSound class methodsFor: 'instruments' stamp: 'jm 1/31/98 16:32'!
default
	"PluckedSound default play"
	"(AbstractSound majorScaleOn: PluckedSound default) play"

	| snd p env |
	snd _ PluckedSound new.
	p _ OrderedCollection new.
	p add: 0@1.0; add: 10@1.0; add: 20@0.0.
	env _ VolumeEnvelope points: p loopStart: 2 loopEnd: 2.
	env target: snd; scale: 0.3.
	^ snd
		addEnvelope: env;
		setPitch: 220 dur: 3.0 loudness: 0.3
! !

!QueueSound methodsFor: 'accessing' stamp: 'len 8/29/1999 21:52'!
add: aSound
	self sounds nextPut: aSound.
	^ aSound! !

!QueueSound methodsFor: 'accessing' stamp: 'jmv 3/2/2010 16:24'!
currentSound
	currentSound ifNil: [currentSound _ self nextSound].
	^ currentSound! !

!QueueSound methodsFor: 'accessing' stamp: 'len 8/29/1999 22:07'!
currentSound: aSound
	currentSound _ aSound! !

!QueueSound methodsFor: 'accessing' stamp: 'len 8/29/1999 22:07'!
done: aBoolean
	done _ aBoolean! !

!QueueSound methodsFor: 'accessing' stamp: 'len 8/29/1999 21:46'!
sounds
	^ sounds! !

!QueueSound methodsFor: 'accessing' stamp: 'len 9/26/1999 17:19'!
startTime
	^ startTime! !

!QueueSound methodsFor: 'accessing' stamp: 'len 9/26/1999 17:19'!
startTime: anInteger
	startTime _ anInteger! !

!QueueSound methodsFor: 'sound generation' stamp: 'jmv 3/2/2010 17:13'!
doControl
	super doControl.
	self currentSound ifNotNil: [self currentSound doControl]! !

!QueueSound methodsFor: 'sound generation' stamp: 'jmv 4/17/2013 12:08'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a collection of sounds in sequence."

	| finalIndex i remaining count rate |
	self currentSound ifNil: [^ self].  "already done"
	self startTime > Time localMillisecondClock ifTrue: [^ self].
	rate _ self samplingRate.
	finalIndex _ (startIndex + n) - 1.
	i _ startIndex.
	[i <= finalIndex] whileTrue: [
		[
			self currentSound ifNil: [^ self].
			(remaining _ self currentSound samplesRemaining) <= 0]
				whileTrue: [self currentSound: self nextSound].
		count _ (finalIndex - i) + 1.
		remaining < count ifTrue: [count _ remaining].
		self currentSound mixSampleCount: count into: aSoundBuffer startingAt: i leftVol: leftVol rightVol: rightVol.
		i _ i + count]! !

!QueueSound methodsFor: 'sound generation' stamp: 'len 11/30/1999 04:13'!
nextSound
	| answer |
	sounds isEmpty ifTrue: [^ nil].
	answer _ sounds next.
	answer reset.
	^ answer! !

!QueueSound methodsFor: 'sound generation' stamp: 'jmv 3/2/2010 17:13'!
reset
	super reset.
	self currentSound
		ifNotNil: [ self currentSound reset]
		ifNil: [ self currentSound: self nextSound]! !

!QueueSound methodsFor: 'sound generation' stamp: 'len 8/29/1999 22:13'!
samplesRemaining
	(done and: [self sounds isEmpty])
		ifTrue: [^ 0]
		ifFalse: [^ 1000000].
! !

!QueueSound methodsFor: 'initialization' stamp: 'jmv 4/17/2013 12:08'!
initialize
	super initialize.
	sounds _ SharedQueue new.
	done _ false.
	startTime _ Time localMillisecondClock! !

!RepeatingSound methodsFor: 'copying' stamp: 'jm 12/15/97 22:34'!
copySound
	"Private!! Support for copying. Copy my component sound."

	sound _ sound copy.
! !

!RepeatingSound methodsFor: 'sound generation' stamp: 'jm 11/25/97 13:40'!
doControl

	super doControl.
	sound doControl.
! !

!RepeatingSound methodsFor: 'sound generation' stamp: 'jm 11/24/97 16:05'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a collection of sounds in sequence."
	"(RepeatingSound new
		setSound: FMSound majorScale
		iterations: 2) play"

	| i count samplesNeeded |
	iteration <= 0 ifTrue: [^ self].
	i _ startIndex.
	samplesNeeded _ n.
	[samplesNeeded > 0] whileTrue: [
		count _ sound samplesRemaining min: samplesNeeded.
		count = 0 ifTrue: [
			iterationCount == #forever
				ifFalse: [
					iteration _ iteration - 1.
					iteration <= 0 ifTrue: [^ self]].  "done"
			sound reset.
			count _ sound samplesRemaining min: samplesNeeded.
			count = 0 ifTrue: [^ self]].  "zero length sound"
		sound mixSampleCount: count
			into: aSoundBuffer
			startingAt: i
			leftVol: leftVol
			rightVol: rightVol.
		i _ i + count.
		samplesNeeded _ samplesNeeded - count].
! !

!RepeatingSound methodsFor: 'sound generation' stamp: 'jm 6/30/1998 18:28'!
reset

	super reset.
	sound reset.
	samplesPerIteration _ sound samplesRemaining.
	iterationCount == #forever
		ifTrue: [iteration _ 1]
		ifFalse: [iteration _ iterationCount].
! !

!RepeatingSound methodsFor: 'sound generation' stamp: 'jm 1/18/1999 10:31'!
samplesRemaining

	iterationCount == #forever ifTrue: [^ 1000000].
	iteration > 0
		ifTrue: [^ sound samplesRemaining + ((iteration - 1) * samplesPerIteration)]
		ifFalse: [^ 0].
! !

!RepeatingSound methodsFor: 'accessing' stamp: 'jm 11/17/97 18:48'!
iterationCount

	^ iterationCount
! !

!RepeatingSound methodsFor: 'accessing' stamp: 'jm 11/17/97 18:48'!
iterationCount: aNumber

	iterationCount _ aNumber.
! !

!RepeatingSound methodsFor: 'accessing' stamp: 'jm 11/17/97 18:48'!
sound

	^ sound
! !

!RepeatingSound methodsFor: 'accessing' stamp: 'jm 12/15/97 22:39'!
sound: aSound

	sound _ aSound.
! !

!RepeatingSound methodsFor: 'initialization' stamp: 'jm 9/12/97 15:54'!
setPitch: p dur: d loudness: l

	self error: 'RepeatingSounds do not support playing notes'.
! !

!RepeatingSound methodsFor: 'initialization' stamp: 'jm 9/12/97 16:39'!
setSound: aSound iterations: anIntegerOrSymbol
	"Initialize the receiver to play the given sound the given number of times. If iteration count is the symbol #forever, then repeat indefinitely."
	"(RepeatingSound repeat: AbstractSound scaleTest count: 2) play"
	"(RepeatingSound repeatForever: PluckedSound lowMajorScale) play"

	super initialize.
	sound _ aSound.
	iterationCount _ anIntegerOrSymbol.
	self reset.
! !

!RepeatingSound class methodsFor: 'instance creation' stamp: 'jm 9/12/97 16:14'!
repeat: aSound count: anInteger
	"Return a RepeatingSound that will repeat the given sound for the given number of iterations."

	^ self new setSound: aSound iterations: anInteger
! !

!RepeatingSound class methodsFor: 'instance creation' stamp: 'jm 9/12/97 16:13'!
repeatForever: aSound
	"Return a RepeatingSound that will repeat the given sound forever."

	^ self new setSound: aSound iterations: #forever
! !

!RestSound methodsFor: 'accessing' stamp: 'jm 8/17/1998 14:07'!
duration
	"Answer the duration of this sound in seconds."

	^ initialCount asFloat / self samplingRate
! !

!RestSound methodsFor: 'accessing' stamp: 'jm 9/11/1998 15:42'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!RestSound methodsFor: 'accessing' stamp: 'di 2/17/1999 21:09'!
samples
	^ SoundBuffer newMonoSampleCount: initialCount! !

!RestSound methodsFor: 'sound generation' stamp: 'jm 11/24/97 16:04'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play silence for a given duration."
	"(RestSound dur: 1.0) play"

	count _ count - n.
! !

!RestSound methodsFor: 'sound generation'!
reset

	super reset.
	count _ initialCount.
! !

!RestSound methodsFor: 'sound generation' stamp: 'jm 12/15/97 22:37'!
samplesRemaining

	^ count
! !

!RestSound methodsFor: 'initialization' stamp: 'jm 1/28/98 13:08'!
setDur: d
	"Set rest duration in seconds."

	initialCount _ (d * self samplingRate asFloat) rounded.
	count _ initialCount.
	self reset.
! !

!RestSound class methodsFor: 'instance creation' stamp: 'jm 3/31/1999 21:05'!
dur: d
	"Return a rest of the given duration."

	^ self new setDur: d
! !

!RestSound class methodsFor: 'instance creation' stamp: 'jm 12/15/97 22:38'!
pitch: p dur: d loudness: l
	"Return a rest of the given duration."
	"Note: This message allows one to silence one or more voices of a multi-voice piece by using RestSound as their instrument."

	^ self new setDur: d
! !

!ReverbSound methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:53'!
copySound
	"Private!! Support for copying. Copy my component sound."

	sound _ sound copy.
	leftBuffer _ leftBuffer copy.
	rightBuffer _ rightBuffer copy.
! !

!ReverbSound methodsFor: 'private' stamp: 'ar 2/3/2001 15:55'!
applyReverbTo: aSoundBuffer startingAt: startIndex count: n

	| delayedLeft delayedRight i tapGain j out |
	<primitive: 'primitiveApplyReverb' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #tapDelays declareC: 'int *tapDelays'.
	self var: #tapGains declareC: 'int *tapGains'.
	self var: #leftBuffer declareC: 'short int *leftBuffer'.
	self var: #rightBuffer declareC: 'short int *rightBuffer'.

	startIndex to: ((startIndex + n) - 1) do: [:sliceIndex |
		delayedLeft _ delayedRight _ 0.
		1 to: tapCount do: [:tapIndex |
			i _ bufferIndex - (tapDelays at: tapIndex).
			i < 1 ifTrue: [i _ i + bufferSize].  "wrap"
			tapGain _ tapGains at: tapIndex.
			delayedLeft _ delayedLeft + (tapGain * (leftBuffer at: i)).
			delayedRight _ delayedRight + (tapGain * (rightBuffer at: i))].

		"left channel"
		j _ (2 * sliceIndex) - 1.
		out _ (aSoundBuffer at: j) + (delayedLeft // ScaleFactor).
		out >  32767 ifTrue: [out _  32767].  "clipping!!"
		out < -32767 ifTrue: [out _ -32767].  "clipping!!"
		aSoundBuffer at: j put: out.
		leftBuffer at: bufferIndex put: out.

		"right channel"
		j _ j + 1.
		out _ (aSoundBuffer at: j) + (delayedRight // ScaleFactor).
		out >  32767 ifTrue: [out _  32767].  "clipping!!"
		out < -32767 ifTrue: [out _ -32767].  "clipping!!"
		aSoundBuffer at: j put: out.
		rightBuffer at: bufferIndex put: out.

		bufferIndex _ (bufferIndex \\ bufferSize) + 1].
! !

!ReverbSound methodsFor: 'sound generation' stamp: 'jm 1/21/98 14:32'!
doControl

	super doControl.
	sound doControl.
! !

!ReverbSound methodsFor: 'sound generation' stamp: 'jm 1/21/98 15:00'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play my sound with reberberation."

	sound mixSampleCount: n
		into: aSoundBuffer
		startingAt: startIndex
		leftVol: leftVol
		rightVol: rightVol.
	self applyReverbTo: aSoundBuffer startingAt: startIndex count: n.
! !

!ReverbSound methodsFor: 'sound generation' stamp: 'jm 1/21/98 16:47'!
reset

	super reset.
	sound reset.
	1 to: bufferSize do: [:i |
		leftBuffer at: i put: 0.
		rightBuffer at: i put: 0].
! !

!ReverbSound methodsFor: 'sound generation' stamp: 'jm 1/21/98 17:02'!
samplesRemaining

	^ sound samplesRemaining
! !

!ReverbSound methodsFor: 'accessing' stamp: 'jm 1/7/98 22:41'!
sound

	^ sound
! !

!ReverbSound methodsFor: 'accessing' stamp: 'jm 1/7/98 22:42'!
sound: aSound

	sound _ aSound.
! !

!ReverbSound methodsFor: 'accessing' stamp: 'jm 1/27/98 19:10'!
tapDelays: delayList gains: gainList
	"ReverbSound new tapDelays: #(537 691 1191) gains: #(0.07 0.07 0.07)"

	| maxDelay gain d |
	delayList size = gainList size
		ifFalse: [self error: 'tap delay and gains lists must be the same size'].
	tapCount _ delayList size.
	tapDelays _ Bitmap new: tapCount.
	tapGains _ Bitmap new: tapCount.

	maxDelay _ 0.
	1 to: tapGains size do: [:i |
		tapDelays at: i put: (delayList at: i) asInteger.
		gain _ gainList at: i.
		gain >= 1.0 ifTrue: [self error: 'reverb tap gains must be under 1.0'].
		tapGains at: i put: (gain * ScaleFactor) asInteger.
		d _ tapDelays at: i.
		d > maxDelay ifTrue: [maxDelay _ d]].
	bufferSize _ maxDelay.
	leftBuffer _ SoundBuffer newMonoSampleCount: maxDelay.
	rightBuffer _ SoundBuffer newMonoSampleCount: maxDelay.
	bufferIndex _ 1.
! !

!SampledSound methodsFor: 'accessing' stamp: 'di 12/7/2000 16:04'!
compressWith: codecClass
	^ codecClass new compressSound: self! !

!SampledSound methodsFor: 'accessing' stamp: 'RAA 12/24/2000 08:49'!
compressWith: codecClass atRate: aSamplingRate

	^ codecClass new compressSound: self atRate: aSamplingRate! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 3/28/98 05:46'!
duration

	^ initialCount asFloat / self samplingRate asFloat
! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 9/11/1998 15:39'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 12/15/97 22:51'!
originalSamplingRate

	^ originalSamplingRate
! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 9/12/97 16:46'!
samples

	^ samples
! !

!SampledSound methodsFor: 'playing' stamp: 'di 10/28/2000 17:08'!
endGracefully
	"See stopGracefully, which affects initialCOunt, and I don't think it should (di)."

	| decayInMs env |
	envelopes isEmpty
		ifTrue: [
			self adjustVolumeTo: 0 overMSecs: 10.
			decayInMs _ 10]
		ifFalse: [
			env _ envelopes first.
			decayInMs _ env attackTime + env decayTime].
	count _ decayInMs * self samplingRate // 1000.
! !

!SampledSound methodsFor: 'playing' stamp: 'ar 2/3/2001 15:23'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Mix the given number of samples with the samples already in the given buffer starting at the given index. Assume that the buffer size is at least (index + count) - 1."

	| lastIndex outIndex sampleIndex sample i s overflow |
	<primitive:'primitiveMixSampledSound' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #samples declareC: 'short int *samples'.

	lastIndex _ (startIndex + n) - 1.
	outIndex _ startIndex.    "index of next stereo output sample pair"
	sampleIndex _ indexHighBits + (scaledIndex >> IncrementFractionBits).
	[(sampleIndex <= samplesSize) and: [outIndex <= lastIndex]] whileTrue: [
		sample _ ((samples at: sampleIndex) * scaledVol) // ScaleFactor.
		leftVol > 0 ifTrue: [
			i _ (2 * outIndex) - 1.
			s _ (aSoundBuffer at: i) + ((sample * leftVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		rightVol > 0 ifTrue: [
			i _ 2 * outIndex.
			s _ (aSoundBuffer at: i) + ((sample * rightVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].

		scaledVolIncr ~= 0 ifTrue: [
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0]].

		scaledIndex _ scaledIndex + scaledIncrement.
		scaledIndex >= ScaledIndexOverflow ifTrue: [
			overflow _ scaledIndex >> IncrementFractionBits.
			indexHighBits _ indexHighBits + overflow.
			scaledIndex _ scaledIndex - (overflow << IncrementFractionBits)].

		sampleIndex _ indexHighBits + (scaledIndex >> IncrementFractionBits).
		outIndex _ outIndex + 1].
	count _ count - n.
! !

!SampledSound methodsFor: 'playing' stamp: 'di 10/28/2000 22:31'!
playSilentlyUntil: startTime
	"Used to fast foward to a particular starting time.
	Overridden to be instant for sampled sounds."

"true ifTrue: [^ super playSilentlyUntil: startTime]."
	indexHighBits _ (startTime * originalSamplingRate) asInteger.
	scaledIndex _ IncrementScaleFactor.
	count _ initialCount - (startTime * self samplingRate).
	mSecsSinceStart _ (startTime * 1000) asInteger.

! !

!SampledSound methodsFor: 'playing' stamp: 'jm 7/9/1999 18:29'!
reset
	"Details: The sample index and increment are scaled to allow fractional increments without having to do floating point arithmetic in the inner loop."

	super reset.
	scaledIncrement _
		((originalSamplingRate asFloat / self samplingRate) * IncrementScaleFactor) rounded.
	count _ initialCount.
	scaledIndex _ IncrementScaleFactor.  "index of the first sample, scaled"
	indexHighBits _ 0.
! !

!SampledSound methodsFor: 'playing' stamp: 'jm 9/13/97 19:07'!
samplesRemaining

	^ count
! !

!SampledSound methodsFor: 'playing' stamp: 'RAA 8/12/2000 15:11'!
setScaledIncrement: aNumber

	scaledIncrement _ (aNumber * IncrementScaleFactor) rounded.

! !

!SampledSound methodsFor: 'playing' stamp: 'jm 9/9/1998 21:58'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds."

	count _ (mSecs * self samplingRate) // 1000.
! !

!SampledSound methodsFor: 'initialization' stamp: 'jm 1/18/1999 06:42'!
pitch: pitchNameOrNumber

	| p |
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	originalSamplingRate _
		((self samplingRate asFloat * p asFloat) / NominalSamplePitch asFloat) asInteger.
	self reset.
! !

!SampledSound methodsFor: 'initialization' stamp: 'jm 3/24/1999 12:01'!
setPitch: pitchNameOrNumber dur: d loudness: vol
	"Used to play scores using the default sample table."
	"(SampledSound pitch: 880.0 dur: 1.5 loudness: 0.6) play"

	| p |
	super setPitch: pitchNameOrNumber dur: d loudness: vol.
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	samples _ DefaultSampleTable.
	samplesSize _ samples size.
	initialCount _ (d * self samplingRate asFloat) rounded.
	originalSamplingRate _
		((self samplingRate asFloat * p asFloat) / NominalSamplePitch asFloat) asInteger.
	self loudness: vol.
	self reset.
! !

!SampledSound methodsFor: 'initialization' stamp: 'jm 7/9/1999 19:23'!
setSamples: anArray samplingRate: rate
	"Set my samples array to the given array with the given nominal sampling rate. Altering the rate parameter allows the sampled sound to be played back at different pitches."
	"Note: There are two ways to use sampled sound: (a) you can play them through once (supported by this method) or (b) you can make them the default waveform with which to play a musical score (supported by the class method defaultSampleTable:)."
	"Assume: anArray is either a SoundBuffer or a collection of signed 16-bit sample values."
	"(SampledSound
		samples: SampledSound coffeeCupClink
		samplingRate: 5000) play"

	"copy the array into a SoundBuffer if necessary"
	anArray class isWords
		ifTrue: [samples _ anArray]
		ifFalse: [samples _ SoundBuffer fromArray: anArray].

	samplesSize _ samples size.
	samplesSize >= SmallInteger maxVal ifTrue: [  "this is unlikely..."
		self error: 'sample count must be under ',  SmallInteger maxVal printString].
	originalSamplingRate _ rate.
	initialCount _ (samplesSize * self samplingRate) // originalSamplingRate.
	self loudness: 1.0.
	self reset.
! !

!SampledSound methodsFor: 'file i/o' stamp: 'jmv 2/26/2016 16:05'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files)."

	self samplingRate ~= originalSamplingRate ifTrue: [
		^ super storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream].

	"optimization: if sampling rates match, just store my buffer"
	(aBinaryStream isKindOf: StandardFileStream)
		ifTrue: [  "optimization for files: write sound buffer directly to file"
			aBinaryStream next: (samples size // 2) putAll: samples startingAt: 1]  "size in words"
		ifFalse: [  "for non-file streams:"
			1 to: samples monoSampleCount do: [:i | aBinaryStream nextSignedInt16Put: (samples at: i) bigEndian: bigEndianFlag ]]! !

!SampledSound methodsFor: 'sound tracks' stamp: 'jmv 12/4/2012 23:42'!
volumeForm: height from: start to: stop nSamplesPerPixel: nPerPixel
	"Note: nPerPixel can be Integer or Float for pixel-perfect alignment."
	"In an inspector of a samplesSound...
		self currentWorld addMorph: (ImageMorph new image:
			(self volumeForm: 32 from: 1 to: samples size nSamplesPerPixel: 225))
	"
	| volPlot width sample min max vol |
	width _ stop-start//nPerPixel.
	volPlot _ Form extent: width@height.
	(start max: 1) to: (stop min: samples size)-nPerPixel by: nPerPixel do:
		[:i | min _ max _ 0.
		i asInteger to: (i+nPerPixel-1) asInteger by: 4 do:  "by: 4 makes it faster yet looks the same"
			[:j | sample _ samples at: j.
			sample < min ifTrue: [min _ sample].
			sample > max ifTrue: [max _ sample]].
		vol _ (max - min) * height // 65536.
		volPlot fillBlack: ((i-start//nPerPixel) @ (height-vol//2) extent: 1@(vol+1))].
	^ volPlot
	
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 6/17/2015 11:26'!
addLibrarySoundNamed: aString fromAIFFfileNamed: fileName
	"Add a sound from the given AIFF file to the library."
	"SampledSound
		addLibrarySoundNamed: 'shutterClick'
		fromAIFFfileNamed: '7.aif'"
	"Add all .aif files in the current directory to the sound library:
	| fileNames |
	fileNames _ FileDirectory smalltalkImageDirectory fileNamesMatching: '*.aif'.
	fileNames do: [:fName |
		SampledSound
			addLibrarySoundNamed: (fName copyUpTo: $.)
			fromAIFFfileNamed: fName]"

	| snd |
	snd _ self fromAIFFfileNamed: fileName.
	self addLibrarySoundNamed: aString
		samples: snd samples
		samplingRate: snd originalSamplingRate.
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 10:41'!
addLibrarySoundNamed: aString samples: sampleData samplingRate: samplesPerSecond
	"Add the given sound to the sound library. The sample data may be either a ByteArray or a SoundBuffer. If the former, it is take to be 8-bit unsigned samples. If the latter, it is taken to be 16 bit signed samples."

	SoundLibrary
		at: aString
		put: (Array with: sampleData with: samplesPerSecond).
! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 4/14/2003 00:01'!
assimilateSoundsFrom: aDictionary
	"assimilate sounds with new keys from the given dictionary"

	aDictionary associationsDo:
		[:assoc | (SoundLibrary includesKey: assoc key) ifFalse:
			[SoundLibrary add: assoc]]! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 11:04'!
playSoundNamed: aString
	"Play the sound with given name. Do nothing if there is no sound of that name in the library."
	"SampledSound playSoundNamed: 'croak'"

	| snd |
	snd _ self soundNamed: aString.
	snd ifNotNil: [snd play].
	^ snd
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 10:40'!
putCoffeeCupClinkInSoundLibrary
	"SampledSound putCoffeeCupClinkInSoundLibrary"

	self addLibrarySoundNamed: 'clink'
		samples: self coffeeCupClink
		samplingRate: 11025! !

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 3/2/2010 10:06'!
removeSoundNamed: aString
	"Remove the sound with the given name from the sound library."

	SoundLibrary removeKey: aString ifAbsent: nil! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 9/27/2001 14:46'!
soundNamed: aString
	"Answer the sound of the given name, or, if there is no sound of that name, put up an informer so stating, and answer nil"

	"(SampledSound soundNamed: 'shutterClick') play"

	^ self soundNamed: aString ifAbsent:
		[self inform: aString, ' not found in the Sound Library'.
		nil]! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 9/27/2001 14:45'!
soundNamed: aString ifAbsent: aBlock
	"Answer the sound of the given name, or if there is no sound of that name, answer the result of evaluating aBlock"
	"(SampledSound soundNamed: 'shutterClick') play"

	| entry samples |
	entry _ SoundLibrary
		at: aString
		ifAbsent:
			[^ aBlock value].
	entry ifNil: [^ aBlock value].
	samples _ entry at: 1.
	samples class isBytes ifTrue: [samples _ self convert8bitSignedTo16Bit: samples].
	^ self samples: samples samplingRate: (entry at: 2)
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 1/16/2011 23:55'!
soundNames
	"Answer a list of sound names for the sounds stored in the sound library."
	"| s |
	 SampledSound soundNames asSortedCollection do: [:n |
		n asParagraph display.
		s _ SampledSound soundNamed: n.
		s ifNotNil: [s playAndWaitUntilDone]]"

	^ SoundLibrary keys! !

!SampledSound class methodsFor: 'sound library' stamp: 'jhm 10/15/97 14:57'!
unusedSoundNameLike: desiredName
	"Pick an unused sound name based on the given string. If necessary, append digits to avoid name conflicts with existing sounds."
	"SampledSound unusedSoundNameLike: 'chirp'"

	| newName i |
	newName _ desiredName.
	i _ 2.
	[SoundLibrary includesKey: newName] whileTrue: [
		newName _ desiredName, i printString.
		i _ i + 1].
	^ newName
! !

!SampledSound class methodsFor: 'instance creation' stamp: 'gk 2/24/2004 08:50'!
beep
	"Beep in the presence of the sound system.
	Not to be used directly - use Beeper class>>beep
	or Beeper class>>beepPrimitive instead."

	(self new
			setSamples: self coffeeCupClink
			samplingRate: 12000) play
			! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:26'!
fromAIFFfileNamed: fileName
	"Read a SampledSound from the AIFF file of the given name, merging stereo to mono if necessary."
	"(SampledSound fromAIFFfileNamed: '1.aif') play"
	"| snd |
	 FileDirectory smalltalkImageDirectory fileNames do: [:n |
		(n endsWith: '.aif')
			ifTrue: [
				snd _ SampledSound fromAIFFfileNamed: n.
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	| aiffFileReader |
	aiffFileReader _ AIFFFileReader new.
	aiffFileReader readFromFile: fileName
		mergeIfStereo: true
		skipDataChunk: false.
	^ self
		samples: (aiffFileReader channelData at: 1)
		samplingRate: aiffFileReader samplingRate
! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jmv 7/21/2017 16:19:57'!
fromFileEntry: aFileEntry
	"(SampledSound fromWaveFileNamed: 'c:\windows\media\chimes.wav') play"
	"| snd fd |
	fd := FileDirectory on:'c:\windows\media\'.
	fd fileNames do: [:n |
		(n asLowercase endsWith: '.wav')
			ifTrue: [
				snd _ SampledSound fromWaveFileNamed: (fd pathName,n).
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	^ aFileEntry readStreamDo: [ :strm |
		self fromWaveStream: strm ]
! !

!SampledSound class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:33'!
fromWaveFileNamed: fileName
	"(SampledSound fromWaveFileNamed: 'c:\windows\media\chimes.wav') play"
	"| snd fd |
	fd := FileDirectory on:'c:\windows\media\'.
	fd fileNames do: [:n |
		(n asLowercase endsWith: '.wav')
			ifTrue: [
				snd _ SampledSound fromWaveFileNamed: (fd pathName,n).
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	^ fileName asFileEntry readStreamDo: [ :strm |
		self fromWaveStream: strm ]
! !

!SampledSound class methodsFor: 'instance creation'!
fromWaveStream: fileStream

	| stream header data type channels samplingRate blockAlign bitsPerSample leftAndRight |
	header _ self readWaveChunk: 'fmt ' inRIFF: fileStream.
	data _ self readWaveChunk: 'data' inRIFF: fileStream.
	fileStream close.
	stream _ ReadStream on: header.
	type _ self next16BitWord: false from: stream.
	type = 1 ifFalse: [^ self error:'Unexpected wave format'].
	channels _ self next16BitWord: false from: stream.
	(channels < 1 or: [channels > 2])
		ifTrue: [^ self error: 'Unexpected number of wave channels'].
	samplingRate _ self next32BitWord: false from: stream.
	stream skip: 4. "skip average bytes per second"
	blockAlign _ self next16BitWord: false from: stream.
	bitsPerSample _ self next16BitWord: false from: stream.
	(bitsPerSample = 8 or: [bitsPerSample = 16])
		ifFalse: [  "recompute bits per sample"
			bitsPerSample _ (blockAlign // channels) * 8].

	bitsPerSample = 8
		ifTrue: [data _ self convert8bitUnsignedTo16Bit: data]
		ifFalse: [data _ self convertBytesTo16BitSamples: data mostSignificantByteFirst: false].

	channels = 2 ifTrue: [
		leftAndRight _ data splitStereo.
		^ MixedSound new
			add: (self samples: leftAndRight first samplingRate: samplingRate) pan: 0.0;
			add: (self samples: leftAndRight last samplingRate: samplingRate) pan: 1.0;
			yourself].

	^ self samples: data samplingRate: samplingRate
! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jm 9/12/97 19:20'!
samples: anArrayOf16BitSamples samplingRate: samplesPerSecond
	"Return a SampledSound with the given samples array and sampling rate."

	^ self new setSamples: anArrayOf16BitSamples samplingRate: samplesPerSecond
! !

!SampledSound class methodsFor: 'coffee cup clink' stamp: 'jm 8/23/97 22:25'!
coffeeCupClink
	"Return the samples array for the sound of a spoon being tapped against a coffee cup."

	CoffeeCupClink ifNil: [self initializeCoffeeCupClink].
	^ CoffeeCupClink
! !

!SampledSound class methodsFor: 'coffee cup clink' stamp: 'jm 9/17/97 12:50'!
initializeCoffeeCupClink
	"Initialize the samples array for the sound of a spoon being tapped against a coffee cup."
	"SampledSound initializeCoffeeCupClink"

	| samples |
	samples _ #(768 1024 -256 2304 -13312 26624 32512 19200 6400 -256 5888 32512 28928 32512 -32768 32512 -32768 18688 26368 -26112 32512 32512 2304 32512 5632 2816 10240 -4608 -1792 32512 32512 -5376 10752 32512 32512 32512 8192 15872 32512 -3584 -32768 -23296 -24832 -32768 -32768 -32768 -2304 32512 32512 -32768 32512 -15360 6400 8448 -18176 -32768 -256 -32768 -29440 9472 20992 17920 32512 32512 -256 32512 -32768 -32768 -23040 -32768 -25088 -32768 -27648 -1536 24320 -32768 32512 20480 27904 22016 16384 -32768 32512 -27648 -32768 -7168 28160 -6400 5376 32512 -256 32512 -7168 -11776 -19456 -27392 -24576 -32768 -24064 -19456 12800 32512 27136 2048 25344 15616 8192 -4608 -28672 -32768 -30464 -2560 17664 256 -8192 8448 32512 27648 -6144 -512 -7424 -18688 7936 -256 -22272 -14080 2048 27648 15616 -12288 -768 5376 3328 5632 3072 -6656 -20480 10240 27136 -10752 -11008 -768 -2048 6144 -7168 -3584 -1024 -7680 19712 26112 1024 -11008 3072 16384 -8960 -14848 -4864 -23808 -11264 12288 8192 7168 4864 23040 32512 512 -11776 -5632 -16896 -21504 -12800 -6144 -16896 -4352 32512 32512 23296 21760 5632 2816 -9472 -20992 -11264 -29440 -32768 -3584 7680 8448 15360 32512 32512 15616 15104 -2048 -27904 -27904 -25600 -12288 -12032 -13568 17152 22272 15360 30208 28160 7680 -5632 -8192 -16384 -31744 -25856 -10752 -3840 6656 13056 24320 26368 12800 20736 12288 -19200 -20992 -16640 -21504 -17920 -6912 8448 11264 14080 23040 18176 8192 -1024 0 256 -20992 -19712 -4608 -11264 -2048 14080 12032 8192 6912 13056 9216 -5632 -5376 -3840 -6656 -9984 -5632 4864 -3584 -1280 17408 7680 -1280 4096 2816 -1024 -4864 3328 8448 -768 -5888 -2048 5120 0 3072 11008 -7680 -15360 2560 6656 -3840 0 11776 7680 2816 1536 -1280 -3840 -8704 -1536 3584 -9728 -9728 11776 18688 7680 6656 6400 -4864 -3840 -256 -6912 -13312 -11264 2304 9728 1792 3328 18944 18432 6912 6144 -1536 -17664 -14336 -2304 -10496 -15616 -4096 9728 17152 14848 13312 11520 2304 -1024 2560 -8704 -26624 -18688 -256 -256 2816 14080 13824 12544 14080 9728 -512 -12032 -8960 -3328 -9984 -15872 -5120 8192 3584 10496 20224 7936 4608 6144 1280 -8704 -12800 -7424 -8448 -8960 -3840 7424 13056 8704 13312 13056 -2304 -4864 -768 -7168 -10496 -4608 -1536 -3072 -1280 6144 13312 11008 4864 4864 1536 -8960 -7680 1792 -4864 -7680 2816 5632 3328 2560 5376 7936 3584 -512 512 -4608 -9728 0 9216 768 -4096 7680 7168 256 4608 -768 -8704 -6400 2048 6144 -3072 -3328 6400 9472 3840 -768 1792 -3840 -5120 6144 768 -9984 -4352 5120 9472 6912 2816 1792 1280 768 512 -2816 -9728 -6912 6912 6912 -3328 -768 8448 11776 10752 3328 -6912 -10752 -8704 -1536 0 -6912 -3328 9984 13568 7424 6144 6656 256 0 256 -12032 -17920 -8192 3584 8960 4096 5632 12032 8704 6912 5632 -3584 -10496 -7936 -2048 -9216 -11776 2304 9472 15104 14848 5888 512 -2816 1024 2560 -9984 -13312 -5120 768 1792 768 8448 12032 11264 12800 -256 -11264 -9728 -2304 3072 -4352 -6912 256 2304 5376 9984 8192 2816 1280 3584 -2048 -11008 -8448 -2048 3072 4864 2304 3072 3072 3072 7168 3328 -5376 -4864 512 512 -1792 -1792 1792 5376 5888 5888 512 -5888 -3584 4096 3584 -6400 -4864 4608 3072 3840 5376 1024 768 2816 5888 -768 -12288 -7936 2304 5888 3328 2048 6144 3072 3072 6400 -3328 -7168 256 4096 -512 -9472 -6656 3328 6912 9216 8704 3840 -2560 -256 6656 -2560 -11264 -4608 -768 -1280 1536 3072 4096 5120 9984 11264 1024 -8192 -6144 -1024 -3840 -5632 -512 1024 2304 9728 9728 1280 512 4096 2816 -3584 -9984 -6912 -2304 512 5632 7680 3584 1024 5632 5888 -1280 -3584 -2304 -2560 -1536 -1024 -1792 -512 1536 7680 9984 2048 -2048 2048 3328 -1280 -4096 -3328 -4608 -1280 4352 3328 1280 1792 5120 6912 1024 -2560 0 -768 -1024 1280 -256 -4608 -1280 6400 5120 768 1792 2560 2048 0 -1536 -1280 -2304 1024 5376 2560 -2560 -512 4096 2048 512 768 -1280 -256 2560 2560 -256 -1024 768 3584 1280 -3328 -1536 1792 2816 3328 2304 -256 256 2816 2304 -1280 -3328 -1536 2304 2304 -256 -256 1024 1536 3840 5120 1024 -2048 0 1536 -768 -2560 -1792 256 2304 2048 1536 256 768 5888 6656 256 -3840 -2304 -1280 -1536 256 0 -512 2304 4352 3840 768 0 2304 3072 256 -3072 -2560 -2560 256 4608 2560 256 1536 3072 3072 1792 256 256 512 -256 -768 -1280 -1536 768 4352 2816 -512 768 2560 2560 2304 -256 -1792 -768 768 1792 256 -2304 -256 3328 3840 2304 2304 1536 256 2048 1024 -1536 -1792 -1024 512 256 -512 0 2304 4864 5120 4352 1024 -1280 0 -768 -2816 -2304 -512 1024 2048 2304 2048 3072 3840 2816 2048 -512 -3072 -1792 -1536 -1280 768 1280 1536 2304 2816 2048 1536 2048 1536 1536 -768 -3840 -2048 0 1280 2816 1792 1536 2560 3584 2816 1024 256 -768 -768 -1280 -2816 -768 1792 3328 5120 3072 1280 1536 1792 768 -1024 -1280 -1536 -768 512 256 1536 2560 2560 3328 1280 0 768 1536 768 -256 -512 -1536 -1280 768 1280 2304 2560 2560 2560 1024 -256 -512 0 1280 1536 768 -1280 -512 2048 1536 2048 1280 -256 256 512 768 768 1280 2304 1792 512 -1280 -1024 768 1536 1536 256 -768 1536 3584 3072 1792 -256 -1536 -512 256 -512 -512 768 2048 2048 1792 1280 1280 3072 2816 768 -1024 -2304 -1024 256 256 1280 1792 2304 2816 2304 1280 512 1024 768 -768 -1280 -1280 -512 1536 2560 2816 2048 512 1024 1792 1280 768 0 -768 -768 0 256 256 1280 2560 2304 2304 1536 512 512 1024 1280 0 -1792 -1536 -512 1280 3072 2816 1792 512 1024 1536 256 -256 768 768 256 256 -256 512 1280 1280 1536 768 1024 1792 1536 1024 0 256 -512 -256 1024 512 256 768 1792 2304 1280 256 768 1024 1280 1792 768 -768 -768 768 512 256 1024 1792 1536 1280 1536 1792 1280 768 512 -512 -1792 -512 512 768 2304 2816 1792 768 1536 2304 1536 0 -256 -256 -768 -768 256 1536 1536 2304 2048 256 768 2048 2304 1280 0 -256 -1024 -1024 0 1024 1792 2304 2304 1280 512 1280 2048 1280 256 -512 -1792 -1536 256 1536 1792 2048 2048 2048 1536 512 512 768 256 -256 0 -512 -1024 768 2048 2304 2304 1280 1280 1024 1024 1024 0 -512 256 768 0 -256 1536 2304 1792 2304 1280 -512 -256 768 1536 1024 256 512 512 1024 1792 1792 1536 1024 1280 0 -1280 256 2048 2560 2048 1024 -256 -256 1024 1280 1536 1024 0 0 256 768 1792 2304 2048 1280 1024 0 -512 -256 256 1024 1024 512 768 768 1280 2048 1792 1024 768 768 -256 -1024 0 256 1024 1536 1024 1280 1536 1792 1792 1024 512 512 0 -512 -256 512 768 1280 1280 1024 1280 1792 1792 1280 512 -256 -256 256 512 1280 1024 1280 1280 1024 1024 768 1024 1024 1024 1280 256 256 768 768 1024 512 256 768 1280 2560 2560 1280 512 -256 -512 -256 1024 1536 768 1024 1280 768 1024 1536 1536 1024 256 0 0 0 768 768 512 1280 1536 1280 1280 1280 1280 768 768 256 -256 768 768 256 768 1280 1792 1536 1536 1536 256 512 1024 0 -768 -256 768 512 1024 2048 1536 1024 1536 1536 768 0 0 -256).

	CoffeeCupClink _ SoundBuffer fromArray: samples.
! !

!SampledSound class methodsFor: 'utilities' stamp: 'ar 2/3/2001 16:14'!
convert8bitSignedFrom: aByteArray to16Bit: aSoundBuffer
	"Copy the contents of the given array of signed 8-bit samples into the given array of 16-bit signed samples."

	| n s |
	<primitive: 'primitiveConvert8BitSigned' module: 'MiscPrimitivePlugin'>
	self var: #aByteArray declareC: 'unsigned char *aByteArray'.
	self var: #aSoundBuffer declareC: 'unsigned short *aSoundBuffer'.
	n _ aByteArray size.
	1 to: n do: [:i |
		s _ aByteArray at: i.
		s > 127
			ifTrue: [aSoundBuffer at: i put: ((s - 256) bitShift: 8)]
			ifFalse: [aSoundBuffer at: i put: (s bitShift: 8)]].
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 2/15/98 18:13'!
convert8bitSignedTo16Bit: aByteArray
	"Convert the given array of samples--assumed to be 8-bit signed, linear data--into 16-bit signed samples. Return an array containing the resulting samples. Typically used to read uncompressed AIFF sound data."

	| result |
	result _ SoundBuffer newMonoSampleCount: aByteArray size.
	self convert8bitSignedFrom: aByteArray to16Bit: result.
	^ result
! !

!SampledSound class methodsFor: 'utilities' stamp: 'ar 1/27/98 23:11'!
convert8bitUnsignedTo16Bit: anArray
	"Convert the given array of samples--assumed to be 8-bit unsigned, linear data--into 16-bit signed samples. Return an array containing the resulting samples. Typically used to read uncompressed WAVE sound data."

	| n samples s |
	n _ anArray size.
	samples _ SoundBuffer newMonoSampleCount: n.
	1 to: n do: [:i |
		s _ anArray at: i.
		samples at: i put: (s - 128 * 256)].
	^ samples
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 3/17/98 21:07'!
convertBytesTo16BitSamples: aByteArray mostSignificantByteFirst: msbFirst
	"Convert the given ByteArray (with the given byte ordering) into 16-bit sample buffer."

	| n data src b1 b2 w |
	n _ aByteArray size // 2.
	data _ SoundBuffer newMonoSampleCount: n.
	src _ 1.
	1 to: n do: [:i |
		b1 _ aByteArray at: src.
		b2 _ aByteArray at: src + 1.
		msbFirst
			ifTrue: [w _ (b1 bitShift: 8) + b2]
			ifFalse: [w _ (b2 bitShift: 8) + b1].
		w > 32767 ifTrue: [w _ w - 65536].
		data at: i put: w.
		src _ src + 2].
	^ data
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/17/97 13:11'!
uLawDecode: aByteArray
	"Convert the given array of uLaw-encoded 8-bit samples into a SoundBuffer of 16-bit signed samples."

	| n out decodingTable |
	n _ aByteArray size.
	out _ SoundBuffer newMonoSampleCount: n.
	decodingTable _ self uLawDecodeTable.
	1 to: n do: [:i | out at: i put: (decodingTable at: (aByteArray at: i) + 1)].
	^ out
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 16:41'!
uLawDecodeTable
	"Return a 256 entry table to be used to decode 8-bit uLaw-encoded samples."
	"Details: This table was computed as follows:
		| d encoded lastEncodedPos lastEncodedNeg |
		d _ Array new: 256.
		lastEncodedPos _ nil.
		lastEncodedNeg _ nil.
		4095 to: 0 by: -1 do: [:s |
			encoded _ SampledSound uLawEncodeSample: s.
			lastEncodedPos = encoded
				ifFalse: [
					d at: (encoded + 1) put: (s bitShift: 3).
					lastEncodedPos _ encoded].
			encoded _ encoded bitOr: 16r80.
			lastEncodedNeg = encoded
				ifFalse: [
					d at: (encoded + 1) put: (s bitShift: 3) negated.
					lastEncodedNeg _ encoded]].
		d "

	^ #(32760 31608 30584 29560 28536 27512 26488 25464 24440 23416 22392 21368 20344 19320 18296 17272 16248 15736 15224 14712 14200 13688 13176 12664 12152 11640 11128 10616 10104 9592 9080 8568 8056 7800 7544 7288 7032 6776 6520 6264 6008 5752 5496 5240 4984 4728 4472 4216 3960 3832 3704 3576 3448 3320 3192 3064 2936 2808 2680 2552 2424 2296 2168 2040 1912 1848 1784 1720 1656 1592 1528 1464 1400 1336 1272 1208 1144 1080 1016 952 888 856 824 792 760 728 696 664 632 600 568 536 504 472 440 408 376 360 344 328 312 296 280 264 248 232 216 200 184 168 152 136 120 112 104 96 88 80 72 64 56 48 40 32 24 16 8 0 -32760 -31608 -30584 -29560 -28536 -27512 -26488 -25464 -24440 -23416 -22392 -21368 -20344 -19320 -18296 -17272 -16248 -15736 -15224 -14712 -14200 -13688 -13176 -12664 -12152 -11640 -11128 -10616 -10104 -9592 -9080 -8568 -8056 -7800 -7544 -7288 -7032 -6776 -6520 -6264 -6008 -5752 -5496 -5240 -4984 -4728 -4472 -4216 -3960 -3832 -3704 -3576 -3448 -3320 -3192 -3064 -2936 -2808 -2680 -2552 -2424 -2296 -2168 -2040 -1912 -1848 -1784 -1720 -1656 -1592 -1528 -1464 -1400 -1336 -1272 -1208 -1144 -1080 -1016 -952 -888 -856 -824 -792 -760 -728 -696 -664 -632 -600 -568 -536 -504 -472 -440 -408 -376 -360 -344 -328 -312 -296 -280 -264 -248 -232 -216 -200 -184 -168 -152 -136 -120 -112 -104 -96 -88 -80 -72 -64 -56 -48 -40 -32 -24 -16 -8 0)
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 15:52'!
uLawEncode: anArray
	"Convert the given array of 16-bit signed samples into a ByteArray of uLaw-encoded 8-bit samples."

	| n out s |
	n _ anArray size.
	out _ ByteArray new: n.
	1 to: n do: [:i |
		s _ anArray at: i.
		s _ s bitShift: -3.  "drop 4 least significant bits"
		s < 0
			ifTrue: [s _ (self uLawEncodeSample: s negated) bitOr: 16r80]
			ifFalse: [s _ (self uLawEncodeSample: s)].
		out at: i put: s].
	^ out
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 15:40'!
uLawEncodeSample: s
	"Encode the given 16-bit signed sample using the uLaw 8-bit encoding."

	s < 496 ifTrue: [
		s < 112 ifTrue: [
			s < 48 ifTrue: [
				s < 16
					ifTrue: [^ 16r70 bitOr: (15 - s)]
					ifFalse: [^ 16r60 bitOr: (15 - ((s - 16) bitShift: -1))]].
			^ 16r50 bitOr: (15 - ((s - 48) bitShift: -2))].
		s < 240
			ifTrue: [^ 16r40 bitOr: (15 - ((s - 112) bitShift: -3))]
			ifFalse: [^ 16r30 bitOr: (15 - ((s - 240) bitShift: -4))]].

	s < 2032 ifTrue: [
		s < 1008
			ifTrue: [^ 16r20 bitOr: (15 - ((s - 496) bitShift: -5))]
			ifFalse: [^ 16r10 bitOr: (15 - ((s - 1008) bitShift: -6))]].

	s < 4080
		ifTrue: [^ 15 - ((s - 2032) bitShift: -7)]
		ifFalse: [^ 0].
! !

!SampledSound class methodsFor: 'default sound' stamp: 'jm 9/17/97 12:49'!
defaultSampleTable: anArray
	"Set the sample table to be used as the default waveform for playing a score such as the Bach fugue. Array is assumed to contain monaural signed 16-bit sample values."

	DefaultSampleTable _ SoundBuffer fromArray: anArray.
! !

!SampledSound class methodsFor: 'default sound' stamp: 'jm 9/17/97 13:10'!
defaultSamples: anArray repeated: n

	| data |
	data _ WriteStream on: (SoundBuffer newMonoSampleCount: anArray size * n).
	n timesRepeat: [
		anArray do: [:sample | data nextPut: sample truncated]].
	DefaultSampleTable _ data contents.
! !

!SampledSound class methodsFor: 'default sound'!
nominalSamplePitch: aNumber
	"Record an estimate of the normal pitch of the sampled sound."

	NominalSamplePitch _ aNumber.
! !

!SampledSound class methodsFor: 'default sound' stamp: 'jm 5/8/1998 18:53'!
useCoffeeCupClink
	"Set the sample table to be used as the default waveform to the sound of a coffee cup being tapped with a spoon."
	"SampledSound useCoffeeCupClink bachFugue play"

	DefaultSampleTable _ self coffeeCupClink.
	NominalSamplePitch _ 400.
! !

!SampledSound class methodsFor: 'class initialization' stamp: 'jmv 6/16/2013 12:36'!
initialize
	"SampledSound initialize"

	SoundPlayer initialize.
	IncrementFractionBits _ 16.
	IncrementScaleFactor _ 2 raisedTo: IncrementFractionBits.
	ScaledIndexOverflow _ 2 raisedTo: 29.  "handle overflow before needing LargePositiveIntegers"
	self useCoffeeCupClink.
	SoundLibrary ifNil: [SoundLibrary _ Dictionary new].
	Beeper setDefault: (self new
						setSamples: self coffeeCupClink
						samplingRate: 12000).
! !

!SampledSound class methodsFor: 'WAV reading' stamp: 'ar 1/27/98 23:06'!
next16BitWord: msbFirst from: stream
	"Read a 16-bit positive integer from the input stream."
	"Assume: Stream has at least two bytes left."

	| n |
	n _ stream next: 2.
	^msbFirst
		ifTrue:[(n at: 1) * 256 + (n at: 2)]
		ifFalse:[(n at: 2) * 256 + (n at: 1)]
! !

!SampledSound class methodsFor: 'WAV reading' stamp: 'ar 1/27/98 23:06'!
next32BitWord: msbFirst from: stream
	"Read a 32-bit positive integer from the input stream."
	"Assume: Stream has at least four bytes left."

	| n |
	n _ stream next: 4.
	^msbFirst
		ifTrue:[(n at: 1) * 256 + (n at: 2) * 256 + (n at: 3) * 256 + (n at: 4)]
		ifFalse:[(n at: 4) * 256 + (n at: 3) * 256 + (n at: 2) * 256 + (n at: 1)]
! !

!SampledSound class methodsFor: 'WAV reading' stamp: 'jm 3/17/98 21:03'!
readWaveChunk: chunkType inRIFF: stream
	"Search the stream for a format chunk of the given type and return its contents."

	| id count |
	stream reset; binary.
	stream skip: 8.  "skip 'RIFF' and total length"
	id _ (stream next: 4) asString.  "contents type"
	id = 'WAVE' ifFalse: [^ ''].     "content type must be WAVE"

	"search for a chunk of the given type"
	[id _ (stream next: 4) asString.
	 count _ self next32BitWord: false from: stream.
	 id = chunkType] whileFalse: [
		"skip this chunk, rounding length up to a word boundary"
		stream skip: (count + 1 bitAnd: 16rFFFFFFFE).
		stream atEnd ifTrue: [^ '']].

	^ stream next: count  "return raw chunk data"
! !

!SequentialSound methodsFor: 'copying' stamp: 'jm 12/15/97 22:48'!
copySounds
	"Private!! Support for copying. Copy my component sounds."

	sounds _ sounds collect: [:s | s copy].
! !

!SequentialSound methodsFor: 'copying' stamp: 'jmv 8/5/2011 16:00'!
transformSounds: tfmBlock
	"Private!! Support for copying. Copy my component sounds."

	sounds _ sounds collect: tfmBlock! !

!SequentialSound methodsFor: 'composition'!
, aSound
	"Return the concatenation of the receiver and the argument sound."

	^ self add: aSound
! !

!SequentialSound methodsFor: 'composition' stamp: 'jm 12/15/97 22:48'!
add: aSound

	sounds _ sounds copyWith: aSound.
! !

!SequentialSound methodsFor: 'composition' stamp: 'di 12/7/2000 16:03'!
compressWith: codecClass
	^ self copy transformSounds: [:s | s compressWith: codecClass]! !

!SequentialSound methodsFor: 'composition' stamp: 'RAA 12/24/2000 08:42'!
compressWith: codecClass atRate: aSamplingRate
	^ self copy transformSounds: [:s | s compressWith: codecClass atRate: aSamplingRate]! !

!SequentialSound methodsFor: 'composition' stamp: 'jm 4/14/1999 10:05'!
pruneFinishedSounds
	"Remove any sounds that have been completely played."

	| newSnds |
	(currentIndex > 1 and: [currentIndex < sounds size]) ifFalse: [^ self].
	newSnds _ sounds copyFrom: currentIndex to: sounds size.
	currentIndex _ 1.
	sounds _ newSnds.
! !

!SequentialSound methodsFor: 'composition' stamp: 'RAA 8/9/2000 16:27'!
removeFirstCompleteSoundOrNil
	"Remove the first sound if it has been completely recorded."

	| firstSound |

	sounds size > 0 ifFalse: [^ nil].
	firstSound _ sounds first.
	sounds _ sounds copyFrom: 2 to: sounds size.
	^firstSound
! !

!SequentialSound methodsFor: 'sound generation' stamp: 'jm 11/25/97 13:41'!
doControl

	super doControl.
	currentIndex > 0
		ifTrue: [(sounds at: currentIndex) doControl].
! !

!SequentialSound methodsFor: 'sound generation' stamp: 'jm 11/24/97 16:16'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a collection of sounds in sequence."
	"PluckedSound chromaticScale play"

	| finalIndex i snd remaining count |
	currentIndex = 0 ifTrue: [^ self].  "already done"
	finalIndex _ (startIndex + n) - 1.
	i _ startIndex.
	[i <= finalIndex] whileTrue: [
		snd _ (sounds at: currentIndex).
		[(remaining _ snd samplesRemaining) <= 0] whileTrue: [
			"find next undone sound"
			currentIndex < sounds size
				ifTrue: [
					currentIndex _ currentIndex + 1.
					snd _ (sounds at: currentIndex)]
				ifFalse: [
					currentIndex _ 0.
					^ self]].  "no more sounds"
		count _ (finalIndex - i) + 1.
		remaining < count ifTrue: [count _ remaining].
		snd mixSampleCount: count into: aSoundBuffer startingAt: i leftVol: leftVol rightVol: rightVol.
		i _ i + count].
! !

!SequentialSound methodsFor: 'sound generation' stamp: 'jm 12/15/97 22:47'!
reset

	super reset.
	sounds do: [:snd | snd reset].
	sounds size > 0 ifTrue: [currentIndex _ 1].
! !

!SequentialSound methodsFor: 'sound generation' stamp: 'jm 12/15/97 22:47'!
samplesRemaining

	currentIndex = 0
		ifTrue: [^ 0]
		ifFalse: [^ 1000000].
! !

!SequentialSound methodsFor: 'accessing' stamp: 'RAA 12/7/2000 17:24'!
duration
	"Answer the duration of this sound in seconds."

	"7 dec 2000 - handle compressed sounds. better way??"

	| dur |
	dur _ 0.
	sounds do: [:snd | dur _ dur + snd asSound duration].
	^ dur
! !

!SequentialSound methodsFor: 'accessing' stamp: 'jm 2/4/98 13:36'!
sounds

	^ sounds
! !

!SequentialSound methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:49'!
initialize

	super initialize.
	sounds _ #().
	currentIndex _ 0.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 10/18/2001 15:51'!
createMixer
	"Create a mixed sound consisting of sampled sounds with one sound buffer's worth of samples."

	| snd |
	mixer _ MixedSound new.
	snd _ SampledSound
		samples: (SoundBuffer newMonoSampleCount: 2)  "buffer size will be adjusted dynamically"
		samplingRate: streamSamplingRate.
	mixer add: snd pan: 0.5 volume: volume.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/21/2001 09:05'!
currentSampleIndex
	"Answer the index of the current sample."

	| bytePosition frameIndex |
	bytePosition _ stream position - audioDataStart.
	codec
		ifNil: [^ bytePosition // 2]
		ifNotNil: [
			frameIndex _ bytePosition // codec bytesPerEncodedFrame.
			^ (frameIndex * codec samplesPerFrame) - leftoverSamples monoSampleCount].
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/21/2001 11:37'!
loadBuffer: aSoundBuffer compressedSampleCount: sampleCount
	"Load the given sound buffer from the compressed sample stream."
	"Details: Most codecs decode in multi-sample units called 'frames'. Since the requested sampleCount is typically not an even multiple of the frame size, we need to deal with partial frames. The unused samples from a partial frame are retained until the next call to this method."

	| n samplesNeeded frameCount encodedBytes r decodedCount buf j |
	"first, use any leftover samples"
	n _ self loadFromLeftovers: aSoundBuffer sampleCount: sampleCount.
	samplesNeeded _ sampleCount - n.
	samplesNeeded <= 0 ifTrue: [^ self].

	"decode an integral number of full compression frames"
	frameCount _ samplesNeeded // codec samplesPerFrame.
	encodedBytes _ stream next: (frameCount * codec bytesPerEncodedFrame).
	r _ codec decodeFrames: frameCount from: encodedBytes at: 1 into: aSoundBuffer at: n + 1.
	decodedCount _ r last.
	decodedCount >= samplesNeeded ifTrue: [^ self].

	"decode one last compression frame to finish filling the buffer"
	buf _ SoundBuffer newMonoSampleCount: codec samplesPerFrame.
	encodedBytes _ stream next: codec bytesPerEncodedFrame.
	codec decodeFrames: 1 from: encodedBytes at: 1 into: buf at: 1.
	j _ 0.
	(n + decodedCount + 1) to: sampleCount do: [:i |
		aSoundBuffer at: i put: (buf at: (j _ j + 1))].

	"save the leftover samples"
	leftoverSamples _ buf copyFrom: (j + 1) to: buf monoSampleCount.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jmv 2/26/2016 16:03'!
loadBuffer: aSoundBuffer uncompressedSampleCount: sampleCount
	"Load the given sound buffer from the uncompressed sample stream."

	"read directly into the sample buffer; count is in 32-bit words"
	stream next: sampleCount // 2 into: aSoundBuffer startingAt: 1.
	aSoundBuffer restoreEndianness.

	"read the final sample if sampleCount is odd:"
	sampleCount odd ifTrue: [aSoundBuffer at: sampleCount put: (stream nextSignedInt16BigEndian: true)].
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/21/2001 08:02'!
loadBuffersForSampleCount: count
	"Load the sound buffers from the stream."

	| snd buf sampleCount |
	snd _ mixer sounds first.
	buf _ snd samples.
	buf monoSampleCount = count ifFalse: [
		buf _ SoundBuffer newMonoSampleCount: count.
		snd setSamples: buf samplingRate: streamSamplingRate].
	sampleCount _ count min: (totalSamples - self currentSampleIndex).
	sampleCount < count ifTrue: [buf primFill: 0].

	codec
		ifNil: [self loadBuffer: buf uncompressedSampleCount: sampleCount]
		ifNotNil: [self loadBuffer: buf compressedSampleCount: sampleCount].

	mixer reset.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/21/2001 09:09'!
loadFromLeftovers: aSoundBuffer sampleCount: sampleCount
	"Load the given sound buffer from the samples leftover from the last frame. Answer the number of samples loaded, which typically is less than sampleCount."

	| leftoverCount n |
	leftoverCount _ leftoverSamples monoSampleCount.
	leftoverCount = 0 ifTrue: [^ 0].

	n _ leftoverCount min: sampleCount.
	1 to: n do: [:i | aSoundBuffer at: i put: (leftoverSamples at: i)].
	n < sampleCount
		ifTrue: [leftoverSamples _ SoundBuffer new]
		ifFalse: [leftoverSamples _ leftoverSamples copyFrom: n + 1 to: leftoverSamples size].
	^ n
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'pb 5/25/2016 01:51'!
positionCodecTo: desiredSampleIndex
	"Position to the closest frame before the given sample index when using a codec. If using the ADPCM codec, try to ensure that it is in sync with the compressed sample stream."

	| desiredFrameIndex desiredPosition tmpStream tmpCodec byteBuf bufFrames sampleBuf frameCount n startOffset |
	(codec isKindOf: ADPCMCodec) ifFalse: [
		"stateless codecs (or relatively stateless ones, like GSM: just jump to frame boundary"
		desiredFrameIndex _ desiredSampleIndex // codec samplesPerFrame.
		stream position: audioDataStart + (desiredFrameIndex * codec bytesPerEncodedFrame).
		codec reset.
		^ self].

	"compute the desired stream position"
	desiredFrameIndex _ desiredSampleIndex // codec samplesPerFrame.
	desiredPosition _ audioDataStart + (desiredFrameIndex * codec bytesPerEncodedFrame).

	"copy stream and codec"
	(stream isKindOf: FileStream)
		ifTrue: [tmpStream _ (stream name asFileEntry readStream ) binary]
		ifFalse: [tmpStream _ "stream deepCopy" stream contents readStream].	"To kill #deepCopy. Not sure if right, though (jmv)"
	tmpCodec _ codec copy reset.

	"reset the codec and start back about 30 seconds to try to get codec in sync"
	startOffset _ ((desiredFrameIndex - 80000) max: 0) * codec bytesPerEncodedFrame.
	tmpStream position: audioDataStart + startOffset.

	"decode forward to the desired position"
	byteBuf _ ByteArray new: (32000 roundTo: codec bytesPerEncodedFrame).
	bufFrames _ byteBuf size // codec bytesPerEncodedFrame.
	sampleBuf _ SoundBuffer newMonoSampleCount: bufFrames * codec samplesPerFrame.
	frameCount _ (desiredPosition - tmpStream position) // codec bytesPerEncodedFrame.
	[frameCount > 0] whileTrue: [
		n _ bufFrames min: frameCount.
		tmpStream next: n * codec bytesPerEncodedFrame into: byteBuf startingAt: 1.
		tmpCodec decodeFrames: n from: byteBuf at: 1 into: sampleBuf at: 1.
		frameCount _ frameCount - n].

	codec _ tmpCodec.
	stream position: tmpStream position.
	(tmpStream isKindOf: FileStream) ifTrue: [tmpStream close].! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/16/2001 10:23'!
readAIFFHeader
	"Read an AIFF file header from stream."

	| aiffReader |
	aiffReader _ AIFFFileReader new.
	aiffReader readFromStream: stream mergeIfStereo: false skipDataChunk: true.
	aiffReader channelCount = 1 ifFalse: [self error: 'not monophonic'].
	aiffReader bitsPerSample = 16 ifFalse: [self error: 'not 16-bit'].

	audioDataStart _ headerStart + aiffReader channelDataOffset.
	streamSamplingRate _ aiffReader samplingRate.
	totalSamples _ aiffReader frameCount min: (stream size - audioDataStart) // 2.
	codec _ nil.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jmv 10/5/2015 12:12'!
readHeader
	"Read the sound file header from my stream."

	| id |
	stream position: headerStart.
	id _ (stream next: 4) asString.
	stream position: headerStart.
	id = 'FORM' ifTrue: [^ self readAIFFHeader].
	self error: 'unrecognized sound file format'.
! !

!StreamingMonoSound methodsFor: 'private' stamp: 'jm 11/27/2001 07:36'!
startOver
	"Jump back to the first sample."

	stream reopen; binary.
	self readHeader.
	stream position: audioDataStart.
	leftoverSamples _ SoundBuffer new.
	lastBufferMSecs _ 0.
	mutex _ Semaphore forMutualExclusion.
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 11/16/2001 17:12'!
duration
	"Answer the duration of this sound in seconds."

	^ totalSamples asFloat / streamSamplingRate
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 10/18/2001 15:46'!
repeat
	"Answer the repeat flag."

	^ repeat
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 6/3/2001 18:39'!
repeat: aBoolean
	"Set the repeat flag. If true, this sound will loop back to the beginning when it gets to the end."

	repeat _ aBoolean.
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 11/16/2001 17:05'!
soundPosition
	"Answer the relative position of sound playback as a number between 0.0 and 1.0."

	(stream isNil or: [stream closed]) ifTrue: [^ 0.0].
	^ self currentSampleIndex asFloat / totalSamples
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 12/14/2001 11:29'!
soundPosition: fraction
	"Jump to the position the given fraction through the sound file. The argument is a number between 0.0 and 1.0."

	| desiredSampleIndex |
	(stream isNil or: [stream closed]) ifTrue: [^ self].
	desiredSampleIndex _ ((totalSamples * fraction) truncated max: 0) min: totalSamples.
	codec
		ifNil: [stream position: audioDataStart + (desiredSampleIndex * 2)]
		ifNotNil: [self positionCodecTo: desiredSampleIndex].
	leftoverSamples _ SoundBuffer new.
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 11/20/2001 16:59'!
streamSamplingRate
	"Answer the sampling rate of the MP3 stream."

	^ streamSamplingRate
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 9/26/2000 07:49'!
volume
	"Answer my volume."

	^ volume
! !

!StreamingMonoSound methodsFor: 'accessing' stamp: 'jm 5/30/2001 16:53'!
volume: aNumber
	"Set my volume to the given number between 0.0 and 1.0."

	volume _ aNumber.
	self createMixer.
! !

!StreamingMonoSound methodsFor: 'other' stamp: 'jmv 2/26/2016 16:02'!
extractFrom: startSecs to: endSecs
	"Extract a portion of this sound between the given start and end times. The current implementation only works if the sound is uncompressed."

	| emptySound first last sampleCount byteStream sndBuf |
	codec ifNotNil: [^ self error: 'only works on uncompressed sounds'].
	emptySound _ SampledSound samples: SoundBuffer new samplingRate: streamSamplingRate.
	first _ (startSecs * streamSamplingRate) truncated max: 0.
	last _ ((endSecs * streamSamplingRate) truncated min: totalSamples) - 1.
	first >= last ifTrue: [^ emptySound].
	codec ifNotNil: [self error: 'extracting from compressed sounds is not supported'].
	sampleCount _ last + 1 - first.
	stream position: audioDataStart + (2 * first).
	byteStream _ ReadStream on: (stream next: 2 * sampleCount).
	sndBuf _ SoundBuffer newMonoSampleCount: sampleCount.
	1 to: sampleCount do: [:i | sndBuf at: i put: (byteStream nextSignedInt16BigEndian: true)].
	^ SampledSound samples: sndBuf samplingRate: streamSamplingRate
! !

!StreamingMonoSound methodsFor: 'initialization' stamp: 'jm 11/16/2001 10:23'!
initStream: aStream headerStart: anInteger
	"Initialize for streaming from the given stream. The audio file header starts at the given stream position."

	stream _ aStream.
	volume _ 1.0.
	repeat _ false.
	headerStart _ anInteger.
	self reset.
! !

!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 4/17/2013 12:11'!
millisecondsSinceStart
	"Answer the number of milliseconds of this sound started playing."

	| mSecs |
	(stream isNil or: [stream closed]) ifTrue: [^ 0].
	mSecs _ self currentSampleIndex * 1000 // streamSamplingRate.
	(self isPlaying and: [lastBufferMSecs > 0]) ifTrue: [
		"adjust mSecs by the milliseconds since the last buffer"
		mutex critical: [
			mSecs _ self currentSampleIndex * 1000 // streamSamplingRate.
			mSecs _ mSecs + (Time localMillisecondClock - lastBufferMSecs)]].
	^ mSecs + 350 - (2 * SoundPlayer bufferMSecs)
! !

!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 4/17/2013 12:11'!
playSampleCount: n into: aSoundBuffer startingAt: startIndex
	"Mix the next n samples of this sound into the given buffer starting at the given index"

	self repeat ifTrue: [  "loop if necessary"
		(totalSamples - self currentSampleIndex) < n ifTrue: [self startOver]].

	mutex critical: [
		lastBufferMSecs _ Time localMillisecondClock.
		self loadBuffersForSampleCount: (n * streamSamplingRate) // SoundPlayer samplingRate.
		mixer playSampleCount: n into: aSoundBuffer startingAt: startIndex].
! !

!StreamingMonoSound methodsFor: 'playing' stamp: 'jm 10/21/2001 09:45'!
reset

	super reset.
	self startOver.
	self createMixer.
! !

!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 5/15/2013 08:09'!
samplesRemaining
	"Answer the number of samples remaining to be played."

	| result |
	(stream isNil or: [stream closed]) ifTrue: [^ 0].
	self repeat ifTrue: [^ 1000000].
	result _ (totalSamples - self currentSampleIndex) max: 0.
	result <= 0 ifTrue: [
		stream close.
		mixer _ nil.
		codec _ nil ].
	^ result! !

!StreamingMonoSound class methodsFor: 'instance creation' stamp: 'jmv 10/14/2015 17:26'!
onFileNamed: fileName
	"Answer an instance of me for playing the file with the given name."

	^self onFileNamed: fileName headerStart: 0! !

!StreamingMonoSound class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:34'!
onFileNamed: fileName headerStart: anInteger
	"Answer an instance of me for playing audio data starting at the given position in the file with the given name."

	| answer |
	fileName asFileEntry readStreamDo: [ :f |
		answer _ self new initStream: f headerStart: anInteger ].
	^answer
! !

!ScorePlayer methodsFor: 'copying' stamp: 'jm 1/29/98 18:32'!
copySounds
	"Private!! Support for copying."

	instruments _ instruments copy.
	leftVols _ leftVols copy.
	rightVols _ rightVols copy.
	muted _ muted copy.
	self reset.
! !

!ScorePlayer methodsFor: 'volume' stamp: 'jmv 12/30/2009 11:33'!
adjustVolumeTo: vol overMSecs: mSecs
	| normalizedVolume incr block |
	normalizedVolume _ (vol asFloat min: 1.0) max: 0.0.
	incr _ (self overallVolume - normalizedVolume) / mSecs * 50.0.
	block _ normalizedVolume > 0.0
		ifTrue: [
			[[(normalizedVolume - self overallVolume) abs > 0.01] whileTrue: [self overallVolume: self overallVolume - incr. (Delay forMilliseconds: 50) wait]]]
		ifFalse: [
			[[self overallVolume > 0.0] whileTrue: [self overallVolume: self overallVolume - incr. (Delay forMilliseconds: 50) wait]. self pause]].
	block fork
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jm 10/12/1998 17:13'!
closeMIDIPort
	"Stop using MIDI for output. Music will be played using the built-in sound synthesis."

	self pause.
	midiPort _ nil.
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jmv 4/17/2013 12:09'!
midiPlayLoop

	| mSecsPerStep tStart mSecs |
	mSecsPerStep _ 5.
	[done] whileFalse: [
		tStart _ Time localMillisecondClock.
		self processAllAtTick: ticksSinceStart asInteger.
		(Delay forMilliseconds: mSecsPerStep) wait.
		mSecs _ Time localMillisecondClock - tStart.
		ticksSinceStart _ ticksSinceStart + (mSecs asFloat / (1000.0 * secsPerTick))]! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jm 10/12/1998 15:56'!
openMIDIPort: portNum
	"Open the given MIDI port. Music will be played as MIDI commands to the given MIDI port."

	midiPort _ SimpleMIDIPort openOnPortNumber: portNum.
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jmv 3/1/2010 14:06'!
processMIDIEventsAtTick: scoreTick
	"Process note events through the given score tick using MIDI."

	| j evt |
	1 to: score tracks size do: [:i |
		j _ trackEventIndex at: i.
		[
			evt _ score eventForTrack: i after: j ticks: scoreTick.
			 evt notNil
		] whileTrue: [
			evt isNoteEvent
				ifTrue: [
					(muted at: i) ifFalse: [
						evt startNoteOnMidiPort: midiPort.
						activeMIDINotes add: (Array with: evt with: i)]]
				ifFalse: [evt outputOnMidiPort: midiPort].
			j _ j + 1.
			trackEventIndex at: i put: j]].
	self turnOffActiveMIDINotesAt: scoreTick.
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jmv 10/8/2012 22:14'!
startMIDIPlaying
	"Start up a process to play this score via MIDI."

	midiPort ensureOpen.
	midiPlayerProcess ifNotNil: [midiPlayerProcess terminate].
	midiPlayerProcess _ [self midiPlayLoop] newProcess.
	midiPlayerProcess
		priority: Processor userInterruptPriority;
		name: 'ScorePlayer';
		resume.
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jm 9/10/1998 20:44'!
stopMIDIPlaying
	"Terminate the MIDI player process and turn off any active notes."

	midiPlayerProcess ifNotNil: [midiPlayerProcess terminate].
	midiPlayerProcess _ nil.
	activeMIDINotes do: [:pair | pair first endNoteOnMidiPort: midiPort].
	activeMIDINotes _ activeMIDINotes species new.
! !

!ScorePlayer methodsFor: 'midi output' stamp: 'jm 9/10/1998 17:48'!
turnOffActiveMIDINotesAt: scoreTick
	"Turn off any active MIDI notes that should be turned off at the given score tick."

	| evt someNoteEnded |
	midiPort ifNil: [^ self].
	someNoteEnded _ false. 
	activeMIDINotes do: [:pair |
		evt _ pair first.
		evt endTime <= scoreTick ifTrue: [
			evt endNoteOnMidiPort: midiPort.
			someNoteEnded _ true]].

	someNoteEnded ifTrue: [
		activeMIDINotes _ activeMIDINotes select: [:p | p first endTime > scoreTick]].
! !

!ScorePlayer methodsFor: 'operating' stamp: 'jm 1/30/98 14:03'!
disableReverb: aBoolean

	aBoolean
		ifTrue: [SoundPlayer stopReverb]
		ifFalse: [SoundPlayer startReverb].
! !

!ScorePlayer methodsFor: 'operating' stamp: 'jmv 4/6/2009 17:16'!
pause
	"Pause this sound. It can be resumed from this point, or reset and resumed to start from the beginning."

	super pause.
	activeSounds _ activeSounds species new.
	midiPort ifNotNil: [self stopMIDIPlaying].
! !

!ScorePlayer methodsFor: 'operating' stamp: 'jmv 4/6/2009 17:16'!
resumePlaying
	"Resume playing. Start over if done."

	done ifTrue: [self reset].
	self jumpToTick: ticksSinceStart.  "Play up to here in case we got scrolled to new position."
	midiPort
		ifNil: [super resumePlaying]  "let the sound player drive sound generation" 
		ifNotNil: [self startMIDIPlaying].  "start a process to drive MIDI output"
! !

!ScorePlayer methodsFor: 'operating' stamp: 'jm 9/10/1998 20:56'!
tempoOrRateChanged
	"This method should be called after changing the tempo or rate."

	secsPerTick _ 60.0 / (tempo * rate * score ticksPerQuarterNote).
	ticksClockIncr _ (1.0 / self controlRate) / secsPerTick.
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jm 9/10/1998 19:07'!
doControl

	super doControl.
	1 to: activeSounds size do: [:i | (activeSounds at: i) first doControl].
	ticksSinceStart _ ticksSinceStart + ticksClockIncr.
	self processAllAtTick: ticksSinceStart asInteger.
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 4/6/2009 17:14'!
isDone

	| track |
	activeSounds size > 0 ifTrue: [^ false].
	activeMIDINotes size > 0 ifTrue: [^ false].
	1 to: score tracks size do: [:i |
		track _ score tracks at: i.
		(trackEventIndex at: i) <= track size ifTrue: [^ false]].
	^ true
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 4/6/2009 17:14'!
jumpToTick: startTick

	| |
	self reset.
	self processTempoMapAtTick: startTick.
	self skipNoteEventsThruTick: startTick.
	ticksSinceStart _ startTick.
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jm 7/4/1998 08:21'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a number of sounds concurrently. The level of each sound can be set independently for the left and right channels."

	| myLeftVol myRightVol someSoundIsDone pair snd trk left right |
	myLeftVol _ (leftVol * overallVolume) asInteger.
	myRightVol _ (rightVol * overallVolume) asInteger.
	someSoundIsDone _ false.
	1 to: activeSounds size do: [:i |
		pair _ activeSounds at: i.
		snd _ pair at: 1.
		trk _ pair at: 2.
		left _ (myLeftVol * (leftVols at: trk)) // ScaleFactor.
		right _ (myRightVol * (rightVols at: trk)) // ScaleFactor.
		snd samplesRemaining > 0
			ifTrue: [
				snd mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: left rightVol: right]
			ifFalse: [someSoundIsDone _ true]].

	someSoundIsDone ifTrue: [
		activeSounds _ activeSounds select: [:p | p first samplesRemaining > 0]].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 4/6/2009 17:10'!
processAllAtTick: scoreTick

	self processTempoMapAtTick: scoreTick.
	midiPort
		ifNil: [self processNoteEventsAtTick: scoreTick]
		ifNotNil: [self processMIDIEventsAtTick: scoreTick].
	self isDone ifTrue: [
		repeat
			ifTrue: [self reset]
			ifFalse: [done _ true]].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 3/1/2010 14:06'!
processNoteEventsAtTick: scoreTick
	"Process note events through the given score tick using internal Squeak sound synthesis."

	| instr j evt snd |
	1 to: score tracks size do: [:i |
		instr _ instruments at: i.
		j _ trackEventIndex at: i.
		[
			evt _ score eventForTrack: i after: j ticks: scoreTick.
			evt notNil
		] whileTrue: [
			(evt isNoteEvent and: [(muted at: i) not]) ifTrue: [
				snd _ instr
					soundForMidiKey: evt midiKey
					dur: secsPerTick * evt duration
					loudness: evt velocity asFloat / 127.0.
				activeSounds add: (Array with: snd with: i)].
			j _ j + 1.
			trackEventIndex at: i put: j]].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jm 6/16/1999 21:04'!
processTempoMapAtTick: scoreTick
	"Process tempo changes through the given score tick."

	| map tempoChanged |
	map _ score tempoMap.
	map ifNil: [^ self].
	tempoChanged _ false.
	[(tempoMapIndex <= map size) and:
	 [(map at: tempoMapIndex) time <= scoreTick]] whileTrue: [
		tempoChanged _ true.
		tempoMapIndex _ tempoMapIndex + 1].

	tempoChanged ifTrue: [
		tempo _ (120.0 * (500000.0 / (map at: tempoMapIndex - 1) tempo)) roundTo: 0.01.
		self tempoOrRateChanged].

! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 4/6/2009 17:16'!
reset

	super reset.
	tempo _ 120.0.
	self tempoOrRateChanged.
	done _ false.
	ticksSinceStart _ 0.
	"one index for each sound track, plus one for the ambient track..."
	trackEventIndex _ Array new: score tracks size+1 withAll: 1.
	tempoMapIndex _ 1.
	activeSounds _ OrderedCollection new.
	activeMIDINotes _ OrderedCollection new.
	overallVolume ifNil: [overallVolume _ 0.5].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jm 12/30/97 17:38'!
samplesRemaining

	done
		ifTrue: [^ 0]
		ifFalse: [^ 1000000].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jmv 3/1/2010 13:18'!
skipNoteEventsThruTick: startTick
	"Skip note events through the given score tick using internal Squeak sound synthesis."

	| j evt |
	1 to: score tracks size do: [:i |
		j _ trackEventIndex at: i.
		[evt _ score eventForTrack: i after: j ticks: startTick.
		 evt == nil] whileFalse: [
			evt isNoteEvent
				ifTrue: [
					(((evt time + evt duration) > startTick) and: [(muted at: i) not]) ifTrue: [
						self startNote: evt forStartTick: startTick trackIndex: i]]
				ifFalse: [
					midiPort ifNotNil: [evt outputOnMidiPort: midiPort]].
			j _ j + 1].
		trackEventIndex at: i put: j].
! !

!ScorePlayer methodsFor: 'sound generation' stamp: 'jm 6/16/1999 20:30'!
startNote: noteEvent forStartTick: startTick trackIndex: trackIndex
	"Prepare a note to begin playing at the given tick. Used to start playing at an arbitrary point in the score. Handle both MIDI and built-in synthesis cases."

	| snd |
	midiPort
		ifNil: [
			snd _ (instruments at: trackIndex)
				soundForMidiKey: noteEvent midiKey
				dur: secsPerTick * (noteEvent endTime - startTick)
				loudness: noteEvent velocity asFloat / 127.0.
			activeSounds add: (Array with: snd with: trackIndex)]
		ifNotNil: [
			noteEvent startNoteOnMidiPort: midiPort.
			activeMIDINotes add: (Array with: noteEvent with: trackIndex)].
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 12/16/2001 11:59'!
duration
	"Answer the duration in seconds of my MIDI score when played at the current rate. Take tempo changes into account."

	| totalSecs currentTempo lastTempoChangeTick |
	totalSecs _ 0.0.
	currentTempo _ 120.0.  "quarter notes per minute"
	lastTempoChangeTick _ 0.
	score tempoMap ifNotNil: [
		score tempoMap do: [:tempoEvt |
			"accumulate time up to this tempo change event"
			secsPerTick _ 60.0 / (currentTempo * rate * score ticksPerQuarterNote).
			totalSecs _ totalSecs + (secsPerTick * (tempoEvt time - lastTempoChangeTick)).

			"set the new tempo"
			currentTempo _ (120.0 * (500000.0 / tempoEvt tempo)) roundTo: 0.01.
			lastTempoChangeTick _ tempoEvt time]].

	"add remaining time through end of score"
	secsPerTick _ 60.0 / (currentTempo * rate * score ticksPerQuarterNote).
	totalSecs _ totalSecs + (secsPerTick * (score durationInTicks - lastTempoChangeTick)).
	^ totalSecs
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jmv 3/1/2010 13:17'!
durationInTicks

	durationInTicks ifNil: [^ 1000].
	^ durationInTicks! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 3/3/98 12:14'!
infoForTrack: i
	"Return the info string for the given track."
	"Note: MIDI files follow varying conventions on their use of comment strings. Often, the first string in the track suggests the role of that track in the score, such as 'flute 1' or 'soprano'."

	^ score trackInfo at: i
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/3/98 09:37'!
instrumentForTrack: trackIndex

	^ instruments at: trackIndex
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/1/1999 20:33'!
instrumentForTrack: trackIndex put: aSoundProto

	trackIndex > instruments size ifTrue: [^ self].
	instruments at: trackIndex put: aSoundProto.
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 12/16/2001 20:20'!
isStereo

	^ true
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/28/1998 22:58'!
midiPort

	^ midiPort
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'di 5/30/1999 12:46'!
millisecondsSinceStart
	"Answer the approximate number of milliseconds of real time since the beginning of the score. Since this calculation uses the current tempo, which can change throughout the piece, it is safer to use ticksSinceStart for synchronization."

	^ (secsPerTick * ticksSinceStart * 1000) asInteger
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/29/98 18:33'!
mutedForTrack: trackIndex

	^ muted at: trackIndex
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jmv 3/2/2010 10:06'!
mutedForTrack: trackIndex put: aBoolean

	trackIndex > muted size ifTrue: [^ self].
	muted at: trackIndex put: aBoolean.
	aBoolean ifFalse: [^ self].

	"silence any currently sounding notes for this track"
	activeSounds do: [:pair |
		pair last = trackIndex ifTrue: [activeSounds remove: pair ifAbsent: nil]].
	midiPort ifNotNil: [
		activeMIDINotes do: [:pair |
			pair last = trackIndex ifTrue: [
				pair first endNoteOnMidiPort: midiPort.
				activeMIDINotes remove: pair ifAbsent: nil]]].
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 5/30/1999 17:16'!
mutedState

	^ muted
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 7/4/1998 08:17'!
overallVolume

	^ overallVolume
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 7/4/1998 08:18'!
overallVolume: aNumber
	"Set the overally playback volume to a value between 0.0 (off) and 1.0 (full blast)."

	overallVolume _ (aNumber asFloat min: 1.0) max: 0.0.

! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 7/4/1998 09:32'!
panForTrack: i

	| left right fullVol pan |
	left _ leftVols at: i.
	right _ rightVols at: i.
	left = right ifTrue: [^ 0.5].  "centered"
	fullVol _ left max: right.
	left < fullVol
		ifTrue: [pan _ left asFloat / (2.0 * fullVol)]
		ifFalse: [pan _ 1.0 - (right asFloat / (2.0 * fullVol))].
	^ pan roundTo: 0.001

! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/2/1999 13:33'!
panForTrack: trackIndex put: aNumber
	"Set the left-right pan for this track to a value in the range [0.0..1.0], where 0.0 means full-left."

	| fullVol pan left right |
	trackIndex > leftVols size ifTrue: [^ self].
	fullVol _ (leftVols at: trackIndex) max: (rightVols at: trackIndex).
	pan _ (aNumber asFloat min: 1.0) max: 0.0.
	pan <= 0.5
		ifTrue: [  "attenuate right channel"
			left _ fullVol.
			right _ 2.0 * pan * fullVol]
		ifFalse: [  "attenuate left channel"
			left _ 2.0 * (1.0 - pan) * fullVol.
			right _ fullVol].
	rightVols at: trackIndex put: right asInteger.
	leftVols at: trackIndex put: left asInteger.
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'di 6/20/1999 00:42'!
positionInScore

	^ self ticksSinceStart asFloat / (self durationInTicks max: 1)! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 6/16/1999 22:50'!
positionInScore: pos

	self isPlaying ifTrue: [^ self "ignore rude intrusion"].
	ticksSinceStart _ pos * durationInTicks.
	done _ false.

! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/3/98 09:33'!
rate

	^ rate
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/4/98 10:07'!
rate: aNumber
	"Set the playback rate. For example, a rate of 2.0 will playback at twice normal speed."

	rate _ aNumber asFloat.
	self tempoOrRateChanged.
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 3/4/98 09:30'!
repeat
	"Return true if this player will repeat when it gets to the end of the score, false otherwise."

	^ repeat
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 3/4/98 09:31'!
repeat: aBoolean
	"Turn repeat mode on or off."

	repeat _ aBoolean.
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/3/98 09:33'!
score

	^ score
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'di 10/19/2000 21:12'!
secsPerTick

	^ secsPerTick! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jmv 3/13/2012 12:45'!
settingsString

	| s |
	s _ WriteStream on: (String new: 1000).
	s nextPutAll: 'player'; newLine.
	s tab; nextPutAll: 'rate: ', self rate printString, ';'; newLine.
	s tab; nextPutAll: 'overallVolume: ', self overallVolume printString, ';'; newLine.
	1 to: self trackCount do: [:t |
		s tab; nextPutAll: 'instrumentForTrack: ', t printString,
			' put: (AbstractSound soundNamed: #default);'; newLine.
		s tab; nextPutAll: 'mutedForTrack: ', t printString,
			' put: ', (self mutedForTrack: t) printString, ';'; newLine.
		s tab; nextPutAll: 'volumeForTrack: ', t printString,
			' put: ', (self volumeForTrack: t) printString, ';'; newLine.
		s tab; nextPutAll: 'panForTrack: ', t printString,
			' put: ', (self panForTrack: t) printString, ';'; newLine].
	s tab; nextPutAll: 'repeat: ', self repeat printString, '.'; newLine.
	^ s contents
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/4/98 10:10'!
tempo
	"Return the current tempo in beats (quarter notes) per minute. The tempo at any given moment is defined by the score and cannot be changed by the client. To change the playback speed, the client may change the rate parameter."

	^ tempo
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/22/1998 09:32'!
ticksForMSecs: mSecs

	^ (mSecs asFloat / (1000.0 * secsPerTick)) rounded
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/10/1998 20:48'!
ticksSinceStart
	"Answer the number of score ticks that have elapsed since this piece started playing. The duration of a tick is determined by the MIDI score."

	^ ticksSinceStart
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'di 6/19/1999 10:45'!
ticksSinceStart: newTicks
	"Adjust ticks to folow, eg, piano roll autoscrolling"

	self isPlaying ifFalse: [ticksSinceStart _ newTicks]
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 1/3/98 09:35'!
trackCount

	^ score tracks size
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 7/4/1998 09:16'!
volumeForTrack: i

	| vol |
	vol _ (leftVols at: i) max: (rightVols at: i).
	^ (vol asFloat / ScaleFactor) roundTo: 0.0001
! !

!ScorePlayer methodsFor: 'accessing' stamp: 'jm 9/2/1999 13:34'!
volumeForTrack: trackIndex put: aNumber

	| newVol oldLeft oldRight oldFullVol left right |
	trackIndex > leftVols size ifTrue: [^ self].
	newVol _ ((aNumber asFloat max: 0.0) min: 1.0) * ScaleFactor.
	oldLeft _ leftVols at: trackIndex.
	oldRight _ rightVols at: trackIndex.
	oldFullVol _ oldLeft max: oldRight.
	oldFullVol = 0 ifTrue: [oldFullVol _ 1.0].
	oldLeft < oldFullVol
		ifTrue: [
			left _ newVol * oldLeft / oldFullVol.
			right _ newVol]
		ifFalse: [
			left _ newVol.
			right _ newVol * oldRight / oldFullVol].
	leftVols at: trackIndex put: left asInteger.
	rightVols at: trackIndex put: right asInteger.
! !

!ScorePlayer methodsFor: 'initialization' stamp: 'jmv 12/2/2011 10:49'!
initialize

	super initialize.
	score _ MIDIScore new.
	instruments _ #().
	overallVolume _ 0.5.
	leftVols _ #().
	rightVols _ #().
	muted _ #().
	rate _ 1.0.
	repeat _ false.
	durationInTicks _ 100! !

!ScorePlayer methodsFor: 'initialization' stamp: 'di 6/15/1999 11:18'!
onScore: aMIDIScore

	| trackCount totalVol incr curr pan |
	score _ aMIDIScore.
	trackCount _ score tracks size.
	durationInTicks _ score durationInTicks.
	instruments _ (1 to: trackCount) collect: [:i | FMSound oboe1].
	leftVols _ Array new: trackCount.
	rightVols _ Array new: trackCount.
	muted  _ Array new: trackCount withAll: false.
	rate _ 1.0.
	repeat _ false.
	tempo _ 120.0.

	trackCount = 0 ifTrue: [^ self].
	1 to: trackCount do: [:i |
		leftVols at: i put: ScaleFactor // 4.
		rightVols at: i put: ScaleFactor // 4].

	"distribute inital panning of tracks left-to-right"
	totalVol _ 1.0.
	incr _ totalVol / (((trackCount // 2) + 1) * 2).
	curr _ 0.
	1 to: trackCount do: [:t |
		t even
			ifTrue: [pan _ curr]
			ifFalse: [
				curr _ curr + incr.
				pan _ totalVol - curr].
		self panForTrack: t put: pan].

! !

!ScorePlayer methodsFor: 'initialization' stamp: 'di 6/20/1999 00:46'!
updateDuration

	durationInTicks _ score durationInTicks.
! !

!ScorePlayer class methodsFor: 'instance creation' stamp: 'jm 1/29/98 18:18'!
onScore: aMIDIScore

	^ self new onScore: aMIDIScore
! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'di 2/17/1999 08:15'!
asSound
	"Answer the result of decompressing the receiver."

	| codecClass |
	codecClass _ Smalltalk at: codecName
		ifAbsent: [^ self error: 'The codec for decompressing this sound is not available'].
	^ (codecClass new decompressSound: self) reset
! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'di 2/17/1999 08:49'!
doControl

	cachedSound doControl
! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'di 2/17/1999 08:49'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol

	cachedSound mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'jmv 3/1/2010 09:47'!
reset
	"This message is the cue to start behaving like a real sound in order to be played.
	We do this by caching a decompressed version of this sound.
	See also samplesRemaining."

	cachedSound
		ifNil: [cachedSound _ self asSound].
	cachedSound reset
! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'di 2/17/1999 20:44'!
samples

	^ self asSound samples! !

!CompressedSoundData methodsFor: 'asSound' stamp: 'di 2/17/1999 20:49'!
samplesRemaining
	"This message is the cue that the cached sound may no longer be needed.
	We know it is done playing when samplesRemaining=0."

	| samplesRemaining |
	samplesRemaining _ cachedSound samplesRemaining.
	samplesRemaining <= 0 ifTrue: [cachedSound _ nil].
	^ samplesRemaining! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 08:10'!
channels
	"Answer an array of ByteArrays containing the compressed sound data for each channel."

	^ channels
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:34'!
channels: anArray

	channels _ anArray.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:34'!
codecName
	"Answer the name of the sound codec used to compress this sound. Typically, this is the name of a class that can be used to decode the sound, but it is possible that the codec has not yet been implemented or is not filed into this image."

	^ codecName
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:45'!
codecName: aStringOrSymbol

	codecName _ aStringOrSymbol asSymbol.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:56'!
firstSample
	"Answer the firstSample of the original sound."

	^ firstSample
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:56'!
firstSample: anInteger

	firstSample _ anInteger.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:55'!
gain
	"Answer the gain of the original sound."

	^ gain
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:56'!
gain: aNumber

	gain _ aNumber.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 08:11'!
loopEnd
	"Answer index of the last sample of the loop, or nil if the original sound was not looped."

	^ loopEnd
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:35'!
loopEnd: anInteger

	loopEnd _ anInteger.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 08:11'!
loopLength
	"Answer length of the loop, or nil if the original sound was not looped."

	^ loopLength
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:35'!
loopLength: anInteger

	loopLength _ anInteger.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:39'!
perceivedPitch
	"Answer the perceived pitch of the original sound. By convention, unpitched sounds (like drum hits) are given an arbitrary pitch of 100.0."

	^ perceivedPitch
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:39'!
perceivedPitch: aNumber

	perceivedPitch _ aNumber.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 08:13'!
samplingRate
	"Answer the samplingRate of the original sound."

	^ samplingRate
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:36'!
samplingRate: aNumber

	samplingRate _ aNumber.
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:46'!
soundClassName
	"Answer the class name of the uncompressed sound."

	^ soundClassName
! !

!CompressedSoundData methodsFor: 'accessing' stamp: 'jm 2/2/1999 09:46'!
soundClassName: aStringOrSymbol

	soundClassName _ aStringOrSymbol asSymbol.
! !

!CompressedSoundData methodsFor: 'as yet unclassified' stamp: 'RAA 12/8/2000 09:50'!
compressWith: codecClass

	codecName == codecClass name asSymbol ifTrue: [^self].
	^self asSound compressWith: codecClass! !

!CompressedSoundData methodsFor: 'as yet unclassified' stamp: 'RAA 12/24/2000 08:53'!
compressWith: codecClass atRate: aSamplingRate

	(codecName == codecClass name asSymbol and: [samplingRate = aSamplingRate]) ifTrue: [^self].
	^self asSound compressWith: codecClass atRate: aSamplingRate! !

!Envelope methodsFor: 'accessing' stamp: 'jm 8/17/1998 15:20'!
attackTime
	"Return the time taken by the attack phase."

	^ (points at: loopStartIndex) x
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 18:10'!
centerPitch: aNumber
	"Set the center pitch of a pitch-controlling envelope. This default implementation does nothing."
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 2/4/98 10:21'!
decayEndIndex

	^ points size
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/26/97 08:53'!
decayTime
	"Return the time taken by the decay phase."

	^ points last x - (points at: loopEndIndex) x
! !

!Envelope methodsFor: 'accessing' stamp: 'jmv 3/1/2010 10:00'!
duration
	"Return the time of the final point."

	loopEndMSecs
		ifNil: [^ points last x].
	^ loopEndMSecs + self decayTime
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 8/19/1998 09:07'!
duration: seconds
	"Set the note duration to the given number of seconds."
	"Details: The duration is reduced by 19 mSec to ensure proper cutoffs even when the sound starts playing between doControl epochs."
	"Note: This is a hack. With a little additional work on the envelope logic, it should be possible to reduce or eliminate this fudge factor. In particular, an envelope should use the time remaining, rather than time-since-start to determine when to enter its decay phase. In addition, an envelope must be able to cut off in minimum time (~5-10 msec) if there isn't enough time to do their normal decay. All of this is to allow instruments with leisurely decays to play very short notes if necessary (say, when fast-forwarding through a score)." 

	| attack decay endTime |
	endMSecs _ (seconds * 1000.0) asInteger - 19.
	attack _ self attackTime.
	decay _ self decayTime.
	endMSecs > (attack + decay)
		ifTrue: [endTime _ endMSecs - decay]
		ifFalse: [
			endMSecs >= attack
				ifTrue: [endTime _ attack]
				ifFalse: [endTime _ endMSecs]].

	self sustainEnd: (endTime max: 0).
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 2/4/98 17:24'!
loopEndIndex

	^ loopEndIndex
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 2/4/98 17:24'!
loopStartIndex

	^ loopStartIndex
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 2/4/98 17:24'!
points

	^ points
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/24/97 14:36'!
scale: aNumber

	scale _ aNumber asFloat.
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/26/97 09:25'!
target

	^ target
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/26/97 09:25'!
target: anObject

	target _ anObject.
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/24/97 14:34'!
updateSelector

	^ updateSelector
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 11/26/97 08:52'!
updateSelector: aSymbol

	updateSelector _ aSymbol.
! !

!Envelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 18:13'!
volume: aNumber
	"Set the maximum volume of a volume-controlling envelope. This default implementation does nothing."
! !

!Envelope methodsFor: 'private' stamp: 'jm 11/26/97 09:03'!
checkParameters
	"Verify that the point array, loopStartIndex, and loopStopIndex obey the rules."

	| lastT t |
	points size > 1
		ifFalse: [^ self error: 'the point list must contain at least two points'].
	points first x = 0
		ifFalse: [^ self error: 'the time of the first point must be zero'].
	lastT _ points first x.
	2 to: points size do: [:i |
		t _ (points at: i) x.
		t >= lastT
			ifFalse: [^ self error: 'the points must be in ascending time order']].

	(loopStartIndex isInteger and:
	 [(loopStartIndex > 0) and: [loopStartIndex <= points size]])
		ifFalse: [^ self error: 'loopStartIndex is not a valid point index'].
	(loopEndIndex isInteger and:
	 [(loopEndIndex > 0) and: [loopEndIndex <= points size]])
		ifFalse: [^ self error: 'loopEndIndex is not a valid point index'].
	 loopStartIndex <= loopEndIndex
		ifFalse: [^ self error: 'loopEndIndex must not precede loopStartIndex'].
! !

!Envelope methodsFor: 'private' stamp: 'jm 2/4/98 20:20'!
computeIncrementAt: mSecs between: p1 and: p2 scale: combinedScale
	"Compute the current and increment values for the given time between the given inflection points."
	"Assume: p1 x <= mSecs <= p2 x"

	| valueRange timeRange |
	valueRange _ (p2 y - p1 y) asFloat.
	timeRange _ (p2 x - p1 x) asFloat.
	currValue _ (p1 y + (((mSecs - p1 x) asFloat / timeRange) * valueRange)) * combinedScale.
	valueIncr _ (((p2 y * combinedScale) - currValue) / (p2 x - mSecs)) * 10.0.
	^ currValue
! !

!Envelope methodsFor: 'private' stamp: 'jmv 3/1/2010 14:05'!
incrementalComputeValueAtMSecs: mSecs
	"Compute the current value, per-step increment, and the time of the next inflection point."
	"Note: This method is part of faster, but less general, way of computing envelope values. It depends on a known, fixed control updating rate."

	| t i |
	(loopEndMSecs notNil and: [mSecs >= loopEndMSecs]) ifTrue: [  "decay phase"
		t _ (points at: loopEndIndex) x + (mSecs - loopEndMSecs).
		i _ self indexOfPointAfterMSecs: t startingAt: loopEndIndex.
		i ifNil: [  "past end"
			currValue _ points last y * scale * decayScale.
			valueIncr _ 0.0.
			nextRecomputeTime _ mSecs + 1000000.
			^ currValue].
		nextRecomputeTime _ mSecs + ((points at: i) x - t).
		^ self computeIncrementAt: t
			between: (points at: i - 1)
			and: (points at: i)
			scale: scale * decayScale].

	mSecs < loopStartMSecs
		ifTrue: [  "attack phase"
			t _ mSecs.
			i _ self indexOfPointAfterMSecs: t startingAt: 1.
			nextRecomputeTime _ mSecs + ((points at: i) x - t)]
		ifFalse: [  "sustain (looping) phase"
			noChangesDuringLoop ifTrue: [
				currValue _ (points at: loopEndIndex) y * scale.
				valueIncr _ 0.0.
					loopEndMSecs
						ifNil: [nextRecomputeTime _ mSecs + 10]	 "unknown end time"
						ifNotNil: [nextRecomputeTime _ loopEndMSecs].
				^ currValue].
			t _ loopStartMSecs + ((mSecs - loopStartMSecs) \\ loopMSecs).
			i _ self indexOfPointAfterMSecs: t startingAt: loopStartIndex.
			nextRecomputeTime _ (mSecs + ((points at: i) x - t)) min: loopEndMSecs].

	^ self computeIncrementAt: t
		between: (points at: i - 1)
		and: (points at: i)
		scale: scale.
! !

!Envelope methodsFor: 'private' stamp: 'jm 12/16/97 16:51'!
indexOfPointAfterMSecs: mSecs startingAt: startIndex
	"Return the index of the first point whose time is greater that mSecs, starting with the given index. Return nil if mSecs is after the last point's time."

	startIndex to: points size do:
		[:i | (points at: i) x > mSecs ifTrue: [^ i]].
	^ nil
! !

!Envelope methodsFor: 'private' stamp: 'jm 2/4/98 21:05'!
interpolate: mSecs between: p1 and: p2
	"Return the scaled, interpolated value for the given time between the given time points."
	"Assume: p1 x <= mSecs <= p2 x"

	| valueRange timeRange |
	valueRange _ (p2 y - p1 y) asFloat.
	valueRange = 0.0 ifTrue: [^ p1 y * scale].
	timeRange _ (p2 x - p1 x) asFloat.
	^ (p1 y + (((mSecs - p1 x) asFloat / timeRange) * valueRange)) * scale.
! !

!Envelope methodsFor: 'private' stamp: 'jm 2/4/98 17:52'!
setPoints: pointList loopStart: startIndex loopEnd: endIndex

	| lastVal |
	points _ pointList asArray collect: [:p | p x asInteger @ p y asFloat].
	loopStartIndex _ startIndex.
	loopEndIndex _ endIndex.
	self checkParameters.
	loopStartMSecs _ (points at: loopStartIndex) x.
	loopMSecs _ (points at: loopEndIndex) x - (points at: loopStartIndex) x.
	loopEndMSecs _ nil.  "unknown end time; sustain until end time is known"
	scale ifNil: [scale _ 1.0].
	decayScale ifNil: [decayScale _ 1.0].

	"note if there are no changes during the loop phase"
	noChangesDuringLoop _ true.
	lastVal _ (points at: loopStartIndex) y.
	loopStartIndex to: loopEndIndex do: [:i | 
		(points at: i) y ~= lastVal ifTrue: [
			noChangesDuringLoop _ false.
			^ self]].
! !

!Envelope methodsFor: 'applying' stamp: 'jmv 3/1/2010 14:04'!
computeValueAtMSecs: mSecs
	"Return the value of this envelope at the given number of milliseconds from its onset. Return zero for times outside the time range of this envelope."
	"Note: Unlike the private method incrementalComputeValueAtMSecs:, this method does is not increment. Thus it is slower, but it doesn't depend on being called sequentially at fixed time intervals."

	| t i |
	mSecs < 0 ifTrue: [^ 0.0].

	(loopEndMSecs notNil and: [mSecs >= loopEndMSecs]) ifTrue: [  "decay phase"
		t _ (points at: loopEndIndex) x + (mSecs - loopEndMSecs).
		i _ self indexOfPointAfterMSecs: t startingAt: loopEndIndex.
		i ifNil: [^ 0.0].  "past end"
		^ (self interpolate: t between: (points at: i - 1) and: (points at: i)) * decayScale].

	mSecs < loopStartMSecs ifTrue: [  "attack phase"
		i _ self indexOfPointAfterMSecs: mSecs startingAt: 1.
		i = 1 ifTrue: [^ (points at: 1) y * scale].
		^ self interpolate: mSecs between: (points at: i - 1) and: (points at: i)].

	"sustain phase"
	loopMSecs = 0 ifTrue: [^ (points at: loopEndIndex) y * scale].  "looping on a single point"
	t _ loopStartMSecs + ((mSecs - loopStartMSecs) \\ loopMSecs).
	i _ self indexOfPointAfterMSecs: t startingAt: loopStartIndex.

	^ self interpolate: t between: (points at: i - 1) and: (points at: i)
! !

!Envelope methodsFor: 'applying' stamp: 'jm 2/4/98 21:15'!
reset
	"Reset the state for this envelope."

	lastValue _ -100000.0.  "impossible value"
	nextRecomputeTime _ 0.
	self updateTargetAt: 0.
! !

!Envelope methodsFor: 'applying' stamp: 'jm 2/4/98 17:39'!
showOnDisplay
	"Envelope example showOnDisplay"

	| xOrigin yOrigin minVal maxVal yScale step x v y |
	xOrigin _ 30.
	yOrigin _ 130.
	minVal _ 1e100.
	maxVal _ -1e100.
	points do: [:p |
		p y < minVal ifTrue: [minVal _ p y].
		p y > maxVal ifTrue: [maxVal _ p y]].

	yScale _ 100.0 / ((maxVal - minVal) * scale).
	step _ (self duration // 150) max: 1.

	Display fillBlack: ((xOrigin + ((points at: loopStartIndex) x // step))@(yOrigin - 100) extent: 1@100).
	Display fillBlack: ((xOrigin + ((points at: loopEndIndex) x // step))@(yOrigin - 100) extent: 1@100).
	Display fillBlack: (xOrigin@(yOrigin - 100) extent: 1@100).
	x _ xOrigin.
	step negated to: self duration + step by: step do: [:mSecs |
		v _ self computeValueAtMSecs: mSecs.
		y _ yOrigin - ((v - minVal) * yScale) asInteger.
		Display fillBlack: ((x - 1)@(y - 1) extent: 2@2).
		Display fillBlack: (x@yOrigin extent: 1@1).
		x _ x + 1].
! !

!Envelope methodsFor: 'applying' stamp: 'jm 2/4/98 18:27'!
sustainEnd: mSecs
	"Set the ending time of the sustain phase of this envelope; the decay phase will start this point. Typically derived from a note's duration."
	"Details: to avoid a sharp transient, the decay phase is scaled so that the beginning of the decay matches the envelope's instantaneous value when the decay phase starts."

	| vIfSustaining firstVOfDecay |
	loopEndMSecs _ nil. "pretend to be sustaining"
	decayScale _ 1.0.
	nextRecomputeTime _ 0.
	vIfSustaining _ self computeValueAtMSecs: mSecs.  "get value at end of sustain phase"
	loopEndMSecs _ mSecs.
	firstVOfDecay _ (points at: loopEndIndex) y * scale.
	firstVOfDecay = 0.0
		ifTrue: [decayScale _ 1.0]
		ifFalse: [decayScale _ vIfSustaining / firstVOfDecay].
! !

!Envelope methodsFor: 'applying' stamp: 'jm 2/4/98 19:46'!
updateTargetAt: mSecs
	"Send my updateSelector to the given target object with the value of this envelope at the given number of milliseconds from its onset. Answer true if the value changed."

	| newValue |
	newValue _ self valueAtMSecs: mSecs.
	newValue = lastValue ifTrue: [^ false].
	target
		perform: updateSelector
		with: newValue.
	lastValue _ newValue.
	^ true
! !

!Envelope methodsFor: 'applying' stamp: 'jm 2/4/98 20:24'!
valueAtMSecs: mSecs
	"Return the value of this envelope at the given number of milliseconds from its onset. Return zero for times outside the time range of this envelope."

	mSecs < 0 ifTrue: [^ 0.0].
	mSecs < nextRecomputeTime
		ifTrue: [currValue _ currValue + valueIncr]
		ifFalse: [currValue _ self incrementalComputeValueAtMSecs: mSecs].
	^ currValue
! !

!Envelope class methodsFor: 'instance creation' stamp: 'jm 11/26/97 09:14'!
example
	"Envelope example showOnDisplay"

	| p |
	p _ Array with: 0@0 with: 100@1.0 with: 250@0.7 with: 400@1.0 with: 500@0.
	^ (self points: p loopStart: 2 loopEnd: 4) sustainEnd: 1200.
! !

!Envelope class methodsFor: 'instance creation' stamp: 'jm 2/4/98 06:52'!
exponentialDecay: multiplier
	"(Envelope exponentialDecay: 0.95) showOnDisplay"

	| mSecsPerStep pList t v last |
	mSecsPerStep _ 10.
	((multiplier > 0.0) and: [multiplier < 1.0])
		ifFalse: [self error: 'multiplier must be greater than 0.0 and less than 1.0'].
	pList _ OrderedCollection new.
	pList add: 0@0.0.
	last _ 0.0.
	v _ 1.0.
	t _ 10.
	[v > 0.01] whileTrue: [
		(v - last) abs > 0.02 ifTrue: [
			"only record substatial changes"
			pList add: t@v.
			last _ v].
		t _ t + mSecsPerStep.
		v _ v * multiplier].
	pList add: (t + mSecsPerStep)@0.0.

	^ self points: pList asArray
		loopStart: pList size 
		loopEnd: pList size
! !

!Envelope class methodsFor: 'instance creation' stamp: 'jm 11/26/97 08:49'!
points: pList loopStart: loopStart loopEnd: loopEnd

	^ self new setPoints: pList asArray
		loopStart: loopStart
		loopEnd: loopEnd
! !

!PitchEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 1/31/98 14:46'!
centerPitch

	^ centerPitch
! !

!PitchEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 1/31/98 14:49'!
centerPitch: aNumber

	centerPitch _ aNumber.
! !

!PitchEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 2/4/98 07:30'!
updateSelector
	"Needed by the envelope editor."

	^ #pitch:
! !

!PitchEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 2/4/98 21:11'!
updateTargetAt: mSecs
	"Update the pitch for my target. Answer true if the value changed."
	"Details: Assume envelope range is 0.0..2.0, with 1 being the center pitch. Subtracting one yields the range -1.0..1.0. Raising two to this power yields pitches between half and double the center pitch; i.e. from an octave below to an octave about the center pitch."

	| newValue |
	newValue _ self valueAtMSecs: mSecs.
	newValue ~= lastValue ifTrue: [
		target pitch: (2.0 raisedTo: newValue - (scale / 2.0)) * centerPitch.
		lastValue _ newValue.
		^ true].

	^ false
! !

!RandomEnvelope methodsFor: 'envelope compatibility' stamp: 'jm 8/13/1998 17:29'!
duration

	^ 1.0
! !

!RandomEnvelope methodsFor: 'envelope compatibility' stamp: 'jm 8/13/1998 17:25'!
duration: seconds
	"Do nothing."
! !

!RandomEnvelope methodsFor: 'envelope compatibility' stamp: 'jm 8/13/1998 17:30'!
sustainEnd: seconds
	"Do nothing."
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 18:18'!
centerPitch: aNumber
	"If this envelope controls pitch, set its scale to the given number. Otherwise, do nothing."

	updateSelector = #pitch: ifTrue: [self scale: aNumber].
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:06'!
delta

	^ delta
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:06'!
delta: aNumber

	delta _ aNumber.
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:06'!
highLimit

	^ highLimit
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:06'!
highLimit: aNumber

	highLimit _ aNumber.
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:05'!
lowLimit

	^ lowLimit
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 17:06'!
lowLimit: aNumber

	lowLimit _ aNumber.
! !

!RandomEnvelope methodsFor: 'accessing' stamp: 'jm 8/13/1998 18:17'!
volume: aNumber
	"If this envelope controls volume, set its scale to the given number. Otherwise, do nothing."

	updateSelector = #volume: ifTrue: [self scale: aNumber].
! !

!RandomEnvelope methodsFor: 'initialization' stamp: 'jm 1/14/1999 13:15'!
initialize

	rand _ Random new.
	lowLimit _ 0.994.
	highLimit _ 1.006.
	delta _ 0.0002.
	currValue _ 1.0.
	scale _ 1.0.
! !

!RandomEnvelope methodsFor: 'envelopeEditor compatibility' stamp: 'jmv 3/2/2010 16:25'!
points

	| env |
	points ifNil: [
		env _ self target envelopes first.
		points _ OrderedCollection new.
		points
			add: 0@(self delta * 5 + 0.5);
			add: (env points at: env loopStartIndex)x@(self highLimit -1 * 5 + 0.5);
			add: (env points at: env loopEndIndex)x@(self highLimit -1 * 5 + 0.5);
			add: (env points last)x@(self lowLimit -1 * 5 + 0.5).
		loopStartIndex _ 2.
		loopEndIndex _ 3.
	].
	^points! !

!RandomEnvelope methodsFor: 'envelopeEditor compatibility' stamp: 'JMV 1/9/2001 13:08'!
setPoints: pointList loopStart: startIndex loopEnd: endIndex

	self delta: pointList first y - 0.5 / 5.
	self highLimit: (pointList at: startIndex) y - 0.5 / 5 + 1.
	self lowLimit: pointList last y - 0.5 / 5 + 1.
	^super setPoints: pointList loopStart: startIndex loopEnd: endIndex! !

!RandomEnvelope methodsFor: 'applying' stamp: 'jm 8/13/1998 18:25'!
updateTargetAt: mSecs
	"Send my updateSelector to the given target object with the value of this envelope at the given number of milliseconds from its onset. Answer true if the value changed."

	| r |
	r _ rand next.
	r > 0.5
		ifTrue: [
			currValue _ currValue + delta.
			currValue > highLimit ifTrue: [currValue _ highLimit]]
		ifFalse: [
			currValue _ currValue - delta.
			currValue < lowLimit ifTrue: [currValue _ lowLimit]].
	currValue = lastValue ifTrue: [^ false].
	((target == nil) or: [updateSelector == nil]) ifTrue: [^ false].
	target
		perform: updateSelector
		with: scale * currValue.
	lastValue _ currValue.
	^ true
! !

!RandomEnvelope class methodsFor: 'instance creation' stamp: 'jm 8/13/1998 18:21'!
for: aSelector
	"Answer a random envelope for the given selector."

	^ self new updateSelector: aSelector
! !

!VolumeEnvelope methodsFor: 'as yet unclassified' stamp: 'jmv 3/1/2010 14:17'!
computeSlopeAtMSecs: mSecs
	"Private!! Find the next inflection point of this envelope and compute its target volume and the number of milliseconds until the inflection point is reached."

	| t i |
	((loopEndMSecs notNil) and: [mSecs >= loopEndMSecs]) ifTrue: [  "decay phase"
		t _ (points at: loopEndIndex) x + (mSecs - loopEndMSecs).
		i _ self indexOfPointAfterMSecs: t startingAt: loopEndIndex.
		i ifNil: [  "past end"
			targetVol _ points last y * decayScale.
			mSecsForChange _ 0.
			nextRecomputeTime _ mSecs + 1000000.
			^ self].
		targetVol _ (points at: i) y * decayScale.
		mSecsForChange _ (((points at: i) x - t) min: (endMSecs - mSecs)) max: 4.
		nextRecomputeTime _ mSecs + mSecsForChange.
		^ self].

	mSecs < loopStartMSecs ifTrue: [  "attack phase"
		i _ self indexOfPointAfterMSecs: mSecs startingAt: 1.
		targetVol _ (points at: i) y.
		mSecsForChange _ ((points at: i) x - mSecs) max: 4.
		nextRecomputeTime _ mSecs + mSecsForChange.
		((loopEndMSecs notNil) and: [nextRecomputeTime > loopEndMSecs])
			ifTrue: [nextRecomputeTime _ loopEndMSecs].
		^ self].

	"sustain and loop phase"
	noChangesDuringLoop ifTrue: [
		targetVol _ (points at: loopEndIndex) y.
		mSecsForChange _ 10.
		loopEndMSecs
			ifNil: [nextRecomputeTime _ mSecs + 10]  "unknown end time"
			ifNotNil: [nextRecomputeTime _ loopEndMSecs].
		^ self].

	loopMSecs = 0 ifTrue: [^ (points at: loopEndIndex) y].  "looping on a single point"
	t _ loopStartMSecs + ((mSecs - loopStartMSecs) \\ loopMSecs).
	i _ self indexOfPointAfterMSecs: t startingAt: loopStartIndex.
	targetVol _ (points at: i) y.
	mSecsForChange _ ((points at: i) x - t) max: 4.
	nextRecomputeTime _ (mSecs + mSecsForChange) min: loopEndMSecs.
! !

!VolumeEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 2/4/98 18:51'!
reset
	"Reset the state for this envelope."

	super reset.
	target initialVolume: points first y * scale.
	nextRecomputeTime _ 0.
! !

!VolumeEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 2/4/98 07:27'!
updateSelector
	"Needed by the envelope editor."

	^ #volume:
! !

!VolumeEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 9/10/1998 07:04'!
updateTargetAt: mSecs
	"Update the volume envelope slope and limit for my target. Answer false."

	mSecs < nextRecomputeTime ifTrue: [^ false].
	self computeSlopeAtMSecs: mSecs.
	mSecsForChange < 5 ifTrue: [mSecsForChange _ 5].  "don't change instantly to avoid clicks"
	target adjustVolumeTo: targetVol * scale overMSecs: mSecsForChange.
	^ false
! !

!VolumeEnvelope methodsFor: 'as yet unclassified' stamp: 'jm 8/17/1998 08:00'!
volume: aNumber
	"Set the maximum volume of a volume-controlling envelope."

	scale _ aNumber asFloat.
! !

!SampledInstrument methodsFor: 'other' stamp: 'jmv 8/23/2010 10:10'!
allNotes
	"Answer a collection containing of all the unique sampled sounds used by this instrument."

	| r |
	r _ IdentitySet new.
	r addAll: sustainedLoud.
	sustainedSoft ~~ sustainedLoud ifTrue: [r addAll: sustainedSoft].
	staccatoLoud ~~ sustainedLoud ifTrue: [r addAll: staccatoLoud].
	staccatoSoft ~~ staccatoLoud ifTrue: [r addAll: staccatoSoft].
	^ r asArray sort: [:n1 :n2 | n1 pitch < n2 pitch]! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/2/1998 12:55'!
chooseSamplesForPitch: pitchInHz from: sortedNotes
	"From the given collection of LoopedSampledSounds, choose the best one to be pitch-shifted to produce the given pitch."
	"Assume: the given collection is sorted in ascending pitch order."

	| i lower higher |
	i _ 1.
	[(i < sortedNotes size) and: [(sortedNotes at: i) pitch < pitchInHz]]
		whileTrue: [i _ i + 1].
	i = 1 ifTrue: [^ sortedNotes at: 1].
	lower _ sortedNotes at: i - 1.
	higher _ sortedNotes at: i.
	"note: give slight preference for down-shifting a higher-pitched sample set"
	(pitchInHz / lower pitch) < ((0.95 * higher pitch) / pitchInHz)
		ifTrue: [^ lower]
		ifFalse: [^ higher].
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 9/8/1998 16:24'!
memorySpace
	"Answer the number of bytes required to store the samples for this instrument."

	| total |
	total _ 0.
	self allNotes do: [:n |
		total _ total + (n leftSamples monoSampleCount * 2).
		n isStereo ifTrue: [total _ total + (n leftSamples monoSampleCount * 2)]].
	^ total
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/3/1998 16:42'!
midiKeyMapFor: sortedNotes
	"Return a 128 element array that maps each MIDI key number to the sampled note from the given set with the closests pitch. A precise match isn't necessary because the selected note will be pitch shifted to play at the correct pitch."

	^ (0 to: 127) collect: [:k |
		self
			chooseSamplesForPitch: (AbstractSound pitchForMIDIKey: k)
			from: sortedNotes].
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/18/1998 10:57'!
playChromaticRunFrom: startPitch to: endPitch

	(AbstractSound chromaticRunFrom: startPitch to: endPitch on: self) play.
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/11/1998 15:52'!
pruneNoteList: aNoteList notesPerOctave: notesPerOctave
	"Return a pruned version of the given note list with only the given number of notes per octave. Assume the given notelist is in sorted order."

	| r interval lastPitch |
	r _ OrderedCollection new: aNoteList size.
	interval _ (2.0 raisedTo: (1.0 / notesPerOctave)) * 0.995.
	lastPitch _ 0.0.
	aNoteList do: [:n |
		n pitch > (lastPitch * interval) ifTrue: [
			r addLast: n.
			lastPitch _ n pitch]].
	^ r
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/6/1998 00:39'!
pruneToNotesPerOctave: notesPerOctave
	"Prune all my keymaps to the given number of notes per octave."

	sustainedLoud _ self midiKeyMapFor:
		(self pruneNoteList: sustainedLoud notesPerOctave: notesPerOctave).
	sustainedSoft _ self midiKeyMapFor:
		(self pruneNoteList: sustainedSoft notesPerOctave: notesPerOctave).
	staccatoLoud _ self midiKeyMapFor:
		(self pruneNoteList: staccatoLoud notesPerOctave: notesPerOctave).
	staccatoSoft _ self midiKeyMapFor:
		(self pruneNoteList: staccatoSoft notesPerOctave: notesPerOctave).
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/11/1998 14:44'!
pruneToSingleNote: aNote
	"Fill all my keymaps with the given note."

	| oneNoteMap |
	oneNoteMap _ Array new: 128 withAll: aNote.
	sustainedLoud _ oneNoteMap.
	sustainedSoft _ oneNoteMap.
	staccatoLoud _ oneNoteMap.
	staccatoSoft _ oneNoteMap.
! !

!SampledInstrument methodsFor: 'other' stamp: 'jmv 10/14/2015 17:46'!
readSampleSetFrom: dirName
	"Answer a collection of sounds read from AIFF files in the given directory and sorted in ascending pitch order."

	| all dir fullName snd |
	all _ SortedCollection sortBlock: [:s1 :s2 | s1 pitch < s2 pitch].
	dir _ dirName asDirectoryEntry.
	dir fileNames do: [ :n |
		fullName _ (dir / n) pathName.
		Utilities
			informUser: 'Reading AIFF file ', n
			during: [
				snd _ LoopedSampledSound new
					fromAIFFFileNamed: fullName
					mergeIfStereo: true].
		all add: snd].
	^ all asArray! !

!SampledInstrument methodsFor: 'other' stamp: 'jmv 10/14/2015 17:47'!
readSampleSetInfoFrom: dirName
	"MessageTally spyOn: [SampledInstrument new readSampleSetFrom: 'Tosh:Desktop Folder:AAA Squeak2.0 Beta:Organ Samples:Flute8'] timeToRun"

	| all dir fullName info |
	all _ OrderedCollection new.
	dir _ dirName asDirectoryEntry.
	dir fileNames do: [ :n |
		fullName _ (dir / n) pathName.
		info _ AIFFFileReader new readFromFile: fullName
			mergeIfStereo: false
			skipDataChunk: true.
		all add: n -> info].
	^ all
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/4/1998 23:13'!
testAtPitch: aPitch
	"SampledInstrument testAtPitch: 'c4'"

	| pattern |
	pattern _ (#(
		(c4 0.64 100) 
		(c4 0.64 200) 
		(c4 0.64 400) 
		(c4 0.64 600) 
		(c4 0.64 800) 
		(c4 1.28 1000) 
		(c4 1.28 400) 
		(c4 0.32 500) 
		(c4 0.32 500) 
		(c4 0.32 500) 
		(c4 0.32 500) 
		(c4 0.16 500) 
		(c4 0.16 500) 
		(c4 0.16 500) 
		(c4 0.16 500) 
		(c4 0.16 500) 
		(c4 0.08 500) 
		(c4 0.08 500) 
		(c4 0.16 500) 
		(c4 0.08 500) 
		(c4 0.08 500) 
		(c4 0.64 500))
			collect: [:triple | triple copy at: 1 put: aPitch; yourself]).
	(AbstractSound noteSequenceOn: self from: pattern) play.
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/5/1998 15:16'!
trimAttackOf: sampleBuffer threshold: threshold
	"Trim 'silence' off the initial attacks of the given sound buffer."

	(sustainedSoft, sustainedLoud, staccatoSoft, staccatoLoud) do: [:snd |
		snd leftSamples: (self trimAttackOf: snd leftSamples threshold: threshold).
		snd isStereo ifTrue: [
			snd rightSamples:
				(self trimAttackOf: snd rightSamples threshold: threshold)]].
! !

!SampledInstrument methodsFor: 'other' stamp: 'jm 8/5/1998 11:07'!
trimAttacks: threshold
	"Trim 'silence' off the initial attacks all my samples."

	(sustainedSoft, sustainedLoud, staccatoSoft, staccatoLoud) do: [:snd |
		snd leftSamples: (self trimAttackOf: snd leftSamples threshold: threshold).
		snd isStereo ifTrue: [
			snd rightSamples:
				(self trimAttackOf: snd rightSamples threshold: threshold)]].
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/3/1998 17:30'!
allSampleSets: sortedNotes

	| keyMap |
	keyMap _ self midiKeyMapFor: sortedNotes.
	sustainedSoft _ keyMap.
	sustainedLoud _ keyMap.
	staccatoSoft _ keyMap.
	staccatoLoud _ keyMap.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/3/1998 19:04'!
initialize

	sustainedThreshold _ 0.15.
	loudThreshold _ 0.5.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/4/1998 23:21'!
loudThreshold

	^ loudThreshold
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/4/1998 23:22'!
loudThreshold: aNumber

	loudThreshold _ aNumber asFloat.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/2/1998 10:02'!
staccatoLoudAndSoftSampleSet: sortedNotes

	staccatoLoud _ self midiKeyMapFor: sortedNotes.
	staccatoSoft _ staccatoLoud.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/2/1998 09:54'!
staccatoLoudSampleSet: sortedNotes

	staccatoLoud _ self midiKeyMapFor: sortedNotes.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/2/1998 09:54'!
staccatoSoftSampleSet: sortedNotes

	staccatoSoft _ self midiKeyMapFor: sortedNotes.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/2/1998 09:54'!
sustainedLoudSampleSet: sortedNotes

	sustainedLoud _ self midiKeyMapFor: sortedNotes.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/2/1998 09:54'!
sustainedSoftSampleSet: sortedNotes

	sustainedSoft _ self midiKeyMapFor: sortedNotes.
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/4/1998 23:21'!
sustainedThreshold

	^ sustainedThreshold
! !

!SampledInstrument methodsFor: 'accessing' stamp: 'jm 8/4/1998 23:22'!
sustainedThreshold: aNumber

	sustainedThreshold _ aNumber asFloat.
! !

!SampledInstrument methodsFor: 'playing' stamp: 'jm 8/3/1998 18:53'!
soundForMidiKey: midiKey dur: d loudness: l
	"Answer an initialized sound object that generates a note for the given MIDI key (in the range 0..127), duration (in seconds), and loudness (in the range 0.0 to 1.0)."

	| keymap note |
	l >= loudThreshold
		ifTrue: [
			d >= sustainedThreshold
				ifTrue: [keymap _ sustainedLoud]
				ifFalse: [keymap _ staccatoLoud]]
		ifFalse: [
			d >= sustainedThreshold
				ifTrue: [keymap _ sustainedSoft]
				ifFalse: [keymap _ staccatoSoft]].
	keymap ifNil: [keymap _ sustainedLoud].
	note _ (keymap at: midiKey) copy.
	^ note
		setPitch: (AbstractSound pitchForMIDIKey: midiKey)
		dur: d
		loudness: (l * note gain)
! !

!SampledInstrument methodsFor: 'playing' stamp: 'jm 8/3/1998 16:53'!
soundForPitch: pitchNameOrNumber dur: d loudness: l
	"Answer an initialized sound object that generates a note of the given pitch, duration, and loudness. Pitch may be a numeric pitch or a string pitch name such as 'c4'. Duration is in seconds and loudness is in the range 0.0 to 1.0."
	"Note:  Generally, SampledInstruments are expected to be played via MIDI key numbers rather than by pitches, since finding the MIDI key for a given pitch is expensive."

	^ self soundForMidiKey: (AbstractSound midiKeyForPitch: pitchNameOrNumber)
		dur: d
		loudness: l
! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jm 6/7/1999 11:26'!
buildSmallOrchestra
	"Example of how to build a skeleton orchestra that uses less memory (about 14 MBytes)."
	"SampledInstrument buildSmallOrchestra"

	| dir |
	AbstractSound unloadSampledTimbres.
	dir _ 'Tosh:Not Backed Up:Sample Library:Orchestra'.
	#(clarinet oboe bassoon trombone tympani) do: [:instName |
		SampledInstrument
			readSimpleInstrument: instName
			fromDirectory: dir.
		(AbstractSound soundNamed: instName, '-f') pruneToNotesPerOctave: 1].
	#(flute bass) do: [:instName |
		SampledInstrument
			readSimpleInstrument: instName
			fromDirectory: dir.
		(AbstractSound soundNamed: instName, '-f') pruneToNotesPerOctave: 2].

	(AbstractSound soundNamed: 'bass-f') allNotes do: [:n |
		n firstSample: (n findStartPointForThreshold: 2500)].

	(AbstractSound soundNamed: 'bassoon-f') allNotes do: [:n |
		n beUnlooped.
		n firstSample: (n findStartPointForThreshold: 0)].

	(AbstractSound soundNamed: 'trombone-f') allNotes do: [:n |
		n firstSample: (n findStartPointForThreshold: 1800)].

	AbstractSound soundNamed: 'trumpet-f' put: (AbstractSound soundNamed: 'trombone-f').
	AbstractSound soundNamed: 'horn-f' put: (AbstractSound soundNamed: 'trombone-f').
	AbstractSound soundNamed: 'violin-f' put: (AbstractSound soundNamed: 'bass-f').
	AbstractSound soundNamed: 'viola-f' put: (AbstractSound soundNamed: 'bass-f').
	AbstractSound soundNamed: 'cello-f' put: (AbstractSound soundNamed: 'bass-f').

	(AbstractSound soundNamed: 'bassoon-f') allNotes do: [:n | n beUnlooped].

! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:44'!
readLoudAndStaccatoInstrument: instName fromDirectory: orchestraDir
	"SampledInstrument
		readLoudAndStaccatoInstrument: 'oboe'
		fromDirectory: 'Tosh:Sample Library:Orchestra'"

	| sampleSetDir memBefore memAfter loud short snd |
	sampleSetDir _ orchestraDir, ':', instName.
	memBefore _ Smalltalk garbageCollect.
	loud _ SampledInstrument new readSampleSetFrom: sampleSetDir, ' f'.
	short _ SampledInstrument new readSampleSetFrom: sampleSetDir, ' stacc'.
	memAfter _ Smalltalk garbageCollect.
	Transcript show:
		instName, ': ', (memBefore - memAfter) printString,
		' bytes; ', memAfter printString, ' bytes left'; newLine.
	AbstractSound soundNamed: instName, '-f&stacc' put:
		(snd _ SampledInstrument new
			allSampleSets: loud;
			staccatoLoudAndSoftSampleSet: short).
	"fix slow attacks"
	snd allNotes do: [:n | n firstSample: (n findStartPointForThreshold: 500)].

	AbstractSound soundNamed: instName, '-f' put:
		(snd _ SampledInstrument new
			allSampleSets: loud).
	"fix slow attacks"
	snd allNotes do: [:n | n firstSample: (n findStartPointForThreshold: 1000)].
! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:44'!
readPizzInstrument: instName fromDirectory: orchestraDir
	"SampledInstrument
		readPizzInstrument: 'violin'
		fromDirectory: 'Tosh:Sample Library:Orchestra'"

	| sampleSetDir memBefore memAfter sampleSet snd |
	sampleSetDir _ orchestraDir, ':', instName, ' pizz'.
	memBefore _ Smalltalk garbageCollect.
	sampleSet _ SampledInstrument new readSampleSetFrom: sampleSetDir.
	memAfter _ Smalltalk garbageCollect.
	Transcript show:
		instName, ': ', (memBefore - memAfter) printString,
		' bytes; ', memAfter printString, ' bytes left'; newLine.
	AbstractSound soundNamed: instName, '-pizz' put:
		(snd _ SampledInstrument new allSampleSets: sampleSet).

	"fix slow attacks"
	snd allNotes do: [:n |
		n firstSample: (n findStartPointForThreshold: 1000)].

	^ snd
! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:45'!
readSimpleInstrument: instName fromDirectory: orchestraDir
	"SampledInstrument
		readSimpleInstrument: 'oboe'
		fromDirectory: 'Tosh:Sample Library:Orchestra'"

	| sampleSetDir memBefore memAfter sampleSet snd |
	sampleSetDir _ orchestraDir, ':', instName, ' f'.
	memBefore _ Smalltalk garbageCollect.
	sampleSet _ SampledInstrument new readSampleSetFrom: sampleSetDir.
	memAfter _ Smalltalk garbageCollect.
	Transcript show:
		instName, ': ', (memBefore - memAfter) printString,
		' bytes; ', memAfter printString, ' bytes left'; newLine.
	AbstractSound soundNamed: instName, '-f' put:
		(snd _ SampledInstrument new allSampleSets: sampleSet).

	"fix slow attacks"
	snd allNotes do: [:n |
		n firstSample: (n findStartPointForThreshold: 1000)].

	^ snd
! !

!SoundCodec methodsFor: 'subclass responsibilities' stamp: 'di 2/8/1999 14:23'!
bytesPerEncodedFrame
	"Answer the number of bytes required to hold one frame of compressed sound data. Answer zero if this codec produces encoded frames of variable size."

	self subclassResponsibility.
! !

!SoundCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/2/1999 15:38'!
decodeFrames: frameCount from: srcByteArray at: srcIndex into: dstSoundBuffer at: dstIndex
	"Decode the given number of monophonic frames starting at the given index in the given ByteArray of compressed sound data and storing the decoded samples into the given SoundBuffer starting at the given destination index. Answer a pair containing the number of bytes of compressed data consumed and the number of decompressed samples produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	self subclassResponsibility.
! !

!SoundCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/2/1999 15:39'!
encodeFrames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex
	"Encode the given number of frames starting at the given index in the given monophonic SoundBuffer and storing the encoded sound data into the given ByteArray starting at the given destination index. Encode only as many complete frames as will fit into the destination. Answer a pair containing the number of samples consumed and the number of bytes of compressed data produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	self subclassResponsibility.
! !

!SoundCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:30'!
reset
	"Reset my encoding and decoding state. Optional. This default implementation does nothing."
! !

!SoundCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/2/1999 15:45'!
samplesPerFrame
	"Answer the number of sound samples per compression frame."

	self subclassResponsibility.
! !

!SoundCodec methodsFor: 'compress/decompress' stamp: 'jm 2/2/1999 16:01'!
compressAndDecompress: aSound
	"Compress and decompress the given sound. Useful for testing."
	"(MuLawCodec new compressAndDecompress: (SampledSound soundNamed: 'camera')) play"

	^ (self compressSound: aSound) asSound
! !

!SoundCodec methodsFor: 'compress/decompress' stamp: 'jm 2/2/1999 16:01'!
compressSound: aSound
	"Compress the entirety of the given sound with this codec. Answer a CompressedSoundData."

	| compressed channels |
	compressed _ CompressedSoundData new
		codecName: self class name;
		soundClassName: aSound class name.
	(aSound isKindOf: SampledSound) ifTrue: [
		channels _ Array new: 1.
		channels at: 1 put: (self encodeSoundBuffer: aSound samples).
		compressed
			channels: channels;
			samplingRate: aSound originalSamplingRate;
			firstSample: 1;
			loopEnd: aSound samples size;
			loopLength: 0.0;
			perceivedPitch: 100.0;
			gain: aSound loudness.
		^ compressed].
	(aSound isKindOf: LoopedSampledSound) ifTrue: [
		aSound isStereo
			ifTrue: [
				channels _ Array new: 2.
				channels at: 1 put: (self encodeSoundBuffer: aSound leftSamples).
				channels at: 2 put: (self encodeSoundBuffer: aSound rightSamples)]
			ifFalse: [
				channels _ Array new: 1.
				channels at: 1 put: (self encodeSoundBuffer: aSound leftSamples)].
		compressed
			channels: channels;
			samplingRate: aSound originalSamplingRate;
			firstSample: aSound firstSample;
			loopEnd: aSound loopEnd;
			loopLength: aSound loopLength;
			perceivedPitch: aSound perceivedPitch;
			gain: aSound gain.
		^ compressed].
	self error: 'you can only compress sampled sounds'.
! !

!SoundCodec methodsFor: 'compress/decompress' stamp: 'RAA 1/2/2001 10:17'!
compressSound: aSound atRate: desiredSampleRate
	"Compress the entirety of the given sound with this codec. Answer a CompressedSoundData."

	| compressed channels samples newRate ratio buffer |

	compressed _ CompressedSoundData new
		codecName: self class name;
		soundClassName: aSound class name.
	(aSound isKindOf: SampledSound) ifTrue: [
		(desiredSampleRate isNil or: 
				[(ratio _ aSound originalSamplingRate // desiredSampleRate) <= 1]) ifTrue: [
			samples _ aSound samples.
			newRate _ aSound originalSamplingRate.
		] ifFalse: [
			buffer _ aSound samples.
			samples _ SoundBuffer 
				averageEvery: ratio 
				from: buffer 
				upTo: buffer monoSampleCount.
			newRate _ aSound originalSamplingRate / ratio.
		].

		channels _ Array new: 1.
		channels at: 1 put: (self encodeSoundBuffer: samples).
		compressed
			channels: channels;
			samplingRate: newRate;
			firstSample: 1;
			loopEnd: samples size;
			loopLength: 0.0;
			perceivedPitch: 100.0;
			gain: aSound loudness.
		^ compressed].
	(aSound isKindOf: LoopedSampledSound) ifTrue: [
		aSound isStereo
			ifTrue: [
				channels _ Array new: 2.
				channels at: 1 put: (self encodeSoundBuffer: aSound leftSamples).
				channels at: 2 put: (self encodeSoundBuffer: aSound rightSamples)]
			ifFalse: [
				channels _ Array new: 1.
				channels at: 1 put: (self encodeSoundBuffer: aSound leftSamples)].
		compressed
			channels: channels;
			samplingRate: aSound originalSamplingRate;
			firstSample: aSound firstSample;
			loopEnd: aSound loopEnd;
			loopLength: aSound loopLength;
			perceivedPitch: aSound perceivedPitch;
			gain: aSound gain.
		^ compressed].
	self error: 'you can only compress sampled sounds'.
! !

!SoundCodec methodsFor: 'compress/decompress' stamp: 'jm 3/30/1999 08:03'!
decompressSound: aCompressedSound
	"Decompress the entirety of the given compressed sound with this codec and answer the resulting sound."

	| channels sound |
	channels _ aCompressedSound channels
		collect: [:compressed | self decodeCompressedData: compressed].
	'SampledSound' = aCompressedSound soundClassName ifTrue: [
		sound _ SampledSound
			samples: channels first
			samplingRate: (aCompressedSound samplingRate).
		sound loudness: aCompressedSound gain.
		^ sound].
	'LoopedSampledSound' = aCompressedSound soundClassName ifTrue: [
		aCompressedSound loopLength = 0
			ifTrue: [
				sound _ LoopedSampledSound
					unloopedSamples: channels first
					pitch: aCompressedSound perceivedPitch
					samplingRate: aCompressedSound samplingRate]
			ifFalse: [
				sound _ LoopedSampledSound
					samples: channels first
					loopEnd: aCompressedSound loopEnd
					loopLength: aCompressedSound loopLength
					pitch: aCompressedSound perceivedPitch
					samplingRate: aCompressedSound samplingRate].
		channels size > 1 ifTrue: [sound rightSamples: channels last].
		sound
			firstSample: aCompressedSound firstSample;
			gain: aCompressedSound gain.
		sound
			setPitch: 100.0
			dur: (channels first size / aCompressedSound samplingRate)
			loudness: 1.0.
		^ sound].
	self error: 'unknown sound class'.
! !

!SoundCodec methodsFor: 'private' stamp: 'di 2/8/1999 19:53'!
decodeCompressedData: aByteArray
	"Decode the entirety of the given encoded data buffer with this codec. Answer a monophonic SoundBuffer containing the uncompressed samples."

	| frameCount result increments |
	frameCount _ self frameCount: aByteArray.
	result _ SoundBuffer newMonoSampleCount: frameCount * self samplesPerFrame.
	self reset.
	increments _ self decodeFrames: frameCount from: aByteArray at: 1 into: result at: 1.
	((increments first = aByteArray size) and: [increments last = result size]) ifFalse: [
		self error: 'implementation problem; increment sizes should match buffer sizes'].
	^ result
! !

!SoundCodec methodsFor: 'private' stamp: 'di 2/8/1999 14:20'!
encodeSoundBuffer: aSoundBuffer
	"Encode the entirety of the given monophonic SoundBuffer with this codec. Answer a ByteArray containing the compressed sound data."

	| codeFrameSize frameSize fullFrameCount lastFrameSamples result increments finalFrame i lastIncs |
	frameSize _ self samplesPerFrame.
	fullFrameCount _ aSoundBuffer monoSampleCount // frameSize.
	lastFrameSamples _ aSoundBuffer monoSampleCount - (fullFrameCount * frameSize).
	codeFrameSize _ self bytesPerEncodedFrame.
	codeFrameSize = 0 ifTrue:
		["Allow room for 1 byte per sample for variable-length compression"
		codeFrameSize _ frameSize].
	lastFrameSamples > 0
		ifTrue: [result _ ByteArray new: (fullFrameCount + 1) * codeFrameSize]
		ifFalse: [result _ ByteArray new: fullFrameCount * codeFrameSize].
	self reset.
	increments _ self encodeFrames: fullFrameCount from: aSoundBuffer at: 1 into: result at: 1.
	lastFrameSamples > 0 ifTrue: [
		finalFrame _ SoundBuffer newMonoSampleCount: frameSize.
		i _ fullFrameCount * frameSize.
		1 to: lastFrameSamples do: [:j |
			finalFrame at: j put: (aSoundBuffer at: (i _ i + 1))].
		lastIncs _ self encodeFrames: 1 from: finalFrame at: 1 into: result at: 1 + increments second.
		increments _ Array with: increments first + lastIncs first
							with: increments second + lastIncs second].
	increments second < result size
		ifTrue: [^ result copyFrom: 1 to: increments second]
		ifFalse: [^ result]
! !

!SoundCodec methodsFor: 'private' stamp: 'di 2/8/1999 19:54'!
frameCount: aByteArray
	"Compute the frame count for this byteArray.  This default computation will have to be overridden by codecs with variable frame sizes."

	| codeFrameSize |
	codeFrameSize _ self bytesPerEncodedFrame.
	(aByteArray size \\ codeFrameSize) = 0 ifFalse:
		[self error: 'encoded buffer is not an even multiple of the encoded frame size'].
	^ aByteArray size // codeFrameSize! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 12/12/2001 17:57'!
bytesPerEncodedFrame
	"Answer the number of bytes required to hold one frame of compressed sound data."
	"Note: When used as a normal codec, the frame size is always 8 samples which results in (8 * bitsPerSample) / 8 = bitsPerSample bytes."

	| bitCount |
	frameSizeMask = 0 ifTrue: [^ bitsPerSample].
	"Following assumes mono:"
	bitCount _ 16 + 6 + ((self samplesPerFrame - 1) * bitsPerSample).
	^ (bitCount + 7) // 8
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 7/2/1999 13:29'!
compressAndDecompress: aSound
	"Compress and decompress the given sound. Overridden to use same bits per sample for both compressing and decompressing."

	| compressed decoder |
	compressed _ self compressSound: aSound.
	decoder _ self class new
		initializeForBitsPerSample: bitsPerSample
		samplesPerFrame: 0.
	^ decoder decompressSound: compressed

! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 3/28/1999 15:37'!
decodeFrames: frameCount from: srcByteArray at: srcIndex into: dstSoundBuffer at: dstIndex
	"Decode the given number of monophonic frames starting at the given index in the given ByteArray of compressed sound data and storing the decoded samples into the given SoundBuffer starting at the given destination index. Answer a pair containing the number of bytes of compressed data consumed and the number of decompressed samples produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	encodedBytes _ srcByteArray.
	byteIndex _ srcIndex - 1.
	bitPosition _ 0.
	currentByte _ 0.
	samples _ dstSoundBuffer.
	sampleIndex _ dstIndex - 1.
	self privateDecodeMono: (frameCount * self samplesPerFrame).
	^ Array with: (byteIndex - (srcIndex - 1)) with: (sampleIndex - (dstIndex - 1))
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 3/28/1999 15:28'!
encodeFrames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex
	"Encode the given number of frames starting at the given index in the given monophonic SoundBuffer and storing the encoded sound data into the given ByteArray starting at the given destination index. Encode only as many complete frames as will fit into the destination. Answer a pair containing the number of samples consumed and the number of bytes of compressed data produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	samples _ srcSoundBuffer.
	sampleIndex _ srcIndex - 1.
	encodedBytes _ dstByteArray.
	byteIndex _ dstIndex - 1.
	bitPosition _ 0.
	currentByte _ 0.
	self privateEncodeMono: (frameCount * self samplesPerFrame).
	^ Array with: frameCount with: (byteIndex - (dstIndex - 1))
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 12/14/2001 11:21'!
reset

	self resetForMono.
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 3/28/1999 20:12'!
resetForMono
	"Reset my encoding and decoding state for mono."

	predicted _ 0.
	index _ 0.
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 3/28/1999 20:12'!
resetForStereo
	"Reset my encoding and decoding state for stereo."

	"keep state as SoundBuffers to allow fast access from primitive"
	predicted _ SoundBuffer new: 2.
	index _ SoundBuffer new: 2.
! !

!ADPCMCodec methodsFor: 'codec stuff' stamp: 'jm 3/27/1999 08:34'!
samplesPerFrame
	"Answer the number of sound samples per compression frame."

	frameSizeMask > 0 ifTrue: [^ frameSizeMask + 1].
	^ 8  "frame size when there are no running headers"
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 06:26'!
decode: aByteArray bitsPerSample: bits

	^ self
		decode: aByteArray
		sampleCount: (aByteArray size * 8) // bits
		bitsPerSample: bits
		frameSize: 0
		stereo: false
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 15:57'!
decode: aByteArray sampleCount: count bitsPerSample: bits frameSize: frameSize stereo: stereoFlag

	self initializeForBitsPerSample: bits samplesPerFrame: frameSize.
	encodedBytes _ aByteArray.
	byteIndex _ 0.
	bitPosition _ 0.
	currentByte _ 0.
	stereoFlag
		ifTrue: [
			self resetForStereo.
			samples _ SoundBuffer newMonoSampleCount: count.
			rightSamples _ SoundBuffer newMonoSampleCount: count.
			sampleIndex _ 0.
			self privateDecodeStereo: count.
			^ Array with: samples with: rightSamples]
		ifFalse: [
			samples _ SoundBuffer newMonoSampleCount: count.
			sampleIndex _ 0.
			self privateDecodeMono: count.
			^ samples]
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/30/1999 08:56'!
decodeFlash: aByteArray sampleCount: sampleCount stereo: stereoFlag

	| bits |
	encodedBytes _ aByteArray.
	byteIndex _ 0.
	bitPosition _ 0.
	currentByte _ 0.
	bits _ 2 + (self nextBits: 2).  "bits per sample"
	self initializeForBitsPerSample: bits samplesPerFrame: 4096.
	stereoFlag
		ifTrue: [
			self resetForStereo.
			samples _ SoundBuffer newMonoSampleCount: sampleCount.
			rightSamples _ SoundBuffer newMonoSampleCount: sampleCount.
			sampleIndex _ 0.
			self privateDecodeStereo: sampleCount.
			^ Array with: samples with: rightSamples]
		ifFalse: [
			samples _ SoundBuffer newMonoSampleCount: sampleCount.
			sampleIndex _ 0.
			self privateDecodeMono: sampleCount.
			^ Array with: samples].
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 08:59'!
encode: aSoundBuffer bitsPerSample: bits

	^ self
		encodeLeft: aSoundBuffer
		right: nil
		bitsPerSample: bits
		frameSize: 0
		forFlash: false
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 08:58'!
encodeFlashLeft: leftSoundBuffer right: rightSoundBuffer bitsPerSample: bits

	^ self
		encodeLeft: leftSoundBuffer
		right: rightSoundBuffer
		bitsPerSample: bits
		frameSize: 4096
		forFlash: true
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 11/21/2001 11:35'!
encodeLeft: leftSoundBuffer right: rightSoundBuffer bitsPerSample: bits frameSize: frameSize forFlash: flashFlag

	| stereoFlag sampleCount sampleBitCount bitCount |
	self initializeForBitsPerSample: bits samplesPerFrame: frameSize.
	stereoFlag _ rightSoundBuffer notNil.
	sampleCount _ leftSoundBuffer monoSampleCount.
	stereoFlag
		ifTrue: [sampleBitCount _ 2 * (sampleCount * bitsPerSample)]
		ifFalse: [sampleBitCount _ sampleCount * bitsPerSample].
	bitCount _ sampleBitCount +
		(self headerBitsForSampleCount: sampleCount stereoFlag: stereoFlag).

	encodedBytes _ ByteArray new: ((bitCount / 8) ceiling roundUpTo: self bytesPerEncodedFrame).
	byteIndex _ 0.
	bitPosition _ 0.
	currentByte _ 0.
	flashFlag ifTrue: [self nextBits: 2 put: bits - 2].
	stereoFlag
		ifTrue: [
			samples _ Array with: leftSoundBuffer with: rightSoundBuffer.
			sampleIndex _ Array with: 0 with: 0.
			self privateEncodeStereo: sampleCount]
		ifFalse: [
			samples _ leftSoundBuffer.
			sampleIndex _ 0.
			self privateEncodeMono: sampleCount].

	^ encodedBytes
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/27/1999 12:14'!
headerBitsForSampleCount: sampleCount stereoFlag: stereoFlag
	"Answer the number of extra header bits required for the given number of samples. This will be zero if I am not using frame headers."

	| frameCount bitsPerHeader |
	frameSizeMask = 0 ifTrue: [^ 0].
	frameCount _ (sampleCount / self samplesPerFrame) ceiling.
	bitsPerHeader _ 16 + 6.
	stereoFlag ifTrue: [bitsPerHeader _ 2 * bitsPerHeader].
	^ frameCount * bitsPerHeader
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 16:08'!
indexForDeltaFrom: thisSample to: nextSample
	"Answer the best index to use for the difference between the given samples."
	"Details: Scan stepSizeTable for the first entry >= the absolute value of the difference between sample values. Since indexes are zero-based, the index used during decoding will be the one in the following stepSizeTable entry. Since the index field of a Flash frame header is only six bits, the maximum index value is 63."
	"Note: Since there does not appear to be any documentation of how Flash acutally computes the indices used in its frame headers, this algorithm was guessed by reverse-engineering the Flash ADPCM decoder."

	| diff bestIndex |
	self inline: true.

	diff _ nextSample - thisSample.
	diff < 0 ifTrue: [diff _ 0 - diff].
	bestIndex _ 63.
	1 to: 62 do: [:j |
		bestIndex = 63 ifTrue: [
			(stepSizeTable at: j) >= diff ifTrue: [bestIndex _ j]]].
	^ bestIndex
! !

!ADPCMCodec methodsFor: 'private' stamp: 'jm 3/28/1999 20:48'!
initializeForBitsPerSample: sampleBits samplesPerFrame: frameSize

	self resetForMono.
	stepSizeTable _ #(7 8 9 10 11 12 13 14 16 17 19 21 23 25 28 31 34 37 41 45 50 55 60 66 73 80 88 97 107 118 130 143 157 173 190 209 230 253 279 307 337 371 408 449 494 544 598 658 724 796 876 963 1060 1166 1282 1411 1552 1707 1878 2066 2272 2499 2749 3024 3327 3660 4026 4428 4871 5358 5894 6484 7132 7845 8630 9493 10442 11487 12635 13899 15289 16818 18500 20350 22385 24623 27086 29794 32767).

	indexTable _ nil.
	sampleBits = 2 ifTrue: [
		indexTable _ #(-1 2)].
	sampleBits = 3 ifTrue: [
		indexTable _ #(-1 -1 2 4)].
	sampleBits = 4 ifTrue: [
		indexTable _ #(-1 -1 -1 -1 2 4 6 8)].
	sampleBits = 5 ifTrue: [
		indexTable _ #(-1 -1 -1 -1 -1 -1 -1 -1 1 2 4 6 8 10 13 16)].
	indexTable ifNil: [self error: 'unimplemented bits/sample'].

	bitsPerSample _ sampleBits.
	deltaSignMask _ 1 bitShift: bitsPerSample - 1.
	deltaValueMask _ deltaSignMask - 1.
	deltaValueHighBit _ deltaSignMask / 2.

	frameSize <= 1
		ifTrue: [frameSizeMask _ 0]
		ifFalse: [
			(frameSize = (1 bitShift: frameSize highBit - 1))
				ifFalse: [self error: 'frameSize must be a power of two'].
			frameSizeMask _ frameSize - 1].

	"keep as SoundBuffer to allow fast access from primitive"
	indexTable _ SoundBuffer fromArray: indexTable.
	stepSizeTable _ SoundBuffer fromArray: stepSizeTable.
! !

!ADPCMCodec methodsFor: 'private' stamp: 'ar 4/23/2001 15:11'!
privateDecodeMono: count

	| delta step predictedDelta bit |
	<primitive: 'primitiveDecodeMono' module: 'ADPCMCodecPlugin'>
	self var: #stepSizeTable declareC: 'short int *stepSizeTable'.
	self var: #indexTable declareC: 'short int *indexTable'.
	self var: #samples declareC: 'short int *samples'.
	self var: #encodedBytes declareC: 'unsigned char *encodedBytes'.

	1 to: count do: [:i |
		(i bitAnd: frameSizeMask) = 1
			ifTrue: [  "start of frame; read frame header"
				predicted _ self nextBits: 16.
				predicted > 32767 ifTrue: [predicted _ predicted - 65536].
				index _ self nextBits: 6.
				samples at: (sampleIndex _ sampleIndex + 1) put: predicted]
			ifFalse: [
				delta _ self nextBits: bitsPerSample.
				step _ stepSizeTable at: index + 1.
				predictedDelta _ 0.
				bit _ deltaValueHighBit.
				[bit > 0] whileTrue: [
					(delta bitAnd: bit) > 0 ifTrue: [predictedDelta _ predictedDelta + step].
					step _ step bitShift: -1.
					bit _ bit bitShift: -1].
				predictedDelta _ predictedDelta + step.

				(delta bitAnd: deltaSignMask) > 0
					ifTrue: [predicted _ predicted - predictedDelta]
					ifFalse: [predicted _ predicted + predictedDelta].
				predicted > 32767
					ifTrue: [predicted _ 32767]
					ifFalse: [predicted < -32768 ifTrue: [predicted _ -32768]].

				index _ index + (indexTable at: (delta bitAnd: deltaValueMask) + 1).
				index < 0
					ifTrue: [index _ 0]
					ifFalse: [index > 88 ifTrue: [index _ 88]].

				samples at: (sampleIndex _ sampleIndex + 1) put: predicted]].
! !

!ADPCMCodec methodsFor: 'private' stamp: 'ar 4/23/2001 15:11'!
privateDecodeStereo: count

	| predictedLeft predictedRight indexLeft indexRight deltaLeft deltaRight
	 stepLeft stepRight predictedDeltaLeft predictedDeltaRight bit |

	<primitive: 'primitiveDecodeStereo' module: 'ADPCMCodecPlugin'>
	self var: #stepSizeTable declareC: 'short int *stepSizeTable'.
	self var: #indexTable declareC: 'short int *indexTable'.
	self var: #samples declareC: 'short int *samples'.
	self var: #encodedBytes declareC: 'unsigned char *encodedBytes'.
	self var: #rightSamples declareC: 'short int *rightSamples'.
	self var: #predicted declareC: 'short int *predicted'.
	self var: #index declareC: 'short int *index'.

	"make local copies of decoder state variables"
	predictedLeft _ predicted at: 1.
	predictedRight _ predicted at: 2.
	indexLeft _ index at: 1.
	indexRight _ index at: 2.

	1 to: count do: [:i |
		(i bitAnd: frameSizeMask) = 1
			ifTrue: [  "start of frame; read frame header"
				predictedLeft _ self nextBits: 16.
				indexLeft _ self nextBits: 6.
				predictedRight _ self nextBits: 16.
				indexRight _ self nextBits: 6.
				predictedLeft > 32767 ifTrue: [predictedLeft _ predictedLeft - 65536].
				predictedRight > 32767 ifTrue: [predictedRight _ predictedRight - 65536].
				samples at: (sampleIndex _ sampleIndex + 1) put: predictedLeft.
				rightSamples at: sampleIndex put: predictedRight]
			ifFalse: [
				deltaLeft _ self nextBits: bitsPerSample.
				deltaRight _ self nextBits: bitsPerSample.
				stepLeft _ stepSizeTable at: indexLeft + 1.
				stepRight _ stepSizeTable at: indexRight + 1.
				predictedDeltaLeft _ predictedDeltaRight _ 0.
				bit _ deltaValueHighBit.
				[bit > 0] whileTrue: [
					(deltaLeft bitAnd: bit) > 0 ifTrue: [
						predictedDeltaLeft _ predictedDeltaLeft + stepLeft].
					(deltaRight bitAnd: bit) > 0 ifTrue: [
						predictedDeltaRight _ predictedDeltaRight + stepRight].
					stepLeft _ stepLeft bitShift: -1.
					stepRight _ stepRight bitShift: -1.
					bit _ bit bitShift: -1].
				predictedDeltaLeft _ predictedDeltaLeft + stepLeft.
				predictedDeltaRight _ predictedDeltaRight + stepRight.

				(deltaLeft bitAnd: deltaSignMask) > 0
					ifTrue: [predictedLeft _ predictedLeft - predictedDeltaLeft]
					ifFalse: [predictedLeft _ predictedLeft + predictedDeltaLeft].
				(deltaRight bitAnd: deltaSignMask) > 0
					ifTrue: [predictedRight _ predictedRight - predictedDeltaRight]
					ifFalse: [predictedRight _ predictedRight + predictedDeltaRight].
				predictedLeft > 32767
					ifTrue: [predictedLeft _ 32767]
					ifFalse: [predictedLeft < -32768 ifTrue: [predictedLeft _ -32768]].
				predictedRight > 32767
					ifTrue: [predictedRight _ 32767]
					ifFalse: [predictedRight < -32768 ifTrue: [predictedRight _ -32768]].

				indexLeft _ indexLeft + (indexTable at: (deltaLeft bitAnd: deltaValueMask) + 1).
				indexLeft < 0
					ifTrue: [indexLeft _ 0]
					ifFalse: [indexLeft > 88 ifTrue: [indexLeft _ 88]].
				indexRight _ indexRight + (indexTable at: (deltaRight bitAnd: deltaValueMask) + 1).
				indexRight < 0
					ifTrue: [indexRight _ 0]
					ifFalse: [indexRight > 88 ifTrue: [indexRight _ 88]].

				samples at: (sampleIndex _ sampleIndex + 1) put: predictedLeft.
				rightSamples at: sampleIndex put: predictedRight]].

	"save local copies of decoder state variables"
	predicted at: 1 put: predictedLeft.
	predicted at: 2 put: predictedRight.
	index at: 1 put: indexLeft.
	index at: 2 put: indexRight.
! !

!ADPCMCodec methodsFor: 'private' stamp: 'ar 4/23/2001 15:11'!
privateEncodeMono: count

	| step sign diff delta predictedDelta bit p |
	<primitive: 'primitiveEncodeMono' module: 'ADPCMCodecPlugin'>
	self var: #stepSizeTable declareC: 'short int *stepSizeTable'.
	self var: #indexTable declareC: 'short int *indexTable'.
	self var: #samples declareC: 'short int *samples'.
	self var: #encodedBytes declareC: 'unsigned char *encodedBytes'.

	step _ stepSizeTable at: 1.
	1 to: count do: [:i |
		(i bitAnd: frameSizeMask) = 1 ifTrue: [
			predicted _ samples at: (sampleIndex _ sampleIndex + 1).
			(p _ predicted) < 0 ifTrue: [p _ p + 65536].
			self nextBits: 16 put: p.
			i < count ifTrue: [
				index _ self indexForDeltaFrom: predicted to: (samples at: sampleIndex + 1)].
			self nextBits: 6 put: index.
		] ifFalse: [
			"compute sign and magnitude of difference from the predicted sample"
			sign _ 0.
			diff _ (samples at: (sampleIndex _ sampleIndex + 1)) - predicted.
			diff < 0 ifTrue: [
				sign _ deltaSignMask.
				diff _ 0 - diff].

			"Compute encoded delta and the difference that this will cause in the predicted sample value during decoding. Note that this code approximates:
				delta _ (4 * diff) / step.
				predictedDelta _ ((delta + 0.5) * step) / 4;
			but in the shift step bits are dropped. Thus, even if you have fast mul/div hardware you cannot use it since you would get slightly different bits what than the algorithm defines."
			delta _ 0.
			predictedDelta _ 0.
			bit _ deltaValueHighBit.
			[bit > 0] whileTrue: [
				diff >= step ifTrue: [
					delta _ delta + bit.
					predictedDelta _ predictedDelta + step.
					diff _ diff - step].
				step _ step bitShift: -1.
				bit _ bit bitShift: -1].
			predictedDelta _ predictedDelta + step.

			"compute and clamp new prediction"
			sign > 0
				ifTrue: [predicted _ predicted - predictedDelta]
				ifFalse: [predicted _ predicted + predictedDelta].
			predicted > 32767
				ifTrue: [predicted _ 32767]
				ifFalse: [predicted < -32768 ifTrue: [predicted _ -32768]].

			"compute new index and step values"
			index _ index + (indexTable at: delta + 1).
			index < 0
				ifTrue: [index _ 0]
				ifFalse: [index > 88 ifTrue: [index _ 88]].
			step _ stepSizeTable at: index + 1.

			"output encoded, signed delta"
			self nextBits: bitsPerSample put: (sign bitOr: delta)]].

	bitPosition > 0 ifTrue: [  "flush the last output byte, if necessary"
		encodedBytes at: (byteIndex _ byteIndex + 1) put: currentByte].
! !

!ADPCMCodec methodsFor: 'private' stamp: 'ar 4/23/2001 15:12'!
privateEncodeStereo: count

	<primitive: 'primitiveEncodeStereo' module: 'ADPCMCodecPlugin'>
	"not yet implemented"
	self inline: false.
	self success: false.! !

!ADPCMCodec methodsFor: 'bit streaming' stamp: 'jm 3/28/1999 16:24'!
nextBits: n
	"Answer the next n bits of my bit stream as an unsigned integer."

	| result remaining shift |
	self inline: true.

	result _ 0.
	remaining _ n.
	[true] whileTrue: [
		shift _ remaining - bitPosition.
		result _ result + (currentByte bitShift: shift).
		shift > 0
			ifTrue: [  "consumed currentByte buffer; fetch next byte"
				remaining _ remaining - bitPosition.			
				currentByte _ (encodedBytes at: (byteIndex _ byteIndex + 1)).
				bitPosition _ 8]
			ifFalse: [  "still some bits left in currentByte buffer"
				bitPosition _ bitPosition - remaining.
				"mask out the consumed bits:"
				currentByte _ currentByte bitAnd: (255 bitShift: (bitPosition - 8)).
				^ result]].
! !

!ADPCMCodec methodsFor: 'bit streaming' stamp: 'jm 3/28/1999 20:21'!
nextBits: n put: anInteger
	"Write the next n bits to my bit stream."

	| buf bufBits bitsAvailable shift |
	self inline: true.

	buf _ anInteger.
	bufBits _ n.
	[true] whileTrue: [
		bitsAvailable _ 8 - bitPosition.
		shift _ bitsAvailable - bufBits.  "either left or right shift"
		"append high bits of buf to end of currentByte:"
		currentByte _ currentByte + (buf bitShift: shift).
		shift < 0
			ifTrue: [  "currentByte buffer filled; output it"
				encodedBytes at: (byteIndex _ byteIndex + 1) put: currentByte.
				bitPosition _ 0.
				currentByte _ 0.
				"clear saved high bits of buf:"
				buf _ buf bitAnd: (1 bitShift: 0 - shift) - 1.
				bufBits _ bufBits - bitsAvailable]
			ifFalse: [  "still some bits available in currentByte buffer"
				bitPosition _ bitPosition + bufBits.
				^ self]].
! !

!ADPCMCodec class methodsFor: 'instance creation' stamp: 'jm 3/27/1999 11:15'!
new

	^ super new
		initializeForBitsPerSample: 4
		samplesPerFrame: 0.
! !

!ADPCMCodec class methodsFor: 'instance creation' stamp: 'jm 11/15/2001 16:02'!
newBitsPerSample: bitsPerSample

	^ super new
		initializeForBitsPerSample: bitsPerSample
		samplesPerFrame: 0.
! !

!ADPCMCodec class methodsFor: 'primitive generation' stamp: 'ar 2/3/2001 15:50'!
translatedPrimitives
	"Answer a string containing the translated C code for my primitives."
	"Note: This code currently must be hand-edited to remove several methods that are inlined (thus not needed) but not pruned out by the ST-to-C translator."

	^#(
		(ADPCMCodec privateDecodeMono:)
		(ADPCMCodec privateDecodeStereo:)
		(ADPCMCodec privateEncodeMono:)
		(ADPCMCodec privateEncodeStereo:)
		(ADPCMCodec indexForDeltaFrom:to:)
		(ADPCMCodec nextBits:)
		(ADPCMCodec nextBits:put:))
! !

!GSMCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:36'!
bytesPerEncodedFrame

	^ 33
! !

!GSMCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:44'!
decodeFrames: frameCount from: srcByteArray at: srcIndex into: dstSoundBuffer at: dstIndex

	| p |
	p _ self	primDecode: decodeState frames: frameCount
			from: srcByteArray at: srcIndex
			into: dstSoundBuffer at: dstIndex.
	^ Array with: p x with: p y
! !

!GSMCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:44'!
encodeFrames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex

	| p |
	p _ self	primEncode: encodeState frames: frameCount
			from: srcSoundBuffer at: srcIndex
			into: dstByteArray at: dstIndex.
	^ Array with: p x with: p y
! !

!GSMCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:32'!
reset
	"Reset my encoding/decoding state to prepare to encode or decode a new sound stream."

	encodeState _ self primNewState.
	decodeState _ self primNewState.
! !

!GSMCodec methodsFor: 'subclass responsibilities' stamp: 'jm 2/4/1999 11:36'!
samplesPerFrame

	^ 160
! !

!GSMCodec methodsFor: 'primitives' stamp: 'jm 2/4/1999 11:33'!
primDecode: state frames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex

	<primitive: 'primitiveGSMDecode' module: 'SoundCodecPrims'>
	self primitiveFailed.
! !

!GSMCodec methodsFor: 'primitives' stamp: 'jm 2/4/1999 11:33'!
primEncode: state frames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex

	<primitive: 'primitiveGSMEncode' module: 'SoundCodecPrims'>
	self primitiveFailed.
! !

!GSMCodec methodsFor: 'primitives' stamp: 'jm 2/4/1999 11:35'!
primNewState

	<primitive: 'primitiveGSMNewState' module: 'SoundCodecPrims'>
	self error: 'The SoundCodecPrims plugin is not available'.
! !

!GSMCodec class methodsFor: 'instance creation' stamp: 'jm 10/21/2001 10:10'!
new

	^ super new reset
! !

!MuLawCodec methodsFor: 'subclass responsibility' stamp: 'jm 2/2/1999 09:15'!
bytesPerEncodedFrame
	"Answer the number of bytes required to hold one frame of compressed sound data. Answer zero if this codec produces encoded frames of variable size."

	^ 1
! !

!MuLawCodec methodsFor: 'subclass responsibility' stamp: 'jm 2/2/1999 14:10'!
decodeFrames: frameCount from: srcByteArray at: srcIndex into: dstSoundBuffer at: dstIndex
	"Decode the given number of monophonic frames starting at the given index in the given ByteArray of compressed sound data and storing the decoded samples into the given SoundBuffer starting at the given destination index. Answer a pair containing the number of bytes of compressed data consumed and the number of decompressed samples produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	| dst |
	dst _ dstIndex.
	srcIndex to: srcIndex + frameCount - 1 do: [:src |
		dstSoundBuffer at: dst put: (DecodingTable at: (srcByteArray at: src) + 1).
		dst _ dst + 1].
	^ Array with: frameCount with: frameCount
! !

!MuLawCodec methodsFor: 'subclass responsibility' stamp: 'di 2/8/1999 22:25'!
encodeFrames: frameCount from: srcSoundBuffer at: srcIndex into: dstByteArray at: dstIndex
	"Encode the given number of frames starting at the given index in the given monophonic SoundBuffer and storing the encoded sound data into the given ByteArray starting at the given destination index. Encode only as many complete frames as will fit into the destination. Answer a pair containing the number of samples consumed and the number of bytes of compressed data produced."
	"Note: Assume that the sender has ensured that the given number of frames will not exhaust either the source or destination buffers."

	srcIndex to: srcIndex + frameCount - 1 do: [:i |
		dstByteArray at: i put: (self uLawEncodeSample: (srcSoundBuffer at: i))].
	^ Array with: frameCount with: frameCount
! !

!MuLawCodec methodsFor: 'subclass responsibility' stamp: 'jm 2/2/1999 09:11'!
samplesPerFrame
	"Answer the number of sound samples per compression frame."

	^ 1
! !

!MuLawCodec methodsFor: 'external access' stamp: 'di 2/8/1999 22:28'!
uLawDecodeSample: byte
	"Decode a 16-bit signed sample from 8 bits using uLaw decoding"

	^ DecodingTable at: byte + 1! !

!MuLawCodec methodsFor: 'external access' stamp: 'di 2/8/1999 22:30'!
uLawEncodeSample: sample
	"Encode a 16-bit signed sample into 8 bits using uLaw encoding"

	| s |
	s _ sample // 8.  "drop 3 least significant bits"
	s < 0 ifTrue: [^ (self uLawEncode12Bits: 0-s) + 16r80]
		ifFalse: [^ (self uLawEncode12Bits: s)].
! !

!MuLawCodec methodsFor: 'private' stamp: 'di 2/9/1999 13:25'!
uLawEncode12Bits: s
	"Encode a 12-bit unsigned sample (0-4095) into 7 bits using uLaw encoding.
	This gets called by a method that scales 16-bit signed integers down to a
		12-bit magnitude, and then ORs in 16r80 if they were negative.
	Detail: May get called with s >= 4096, and this works fine."

	s < 496 ifTrue: [
		s < 112 ifTrue: [
			s < 48 ifTrue: [
				s < 16
					ifTrue: [^ 16r70 bitOr: (15 - s)]
					ifFalse: [^ 16r60 bitOr: (15 - ((s - 16) bitShift: -1))]].
			^ 16r50 bitOr: (15 - ((s - 48) bitShift: -2))].
		s < 240
			ifTrue: [^ 16r40 bitOr: (15 - ((s - 112) bitShift: -3))]
			ifFalse: [^ 16r30 bitOr: (15 - ((s - 240) bitShift: -4))]].

	s < 2032 ifTrue: [
		s < 1008
			ifTrue: [^ 16r20 bitOr: (15 - ((s - 496) bitShift: -5))]
			ifFalse: [^ 16r10 bitOr: (15 - ((s - 1008) bitShift: -6))]].

	s < 4080
		ifTrue: [^ 15 - ((s - 2032) bitShift: -7)]
		ifFalse: [^ 0].
! !

!MuLawCodec class methodsFor: 'class initialization' stamp: 'di 2/9/1999 14:57'!
initialize
	"Build the 256 entry table to be used to decode 8-bit uLaw-encoded samples."
	"MuLawCodec initialize"

	| encoded codec lastEncodedPos lastEncodedNeg |
	DecodingTable _ Array new: 256.
	codec _ self new.
	lastEncodedPos _ nil.
	lastEncodedNeg _ nil.
	4095 to: 0 by: -1 do: [:s |
		encoded _ codec uLawEncode12Bits: s.
		lastEncodedPos = encoded
			ifFalse: [
				DecodingTable at: (encoded + 1) put: (s bitShift: 3).
				lastEncodedPos _ encoded].
		encoded _ encoded bitOr: 16r80.
		lastEncodedNeg = encoded
			ifFalse: [
				DecodingTable at: (encoded + 1) put: (s bitShift: 3) negated.
				lastEncodedNeg _ encoded]].
! !

!SoundPlayer class methodsFor: 'snapshotting' stamp: 'ar 2/4/2001 17:59'!
startUpWithSound: aSound
	"Start up the player process."

	SoundPlayer initialize.
	SoundPlayer
		startPlayerProcessBufferSize: (BufferMSecs * SamplingRate) // 1000
		rate: SamplingRate
		stereo: Stereo
		sound: aSound.
! !

!SoundPlayer class methodsFor: 'primitive test' stamp: 'jm 9/13/97 20:01'!
boinkPitch: p dur: d loudness: l waveTable: waveTable pan: pan
	"Play a decaying note on the given stream using the given wave table. Used for testing only."

	| decay tableSize amplitude increment cycles i |
	decay _ 0.96.
	tableSize _ waveTable size.
	amplitude _ l asInteger min: 1000.
	increment _ ((p asFloat * tableSize asFloat) / SamplingRate asFloat) asInteger.
	increment _ (increment max: 1) min: (tableSize // 2).
	cycles _ (d * SamplingRate asFloat) asInteger.

	i _ 1.
	1 to: cycles do: [:cycle |
		(cycle \\ 100) = 0
			ifTrue: [amplitude _ (decay * amplitude asFloat) asInteger].
		i _ (((i - 1) + increment) \\ tableSize) + 1.
		self playTestSample: (amplitude * (waveTable at: i)) // 1000 pan: pan].
! !

!SoundPlayer class methodsFor: 'primitive test' stamp: 'jm 1/5/98 17:56'!
boinkScale
	"Tests the sound output primitives by playing a scale."
	"SoundPlayer boinkScale"

	| sineTable pan |
	self shutDown.
	SamplingRate _ 11025.
	Stereo _ true.
	sineTable _ self sineTable: 1000.
	Buffer _ SoundBuffer newStereoSampleCount: 1000.
	BufferIndex _ 1.
	self primSoundStartBufferSize: Buffer stereoSampleCount
		rate: SamplingRate
		stereo: Stereo.
	pan _ 0.
	#(261.626 293.665 329.628 349.229 391.996 440.001 493.884 523.252) do: [:p |
		self boinkPitch: p dur: 0.3 loudness: 300 waveTable: sineTable pan: pan.
		pan _ pan + 125].

	self boinkPitch: 261.626 dur: 1.0 loudness: 300 waveTable: sineTable pan: 500.
	self primSoundStop.
	self shutDown.
	SoundPlayer initialize.  "reset sampling rate, buffer size, and stereo flag"
! !

!SoundPlayer class methodsFor: 'primitive test' stamp: 'jm 9/17/97 12:55'!
playTestSample: s pan: pan
	"Append the given sample in the range [-32767..32767] to the output buffer, playing the output buffer when it is full. Used for testing only."

	| sample leftSample |
	BufferIndex >= Buffer size
		ifTrue: [
			"current buffer is full; play it"
			[self primSoundAvailableBytes > 0]
				whileFalse. "wait for space to be available"
			self primSoundPlaySamples: Buffer stereoSampleCount from: Buffer startingAt: 1.
			Buffer primFill: 0.
			BufferIndex _ 1].

	sample _ s.
	sample >  32767 ifTrue: [ sample _  32767 ]. 
	sample < -32767 ifTrue: [ sample _ -32767 ].

	Stereo
		ifTrue: [
			leftSample _ (sample * pan) // 1000.
			Buffer at: BufferIndex		put: sample - leftSample.
			Buffer at: BufferIndex + 1	put: leftSample]
		ifFalse: [
			Buffer at: BufferIndex + 1 put: sample].
	BufferIndex _ BufferIndex + 2.
! !

!SoundPlayer class methodsFor: 'primitive test' stamp: 'jm 9/17/97 20:06'!
sineTable: size
	"Compute a sine table of the given size. Used for testing only."

	| radiansPerStep table |
	table _ Array new: size.
	radiansPerStep _ (2.0 * Float pi) / table size asFloat.
	1 to: table size do: [:i |
		table at: i put:
			(32767.0 * (radiansPerStep * i) sin) asInteger].

	^ table
! !

!SoundPlayer class methodsFor: 'accessing' stamp: 'jm 8/13/1998 15:00'!
bufferMSecs

	^ BufferMSecs
! !

!SoundPlayer class methodsFor: 'accessing' stamp: 'jm 1/27/98 09:28'!
reverbState

	^ ReverbState! !

!SoundPlayer class methodsFor: 'accessing'!
samplingRate

	^ SamplingRate! !

!SoundPlayer class methodsFor: 'accessing' stamp: 'JMM 11/6/2000 10:16'!
setVolumeLeft: aLeftVolume volumeRight: aRightVolume
	"Set sound pass in float 0.0-1.0 for left and right channel, with possible 2.0 or  higher to overdrive sound channel "
	self primSoundSetVolumeLeft: aLeftVolume volumeRight: aRightVolume! !

!SoundPlayer class methodsFor: 'accessing' stamp: 'JMM 11/6/2000 10:17'!
soundVolume
	"Return sound as array of doubles left then right channel, range is 0.0 to 1.0 but may be overdriven"
	^self primSoundGetVolume! !

!SoundPlayer class methodsFor: 'accessing'!
stereo

	^ Stereo
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'ar 2/1/2001 15:20'!
canStartPlayer
	"Some platforms do no support simultaneous record and play. If this is one of those platforms, return false if there is a running SoundRecorder."

	Preferences canRecordWhilePlaying ifTrue: [^ true].
	SoundRecorder anyActive ifTrue:[^false].
	^ true
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'di 8/5/1998 23:08'!
isPlaying: aSound
	^ ActiveSounds includes: aSound! !

!SoundPlayer class methodsFor: 'playing' stamp: 'jmv 3/2/2010 10:09'!
pauseSound: aSound
	"Stop playing the given sound. Playing can be resumed from this point later."

	PlayerSemaphore critical: [
		ActiveSounds remove: aSound ifAbsent: nil].
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'ar 2/19/2001 01:28'!
playSound: aSound
	"Reset and start playing the given sound from its beginning."

	aSound reset.
	aSound samplesRemaining = 0 ifTrue:[^self].
	self resumePlaying: aSound.
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'jm 9/8/1998 17:54'!
resumePlaying: aSound
	"Start playing the given sound without resetting it; it will resume playing from where it last stopped."
	"Implementation detail: On virtual machines that don't support the quickstart primitive, you may need to edit this method to pass false to resumePlaying:quickStart:."

	self resumePlaying: aSound quickStart: true.
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'jmv 3/1/2010 13:34'!
resumePlaying: aSound quickStart: quickStart
	"Start playing the given sound without resetting it; it will resume playing from where it last stopped. If quickStart is true, then try to start playing the given sound immediately."

	| doQuickStart |
	Preferences soundsEnabled ifFalse: [^ self].
	doQuickStart _ quickStart.
	Preferences soundQuickStart ifFalse: [doQuickStart _ false].
	PlayerProcess ifNil: [
		self canStartPlayer ifFalse: [^ self].
		^self startUpWithSound: aSound].

	PlayerSemaphore critical: [
		(ActiveSounds includes: aSound)
			ifTrue: [doQuickStart _ false]
			ifFalse: [
				doQuickStart ifFalse: [ActiveSounds add: aSound]]].

	"quick-start the given sound, unless the sound player has just started"
	doQuickStart ifTrue: [self startPlayingImmediately: aSound].
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'jm 1/27/98 09:47'!
stopPlayingAll
	"Stop playing all sounds."

	PlayerSemaphore critical: [
		ActiveSounds _ ActiveSounds species new].
! !

!SoundPlayer class methodsFor: 'playing' stamp: 'jm 9/13/97 19:49'!
waitUntilDonePlaying: aSound
	"Wait until the given sound is no longer playing."

	[PlayerSemaphore critical: [ActiveSounds includes: aSound]]
		whileTrue: [(Delay forMilliseconds: 100) wait].
! !

!SoundPlayer class methodsFor: 'initialization' stamp: 'jm 1/14/1999 13:13'!
initialize
	"SoundPlayer initialize; shutDown; startUp"
	"Details: BufferMSecs represents a tradeoff between latency and quality. If BufferMSecs is too low, the sound will not play smoothly, especially during long-running primitives such as large BitBlts. If BufferMSecs is too high, there will be a long time lag between when a sound buffer is submitted to be played and when that sound is actually heard. BufferMSecs is typically in the range 50-200."

	SamplingRate _ 22050.
	BufferMSecs _ 120.
	Stereo _ true.
	UseReverb ifNil: [UseReverb _ true].
! !

!SoundPlayer class methodsFor: 'initialization' stamp: 'ar 1/24/2002 18:40'!
useLastBuffer
	^LastBuffer notNil! !

!SoundPlayer class methodsFor: 'initialization' stamp: 'ar 1/24/2002 18:47'!
useLastBuffer: aBool
	Buffer ifNil:[^self].
	aBool 
		ifTrue:[LastBuffer _ SoundBuffer basicNew: Buffer basicSize]
		ifFalse:[LastBuffer _ nil]	! !

!SoundPlayer class methodsFor: 'initialization' stamp: 'jm 1/14/1999 13:14'!
useShortBuffer
	"Experimental support for real-time MIDI input. This only works on platforms whose hardware allows very short buffer sizes. It has been tested on a Macintosh Powerbook G3."
	"SoundPlayer useShortBuffer"

	self shutDown.
	BufferMSecs _ 15.
	SoundPlayer
		startPlayerProcessBufferSize: (BufferMSecs * SamplingRate) // 1000
		rate: SamplingRate
		stereo: Stereo.
! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 1/31/2001 01:32'!
isAllSilence: buffer size: count
	"return true if the buffer is all silence after reverb has ended"
	| value |
	value _ buffer at: 1.
	2 to: count do:[:i| (buffer at: i) = value ifFalse:[^false]].
	^true! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primSoundAvailableBytes
	"Return the number of bytes of available space in the sound output buffer."
	"Note: Squeak always uses buffers containing 4-bytes per sample (2 channels at 2 bytes per channel) regardless of the state of the Stereo flag."

	<primitive: 'primitiveSoundAvailableSpace' module: 'SoundPlugin'>
	^ self primitiveFailed
! !

!SoundPlayer class methodsFor: 'private' stamp: 'JMM 11/6/2000 10:17'!
primSoundGetVolume
	"Return sound as array of doubles left then right channel, range is 0.0 to 1.0 but may be overdriven"
	<primitive: 'primitiveSoundGetVolume' module: 'SoundPlugin'>
	^Array with: 1.0 with: 1.0! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primSoundInsertSamples: count from: aSoundBuffer samplesOfLeadTime: anInteger
	"Mix the given number of sample frames from the given sound buffer into the queue of samples that has already been submitted to the sound driver. This primitive is used to start a sound playing with minimum latency, even if large sound output buffers are being used to ensure smooth sound output. Returns the number of samples consumed, or zero if the primitive is not implemented or fails."

	<primitive: 'primitiveSoundInsertSamples' module: 'SoundPlugin'>
	^ 0
! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primSoundPlaySamples: count from: aSampleBuffer startingAt: index
	"Copy count bytes into the current sound output buffer from the given sample buffer starting at the given index."

	<primitive: 'primitiveSoundPlaySamples' module: 'SoundPlugin'>
	^ self primitiveFailed
! !

!SoundPlayer class methodsFor: 'private' stamp: 'JMM 11/6/2000 10:14'!
primSoundSetVolumeLeft: aLeftVolume volumeRight: aRightVolume
	"Set sound pass in float 0.0-1.0 for left and right channel, with possible 2.0 or  higher to overdrive sound channel "
	<primitive: 'primitiveSoundSetLeftVolume' module: 'SoundPlugin'>
! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primSoundStartBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag
	"Start double-buffered sound output with the given buffer size and sampling rate. This version has been superceded by primitive 171 (primSoundStartBufferSize:rate:stereo:semaIndex:)."
	"ar 12/5/1998 Turn off the sound if not supported"
	<primitive: 'primitiveSoundStart' module: 'SoundPlugin'>
	SoundSupported _ false.! !

!SoundPlayer class methodsFor: 'private' stamp: 'ar 2/2/2001 15:09'!
primSoundStartBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag semaIndex: anInteger
	"Start double-buffered sound output with the given buffer size and sampling rate. If the given semaphore index is > 0, it is taken to be the index of a Semaphore in the external objects array to be signalled when the sound driver is ready to accept another buffer of samples."
	"Details: If this primitive fails, this method tries to use the older version instead."

	<primitive: 'primitiveSoundStartWithSemaphore' module: 'SoundPlugin'>
	UseReadySemaphore _ false.
	self primSoundStartBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag.
! !

!SoundPlayer class methodsFor: 'private' stamp: 'tpr 2/2/2001 19:46'!
primSoundStop
	"Stop double-buffered sound output. Must not raise an error because it is used inside error handling and at system shutdown"

	<primitive: 'primitiveSoundStop' module: 'SoundPlugin'>! !

!SoundPlayer class methodsFor: 'private' stamp: 'jmv 3/1/2010 13:34'!
startPlayingImmediately: aSound
	"Private!! Start playing the given sound as soon as possible by mixing it into the sound output buffers of the underlying sound driver."

	| totalSamples buf n leftover src rest |
	"first, fill a double-size buffer with samples"
	"Note: The code below assumes that totalSamples contains two
	 buffers worth of samples, and the insertSamples primitive is
	 expected to consume at least one buffer's worth of these
	 samples. The remaining samples are guaranteed to fit into
	 a single buffer."
	totalSamples _ Buffer stereoSampleCount * 2.  "two buffer's worth"
	buf _ SoundBuffer newStereoSampleCount: totalSamples.
	aSound playSampleCount: totalSamples into: buf startingAt: 1.
	ReverbState ifNotNil: [
		ReverbState applyReverbTo: buf startingAt: 1 count: totalSamples].

	PlayerSemaphore critical: [
		"insert as many samples as possible into the sound driver's buffers"
		n _ self primSoundInsertSamples: totalSamples
			from: buf
			samplesOfLeadTime: 1024.
		n > 0 ifTrue:[
			leftover _ totalSamples - n.

			"copy the remainder of buf into Buffer"
			"Note: the following loop iterates over 16-bit words, not two-word stereo slices"
			"assert: 0 < leftover <= Buffer stereoSampleCount"
			src _ 2 * n.
			1 to: 2 * leftover do:
				[:dst | Buffer at: dst put: (buf at: (src _ src + 1))].

			"generate enough additional samples to finish filling Buffer"
			rest _ Buffer stereoSampleCount - leftover.
			aSound playSampleCount: rest into: Buffer startingAt: leftover + 1.
			ReverbState == nil ifFalse: [
				ReverbState applyReverbTo: Buffer startingAt: leftover + 1 count: rest].

			"record the fact that this sound has already been played into Buffer so that we don't process it again this time around"
			SoundJustStarted _ aSound.
		] ifFalse:[
			"quick start failed; reset the sound so we start over"
			aSound reset.
		].
		ActiveSounds add: aSound].
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jmv 3/1/2010 14:07'!
isReverbOn

	^ ReverbState notNil
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'ar 1/24/2002 18:41'!
lastPlayBuffer
	^LastBuffer! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jmv 3/1/2010 13:33'!
oldStylePlayLoop
	"This version of the play loop is used if the VM does not yet support sound primitives that signal a semaphore when a sound buffer becomes available."

	| bytesPerSlice count |
	bytesPerSlice _ Stereo ifTrue: [4] ifFalse: [2].
	[true] whileTrue: [
		[(count _ self primSoundAvailableBytes // bytesPerSlice) > 100]
			whileFalse: [(Delay forMilliseconds: 1) wait].

		count _ count min: Buffer stereoSampleCount.
		PlayerSemaphore critical: [
			ActiveSounds _ ActiveSounds select: [:snd | snd samplesRemaining > 0].
			ActiveSounds do: [:snd |
				snd ~~ SoundJustStarted ifTrue: [
					snd playSampleCount: count into: Buffer startingAt: 1]].
			ReverbState ifNotNil: [
				ReverbState applyReverbTo: Buffer startingAt: 1 count: count].
			self primSoundPlaySamples: count from: Buffer startingAt: 1.
			Buffer primFill: 0.
			SoundJustStarted _ nil]].
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jmv 3/1/2010 13:34'!
playLoop
	"The sound player process loop."

	| bytesPerSlice count willStop mayStop |
	mayStop _ Preferences soundStopWhenDone.
	bytesPerSlice _ Stereo ifTrue: [4] ifFalse: [2].
	[true] whileTrue: [
		[(count _ self primSoundAvailableBytes // bytesPerSlice) > 100]
			whileFalse: [ReadyForBuffer wait].

		count _ count min: Buffer stereoSampleCount.
		PlayerSemaphore critical: [
			ActiveSounds _ ActiveSounds select: [:snd | snd samplesRemaining > 0].
			ActiveSounds do: [:snd |
				snd ~~ SoundJustStarted ifTrue: [
					snd playSampleCount: count into: Buffer startingAt: 1]].
			ReverbState ifNotNil: [
				ReverbState applyReverbTo: Buffer startingAt: 1 count: count].
			self primSoundPlaySamples: count from: Buffer startingAt: 1.
			willStop _ mayStop and:[
						(ActiveSounds size = 0) and:[
							self isAllSilence: Buffer size: count]].
			LastBuffer ifNotNil:[
				LastBuffer replaceFrom: 1 to: LastBuffer size with: Buffer startingAt: 1.
			].
			willStop
				ifTrue:[self shutDown. PlayerProcess _ nil]
				ifFalse:[Buffer primFill: 0].
			SoundJustStarted _ nil].
		willStop ifTrue:[^self].
	].
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'nk 2/16/2001 13:26'!
playerProcess
	^PlayerProcess! !

!SoundPlayer class methodsFor: 'player process' stamp: 'ar 2/4/2001 18:01'!
startPlayerProcessBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag
	"Start the sound player process. Terminate the old process, if any."
	"SoundPlayer startPlayerProcessBufferSize: 1000 rate: 11025 stereo: false"
	^self startPlayerProcessBufferSize: bufferSize 
			rate: samplesPerSecond 
			stereo: stereoFlag 
			sound: nil! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jmv 10/8/2012 22:14'!
startPlayerProcessBufferSize: bufferSize rate: samplesPerSecond stereo: stereoFlag sound: aSound
	"Start the sound player process. Terminate the old process, if any."
	"SoundPlayer startPlayerProcessBufferSize: 1000 rate: 11025 stereo: false"

	self stopPlayerProcess.
	aSound
		ifNil:[ActiveSounds _ OrderedCollection new]
		ifNotNil:[ActiveSounds _ OrderedCollection with: aSound].
	Buffer _ SoundBuffer newStereoSampleCount: (bufferSize // 4) * 4.
	LastBuffer ifNotNil:[LastBuffer _ SoundBuffer basicNew: Buffer basicSize].
	PlayerSemaphore _ Semaphore forMutualExclusion.
	SamplingRate _ samplesPerSecond.
	Stereo _ stereoFlag.
	ReadyForBuffer _ Semaphore new.
	SoundSupported _ true. "Assume so"
	UseReadySemaphore _ true.  "set to false if ready semaphore not supported by VM"
	self primSoundStartBufferSize: Buffer stereoSampleCount
		rate: samplesPerSecond
		stereo: Stereo
		semaIndex: (Smalltalk registerExternalObject: ReadyForBuffer).
	"Check if sound start prim was successful"
	SoundSupported ifFalse:[^self].
	UseReadySemaphore
		ifTrue: [PlayerProcess _ [SoundPlayer playLoop] newProcess]
		ifFalse: [PlayerProcess _ [SoundPlayer oldStylePlayLoop] newProcess].
	UseReverb ifTrue: [self startReverb].

	PlayerProcess
		priority: Processor userInterruptPriority;
		name: 'Sound Player';
		resume! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jm 6/7/1999 10:40'!
startReverb
	"Start a delay-line style reverb with the given tap delays and gains. Tap delays are given in samples and should be prime integers; the following comment gives an expression that generates primes."
	"Integer primesUpTo: 22050"

	UseReverb _ true.
	ReverbState _ ReverbSound new
		tapDelays: #(1601 7919) gains: #(0.12 0.07).
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'ar 1/31/2001 01:13'!
stopPlayerProcess
	"Stop the sound player process."
	"SoundPlayer stopPlayerProcess"

	(PlayerProcess == nil or:[PlayerProcess == Processor activeProcess]) 
		ifFalse:[PlayerProcess terminate].
	PlayerProcess _ nil.
	self primSoundStop.
	ActiveSounds _ OrderedCollection new.
	Buffer _ nil.
	PlayerSemaphore _ Semaphore forMutualExclusion.
	ReadyForBuffer ifNotNil:
		[Smalltalk unregisterExternalObject: ReadyForBuffer].
	ReadyForBuffer _ nil.
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jm 1/27/98 09:43'!
stopReverb

	UseReverb _ false.
	ReverbState _ nil.
! !

!SoundRecorder methodsFor: 'private' stamp: 'di 2/17/1999 11:13'!
allocateBuffer
	"Allocate a new buffer and reset nextIndex."

	| bufferTime |
	bufferTime _ stereo  "Buffer time = 1/2 second"
		ifTrue: [self samplingRate asInteger]
		ifFalse: [self samplingRate asInteger // 2].
	currentBuffer _ SoundBuffer newMonoSampleCount:
		"Multiple of samplesPerFrame that is approx. bufferTime long"
		(bufferTime truncateTo: self samplesPerFrame).
	nextIndex _ 1.
! !

!SoundRecorder methodsFor: 'private' stamp: 'RAA 1/2/2001 10:17'!
emitBuffer: buffer

	| sound ratio resultBuf |

	"since some sound recording devices cannot (or will not) record below a certain sample rate,
	trim the samples down if the user really wanted fewer samples"

	(desiredSampleRate isNil or: [(ratio _ samplingRate // desiredSampleRate) <= 1]) ifTrue: [
		sound _ SampledSound new setSamples: buffer samplingRate: samplingRate.
	] ifFalse: [
		resultBuf _ SoundBuffer 
			averageEvery: ratio 
			from: buffer 
			upTo: buffer monoSampleCount.
		sound _ SampledSound new setSamples: resultBuf samplingRate: samplingRate / ratio.
	].

	recordedSound add: (codec ifNil: [sound] ifNotNil: [codec compressSound: sound])! !

!SoundRecorder methodsFor: 'private' stamp: 'di 2/17/1999 11:13'!
emitPartialBuffer
	| s |
	s _ self samplesPerFrame.
	self emitBuffer: (currentBuffer copyFrom: 1 to: ((nextIndex-1) +( s-1) truncateTo: s))! !

!SoundRecorder methodsFor: 'private' stamp: 'jm 9/2/97 16:16'!
meterFrom: start count: count in: buffer
	"Update the meter level with the maximum signal level in the given range of the given buffer."

	| last max sample |
	count = 0 ifTrue: [^ self].  "no new samples"
	last _ start + count - 1.
	max _ 0.
	start to: last do: [:i |
		sample _ buffer at: i.
		sample < 0 ifTrue: [sample _ sample negated].
		sample > max ifTrue: [max _ sample]].
	meterLevel _ max.
! !

!SoundRecorder methodsFor: 'private' stamp: 'di 2/16/1999 08:55'!
recordLoop
	"Record process loop that records samples."

	| n sampleCount |
	n _ 0.
	[true] whileTrue: [
		n = 0 ifTrue: [bufferAvailableSema wait].
		paused
			ifTrue: [
				n _ self primRecordSamplesInto: meteringBuffer startingAt: 1.
				self meterFrom: 1 count: n in: meteringBuffer]
			ifFalse: [
				n _ self primRecordSamplesInto: currentBuffer startingAt: nextIndex.
				self meterFrom: nextIndex count: n in: currentBuffer.
				nextIndex _ nextIndex + n.
				stereo
					ifTrue: [sampleCount _ currentBuffer stereoSampleCount]
					ifFalse: [sampleCount _ currentBuffer monoSampleCount].
				nextIndex > sampleCount
					ifTrue: [
						self emitBuffer: currentBuffer.
						self allocateBuffer]]].
! !

!SoundRecorder methodsFor: 'private' stamp: 'jmv 3/1/2010 13:35'!
samplesPerFrame
	"Can be overridden to quantize buffer size for, eg, fixed-frame codecs"

	^codec
		ifNil: [1]
		ifNotNil: [codec samplesPerFrame]! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'di 2/17/1999 10:54'!
clearRecordedSound
	"Clear the sound recorded thus far. Go into pause mode if currently recording."

	paused _ true.
	recordedSound _ SequentialSound new.
	self allocateBuffer.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'sw 6/10/2003 12:34'!
hasRecordedSound
	"Answer whether the receiver currently has any recorded sound"

	^ self recordedSound notNil! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jmv 3/1/2010 14:07'!
pause
	"Go into pause mode. The record level continues to be updated, but no sound is recorded."

	paused _ true.
	(currentBuffer notNil and: [nextIndex > 1])
		ifTrue: [
			self emitPartialBuffer.
			self allocateBuffer].

	soundPlaying ifNotNil: [
		soundPlaying pause.
		soundPlaying _ nil].
	"Note: there can be problems if canRecordWhilePlaying is true. Recorders which only pause will inhibit other recorders from recording. I chose to make #stopPlaying unconditional in a subclass. The same might be appropriate here at the expense of making recorders resumable"

	Preferences canRecordWhilePlaying ifFalse: [self stopRecording].
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jm 10/17/97 14:53'!
playback
	"Playback the sound that has been recorded."

	self pause.
	soundPlaying _ self recordedSound.
	soundPlaying play.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'RAA 8/13/2000 11:41'!
resumeRecording
	"Continue recording from the point at which it was last paused."

	self flag: #bob.
	"Note: If canRecordWhilePlaying is true, then recordings may never get started (at least by this method). One possibility, used in a subclass, is to make the #startPlaying unconditional. Another would be to use #startPlaying instead of #resumePlaying in appropriate cases"

	Preferences canRecordWhilePlaying ifFalse: [self startRecording].
	paused _ false.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jmv 10/8/2012 22:13'!
startRecording
	"Turn of the sound input driver and start the recording process. Initially, recording is paused."

	| semaIndex |
	recordLevel ifNil: [recordLevel _ 0.5].  "lazy initialization"
	Preferences canRecordWhilePlaying ifFalse: [SoundPlayer shutDown].
	recordProcess ifNotNil: [self stopRecording].
	paused _ true.
	meteringBuffer _ SoundBuffer newMonoSampleCount: 1024.
	meterLevel _ 0.
	self allocateBuffer.
	bufferAvailableSema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: bufferAvailableSema.
	self primStartRecordingDesiredSampleRate: samplingRate asInteger
		stereo: stereo
		semaIndex: semaIndex.
	RecorderActive _ true.
	samplingRate _ self primGetActualRecordingSampleRate.
	self primSetRecordLevel: (1000.0 * recordLevel) asInteger.
	recordProcess _ [self recordLoop] newProcess.
	recordProcess
		priority: Processor userInterruptPriority;
		name: 'Sound Recorder';
		resume.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jmv 3/1/2010 14:08'!
stopRecording
	"Stop the recording process and turn of the sound input driver."

	recordProcess ifNotNil: [recordProcess terminate].
	recordProcess _ nil.
	self primStopRecording.
	RecorderActive _ false.
	Smalltalk unregisterExternalObject: bufferAvailableSema.
	(currentBuffer notNil and: [nextIndex > 1])
		ifTrue: [self emitPartialBuffer].
	self initializeRecordingState.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jmv 2/22/2011 22:51'!
verifyExistenceOfRecordedSound
	"If the receiver has a recorded sound, answer true; if not, put up an informer and answer false"

	^ self recordedSound
		ifNotNil:
			[true]
		ifNil: [
			self inform: 'please record a sound first'.
			false]! !

!SoundRecorder methodsFor: 'accessing' stamp: 'di 2/17/1999 11:08'!
codec: aSoundCodec

	codec _ aSoundCodec! !

!SoundRecorder methodsFor: 'accessing' stamp: 'RAA 12/30/2000 10:28'!
desiredSampleRate: newRate

	"use of this method indicates a strong desire for the specified rate, even if
	the OS/hardware are not cooperative"

	desiredSampleRate _ samplingRate _ newRate  "Best are 44100 22050 11025"
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'jm 9/2/97 16:16'!
isPaused
	"Return true if recording is paused."

	^ paused
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'jm 9/18/97 19:19'!
meterLevel
	"Return the meter level, an integer in the range [0..100] where zero is silence and 100 represents the maximum signal level possible without clipping."

	^ (100 * meterLevel) // 32768
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'jm 7/4/1998 15:03'!
recordLevel

	^ recordLevel
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'jm 7/4/1998 15:04'!
recordLevel: level
	"Set the desired recording level to the given value in the range 0.0 to 1.0, where 0.0 is the lowest recording level and 1.0 is the maximum. Do nothing if the sound input hardware does not support changing the recording level."
	"Details: On the Macintosh, the lowest possible record level attenuates the input signal, but does not silence it entirely." 

	recordLevel _ (level asFloat min: 1.0) max: 0.0.
	recordProcess ifNotNil: [
		self primSetRecordLevel: (1000.0 * recordLevel) asInteger].
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'jm 12/15/97 14:28'!
samplingRate

	^ samplingRate
! !

!SoundRecorder methodsFor: 'accessing' stamp: 'di 2/16/1999 09:58'!
samplingRate: newRate

	samplingRate _ newRate  "Best are 44100 22050 11025"
! !

!SoundRecorder methodsFor: 'results' stamp: 'di 3/4/1999 21:40'!
condensedSamples
	"Return a single SoundBuffer that is the contatenation of all my recorded buffers."

	| sz newBuf i |
	recordedBuffers _ recordedSound sounds collect: [:snd | snd samples].
	recordedBuffers isEmpty ifTrue: [^ SoundBuffer new: 0].
	recordedBuffers size = 1 ifTrue: [^ recordedBuffers first copy].
	sz _ recordedBuffers inject: 0 into: [:tot :buff | tot + buff size].
	newBuf _ SoundBuffer newMonoSampleCount: sz.
	i _ 1.
	recordedBuffers do: [:b |
		1 to: b size do: [:j |
			newBuf at: i put: (b at: j).
			i _ i + 1]].
	recordedBuffers _ nil.
	^ newBuf
! !

!SoundRecorder methodsFor: 'results' stamp: 'di 2/16/1999 20:49'!
condensedStereoSound
	"Decompose my buffers into left and right channels and return a mixed sound consisting of the those two channels. This may be take a while, since the data must be copied into new buffers."

	| sz leftBuf rightBuf leftI rightI left |
	sz _ recordedBuffers inject: 0 into: [:tot :buff | tot + buff size].
	leftBuf _ SoundBuffer newMonoSampleCount: (sz + 1) // 2.
	rightBuf _ SoundBuffer newMonoSampleCount: (sz + 1) // 2.
	leftI _ rightI _ 1.
	left _ true.
	recordedBuffers do: [:b |
		1 to: b size do: [:j |
			left
				ifTrue: [leftBuf at: leftI put: (b at: j). leftI _ leftI + 1. left _ false]
				ifFalse: [rightBuf at: rightI put: (b at: j). rightI _ rightI + 1. left _ true]]].
	^ MixedSound new
		add: (SampledSound new setSamples: leftBuf samplingRate: samplingRate) pan: 0.0;
		add: (SampledSound new setSamples: rightBuf samplingRate: samplingRate) pan: 1.0
! !

!SoundRecorder methodsFor: 'results' stamp: 'di 2/17/1999 11:07'!
recordedSound
	"Return the sound that was recorded."

	^ recordedSound
! !

!SoundRecorder methodsFor: 'results' stamp: 'di 2/17/1999 21:24'!
soundSegments

	^ self segmentsAbove: 1000 normalizedVolume: 80.0
! !

!SoundRecorder methodsFor: 'trimming' stamp: 'jm 10/17/97 17:43'!
copyFrom: startPlace to: endPlace normalize: nFactor dcOffset: dcOffset
	"Return a new SoundBuffer containing the samples in the given range."

	| startBufIndex startSampleIndex endBufIndex endSampleIndex
	 count resultBuf j buf firstInBuf n |
	startBufIndex _ startPlace at: 1.
	startSampleIndex _ startPlace at: 2.
	endBufIndex _ endPlace at: 1.
	endSampleIndex _ endPlace at: 2.

	startBufIndex = endBufIndex
		ifTrue: [count _ endSampleIndex + 1 - startSampleIndex]
		ifFalse: [
			count _ ((recordedBuffers at: startBufIndex) size + 1 - startSampleIndex).  "first buffer"
			count _ count + endSampleIndex.  "last buffer"
			startBufIndex + 1 to: endBufIndex - 1 do:
				[:i | count _ count + (recordedBuffers at: i) size]].  "middle buffers"
	resultBuf _ SoundBuffer newMonoSampleCount: count.

	j _ 1.  "next destination index in resultBuf"
	startBufIndex to: endBufIndex do: [:i |
		buf _ recordedBuffers at: i.
		firstInBuf _ 1.
	 	n _ buf size.
		i = startBufIndex ifTrue: [
			n _ (recordedBuffers at: startBufIndex) size + 1 - startSampleIndex.
			firstInBuf _ startSampleIndex].
		i = endBufIndex ifTrue: [
			i = startBufIndex
				ifTrue: [n _ endSampleIndex + 1 - startSampleIndex]
				ifFalse: [n _ endSampleIndex]].
		self copyTo: resultBuf from: j to: (j + n - 1)
			from: buf startingAt: firstInBuf
			normalize: nFactor dcOffset: dcOffset.
		j _ j + n].
	^ resultBuf
! !

!SoundRecorder methodsFor: 'trimming' stamp: 'jm 10/18/97 11:23'!
copyTo: resultBuf from: startIndex to: endIndex from: buf startingAt: firstInBuf normalize: nFactor dcOffset: dcOffset
	"Copy samples from buf to resultBuf removing the DC offset and normalizing their volume in the process."

	| indexOffset |
	indexOffset _ firstInBuf - startIndex.
	startIndex to: endIndex do: [:i |
		resultBuf at: i put: (((buf at: (i + indexOffset)) - dcOffset) * nFactor) // 1000].
! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 2/16/1999 22:11'!
endPlace

	^ Array with: recordedBuffers size with: recordedBuffers last size! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 2/16/1999 22:11'!
firstSampleOverThreshold: threshold dcOffset: dcOffset startingAt: startPlace
	"Beginning at startPlace, this routine will return the first place at which a sample exceeds the given threshold."

	| buf s iStart jStart nThreshold |
	nThreshold _ threshold negated.
	iStart _ startPlace first.
	jStart _ startPlace second.
	iStart to: recordedBuffers size do:
		[:i | buf _ recordedBuffers at: i.
		jStart to: buf size do:
			[:j | s _ (buf at: j) - dcOffset.
			(s < nThreshold or: [s > threshold]) ifTrue:
				["found a sample over threshold"
				^ Array with: i with: j]].
		jStart _ 1].
	^ self endPlace! !

!SoundRecorder methodsFor: 'trimming' stamp: 'jm 10/18/97 11:22'!
normalizeFactorFor: percentOfMaxVolume min: min max: max dcOffset: dcOffset
	"Return a normalization factor for the range of sample values and DC offset. A normalization factor is a fixed-point number that will be divided by 1000 after multiplication with each sample value."

	| peak factor |
	peak _ (max - dcOffset) max: (min - dcOffset) negated.
	peak = 0 ifTrue: [^ 1000].
	factor _ (32767.0 * percentOfMaxVolume) / (100.0 * peak).
	^ (factor * 1000.0) asInteger
! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 2/16/1999 23:01'!
place: startPlace plus: nSamples
	"Return the place that is nSamples (may be negative) beyond thisPlace."

	| i j remaining buf |
	i _ startPlace first.
	j _ startPlace second.
	nSamples >= 0
	ifTrue: [remaining _ nSamples.
			[buf _ recordedBuffers at: i.
			(j + remaining) <= buf size ifTrue: [^ Array with: i with: j + remaining].
			i < recordedBuffers size]
				whileTrue: [remaining _ remaining - (buf size - j + 1).
							i _ i+1.  j _ 1].
			^ self endPlace]
	ifFalse: [remaining _ nSamples negated.
			[buf _ recordedBuffers at: i.
			(j - remaining) >= 1 ifTrue: [^ Array with: i with: j - remaining].
			i > 1]
				whileTrue: [remaining _ remaining - j.
							i _ i-1.  j _ (recordedBuffers at: i) size].
			^ #(1 1)]! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 2/16/1999 23:19'!
scanForEndThreshold: threshold dcOffset: dcOffset minLull: lull startingAt: startPlace
	"Beginning at startPlace, this routine will find the last sound that exceeds threshold, such that if you look lull samples later you will not find another sound over threshold within the following block of lull samples.
	Return the place that is lull samples beyond to that last sound.
	If no end of sound is found, return endPlace."

	| buf s iStart jStart nThreshold n |
	nThreshold _ threshold negated.
	iStart _ startPlace first.
	jStart _ startPlace second.
	n _ 0.
	iStart to: recordedBuffers size do:
		[:i | buf _ recordedBuffers at: i.
		jStart to: buf size do:
			[:j | s _ (buf at: j) - dcOffset.
			(s < nThreshold or: [s > threshold])
				ifTrue: ["found a sample over threshold"
						n _ 0]
				ifFalse: ["still not over threshold"
						n _ n + 1.
						n >= lull ifTrue: [^ Array with: i with: j]]].
		jStart _ 1].
	^ self endPlace! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 3/4/1999 22:13'!
scanForStartThreshold: threshold dcOffset: dcOffset minDur: duration startingAt: startPlace
	"Beginning at startPlace, this routine will find the first sound that exceeds threshold, such that if you look duration samples later you will find another sound over threshold within the following block of duration samples.
	Return the place that is duration samples prior to that first sound.
	If no sound is found, return endPlace."

	| soundPlace lookPlace nextSoundPlace thirdPlace |
	soundPlace _ self firstSampleOverThreshold: threshold dcOffset: dcOffset
					startingAt: startPlace.
	[soundPlace = self endPlace ifTrue: [^ soundPlace].
	"Found a sound -- look duration later"
	lookPlace _ self place: soundPlace plus: duration.
	nextSoundPlace _ self firstSampleOverThreshold: threshold dcOffset: dcOffset
					startingAt: lookPlace.
	thirdPlace _ self place: lookPlace plus: duration.
	nextSoundPlace first < thirdPlace first
		or: [nextSoundPlace first = thirdPlace first
			and: [nextSoundPlace second < thirdPlace second]]]
		whileFalse: [soundPlace _ nextSoundPlace].

	"Yes, there is sound in the next interval as well"
	^ self place: soundPlace plus: 0-duration
! !

!SoundRecorder methodsFor: 'trimming' stamp: 'jmv 3/1/2010 13:35'!
segmentsAbove: threshold normalizedVolume: percentOfMaxVolume
	"Break the current recording up into a sequence of sound segments separated by silences."

	| max min sum totalSamples bufSize s dcOffset firstPlace endPlace resultBuf nFactor lastPlace segments gapSize minDur minLull soundSize restSize |
	stereo ifTrue: [self error: 'stereo trimming is not yet supported'].
	paused ifFalse: [self error: 'must stop recording before trimming'].
	(recordedSound == nil or: [recordedSound sounds isEmpty]) ifTrue:[^ self].
	"Reconstruct buffers so old trimming code will work"
	recordedBuffers _ recordedSound sounds collect: [:snd | snd samples].
	soundSize _ restSize _ 0.

	max _ min _ sum _ totalSamples _ 0.
	recordedBuffers do: [:buf |
		bufSize _ buf size.
		totalSamples _ totalSamples + buf size.
		1 to: bufSize do: [:i |
			s _ buf at: i.
			s > max ifTrue: [max _ s].
			s < min ifTrue: [min _ s].
			sum _ sum + s]].
	dcOffset _ sum // totalSamples.

	minDur _ (samplingRate/20.0) asInteger.  " 1/20 second "
	minLull _ (samplingRate/4.0) asInteger.  " 1/2 second "
	segments _ SequentialSound new.
	endPlace _ self endPlace.
	lastPlace _ #(1 1).
	[firstPlace _ self scanForStartThreshold: threshold
						dcOffset: dcOffset
						minDur: minDur
						startingAt: lastPlace.
	firstPlace = endPlace]
		whileFalse:
		[firstPlace = lastPlace ifFalse:
			["Add a silence equal to the gap size"
			"Wasteful but simple way to get gap size..."
			gapSize _ (self copyFrom: lastPlace to: firstPlace
						normalize: 1000 dcOffset: dcOffset) size - 2.
			"... -2 makes up for overlap of one sample on either end"
			segments add: (RestSound dur: gapSize asFloat / samplingRate).
			restSize _ restSize + gapSize.
"Transcript cr; print: firstPlace; space; print: lastPlace; space; print: gapSize; space; show: 'gap'."
			].
		lastPlace _ self scanForEndThreshold: threshold
						dcOffset: dcOffset
						minLull: minLull + minDur
						startingAt: firstPlace.
		"Allow room for lead time of next sound"
		lastPlace _ self place: lastPlace plus: minDur negated.
		nFactor _ self normalizeFactorFor: percentOfMaxVolume
						min: min max: max dcOffset: dcOffset.
		resultBuf _ self copyFrom: firstPlace to: lastPlace
						normalize: nFactor dcOffset: dcOffset.
		soundSize _ soundSize + resultBuf size.
"Transcript cr; print: firstPlace; space; print: lastPlace; space; print: resultBuf size; space; show: 'sound'."
		segments add: (codec
			ifNil: [SampledSound new setSamples: resultBuf samplingRate: samplingRate]
			ifNotNil: [codec compressSound: (SampledSound new setSamples: resultBuf samplingRate: samplingRate)])].

	"Final gap for consistency"
	gapSize _ (self copyFrom: lastPlace to: self endPlace
				normalize: 1000 dcOffset: dcOffset) size - 1.
	segments add: (RestSound dur: gapSize asFloat / samplingRate).
	restSize _ restSize + gapSize.
	self inform: ((soundSize+restSize/samplingRate) roundTo: 0.1) printString , ' secs reduced to ' , ((soundSize/samplingRate) roundTo: 0.1) printString.
	recordedBuffers _ nil.
	^ segments! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 2/17/1999 20:38'!
suppressSilence

	recordedSound _ self soundSegments! !

!SoundRecorder methodsFor: 'trimming' stamp: 'di 3/4/1999 22:52'!
trim: threshold normalizedVolume: percentOfMaxVolume
	"Remove the leading and trailing parts of this recording that are below the given threshold. Remove any DC offset and scale the recording so that its peaks are the given percent of the maximum volume."

	| max min sum totalSamples bufSize s dcOffset startPlace endPlace resultBuf nFactor |
	stereo ifTrue: [self error: 'stereo trimming is not yet supported'].
	paused ifFalse: [self error: 'must stop recording before trimming'].
	recordedBuffers _ recordedSound sounds collect: [:snd | snd samples].
	recordedBuffers isEmpty ifTrue: [^ self].

	max _ min _ sum _ totalSamples _ 0.
	recordedBuffers do: [:buf |
		bufSize _ buf size.
		totalSamples _ totalSamples + buf size.
		1 to: bufSize do: [:i |
			s _ buf at: i.
			s > max ifTrue: [max _ s].
			s < min ifTrue: [min _ s].
			sum _ sum + s]].
	dcOffset _ sum // totalSamples.

	"a place is an array of <buffer index><index of sample in buffer>"
	startPlace _ self scanForStartThreshold: threshold
					dcOffset: dcOffset
					minDur: (samplingRate/60.0) asInteger "at least 1/60th of a second"
					startingAt: #(1 1).
	startPlace = self endPlace ifTrue:
		["no samples above threshold"
		recordedBuffers _ nil.  ^ self].

	endPlace _ self scanForEndThreshold: threshold
					dcOffset: dcOffset
					minLull: (samplingRate/5) asInteger
					startingAt: startPlace.
	nFactor _ self normalizeFactorFor: percentOfMaxVolume min: min max: max dcOffset: dcOffset.
	resultBuf _ self copyFrom: startPlace to: endPlace normalize: nFactor dcOffset: dcOffset.
	recordedSound _ SampledSound new setSamples: resultBuf samplingRate: samplingRate.
	recordedBuffers _ nil
! !

!SoundRecorder methodsFor: 'initialization' stamp: 'jm 4/22/1999 14:30'!
initialize
	"SoundRecorder new"

	stereo _ false.
	samplingRate _ 11025.
	recordLevel _ 0.5.
	self initializeRecordingState.
! !

!SoundRecorder methodsFor: 'initialization' stamp: 'jhm 10/15/97 14:30'!
initializeRecordingState

	recordProcess _ nil.
	bufferAvailableSema _ nil.
	paused _ true.
	meteringBuffer _ nil.
	meterLevel _ 0.
	soundPlaying _ nil.
	currentBuffer _ nil.
	nextIndex _ 1.
! !

!SoundRecorder methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primGetActualRecordingSampleRate
	"Return the actual sample rate being used for recording. This primitive fails unless sound recording is currently in progress."

	<primitive: 'primitiveSoundGetRecordingSampleRate' module: 'SoundPlugin'>
	self primitiveFailed
! !

!SoundRecorder methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primRecordSamplesInto: aWordArray startingAt: index
	"Record a sequence of 16-bit sound samples into the given array starting at the given sample index. Return the number of samples recorded, which may be zero if no samples are currently available."

	<primitive: 'primitiveSoundRecordSamples' module: 'SoundPlugin'>
	self primitiveFailed
! !

!SoundRecorder methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primSetRecordLevel: anInteger
	"Set the desired recording level to the given value in the range 0-1000, where 0 is the lowest recording level and 1000 is the maximum. Do nothing if the sound input hardware does not support changing the recording level."

	<primitive: 'primitiveSoundSetRecordLevel' module: 'SoundPlugin'>
	self primitiveFailed
! !

!SoundRecorder methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primStartRecordingDesiredSampleRate: samplesPerSec stereo: stereoFlag semaIndex: anInteger
	"Start sound recording with the given stereo setting. Use a sampling rate as close to the desired rate as the underlying platform will support. If the given semaphore index is > 0, it is taken to be the index of a Semaphore in the external objects array to be signalled every time a recording buffer is filled."

	<primitive: 'primitiveSoundStartRecording' module: 'SoundPlugin'>
	self primitiveFailed
! !

!SoundRecorder methodsFor: 'primitives' stamp: 'tpr 2/15/2001 17:13'!
primStopRecording
	"Stop sound recording. Does nothing if recording is not currently in progress. Do not fail if plugin is not available"

	<primitive: 'primitiveSoundStopRecording' module: 'SoundPlugin'>! !

!SoundRecorder class methodsFor: 'accessing' stamp: 'ar 2/1/2001 15:20'!
anyActive
	"Return true if any sound recorder is actively recording"
	^RecorderActive == true! !

!SoundRecorder class methodsFor: 'accessing' stamp: 'RAA 8/7/2000 19:23'!
canRecordWhilePlaying
	"Return true if this platform supports simultaneous sound recording and playback."

	^Preferences canRecordWhilePlaying.		"now in preferences"
! !

!SoundRecorder class methodsFor: 'class initialization' stamp: 'RAA 8/7/2000 19:23'!
initialize
	"SoundRecorder initialize"
	"Details: Some computers cannot record and playback sound at the same time. If CanRecordWhilePlaying is false, then the SoundRecorder alternates between recording and playing. If it is true, sounds can be playing during recording."

	CanRecordWhilePlaying _ #ignoredNowInPreferences.
! !

!SoundInputStream methodsFor: 'private' stamp: 'jm 9/8/1999 15:24'!
allocateBuffer
	"Allocate a new buffer and reset nextIndex. This message is sent by the sound input process."

	currentBuffer _ SoundBuffer newMonoSampleCount: bufferSize.
	nextIndex _ 1.
! !

!SoundInputStream methodsFor: 'private' stamp: 'jm 9/8/1999 15:24'!
emitBuffer: buffer
	"Queue a buffer for later processing. This message is sent by the sound input process."

	mutex critical: [recordedBuffers addLast: buffer].
! !

!SoundInputStream methodsFor: 'accessing' stamp: 'jm 9/8/1999 15:26'!
bufferCount
	"Answer the number of sound buffers that have been queued."

	| n |
	mutex ifNil: [^ 0].  "not recording"
	mutex critical: [n _ recordedBuffers size].
	^ n
! !

!SoundInputStream methodsFor: 'accessing' stamp: 'jm 9/6/1999 10:36'!
bufferSize

	^ bufferSize
! !

!SoundInputStream methodsFor: 'accessing' stamp: 'jm 9/8/1999 15:26'!
bufferSize: aNumber
	"Set the sound buffer size. Buffers of this size will be queued for the client to process."

	bufferSize _ aNumber truncated.
! !

!SoundInputStream methodsFor: 'accessing' stamp: 'jmv 3/1/2010 14:07'!
isRecording
	"Answer true if the sound input process is running."

	^ recordProcess notNil
! !

!SoundInputStream methodsFor: 'accessing' stamp: 'jm 9/6/1999 10:32'!
nextBufferOrNil
	"Answer the next input buffer or nil if no buffer is available."

	| result |
	mutex ifNil: [^ nil].  "not recording"
	mutex critical: [
		recordedBuffers size > 0
			ifTrue: [result _ recordedBuffers removeFirst]
			ifFalse: [result _ nil]].
	^ result
! !

!SoundInputStream methodsFor: 'initialization' stamp: 'jm 9/8/1999 15:22'!
initialize

	super initialize.
	bufferSize _ 1024.
	mutex _ nil.
! !

!SoundInputStream methodsFor: 'recording controls' stamp: 'jm 9/8/1999 15:23'!
startRecording
	"Start the sound input process."

	recordProcess ifNotNil: [self stopRecording].
	recordedBuffers _ OrderedCollection new: 100.
	mutex _ Semaphore forMutualExclusion.
	super startRecording.
	paused _ false.
! !

!SoundInputStream methodsFor: 'recording controls' stamp: 'jm 9/8/1999 15:23'!
stopRecording
	"Turn off the sound input process and close the driver."

	super stopRecording.
	recordedBuffers _ nil.
	mutex _ nil.
! !

!AbstractScoreEvent methodsFor: 'accessing' stamp: 'di 6/17/1999 14:28'!
adjustTimeBy: delta

	time _ time + delta
! !

!AbstractScoreEvent methodsFor: 'accessing' stamp: 'jm 8/27/1998 16:38'!
endTime
	"Subclasses should override to return the ending time if the event has some duration."

	^ time
! !

!AbstractScoreEvent methodsFor: 'accessing' stamp: 'jm 12/31/97 11:43'!
time

	^ time
! !

!AbstractScoreEvent methodsFor: 'accessing' stamp: 'jm 12/31/97 11:43'!
time: aNumber

	time _ aNumber.
! !

!AbstractScoreEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:43'!
isControlChange

	^ false
! !

!AbstractScoreEvent methodsFor: 'classification' stamp: 'jm 12/31/97 11:46'!
isNoteEvent

	^ false
! !

!AbstractScoreEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:43'!
isPitchBend

	^ false
! !

!AbstractScoreEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:43'!
isProgramChange

	^ false
! !

!AbstractScoreEvent methodsFor: 'classification' stamp: 'jm 12/31/97 11:46'!
isTempoEvent

	^ false
! !

!AbstractScoreEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 18:31'!
outputOnMidiPort: aMidiPort
	"Output this event to the given MIDI port. This default implementation does nothing."
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel

	^ channel
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel: midiChannel

	channel _ midiChannel.
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:49'!
control

	^ control
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:48'!
control: midiControl

	control _ midiControl.
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 08:03'!
control: midiControl value: midiControlValue channel: midiChannel

	control _ midiControl.
	value _ midiControlValue.
	channel _ midiChannel.
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 08:02'!
value

	^ value
! !

!ControlChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 08:03'!
value: midiControlValue

	value _ midiControlValue.
! !

!ControlChangeEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:45'!
isControlChange

	^ true
! !

!ControlChangeEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 18:31'!
outputOnMidiPort: aMidiPort
	"Output this event to the given MIDI port."

	aMidiPort
		midiCmd: 16rB0
		channel: channel
		byte: control
		byte: value.
! !

!NoteEvent methodsFor: 'printing' stamp: 'jm 1/3/98 08:58'!
keyName
	"Return a note name for my pitch."

	| pitchName octave |
	pitchName _ #(c cs d ef e f fs g af a bf b) at: (midiKey \\ 12) + 1.
	octave _ (#(-1 0 1 2 3 4 5 6 7 8 9) at: (midiKey // 12) + 1) printString.
	^ pitchName, octave
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/30/97 22:02'!
channel

	^ channel
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/30/97 22:02'!
channel: midiChannel

	channel _ midiChannel.
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/18/97 19:10'!
duration

	^ duration
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/31/97 11:49'!
duration: aNumber

	duration _ aNumber.
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 8/27/1998 16:38'!
endTime

	^ time + duration
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/30/97 22:07'!
key: midiKeyNum velocity: midiVelocity channel: midiChannel

	midiKey _ midiKeyNum.
	velocity _ midiVelocity.
	channel _ midiChannel.
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/18/97 20:58'!
midiKey

	^ midiKey
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/30/97 09:35'!
midiKey: midiKeyNum

	midiKey _ midiKeyNum.
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 8/3/1998 17:06'!
pitch
	"Convert my MIDI key number to a pitch and return it."

	^ AbstractSound pitchForMIDIKey: midiKey
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/30/97 09:32'!
velocity

	^ velocity
! !

!NoteEvent methodsFor: 'accessing' stamp: 'jm 12/31/97 11:51'!
velocity: midiVelocity

	velocity _ midiVelocity.
! !

!NoteEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 15:58'!
endNoteOnMidiPort: aMidiPort
	"Output a noteOff event to the given MIDI port. (Actually, output a noteOff event with zero velocity. This does the same thing, but allows running status to be used when sending a mixture of note on and off commands.)"

	aMidiPort
		midiCmd: 16r90
		channel: channel
		byte: midiKey
		byte: 0.
! !

!NoteEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 15:56'!
startNoteOnMidiPort: aMidiPort
	"Output a noteOn event to the given MIDI port."

	aMidiPort
		midiCmd: 16r90
		channel: channel
		byte: midiKey
		byte: velocity.
! !

!NoteEvent methodsFor: 'classification' stamp: 'jm 12/31/97 11:48'!
isNoteEvent

	^ true
! !

!PitchBendEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:49'!
bend

	^ bend
! !

!PitchBendEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:48'!
bend: midiPitchBend

	bend _ midiPitchBend.
! !

!PitchBendEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:48'!
bend: midiPitchBend channel: midiChannel

	bend _ midiPitchBend.
	channel _ midiChannel.
! !

!PitchBendEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel

	^ channel
! !

!PitchBendEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel: midiChannel

	channel _ midiChannel.
! !

!PitchBendEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:45'!
isPitchBend

	^ true
! !

!PitchBendEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 18:31'!
outputOnMidiPort: aMidiPort
	"Output this event to the given MIDI port."

	aMidiPort
		midiCmd: 16rE0
		channel: channel
		byte: (bend bitAnd: 16r7F)
		byte: (bend bitShift: -7).
! !

!ProgramChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel

	^ channel
! !

!ProgramChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:47'!
channel: midiChannel

	channel _ midiChannel.
! !

!ProgramChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:49'!
program

	^ program
! !

!ProgramChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:48'!
program: midiProgramChange

	program _ midiProgramChange.
! !

!ProgramChangeEvent methodsFor: 'accessing' stamp: 'jm 9/10/1998 07:48'!
program: midiProgramChange channel: midiChannel

	program _ midiProgramChange.
	channel _ midiChannel.
! !

!ProgramChangeEvent methodsFor: 'classification' stamp: 'jm 9/10/1998 09:46'!
isProgramChange

	^ true
! !

!ProgramChangeEvent methodsFor: 'midi' stamp: 'jm 9/10/1998 18:31'!
outputOnMidiPort: aMidiPort
	"Output this event to the given MIDI port."

	aMidiPort
		midiCmd: 16rC0
		channel: channel
		byte: program.
! !

!TempoEvent methodsFor: 'as yet unclassified' stamp: 'jm 12/30/97 10:10'!
isTempoEvent

	^ true
! !

!TempoEvent methodsFor: 'as yet unclassified' stamp: 'jm 12/30/97 10:09'!
tempo

	^ tempo
! !

!TempoEvent methodsFor: 'as yet unclassified' stamp: 'jm 12/30/97 10:09'!
tempo: anInteger

	tempo _ anInteger.
! !

!MIDIFileReader methodsFor: 'chunk reading' stamp: 'jm 1/6/98 23:20'!
asScore

	^ MIDIScore new
		tracks: tracks;
		trackInfo: trackInfo;
		tempoMap: tempoMap;
		ticksPerQuarterNote: ticksPerQuarter
! !

!MIDIFileReader methodsFor: 'chunk reading' stamp: 'jmv 3/13/2012 12:37'!
readHeaderChunk

	| chunkType chunkSize division |
	chunkType _ self readChunkType.
	chunkType = 'RIFF' ifTrue:[chunkType _ self riffSkipToMidiChunk].
	chunkType = 'MThd' ifFalse: [self scanForMIDIHeader].
	chunkSize _ self readChunkSize.
	fileType _ self next16BitWord.
	trackCount _ self next16BitWord.
	division _ self next16BitWord.
	(division anyMask: 16r8000)
		ifTrue: [self error: 'SMPTE time formats are not yet supported']
		ifFalse: [ticksPerQuarter _ division].
	maxNoteTicks _ 12 * 4 * ticksPerQuarter.
		"longest acceptable note; used to detect stuck notes"

	"sanity checks"
	((chunkSize < 6) or: [chunkSize > 100])
		ifTrue: [self error: 'unexpected MIDI header size ', chunkSize printString].
	(#(0 1 2) includes: fileType)
		ifFalse: [self error: 'unknown MIDI file type ', fileType printString].

	Transcript
		show: 'Reading Type ', fileType printString, ' MIDI File (';
		show: trackCount printString, ' tracks, ';
		show: ticksPerQuarter printString, ' ticks per quarter note)';
		newLine.
! !

!MIDIFileReader methodsFor: 'chunk reading' stamp: 'jm 9/12/1998 19:08'!
readMIDIFrom: aBinaryStream
	"Read one or more MIDI tracks from the given binary stream."

	stream _ aBinaryStream.
	tracks _ OrderedCollection new.
	trackInfo _ OrderedCollection new.
	self readHeaderChunk.
	trackCount timesRepeat: [self readTrackChunk].
	stream atEnd ifFalse: [self report: 'data beyond final track'].
	fileType = 0 ifTrue: [self splitIntoTracks].
	self guessMissingInstrumentNames.
! !

!MIDIFileReader methodsFor: 'chunk reading' stamp: 'jm 12/31/97 10:41'!
readTrackChunk

	| chunkType chunkSize |
	chunkType _ self readChunkType.
	[chunkType = 'MTrk'] whileFalse: [
		self report: 'skipping unexpected chunk type "', chunkType, '"'.
		stream skip: (self readChunkSize).  "skip it"
		chunkType _ (stream next: 4) asString].
	chunkSize _ self readChunkSize.
	chunkSize < 10000000 ifFalse: [
		self error: 'suspiciously large track chunk; this may not be MIDI file'].

	self readTrackContents: chunkSize.
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jm 3/28/98 05:44'!
endAllNotesAt: endTicks
	"End of score; end any notes still sounding."
	"Details: Some MIDI files have missing note-off events, resulting in very long notes. Truncate any such notes encountered."

	| dur |
	activeEvents do: [:e |
		dur _ endTicks - e time.
		dur > maxNoteTicks ifTrue: [dur _ ticksPerQuarter].  "truncate long note"
		e duration: dur].
	activeEvents _ activeEvents species new.
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jmv 3/2/2010 09:56'!
endNote: midiKey chan: channel at: endTicks

	| evt |
	evt _ activeEvents
		detect: [:e | (e midiKey = midiKey) and: [e channel = channel]]
		ifNone: [^ self].
	evt duration: (endTicks - evt time).
	activeEvents remove: evt ifAbsent: nil! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jm 1/3/98 09:45'!
isTempoTrack: anEventList
	"Return true if the given event list is non-empty and contains only tempo change events."

	anEventList isEmpty ifTrue: [^ false].
	anEventList do: [:evt | evt isTempoEvent ifFalse: [^ false]].
	^ true
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jm 9/27/1998 22:15'!
metaEventAt: ticks
	"Read a meta event. Event types appear roughly in order of expected frequency."

	| type length tempo |
	type _ trackStream next.
	length _ self readVarLengthIntFrom: trackStream.

	type = 16r51 ifTrue: [  "tempo"
		tempo _ 0.
		length timesRepeat: [tempo _ (tempo bitShift: 8) + trackStream next].
		track add: (TempoEvent new tempo: tempo; time: ticks).
		^ self].

	type = 16r2F ifTrue: [  "end of track"
		length = 0 ifFalse: [self error: 'length of end-of-track chunk should be zero'].
		self endAllNotesAt: ticks.
		trackStream skip: length.
		^ self].

	type = 16r58 ifTrue: [  "time signature"
		length = 4 ifFalse: [self error: 'length of time signature chunk should be four'].
		trackStream skip: length.
		^ self].

	type = 16r59 ifTrue: [  "key signature"
		length = 2 ifFalse: [self error: 'length of key signature chunk should be two'].
		trackStream skip: length.
		^ self].

	((type >= 1) and: [type <= 7]) ifTrue: [  "string"
		strings add: (trackStream next: length) asString.
		^ self].

	(  type = 16r21 or:   "mystery; found in MIDI files but not in MIDI File 1.0 Spec"
	 [(type = 16r7F) or:  "sequencer specific meta event"
	 [(type = 16r00) or:  "sequence number"
	 [(type = 16r20)]]])  "MIDI channel prefix"
		ifTrue: [
			trackStream skip: length.
			^ self].

	type = 16r54 ifTrue: [
		"SMPTE offset"
		self report: 'Ignoring SMPTE offset'.
		trackStream skip: length.
		^ self].

	"skip unrecognized meta event"
	self report:
		'skipping unrecognized meta event: ', (type printStringBase: 16),
		' (', length printString, ' bytes)'.
	trackStream skip: length.
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jmv 3/13/2012 12:37'!
readTrackContents: byteCount

	| info |
	strings _ OrderedCollection new.
	track _ OrderedCollection new.
	trackStream _ ReadStream on: (stream next: byteCount).
	activeEvents _ OrderedCollection new.
	self readTrackEvents.
	(tracks isEmpty and: [self isTempoTrack: track])
		ifTrue: [tempoMap _ track asArray]
		ifFalse: [
			"Note: Tracks without note events are currently not saved to
			 eliminate clutter in the score player. In control applications,
			 this can be easily changed by modifying the following test."
			(self trackContainsNotes: track) ifTrue: [
				tracks add: track asArray.
				info _ WriteStream on: (String new: 100).
				strings do: [:s | info nextPutAll: s; newLine].
				trackInfo add: info contents]].
	strings _ track _ trackStream _ activeEvents _ nil.
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jm 9/10/1998 09:57'!
readTrackEvents
	"Read the events of the current track."

	| cmd chan key vel ticks byte length evt |
	cmd _ #unknown.
	chan _ key _ vel _ 0.
	ticks _ 0.
	[trackStream atEnd] whileFalse: [
		ticks _ ticks + (self readVarLengthIntFrom: trackStream).
		byte _ trackStream next.
		byte >= 16rF0
			ifTrue: [  "meta or system exclusive event"
				byte = 16rFF ifTrue: [self metaEventAt: ticks].
				((byte = 16rF0) or: [byte = 16rF7]) ifTrue: [  "system exclusive data"
					length _ self readVarLengthIntFrom: trackStream.
					trackStream skip: length].
				cmd _ #unknown]
			ifFalse: [  "channel message event"
				byte >= 16r80
					ifTrue: [  "new command"
						cmd _ byte bitAnd: 16rF0.
						chan _ byte bitAnd: 16r0F.
						key _ trackStream next]
					ifFalse: [  "use running status"
						cmd == #unknown
							ifTrue: [self error: 'undefined running status; bad MIDI file?'].
						key _ byte].

				((cmd = 16rC0) or: [cmd = 16rD0]) ifFalse: [
					"all but program change and channel pressure have two data bytes"
					vel _ trackStream next].

				cmd = 16r80 ifTrue: [  "note off"
					self endNote: key chan: chan at: ticks].

				cmd = 16r90 ifTrue: [  "note on"
					vel = 0
						ifTrue: [self endNote: key chan: chan at: ticks]
						ifFalse: [self startNote: key vel: vel chan: chan at: ticks]].

				"cmd = 16A0 -- polyphonic key pressure; skip"

				cmd = 16rB0 ifTrue: [
					evt _ ControlChangeEvent new control: key value: vel channel: chan.
					evt time: ticks.
					track add: evt].

				cmd = 16rC0 ifTrue: [
					evt _ ProgramChangeEvent new program: key channel: chan.
					evt time: ticks.
					track add: evt].

				"cmd = 16D0 -- channel aftertouch pressure; skip"

				cmd = 16rE0 ifTrue: [
					evt _ PitchBendEvent new bend: key + (vel bitShift: 7) channel: chan.
					evt time: ticks.
					track add: evt]
	]].
! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jmv 3/2/2010 09:56'!
startNote: midiKey vel: vel chan: chan at: startTicks
	"Record the beginning of a note."
	"Details: Some MIDI scores have missing note-off events, causing a note-on to be received for a (key, channel) that is already sounding. If the previous note is suspiciously long, truncate it."

	| newActiveEvents dur noteOnEvent |
	newActiveEvents _ nil.
	activeEvents do: [:e |
		((e midiKey = midiKey) and: [e channel = chan]) ifTrue: [
			"turn off key already sounding"
			dur _ startTicks - e time.
			dur > maxNoteTicks ifTrue: [dur _ ticksPerQuarter].  "truncate"
			e duration: dur.
			newActiveEvents ifNil: [newActiveEvents _ activeEvents copy].
			newActiveEvents remove: e ifAbsent: nil]].
	newActiveEvents ifNotNil: [activeEvents _ newActiveEvents].

	noteOnEvent _ NoteEvent new key: midiKey velocity: vel channel: chan.
	noteOnEvent time: startTicks.
	track add: noteOnEvent.
	activeEvents add: noteOnEvent! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jm 9/12/1998 17:15'!
trackContainsNotes: eventList
	"Answer true if the given track contains at least one note event."

	eventList do: [:e | e isNoteEvent ifTrue: [^ true]].
	^ false
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jmv 3/2/2010 09:56'!
guessMissingInstrumentNames
	"Attempt to guess missing instrument names from the first program change in that track."

	| progChange instrIndex instrName |
	1 to: tracks size do: [:i |
		(trackInfo at: i) isEmpty ifTrue: [
			progChange _ (tracks at: i) detect: [:e | e isProgramChange] ifNone: nil.
			progChange ifNotNil: [
				instrIndex _ progChange program + 1.
				instrName _ self class standardMIDIInstrumentNames at: instrIndex.
				trackInfo at: i put: instrName]]].
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 12/31/97 10:30'!
next16BitWord
	"Read a 16-bit positive integer from the input stream, most significant byte first."
	"Assume: Stream has at least two bytes left."

	| n |
	n _ stream next.
	^ (n bitShift: 8) + stream next
! !

!MIDIFileReader methodsFor: 'private' stamp: 'ar 1/27/98 17:27'!
next32BitWord: msbFirst
	"Read a 32-bit positive integer from the input stream."
	"Assume: Stream has at least four bytes left."

	| n |
	n _ stream next: 4.
	^msbFirst
		ifTrue:[((n at: 1) bitShift: 24) + ((n at: 2) bitShift: 16) + ((n at: 3) bitShift: 8) + (n at: 4)]
		ifFalse:[((n at: 4) bitShift: 24) + ((n at: 3) bitShift: 16) + ((n at: 2) bitShift: 8) + (n at: 1)]
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 12/31/97 10:29'!
readChunkSize
	"Read a 32-bit positive integer from the next 4 bytes, most significant byte first."
	"Assume: Stream has at least four bytes left."

	| n |
	n _ 0.
	1 to: 4 do: [:ignore | n _ (n bitShift: 8) + stream next].
	^ n
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 9/12/1998 17:32'!
readChunkType
	"Read a chunk ID string from the next 4 bytes."
	"Assume: Stream has at least four bytes left."

	| s |
	s _ String new: 4.
	1 to: 4 do: [:i | s at: i put: (stream next) asCharacter].
	^ s
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 12/31/97 11:33'!
readVarLengthIntFrom: aBinaryStream
	"Read a one to four byte positive integer from the given stream, most significant byte first. Use only the lowest seven bits of each byte. The highest bit of a byte is set for all bytes except the last."

	| n byte |
	n _ 0.
	1 to: 4 do: [:ignore |
		byte _ aBinaryStream next.
		byte < 128 ifTrue: [
			n = 0
				ifTrue: [^ byte]  "optimization for one-byte lengths"
				ifFalse: [^ (n bitShift: 7) + byte]].
		n _ (n bitShift: 7) + (byte bitAnd: 16r7F)].

	self error: 'variable length quantity must not exceed four bytes'.
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jmv 3/13/2012 12:37'!
report: aString

	Transcript show: aString; newLine! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 2/1/98 19:20'!
riffSkipToMidiChunk
	"This file is a RIFF file which may (or may not) contain a MIDI chunk. Thanks to Andreas Raab for this code."

	| dwLength fourcc |
	"Read length of all data"
	dwLength := self next32BitWord: false.
	"Get RIFF contents type "
	fourcc := self readChunkType.
	fourcc = 'RMID' ifFalse:[^fourcc]. "We can only read RMID files here"
	"Search for data"
	[[fourcc := self readChunkType.
	dwLength := self next32BitWord: false.
	fourcc = 'data'] whileFalse:[
		"Skip chunk - rounded to word boundary"
		stream skip: (dwLength + 1 bitAnd: 16rFFFFFFFE).
		stream atEnd ifTrue:[^'']].
	"Data chunk is raw - look into if it contains MIDI data and skip if not"
	fourcc := self readChunkType.
	fourcc = 'MThd'] whileFalse:[
		"Skip data (chunk - 4bytes) rounded to word boundary"
		stream skip: (dwLength - 3 bitAnd: 16rFFFFFFFE)].
	^fourcc! !

!MIDIFileReader methodsFor: 'private' stamp: 'jmv 3/1/2010 14:05'!
scanForMIDIHeader
	"Scan the first part of this file in search of the MIDI header string 'MThd'. Report an error if it is not found. Otherwise, leave the input stream positioned to the first byte after this string."

	| asciiM p lastSearchPosition byte restOfHeader |
	asciiM _ $M asciiValue.
	stream skip: -3.
	p _ stream position.
	lastSearchPosition _ p + 10000.  "search only the first 10000 bytes of the file"
	[p < lastSearchPosition and: [stream atEnd not]] whileTrue: [
		[(byte _ stream next) ~= asciiM and: [byte notNil]] whileTrue.  "find the next 'M' or file end"
		restOfHeader _ (stream next: 3) asString.
		restOfHeader = 'Thd'
			ifTrue: [^ self]
			ifFalse: [restOfHeader size = 3 ifTrue: [stream skip: -3]].
		p _ stream position].

	self error: 'MIDI header chunk not found'.
! !

!MIDIFileReader methodsFor: 'private' stamp: 'jm 9/12/1998 20:10'!
splitIntoTracks
	"Split a type zero MIDI file into separate tracks by channel number."

	| newTempoMap newTracks |
	tracks size = 1 ifFalse: [self error: 'expected exactly one track in type 0 file'].
	tempoMap ifNotNil: [self error: 'did not expect a tempo map in type 0 file'].
	newTempoMap _ OrderedCollection new.
	newTracks _ (1 to: 16) collect: [:i | OrderedCollection new].
	tracks first do: [:e |
		e isTempoEvent
			ifTrue: [newTempoMap addLast: e]
			ifFalse: [(newTracks at: e channel + 1) addLast: e]].
	newTempoMap size > 0 ifTrue: [tempoMap _ newTempoMap asArray].
	newTracks _ newTracks select: [:t | self trackContainsNotes: t].
	tracks _ newTracks collect: [:t | t asArray].
	trackInfo _ trackInfo, ((2 to: tracks size) collect: [:i | '']).
! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'jmv 4/1/2009 21:42'!
playFileNamed: fileName 
"
	ScorePlayerMorph openOn: (self scoreFromFileNamed: fileName)
		title: (FileDirectory localNameFor: fileName)"! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'jmv 4/1/2009 21:43'!
playStream: binaryStream 
"
	ScorePlayerMorph openOn: (self scoreFromStream: binaryStream)
		title: 'a MIDI stream'"! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'jmv 4/1/2009 21:43'!
playURLNamed: urlString 
"
	| titleString |
	titleString := urlString copyFrom: (urlString findLast: [:c | c = $/]) + 1
				to: urlString size.
	ScorePlayerMorph openOn: (self scoreFromURL: urlString)
		title: titleString"! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'pb 5/25/2016 01:51'!
scoreFromFileNamed: fileName

	| f score |
	f _ (fileName asFileEntry readStream) binary.
	score _ (self new readMIDIFrom: f) asScore.
	f close.
	^ score
! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'ls 8/8/1998 03:14'!
scoreFromStream: binaryStream

	|  score |
	score _ (self new readMIDIFrom: binaryStream) asScore.
	^ score
! !

!MIDIFileReader class methodsFor: 'as yet unclassified' stamp: 'jm 9/12/1998 19:57'!
standardMIDIInstrumentNames
	"Answer an array of Standard MIDI instrument names."

	^ #(
		'Grand Piano'
		'Bright Piano'
		'Electric Grand Piano'
		'Honky-tonk Piano'
		'Electric Piano 1'
		'Electric Piano 2'
		'Harpsichord'
		'Clavichord'
		'Celesta'
		'Glockenspiel'
		'Music Box'
		'Vibraphone'
		'Marimba'
		'Xylophone'
		'Tubular Bells'
		'Duclimer'
		'Drawbar Organ'
		'Percussive Organ'
		'Rock Organ'
		'Church Organ'
		'Reed Organ'
		'Accordion'
		'Harmonica'
		'Tango Accordion'
		'Nylon Guitar'
		'Steel Guitar'
		'Electric Guitar 1'
		'Electric Guitar 2'
		'Electric Guitar 3'
		'Overdrive Guitar'
		'Distorted Guitar'
		'Guitar Harmonics'
		'Acoustic Bass'
		'Electric Bass 1'
		'Electric Bass 2'
		'Fretless Bass'
		'Slap Bass 1'
		'Slap Bass 2'
		'Synth Bass 1'
		'Synth Bass 2'
		'Violin'
		'Viola'
		'Cello'
		'Contrabass'
		'Tremolo Strings'
		'Pizzicato Strings'
		'Orchestral Harp'
		'Timpani'
		'String Ensemble 1'
		'String Ensemble 2'
		'Synth Strings 1'
		'Synth Strings 2'
		'Choir Ahhs'
		'Choir Oohs'
		'Synth Voice'
		'Orchestra Hit'
		'Trumpet'
		'Trombone'
		'Tuba'
		'Muted Trumpet'
		'French Horn'
		'Brass Section'
		'Synth Brass 1'
		'Synth Brass 2'
		'Soprano Sax'
		'Alto Sax'
		'Tenor Sax'
		'Baritone Sax'
		'Oboe'
		'English Horn'
		'Bassoon'
		'Clarinet'
		'Piccolo'
		'Flute'
		'Recorder'
		'Pan Flute'
		'Blown Bottle'
		'Shakuhachi'
		'Whistle'
		'Ocarina'
		'Lead 1 (square)'
		'Lead 2 (sawtooth)'
		'Lead 3 (calliope)'
		'Lead 4 (chiff)'
		'Lead 5 (charang)'
		'Lead 6 (voice)'
		'Lead 7 (fifths)'
		'Lead 8 (bass+lead)'
		'Pad 1 (new age)'
		'Pad 2 (warm)'
		'Pad 3 (polysynth)'
		'Pad 4 (choir)'
		'Pad 5 (bowed)'
		'Pad 6 (metallic)'
		'Pad 7 (halo)'
		'Pad 8 (sweep)'
		'FX 1 (rain)'
		'FX 2 (soundtrack)'
		'FX 3 (crystals)'
		'FX 4 (atmosphere)'
		'FX 5 (brightness)'
		'FX 6 (goblins)'
		'FX 7 (echoes)'
		'FX 8 (sci-fi)'
		'Sitar'
		'Banjo'
		'Shamisen'
		'Koto'
		'Kalimba'
		'Bagpipe'
		'Fiddle'
		'Shanai'
		'Tinkle Bell'
		'Agogo'
		'Steel Drum'
		'Woodblock'
		'Taiko Drum'
		'Melodic Tom'
		'Synth Drum'
		'Reverse Cymbal'
		'Guitar Fret Noise'
		'Breath Noise'
		'Seashore'
		'Bird Tweet'
		'Telephone Ring'
		'Helicopter'
		'Applause'
		'Gunshot')
! !

!MIDIInputParser methodsFor: 'recording' stamp: 'jmv 4/17/2013 12:05'!
clearBuffers
	"Clear the MIDI record buffers. This should be called at the start of recording or real-time MIDI processing."	

	received _ received species new: 5000.
	rawDataBuffer _ ByteArray new: 1000.
	sysExBuffer _ WriteStream on: (ByteArray new: 100).
	midiPort ifNotNil: [midiPort ensureOpen; flushInput].
	startTime _ Time localMillisecondClock.
	state _ #idle.
! !

!MIDIInputParser methodsFor: 'recording' stamp: 'jm 10/8/1998 21:06'!
processMIDIData
	"Process all MIDI data that has arrived since the last time this method was executed. This method should be called frequently to process, filter, and timestamp MIDI data as it arrives."

	| bytesRead |
	[(bytesRead _ midiPort readInto: rawDataBuffer) > 0] whileTrue: [
		timeNow _ (midiPort bufferTimeStampFrom: rawDataBuffer) - startTime.
		5 to: bytesRead do: [:i | self processByte: (rawDataBuffer at: i)]].
! !

!MIDIInputParser methodsFor: 'recording' stamp: 'jm 10/8/1998 20:24'!
received
	"Answer my current collection of all MIDI commands received. Items in this list have the form (<time><cmd byte>[<arg1>[<arg2>]]). Note that the real-time processing facility, midiDo:, removes items from this list as it processes them."

	^ received
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 18:34'!
endSysExclusive: cmdByte
	"Error!! Received 'end system exclusive' command when not receiving system exclusive data."

	self error: 'unexpected ''End of System Exclusive'' command'.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 19:24'!
ignoreOne: cmdByte
	"Ignore a one argument command."	

	lastCmdByte _ cmdByte.
	lastSelector _ #ignoreOne:.
	state _ #ignore1.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 19:24'!
ignoreTwo: cmdByte
	"Ignore a two argument command."	

	lastCmdByte _ cmdByte.
	lastSelector _ #ignoreTwo:.
	state _ #ignore2.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/9/1998 07:45'!
ignoreZero: cmdByte
	"Ignore a zero argument command, such as tune request or a real-time message. Stay in the current and don't change active status. Note that real-time messages can arrive between data bytes without disruption."	

	"do nothing"
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/9/1998 09:36'!
processByte: aByte
	"Process the given incoming MIDI byte and record completed commands."
	"Details: Because this must be fast, it has been hand-tuned. Be careful!!"

	aByte > 247 ifTrue: [  "real-time message; can arrive at any time"
		^ self perform: (cmdActionTable at: aByte) with: aByte].

	#idle = state ifTrue: [
		aByte >= 128
			ifTrue: [  "command byte in idle state: start new command"
				^ self perform: (cmdActionTable at: aByte) with: aByte]
			ifFalse: [  "data byte in idle state: use running status if possible"
				lastCmdByte ifNil: [^ self].  "running status unknown; skip byte"
				"process this data as if it had the last command byte in front of it"
				 self perform: lastSelector with: lastCmdByte.

				"the previous line put us into a new state; we now 'fall through'
				 to process the data byte given this new state."]].

	#ignore1 = state ifTrue: [^ state _ #idle].
	#ignore2 = state ifTrue: [^ state _ #ignore1].

	#want1of2 = state ifTrue: [
		argByte1 _ aByte.
		^ state _ #want2of2].

	#want2of2 = state ifTrue: [
		argByte2 _ aByte.
		received addLast: (Array with: timeNow with: lastCmdByte with: argByte1 with: argByte2).
		^ state _ #idle].

	#want1only = state ifTrue: [
		argByte1 _ aByte.
		received addLast: (Array with: timeNow with: lastCmdByte with: argByte1).
		^ state _ #idle].

	#sysExclusive = state ifTrue: [
		aByte < 128 ifTrue: [
			"record a system exclusive data byte"
			ignoreSysEx ifFalse: [sysExBuffer nextPut: aByte].
			^ self]
		ifFalse: [
			aByte < 248 ifTrue: [
				"a system exclusive message is terminated by any non-real-time command byte"
				ignoreSysEx ifFalse: [
					received addLast: (Array with: timeNow with: lastCmdByte with: sysExBuffer contents)].
				state _ #idle.
				aByte = 247
					ifTrue: [^ self]							"endSysExclusive command, nothing left to do"
					ifFalse: [^ self processByte: aByte]]]].  	"no endSysExclusive; just start the next command"
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 19:24'!
recordOne: cmdByte
	"Record a one argument command at the current time."	

	lastCmdByte _ cmdByte.
	lastSelector _ #recordOne:.
	state _ #want1only.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 19:24'!
recordTwo: cmdByte
	"Record a two argument command at the current time."	

	lastCmdByte _ cmdByte.
	lastSelector _ #recordTwo:.
	state _ #want1of2.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/9/1998 07:43'!
recordZero: cmdByte
	"Record a zero-byte message, such as tune request or a real-time message. Don't change active status. Note that real-time messages can arrive between data bytes without disruption."	

	received addLast: (Array with: timeNow with: cmdByte).
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/9/1998 09:38'!
startSysExclusive: cmdByte
	"The beginning of a variable length 'system exclusive' command."

	sysExBuffer resetContents.
	lastCmdByte _ nil.  "system exclusive commands clear running status"
	lastSelector _ nil.
	state _ #sysExclusive.
! !

!MIDIInputParser methodsFor: 'private-state machine' stamp: 'jm 10/8/1998 17:12'!
undefined: cmdByte
	"We have received an unexpected MIDI byte (e.g., a data byte when we were expecting a command). This should never happen."

	self error: 'unexpected MIDI byte ', cmdByte printString.
! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jm 10/8/1998 20:39'!
ignoreChannel: channel
	"Don't record any events arriving on the given MIDI channel (in the range 1-16)."

	((channel isInteger not) | (channel < 1) | (channel > 16))
		ifTrue: [^ self error: 'bad MIDI channel number', channel printString].

	"two-arg channel messages"
	#(128 144 160 176 224) do: [:i | cmdActionTable at: (i bitOr: channel - 1) put: #ignoreTwo:].

	"one-arg channel messages"
	#(192 208) do: [:i | cmdActionTable at: (i bitOr: channel - 1) put: #ignoreOne:].
! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jm 10/8/1998 20:40'!
ignoreCommand: midiCmd
	"Don't record the given MIDI command on any channel."

	| cmd sel | 
	((midiCmd isInteger not) | (midiCmd < 128) | (midiCmd > 255))
		ifTrue: [^ self error: 'bad MIDI command'].

	midiCmd < 240 ifTrue: [  "channel commands; ignore on all channels"
		cmd _ midiCmd bitAnd: 2r11110000.
		sel _ (#(128 144 160 176 224) includes: cmd)
			ifTrue: [#ignoreTwo:]
			ifFalse: [#ignoreOne:].
		 1 to: 16 do: [:ch | cmdActionTable at: (cmd bitOr: ch - 1) put: sel].
		^ self].

	(#(240 241 244 245 247 249 253) includes: midiCmd) ifTrue: [
		^ self error: 'You can''t ignore the undefined MIDI command: ', midiCmd printString].

	midiCmd = 242 ifTrue: [  "two-arg command"
		cmdActionTable at: midiCmd put: #ignoreTwo:.
		 ^ self].

	midiCmd = 243 ifTrue: [  "one-arg command"
		cmdActionTable at: midiCmd put: #ignoreOne:.
		^ self].

	(#(246 248 250 251 252 254 255) includes: midiCmd) ifTrue:	[  "zero-arg command"
		cmdActionTable at: midiCmd put: #ignore.
		 ^ self].

	"we should not get here"
	self error: 'implementation error'.
! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jm 10/8/1998 20:38'!
ignoreSysEx: aBoolean
	"If the argument is true, then ignore incoming system exclusive message."

	ignoreSysEx _ aBoolean.
! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jm 10/9/1998 07:46'!
ignoreTuneAndRealTimeCommands
	"Ignore tuning requests and real-time commands."

	cmdActionTable at: 246 put: #ignoreZero:.	"tune request"
	cmdActionTable at: 248 put: #ignoreZero:.	"timing clock"
	cmdActionTable at: 250 put: #ignoreZero:.	"start"
	cmdActionTable at: 251 put: #ignoreZero:.		"continue"
	cmdActionTable at: 252 put: #ignoreZero:.	"stop/Clock"
	cmdActionTable at: 254 put: #ignoreZero:.	"active sensing"
	cmdActionTable at: 255 put: #ignoreZero:.	"system reset"
! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jmv 5/20/2011 11:14'!
noFiltering
	"Revert to accepting all MIDI commands on all channels. This undoes any earlier request to filter the incoming MIDI stream."

	cmdActionTable _ DefaultMidiTable copy.
	ignoreSysEx _ false! !

!MIDIInputParser methodsFor: 'midi filtering' stamp: 'jm 10/9/1998 07:50'!
recordOnlyChannels: channelList
	"Record only MIDI data arriving on the given list of channel numbers (in the range 1-16)."

	channelList do: [:ch |
		((ch isInteger not) | (ch < 1) | (ch > 16))
			ifTrue: [^ self error: 'bad Midi channel specification: ', ch printString]].

	1 to: 16 do: [:ch | (channelList includes: ch) ifFalse: [self ignoreChannel: ch]].
! !

!MIDIInputParser methodsFor: 'real-time processing' stamp: 'jm 10/9/1998 07:53'!
midiDo: aBlock
	"Poll the incoming MIDI stream in real time and call the given block for each complete command that has been received. The block takes one argument, which is an array of the form (<time><cmd byte>[<arg1>[<arg2>]]). The number of arguments depends on the command byte. For system exclusive commands, the argument is a ByteArray containing the system exclusive message."

	self processMIDIData.
	[received isEmpty] whileFalse:
		[aBlock value: received removeFirst].
! !

!MIDIInputParser methodsFor: 'real-time processing' stamp: 'jmv 1/14/2013 21:13'!
midiDoUntilMouseDown: midiActionBlock
	"Process the incoming MIDI stream in real time by calling midiActionBlock for each MIDI event. This block takes three arguments: the MIDI command byte and two argument bytes. One or both argument bytes may be nil, depending on the MIDI command. If not nil, evaluatue idleBlock regularly whether MIDI data is available or not. Pressing any mouse button terminates the interaction."

	| time cmd arg1 arg2 |
	self clearBuffers.
	[ Sensor isAnyButtonPressed ] whileFalse: [
		self midiDo: [ :item |
			time _ item at: 1.
			cmd _ item at: 2.
			arg1 _ arg2 _ nil.
			item size > 2 ifTrue: [
				arg1 _ item at: 3.
				item size > 3 ifTrue: [arg2 _ item at: 4]].
				midiActionBlock value: cmd value: arg1 value: arg2]].
! !

!MIDIInputParser methodsFor: 'accessing' stamp: 'jm 1/6/1999 08:25'!
midiPort

	^ midiPort
! !

!MIDIInputParser methodsFor: 'accessing' stamp: 'jm 1/6/1999 08:24'!
midiPort: aMIDIPort
	"Use the given MIDI port."

	midiPort _ aMIDIPort.
	self clearBuffers.
! !

!MIDIInputParser methodsFor: 'midi monitor' stamp: 'jm 10/8/1998 21:22'!
monitor
	"Print MIDI messages to the transcript until any mouse button is pressed."

	self midiDoUntilMouseDown: [:cmd :arg1 :arg2 |
		self printCmd: cmd with: arg1 with: arg2].
! !

!MIDIInputParser methodsFor: 'midi monitor' stamp: 'jmv 3/13/2012 12:38'!
printCmd: cmdByte with: arg1 with: arg2
	"Print the given MIDI command."

	| cmd ch bend |
	cmdByte < 240
		ifTrue: [  "channel message" 
			cmd _ cmdByte bitAnd: 2r11110000.
			ch _ (cmdByte bitAnd: 2r00001111) + 1]
		ifFalse: [cmd _ cmdByte].  "system message"

	cmd = 128 ifTrue: [
		^ Transcript show: ('key up ', arg1 printString, ' vel: ', arg2 printString, ' chan: ', ch printString); newLine].
	cmd = 144 ifTrue: [
		^ Transcript show: ('key down: ', arg1 printString, ' vel: ', arg2 printString, ' chan: ', ch printString); newLine].
	cmd = 160 ifTrue: [
		^ Transcript show: ('key pressure: ', arg1 printString, ' val: ', arg2 printString, ' chan: ', ch printString); newLine].
	cmd = 176 ifTrue: [
		^ Transcript show: ('CC', arg1 printString, ': val: ', arg2 printString, ' chan: ', ch printString); newLine].
	cmd = 192 ifTrue: [
		^ Transcript show: ('prog: ', (arg1 + 1) printString, ' chan: ', ch printString); newLine].
	cmd = 208 ifTrue: [
		^ Transcript show: ('channel pressure ', arg1 printString, ' chan: ', ch printString); newLine].
	cmd = 224 ifTrue: [
		bend _ ((arg2 bitShift: 7) + arg1) - 8192.
		^ Transcript show: ('bend: ', bend printString, ' chan: ', ch printString); newLine].

	cmd = 240 ifTrue: [
		^ Transcript show: ('system exclusive: ', (arg1 at: 1) printString, ' (', arg1 size printString, ' bytes)'); newLine].

	Transcript show: 'cmd: ', cmd printString, ' arg1: ', arg1 printString, ' arg2: ', arg2 printString; newLine.
! !

!MIDIInputParser methodsFor: 'private-other' stamp: 'jm 10/9/1998 07:56'!
setMIDIPort: aMIDIPort
	"Initialize this instance for recording from the given MIDI port. Tune and real-time commands are filtered out by default; the client can send noFiltering to receive these messages."

	midiPort _ aMIDIPort.
	received _ OrderedCollection new.
	self noFiltering.  "initializes cmdActionTable"
	self ignoreTuneAndRealTimeCommands.
! !

!MIDIInputParser class methodsFor: 'class initialization' stamp: 'jm 10/9/1998 07:35'!
initialize
	"Build the default MIDI command-byte action table. This table maps MIDI command bytes to the action to be performed when that is received. Note that MIDI data bytes (bytes whose value is < 128) are never used to index into this table."
	"MIDIInputParser initialize"

	DefaultMidiTable _ Array new: 255 withAll: #undefined:.
	128 to: 143 do: [:i | DefaultMidiTable at: i put: #recordTwo:].		"key off"
	144 to: 159 do: [:i | DefaultMidiTable at: i put: #recordTwo:].		"key on"
	160 to: 175 do: [:i | DefaultMidiTable at: i put: #recordTwo:].		"polyphonic after-touch"
	176 to: 191 do: [:i | DefaultMidiTable at: i put: #recordTwo:].		"control change"
	192 to: 207 do: [:i | DefaultMidiTable at: i put: #recordOne:].		"program change"
	208 to: 223 do: [:i | DefaultMidiTable at: i put: #recordOne:].		"channel after-touch"
	224 to: 239 do: [:i | DefaultMidiTable at: i put: #recordTwo:].		"pitch bend"

	DefaultMidiTable at: 240 put: #startSysExclusive:.		"start a system exclusive block"
	DefaultMidiTable at: 241 put: #recordOne:.			"MIDI time code quarter frame"
	DefaultMidiTable at: 242 put: #recordTwo:.			"song position select"
	DefaultMidiTable at: 243 put: #recordOne:.			"song select"
	DefaultMidiTable at: 244 put: #undefined:.
	DefaultMidiTable at: 245 put: #undefined:.
	DefaultMidiTable at: 246 put: #recordZero:.			"tune request"
	DefaultMidiTable at: 247 put: #endSysExclusive:.		"end a system exclusive block"
	DefaultMidiTable at: 248 put: #recordZero:.			"timing clock"
	DefaultMidiTable at: 249 put: #undefined:.
	DefaultMidiTable at: 250 put: #recordZero:.			"start"
	DefaultMidiTable at: 251 put: #recordZero:.			"continue"
	DefaultMidiTable at: 252 put: #recordZero:.			"stop/Clock"
	DefaultMidiTable at: 253 put: #undefined:.
	DefaultMidiTable at: 254 put: #recordZero:.			"active sensing"
	DefaultMidiTable at: 255 put: #recordZero:.			"system reset"
! !

!MIDIInputParser class methodsFor: 'instance creation' stamp: 'jm 10/8/1998 20:29'!
on: aSimpleMIDIPort
	"Answer a new MIDI parser on the given port."

	^ super new setMIDIPort: aSimpleMIDIPort
! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/20/1999 00:08'!
appendEvent: noteEvent fullDuration: fullDuration at: selection
	"It is assumed that the noteEvent already has the proper time"

	| track noteLoc |
	track _ tracks at: selection first.
	noteLoc _ selection third + 1.
	noteEvent midiKey = -1
		ifTrue: [noteLoc _ noteLoc - 1]
		ifFalse: ["If not a rest..."
				track _ track copyReplaceFrom: noteLoc to: noteLoc - 1
								with: (Array with: noteEvent)].
	track size >= (noteLoc + 1) ifTrue:
		["Adjust times of following events"
		noteLoc + 1 to: track size do:
			[:i | (track at: i) adjustTimeBy: fullDuration]].
	tracks at: selection first put: track! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/17/1999 15:12'!
cutSelection: selection

	| track selStartTime delta |
	track _ tracks at: selection first.
	selStartTime _ (track at: selection second) time.
	track _ track copyReplaceFrom: selection second to: selection third with: Array new.
	track size >=  selection second ifTrue:
		["Adjust times of following events"
		delta _ selStartTime - (track at: selection second) time.
		selection second to: track size do:
			[:i | (track at: i) adjustTimeBy: delta]].
	tracks at: selection first put: track! !

!MIDIScore methodsFor: 'editing' stamp: 'jm 9/10/1998 17:22'!
eventForTrack: trackIndex after: eventIndex ticks: scoreTick

	| track evt |
	track _ tracks at: trackIndex.
	eventIndex > track size ifTrue: [^ nil].
	evt _ track at: eventIndex.
	evt time > scoreTick ifTrue: [^ nil].
	^ evt
! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/17/1999 16:06'!
gridToNextQuarterNote: tickTime

	^ self gridToQuarterNote: tickTime + ticksPerQuarterNote! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/17/1999 14:55'!
gridToQuarterNote: tickTime

	^ tickTime truncateTo: ticksPerQuarterNote! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/17/1999 16:14'!
gridTrack: trackIndex toQuarter: quarterDelta at: indexInTrack

	| track selStartTime delta |
	track _ tracks at: trackIndex.
	selStartTime _ (track at: indexInTrack) time.
	delta _ (self gridToQuarterNote: selStartTime + (quarterDelta*ticksPerQuarterNote))
				- selStartTime.
	indexInTrack to: track size do:
		[:i | (track at: i) adjustTimeBy: delta].
! !

!MIDIScore methodsFor: 'editing' stamp: 'di 6/21/1999 10:56'!
insertEvents: events at: selection

	| track selStartTime delta |
	track _ tracks at: selection first.
	selection second = 0
		ifTrue: [selStartTime _ 0.
				selection at: 2 put: 1]
		ifFalse: [selStartTime _ (track at: selection second) time].
	track _ track copyReplaceFrom: selection second to: selection second - 1
				with: (events collect: [:e | e copy]).
	track size >=  (selection second + events size) ifTrue:
		["Adjust times of following events"
		delta _ selStartTime - (track at: selection second) time.
		selection second to: selection second + events size - 1 do:
			[:i | (track at: i) adjustTimeBy: delta].
		delta _ (self gridToNextQuarterNote: (track at: selection second + events size - 1) endTime)
					- (track at: selection second + events size) time.
		selection second + events size to: track size do:
			[:i | (track at: i) adjustTimeBy: delta].
		].
	tracks at: selection first put: track! !

!MIDIScore methodsFor: 'editing' stamp: 'jm 8/6/1998 21:16'!
jitterStartAndEndTimesBy: mSecs

	| r range halfRange oldEnd newEnd newStart |
	r _ Random new.
	range _ 2.0 * mSecs.
	halfRange _ mSecs.
	tracks do: [:t |
		t do: [:e |
			e isNoteEvent ifTrue: [
				oldEnd _ e time + e duration.
				newEnd _ oldEnd + ((r next * range) asInteger - halfRange).
				newStart _ e time + ((r next * range) asInteger - halfRange).
				e time: newStart.
				e duration: (newEnd - newStart)]]].

				! !

!MIDIScore methodsFor: 'nil' stamp: 'jmv 4/6/2009 17:15'!
durationInTicks
	
	| t |
	t _ 0.
	tracks do: [ :track |
		track do:
			[:n | (n isNoteEvent)
				ifTrue: [t _ t max: n endTime]
				ifFalse: [t _ t max: n time]]].
	^ t
! !

!MIDIScore methodsFor: 'initialization' stamp: 'jmv 4/6/2009 17:15'!
initialize

	tracks _ #().
	tempoMap _ #().
	ticksPerQuarterNote _ 100.
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/3/98 07:36'!
tempoMap

	^ tempoMap
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/29/98 18:09'!
tempoMap: tempoEventList

	tempoEventList ifNil: [
		tempoMap _ #().
		^ self].
	tempoMap _ tempoEventList asArray.
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/3/98 07:37'!
ticksPerQuarterNote

	^ ticksPerQuarterNote
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/3/98 07:37'!
ticksPerQuarterNote: anInteger

	ticksPerQuarterNote _ anInteger.
! !

!MIDIScore methodsFor: 'accessing' stamp: 'di 10/21/2000 10:02'!
trackInfo

	^ trackInfo ifNil: [tracks collect: [:i | String new]]
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/6/98 23:20'!
trackInfo: trackInfoList

	trackInfo _ trackInfoList asArray.
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jm 1/3/98 08:15'!
tracks

	^ tracks
! !

!MIDIScore methodsFor: 'accessing' stamp: 'jmv 4/6/2009 17:14'!
tracks: trackList

	tracks _ trackList asArray collect: [:trackEvents | trackEvents asArray]! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 10/14/1998 22:16'!
channel: i

	^ channels at: i
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 10:50'!
closeMIDIPort

	midiParser midiPort ifNil: [^ self].
	midiParser midiPort close.
	midiParser midiPort: nil.
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jmv 12/2/2011 09:43'!
initialize

	midiParser _ MIDIInputParser on: nil.
	channels _ (1 to: 16) collect: [:ch | MIDISynthChannel new]! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 08:13'!
instrumentForChannel: channelIndex

	^ (channels at: channelIndex) instrument
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 08:14'!
instrumentForChannel: channelIndex put: aSoundProto

	(channels at: channelIndex) instrument: aSoundProto.
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 10:27'!
isOn

	^ process notNil
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 10/14/1998 21:52'!
midiParser

	^ midiParser
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 08:26'!
midiPort

	^ midiParser midiPort
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 16:32'!
midiPort: aMIDIPortOrNil

	midiParser midiPort: aMIDIPortOrNil.
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 10/13/1998 12:09'!
midiTrackingLoop

	midiParser clearBuffers.
	[true] whileTrue: [
		self processMIDI ifFalse: [(Delay forMilliseconds: 5) wait]].
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 20:12'!
mutedForChannel: channelIndex put: aBoolean

	^ (channels at: channelIndex) muted: aBoolean
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 19:45'!
panForChannel: channelIndex

	^ (channels at: channelIndex) pan
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 19:45'!
panForChannel: channelIndex put: newPan

	(channels at: channelIndex) pan: newPan.
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 10/14/1998 14:13'!
processMIDI
	"Process some MIDI commands. Answer true if any commands were processed."

	| didSomething cmdByte byte1 byte2 cmd chan |
	didSomething _ false.
	midiParser midiDo: [:item |
		didSomething _ true.
		cmdByte _ item at: 2.
		byte1 _ byte2 _ nil.
		item size > 2 ifTrue: [
			byte1 _ item at: 3.
			item size > 3 ifTrue: [byte2 _ item at: 4]].
		cmdByte < 240
			ifTrue: [  "channel message" 
				cmd _ cmdByte bitAnd: 2r11110000.
				chan _ (cmdByte bitAnd: 2r00001111) + 1.
				(channels at: chan) doChannelCmd: cmd byte1: byte1 byte2: byte2]
			ifFalse: [  "system message"
				"process system messages here"
			]].
	^ didSomething
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jmv 1/14/2013 21:13'!
processMIDIUntilMouseDown
	"Used for debugging. Do MIDI processing until the mouse is pressed."

	midiParser clearBuffers.
	[ Sensor isAnyButtonPressed ] whileFalse: [ self processMIDI ]! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 16:36'!
setAllChannelMasterVolumes: aNumber

	| vol |
	vol _ (aNumber asFloat min: 1.0) max: 0.0.
	channels do: [:ch | ch masterVolume: vol].
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jmv 10/8/2012 22:16'!
startMIDITracking

	midiParser ifNil: [^ self].
	midiParser midiPort ifNil: [^ self].
	midiParser midiPort ensureOpen.
	self stopMIDITracking.
	SoundPlayer useShortBuffer.
	process _ [self midiTrackingLoop] newProcess.
	process priority: Processor userInterruptPriority.
	process name: 'MIDISynth'.
	process resume! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 10:34'!
stopMIDITracking

	process ifNotNil: [
		process terminate.
		process _ nil].
	SoundPlayer shutDown; initialize.  "revert to normal buffer size"
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 16:40'!
volumeForChannel: channelIndex

	^  (channels at: channelIndex) masterVolume
! !

!MIDISynth methodsFor: 'as yet unclassified' stamp: 'jm 1/6/1999 16:40'!
volumeForChannel: channelIndex put: newVolume

	(channels at: channelIndex) masterVolume: newVolume.
! !

!MIDISynth class methodsFor: 'examples' stamp: 'jm 1/6/1999 16:39'!
example
	"Here's one way to run the MIDI synth. It will get a nice Morphic UI later. Click the mouse to stop running it. (Mac users note: be sure you have MIDI interface adaptor plugged in, or Squeak will hang waiting for the external clock signal.)."
	"MIDISynth example"

	| portNum synth |
	portNum _ SimpleMIDIPort inputPortNumFromUser.
	portNum ifNil: [^ self].
	SoundPlayer useShortBuffer.
	synth _ MIDISynth new
		midiPort: (SimpleMIDIPort openOnPortNumber: portNum).
	synth midiParser ignoreCommand: 224.  "filter out pitch bends"
	1 to: 16 do: [:i |
		(synth channel: i) instrument:
 			 (AbstractSound soundNamed: 'oboe1')].
	1 to: 16 do: [:ch | synth volumeForChannel: ch put: 0.2].

	synth processMIDIUntilMouseDown.
	SoundPlayer shutDown; initialize.  "revert to normal buffer size"
! !

!MIDISynthChannel methodsFor: 'other' stamp: 'jm 10/14/1998 21:45'!
adjustPitch: bend
	"Handle a pitch-bend change."

	| snd pitchAdj centerPitch |
	pitchBend _ bend.
	pitchAdj _ 2.0 raisedTo: (bend asFloat / 8192.0) / 6.0.
	activeSounds copy do: [:entry |
		snd _ entry at: 2.
		centerPitch _ entry at: 3.
		snd pitch: pitchAdj * centerPitch.
		snd internalizeModulationAndRatio].
! !

!MIDISynthChannel methodsFor: 'other' stamp: 'jm 10/14/1998 15:43'!
convertVelocity: valueByte
	"Map a value in the range 0..127 to a volume in the range 0.0..1.0."
	"Details: A quadratic function seems to give a good keyboard feel."

	| r |
	r _ (valueByte * valueByte) / 12000.0.
	r > 1.0 ifTrue: [^ 1.0].
	r < 0.08 ifTrue: [^ 0.08].
	^ r
! !

!MIDISynthChannel methodsFor: 'other' stamp: 'jm 10/14/1998 15:41'!
newVolume: valueByte
	"Set the channel volume to the level given by the given number in the range 0..127."

	| snd newVolume |
	channelVolume _ valueByte asFloat / 127.0.
	newVolume _ masterVolume * channelVolume.
	activeSounds do: [:entry |
		snd _ entry at: 2.
		snd adjustVolumeTo: newVolume overMSecs: 10].
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 15:40'!
channelPressure: newPressure
	"Handle a channel pressure (channel aftertouch) change."

	self newVolume: newPressure.
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 13:48'!
control: control value: newValue
	"Handle a continuous controller change."

	control = 2 ifTrue: [self newVolume: newValue].  "breath controller"
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 13:44'!
doChannelCmd: cmdByte byte1: byte1 byte2: byte2
	"Dispatch a channel command with the given arguments."
	"Details: Cases appear in order of expected frequency, most frequent cases first."

	cmdByte = 144 ifTrue: [
		byte2 = 0
			ifTrue: [^ self keyUp: byte1 vel: 0]
			ifFalse: [^ self keyDown: byte1 vel: byte2]].
	cmdByte = 128 ifTrue: [^ self keyUp: byte1 vel: byte2].
	cmdByte = 224 ifTrue: [^ self pitchBend: ((byte2 bitShift: 7) + byte1) - 8192].
	cmdByte = 176 ifTrue: [^ self control: byte1 value: byte2].
	cmdByte = 208 ifTrue: [^ self channelPressure: byte1].
	cmdByte = 160 ifTrue: [^ self key: byte1 pressure: byte2].
	cmdByte = 192 ifTrue: [^ self programChange: byte1].
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 13:49'!
key: key pressure: press
	"Handle a key pressure (polyphonic aftertouch) change. Rarely implemented."

	"Do nothing for now."
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 1/10/1999 08:42'!
keyDown: key vel: vel
	"Handle a key down event with non-zero velocity."

	| pitch snd |
	muted ifTrue: [^ self].
	pitch _ AbstractSound pitchForMIDIKey: key.
	snd _ instrument
		soundForPitch: pitch
		dur: 10000.0  "sustain a long time, or until turned off"
		loudness: masterVolume * channelVolume * (self convertVelocity: vel).
	snd _ (MixedSound new add: snd pan: pan) reset.
	SoundPlayer resumePlaying: snd quickStart: false.
	activeSounds add: (Array with: key with: snd with: pitch).
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 13:49'!
keyUp: key vel: vel
	"Handle a key up event."

	| snd |
	activeSounds copy do: [:entry |
		(entry at: 1) = key ifTrue: [
			snd _ entry at: 2.
			snd stopGracefully.
			activeSounds remove: entry]].
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 1/11/1999 11:32'!
pitchBend: bend
	"Handle a pitch-bend change."

	self adjustPitch: bend.
! !

!MIDISynthChannel methodsFor: 'midi dispatching' stamp: 'jm 10/14/1998 13:50'!
programChange: newProgram
	"Handle a program (instrument) change."

	"Do nothing for now."
! !

!MIDISynthChannel methodsFor: 'initialization' stamp: 'jm 1/6/1999 20:10'!
initialize

	instrument _ FMSound default.
	muted _ false.
	masterVolume _ 0.5.
	channelVolume _ 1.0.
	pan _ 0.5.
	pitchBend _ 0.0.
	activeSounds _ OrderedCollection new.
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 10/13/1998 09:45'!
instrument

	^ instrument
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 10/13/1998 09:45'!
instrument: aSound

	instrument _ aSound.
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 10/13/1998 09:47'!
masterVolume

	^ masterVolume
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 10/13/1998 11:49'!
masterVolume: aNumber
	"Set the master volume the the given value (0.0 to 1.0)."

	masterVolume _ aNumber asFloat.
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 1/6/1999 20:10'!
muted

	^ muted
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 1/6/1999 20:11'!
muted: aBoolean

	muted _ aBoolean.
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 1/6/1999 19:43'!
pan

	^ pan
! !

!MIDISynthChannel methodsFor: 'accessing' stamp: 'jm 1/6/1999 19:43'!
pan: aNumber
	"Set the left-right pan to the given value (0.0 to 1.0)."

	pan _ aNumber asFloat.
! !

!SimpleMIDIPort methodsFor: 'input' stamp: 'jm 10/8/1998 19:47'!
bufferTimeStampFrom: aByteArray
	"Return the timestamp from the given MIDI input buffer. Assume the given buffer is at least 4 bytes long."

	^ ((aByteArray at: 1) bitShift: 24) +
	  ((aByteArray at: 2) bitShift: 16) +
	  ((aByteArray at: 3) bitShift: 8) +
	   (aByteArray at: 4)
! !

!SimpleMIDIPort methodsFor: 'input' stamp: 'jm 10/8/1998 19:53'!
flushInput
	"Read any lingering MIDI data from this port's input buffer."

	| buf |
	buf _ ByteArray new: 1000.
	[(self readInto: buf) > 0] whileTrue.
! !

!SimpleMIDIPort methodsFor: 'input' stamp: 'jm 10/12/1998 15:49'!
readInto: aByteArray
	"Read any data from this port into the given buffer."

	^ self primMIDIReadPort: portNumber into: aByteArray
! !

!SimpleMIDIPort methodsFor: 'open/close' stamp: 'jm 1/13/1999 08:10'!
close
	"Close this MIDI port."

	portNumber ifNotNil: [self primMIDIClosePort: portNumber].
	accessSema _ nil.
	lastCommandByteOut _ nil.
! !

!SimpleMIDIPort methodsFor: 'open/close' stamp: 'jm 1/13/1999 08:23'!
ensureOpen
	"Make sure this MIDI port is open. It is good to call this before starting to use a port in case an intervening image save/restore has caused the underlying hardware port to get closed."

	portNumber ifNil: [^ self error: 'Use "openOn:" to open a MIDI port initially'].
	self primMIDIClosePort: portNumber.
	self primMIDIOpenPort: portNumber readSemaIndex: 0 interfaceClockRate: InterfaceClockRate.
	accessSema _ Semaphore forMutualExclusion.
	lastCommandByteOut _ Array new: 16 withAll: 0.  "clear running status"
! !

!SimpleMIDIPort methodsFor: 'open/close' stamp: 'jm 1/13/1999 08:09'!
openOnPortNumber: portNum
	"Open this MIDI port on the given port number."

	self close.
	portNumber _ portNum.
	accessSema _ Semaphore forMutualExclusion.
	self ensureOpen.
! !

!SimpleMIDIPort methodsFor: 'open/close' stamp: 'jm 10/12/1998 15:48'!
portNumber
	"Answer my port number."

	^ portNumber
! !

!SimpleMIDIPort methodsFor: 'output' stamp: 'jm 9/28/1998 22:00'!
midiCmd: cmd channel: channel byte: dataByte
	"Immediately output the given MIDI command with the given channel and argument byte to this MIDI port. Assume that the port is open."

	accessSema critical: [
		self primMIDIWritePort: portNumber
			from: (ByteArray
					with: (cmd bitOr: channel)
					with: dataByte)
			at: 0].
! !

!SimpleMIDIPort methodsFor: 'output' stamp: 'jm 9/28/1998 22:00'!
midiCmd: cmd channel: channel byte: dataByte1 byte: dataByte2
	"Immediately output the given MIDI command with the given channel and argument bytes to this MIDI port. Assume that the port is open."

	accessSema critical: [
		self primMIDIWritePort: portNumber
			from: (ByteArray
					with: (cmd bitOr: channel)
					with: dataByte1
					with: dataByte2)
			at: 0].
! !

!SimpleMIDIPort methodsFor: 'output' stamp: 'jm 9/28/1998 22:00'!
midiOutput: aByteArray
	"Output the given bytes to this MIDI port immediately. Assume that the port is open."

	accessSema critical: [
		self primMIDIWritePort: portNumber from: aByteArray at: 0].
! !

!SimpleMIDIPort methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primMIDIClosePort: portNum
	"Close the given MIDI port. Don't fail if port is already closed."

	<primitive: 'primitiveMIDIClosePort' module: 'MIDIPlugin'>
! !

!SimpleMIDIPort methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primMIDIOpenPort: portNum readSemaIndex: readSemaIndex interfaceClockRate: interfaceClockRate
	"Open the given MIDI port. If non-zero, readSemaIndex specifies the index in the external objects array of a semaphore to be signalled when incoming MIDI data is available. Not all platforms support signalling the read semaphore. InterfaceClockRate specifies the clock rate of the external MIDI interface adaptor on Macintosh computers; it is ignored on other platforms."

	<primitive: 'primitiveMIDIOpenPort' module: 'MIDIPlugin'>
	self primitiveFailed.
! !

!SimpleMIDIPort methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primMIDIReadPort: portNum into: byteArray
	"Read any available MIDI data into the given buffer (up to the size of the buffer) and answer the number of bytes read."

	<primitive: 'primitiveMIDIRead' module: 'MIDIPlugin'>
	self primitiveFailed.
! !

!SimpleMIDIPort methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primMIDIWritePort: portNum from: byteArray at: midiClockValue
	"Queue the given data to be sent through the given MIDI port at the given time. If midiClockValue is zero, send the data immediately."

	<primitive: 'primitiveMIDIWrite' module: 'MIDIPlugin'>
	self primitiveFailed.
! !

!SimpleMIDIPort class methodsFor: 'utilities' stamp: 'jm 1/13/1999 08:11'!
closeAllPorts
	"Close all MIDI ports."
	"SimpleMIDIPort closeAllPorts"

	| lastPortNum |
	lastPortNum _ self primPortCount - 1.
	0 to: lastPortNum do: [:portNum | self basicNew primMIDIClosePort: portNum].
! !

!SimpleMIDIPort class methodsFor: 'utilities' stamp: 'jmv 5/13/2018 19:06:00'!
inputPortNumFromUser
	"Prompt the user for a MIDI input port. Answer a port number, or nil if the user does not select a port or if MIDI is not supported on this platform."
	"
	SimpleMIDIPort inputPortNumFromUser
	"

	| portCount aMenu dir |
	portCount _ self primPortCount.
	portCount = 0 ifTrue: [^ nil].
	aMenu _ PopUpMenu new label: 'MIDI port for input:'.
	0 to: portCount - 1 do:[:i |
		dir _ self primPortDirectionalityOf: i.
		(dir = 1) | (dir = 3) ifTrue:[
			aMenu add: (self portDescription: i) action: i]].
	 ^ aMenu startUpMenu! !

!SimpleMIDIPort class methodsFor: 'utilities' stamp: 'jm 10/12/1998 18:10'!
midiIsSupported
	"Answer true if this platform supports MIDI."

	^ self primPortCount > 0
! !

!SimpleMIDIPort class methodsFor: 'utilities' stamp: 'jmv 5/13/2018 19:06:33'!
outputPortNumFromUser
	"Prompt the user for a MIDI output port. Answer a port number, or nil if the user does not select a port or if MIDI is not supported on this platform."
	"
	SimpleMIDIPort outputPortNumFromUser
	"

	| portCount aMenu dir |
	portCount _ self primPortCount.
	portCount = 0 ifTrue: [^ nil].
	aMenu _ PopUpMenu new label: 'MIDI port for output:'.
	0 to: portCount - 1 do:[:i |
		dir _ self primPortDirectionalityOf: i.
		(dir = 2) | (dir = 3) ifTrue:[
			aMenu add: (self portDescription: i) action: i]].
	 ^ aMenu startUpMenu! !

!SimpleMIDIPort class methodsFor: 'utilities' stamp: 'jm 10/12/1998 17:46'!
portDescription: portNum
	"Answer a string indicating the directionality of the given MIDI port."
	"(0 to: SimpleMIDIPort primPortCount - 1) collect:
		[:i | SimpleMIDIPort portDescription: i]"

	| portName dir |
	portName _ self primPortNameOf: portNum.
	dir _ self primPortDirectionalityOf: portNum.
	dir = 1 ifTrue: [^ portName, ' (in)'].
	dir = 2 ifTrue: [^ portName, ' (out)'].
	dir = 3 ifTrue: [^ portName, ' (in/out)'].
	^ self error: 'unknown MIDI port directionality'
! !

!SimpleMIDIPort class methodsFor: 'class initialization' stamp: 'jm 9/10/1998 15:33'!
initialize
	"SimpleMIDIPort initialize"

	InterfaceClockRate _ 1000000.
	DefaultPortNumber _ 0.
! !

!SimpleMIDIPort class methodsFor: 'instance creation' stamp: 'jm 9/10/1998 18:36'!
openDefault
	"Answer a new instance of me opened on the default MIDI port."

	^ self openOnPortNumber: DefaultPortNumber
! !

!SimpleMIDIPort class methodsFor: 'instance creation' stamp: 'tk 6/24/1999 11:42'!
openOnPortNumber: portNum
	"Answer a new instance of me for the given MIDI port number."
	"Details: All clients of a particular MIDI port should share the same instance of me. This allows accesses to the port to be serialized and shared port-related state state to be maintained."

	SimpleMIDIPort allSubInstancesDo: [:p |
		p portNumber = portNum ifTrue: [
			"share the existing port object for this port number"
			^ p]].

	^ super new openOnPortNumber: portNum
! !

!SimpleMIDIPort class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primPortCount
	"Answer the number of MIDI ports supported by this platform, or zero if this primitive is not implemented."

	<primitive: 'primitiveMIDIGetPortCount' module: 'MIDIPlugin'>
	^ 0
! !

!SimpleMIDIPort class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primPortDirectionalityOf: portNum
	"Answer the platform-specific name for the given MIDI port."

	<primitive: 'primitiveMIDIGetPortDirectionality' module: 'MIDIPlugin'>
	self primitiveFailed.
! !

!SimpleMIDIPort class methodsFor: 'primitives' stamp: 'ar 2/2/2001 15:09'!
primPortNameOf: portNum
	"Answer the platform-specific name for the given MIDI port."

	<primitive: 'primitiveMIDIGetPortName' module: 'MIDIPlugin'>
	self primitiveFailed.
! !

!Beeper methodsFor: 'play interface' stamp: 'jmv 6/7/2013 22:03'!
play
	"This is how the default Beeper makes a beep,
	by sending beep to the default sound service.
	The sound system will check if sounds are enabled."

	SoundSystem default beep! !

!Beeper class methodsFor: 'beeping' stamp: 'gk 2/24/2004 08:38'!
beep
	"The preferred way of producing an audible feedback.
	The default playable entity (an instance of Beeper)
	also uses the pluggable SoundService
	mechanism, so it will use the primitive beep only
	if there is no other sound mechanism available."

	self default play
! !

!Beeper class methodsFor: 'beeping' stamp: 'gk 2/24/2004 08:38'!
beepPrimitive
	"Make a primitive beep. Only use this if
	you want to force this to be a primitive beep.
	Otherwise use Beeper class>>beep
	since this method bypasses the current
	registered playable entity."

	Preferences soundsEnabled ifTrue: [
		self primitiveBeep]! !

!Beeper class methodsFor: 'customize' stamp: 'gk 2/22/2004 17:51'!
clearDefault
	"Clear the default playable.
	Will be lazily initialized in Beeper class >>default."

	default := nil! !

!Beeper class methodsFor: 'customize' stamp: 'jmv 3/2/2010 15:31'!
default
	"When the default is not defined it is
	initialized using #newDefault."

	default ifNil: [ default := self newDefault ].
	^ default! !

!Beeper class methodsFor: 'customize' stamp: 'gk 2/24/2004 22:12'!
newDefault
	"Subclasses may override me to provide a default beep.
	This base implementation returns an instance of Beeper
	which uses the pluggable sound service."

	^ self new! !

!Beeper class methodsFor: 'customize' stamp: 'gk 2/22/2004 17:54'!
setDefault: aPlayableEntity
	"Set the playable entity used when making a beep.
	The playable entity should implement the message #play."

	default := aPlayableEntity! !

!Beeper class methodsFor: 'private' stamp: 'gk 2/24/2004 23:51'!
primitiveBeep
	"Make a primitive beep. Not to be called directly.
	It is much better to use Beeper class>>beep
	or Beeper class>>beepPrimitive
	since this method bypasses the current
	registered playable entity and does not
	check Preferences class>>soundsEnabled."

	<primitive: 140>
	self primitiveFailed! !

!SoundSystem methodsFor: 'misc' stamp: 'jmv 6/7/2013 21:58'!
randomBitsFromSoundInput: bitCount
	"Answer a positive integer with the given number of random bits of 'noise' from a sound input source. Typically, one would use a microphone or line input as the sound source, although many sound cards have enough thermal noise that you get random low-order sample bits even with no microphone connected. Only the least signficant bit of the samples is used. Since not all sound cards support 16-bits of sample resolution, we use the lowest bit that changes."
	"(1 to: 10) collect: [:i | BaseSoundSystem new randomBitsFromSoundInput: 512]"

	| recorder buf mid samples bitMask randomBits bit |
	"collect some sound data"
	recorder _ SoundRecorder new clearRecordedSound.
	recorder resumeRecording.
	(Delay forSeconds: 1) wait.
	recorder stopRecording.
	buf _ recorder condensedSamples.

	"grab bitCount samples from the middle"
	mid _ buf monoSampleCount // 2.
	samples _ buf copyFrom: mid to: mid + bitCount - 1.

	"find the least significant bit that varies"
	bitMask _ 1.
	[bitMask < 16r10000 and:
	 [(samples collect: [:s | s bitAnd: bitMask]) asSet size < 2]]
		whileTrue: [bitMask _ bitMask bitShift: 1].
	bitMask = 16r10000 ifTrue: [^ self error: 'sound samples do not vary'].

	"pack the random bits into a positive integer"
	randomBits _ 0.
	1 to: samples size do: [:i |
		bit _ ((samples at: i) bitAnd: bitMask) = 0 ifTrue: [0] ifFalse: [1].
		randomBits _ (randomBits bitShift: 1) + bit].

	^ randomBits	
! !

!SoundSystem methodsFor: 'misc' stamp: 'jmv 6/7/2013 21:58'!
sampledSoundChoices
	^ SampledSound soundNames! !

!SoundSystem methodsFor: 'misc' stamp: 'jmv 8/9/2018 11:33:08'!
soundNameFromUser
	"Pop up a list of available sound names and answer the one the user chooses, or nil if no choice made"

	^ (SelectionMenu selections: self sampledSoundChoices sorted) startUpWithCaption: 'Sounds'

"
SoundService default soundNameFromUser
"! !

!SoundSystem methodsFor: 'misc' stamp: 'jmv 6/7/2013 21:58'!
soundNamed: soundName
	^ SampledSound soundNamed: soundName! !

!SoundSystem methodsFor: 'playing' stamp: 'jmv 6/7/2013 21:58'!
beep
	"There is sound support, so we use the default
	sampled sound for a beep."

	Preferences soundsEnabled ifTrue: [
		SampledSound beep]! !

!SoundSystem methodsFor: 'playing' stamp: 'jmv 6/7/2013 21:58'!
playSampledSound: samples rate: rate

	Preferences soundsEnabled ifTrue: [
		(SampledSound samples: samples samplingRate: rate) play]! !

!SoundSystem methodsFor: 'playing' stamp: 'jmv 6/7/2013 21:58'!
playSoundNamed: soundName
	"There is sound support, so we play the given sound."

	Preferences soundsEnabled ifTrue: [
		SampledSound playSoundNamed: soundName asString]! !

!SoundSystem methodsFor: 'playing' stamp: 'pb 5/25/2016 01:12'!
playSoundNamed: soundName ifAbsentReadFrom: aifFileName

	Preferences soundsEnabled ifTrue: [
		(SampledSound soundNames includes: soundName) ifFalse: [
			(DirectoryEntry smalltalkImageDirectory // aifFileName) exists ifTrue: [
				SampledSound
					addLibrarySoundNamed: soundName
					fromAIFFfileNamed: aifFileName]].
		(SampledSound soundNames includes: soundName) ifTrue: [
			SampledSound playSoundNamed: soundName]]! !

!SoundSystem methodsFor: 'playing' stamp: 'jmv 6/7/2013 21:58'!
playSoundNamedOrBeep: soundName
	"There is sound support, so we play the given sound
	instead of beeping."

	Preferences soundsEnabled ifTrue: [
		^self playSoundNamed: soundName]! !

!SoundSystem class methodsFor: 'class initialization' stamp: 'jmv 6/7/2013 22:03'!
default
	Default ifNil: [ self initialize ].
	^Default! !

!SoundSystem class methodsFor: 'class initialization' stamp: 'jmv 6/7/2013 22:01'!
initialize
	Default _ self new! !

!FileEntry methodsFor: '*Sound' stamp: 'jmv 7/24/2017 16:25:59'!
wavContents
	^SampledSound fromFileEntry: self! !
SoundBuffer initialize!
AbstractSound initialize!
FMSound initialize!
LoopedSampledSound initialize!
SampledSound initialize!
MuLawCodec initialize!
SoundPlayer initialize!
SoundRecorder initialize!
MIDIInputParser initialize!
SimpleMIDIPort initialize!
SoundSystem initialize!
