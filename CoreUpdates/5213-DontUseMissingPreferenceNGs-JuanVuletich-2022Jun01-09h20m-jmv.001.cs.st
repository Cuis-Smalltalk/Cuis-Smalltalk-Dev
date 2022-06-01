'From Cuis 6.0 [latest update: #5211] on 1 June 2022 at 9:21:20 am'!

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 5/25/2014 15:54'!
beep
	"
	Smalltalk beep
	"
	Preferences soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !

