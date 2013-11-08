'From Cuis 4.2 of 25 July 2013 [latest update: #1864] on 7 November 2013 at 7:04:54.190917 pm'!

!Character methodsFor: 'converting' stamp: 'jmv 11/7/2013 19:01'!
withDiacriticalMark: anUnicodeCodePoint
	"Answer the character resulting from adding a diacritical mark (accent) to a letter.
	If the result is unsupported in ISO 8859-15, answer the receiver.
	Supported diacritical marks are:
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
		$a withDiacriticalMark: 16r301
		$N withDiacriticalMark: $~
		$Z withDiacriticalMark: $v
		
		invalid:
		$9 withDiacriticalMark:$v
		$A withDiacriticalMark: $v
		$Á withDiacriticalMark: $v
		$A withDiacriticalMark: 1244
		"
	| answer i |
	i _ ((anUnicodeCodePoint isNumber
			ifTrue: [#(16r300 16r301 16r302 16r303 16r308 16r30A 16r30C)]
			ifFalse: [#($` $' $^ $~ $" $° $v)]) indexOf: anUnicodeCodePoint
				) + 1.
	answer _ (Character accentedLetters detect: [ :group | group first = self ] ifNone: [ ^self ]) at: i.
	^answer = $- ifFalse: [answer] ifTrue: [self]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 11/7/2013 18:33'!
accentedLetters
	"Answer all letters with all possible accents (combining diacritical marks).
	Order of this array is:
		unaccented
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
	Do not include letters that can't have any accent in ISO 8859-15
	Denote unsupported combinations (for example A with caron, or N with accute accent) by a $-"
	^#(
		'AÀÁÂÃÄÅ-'
		'aàáâãäå-'
		'EÈÉÊ-Ë--'
		'eèéê-ë--'
		'IÌÍÎ-Ï--'
		'iìíî-ï--'
		'OÒÓÔÕÖ--'
		'oòóôõö--'
		'UÙÚÛ-Ü--'
		'uùúû-ü--'
		'N---Ñ---'
		'n---ñ---'
		'S------¦'
		's------¨'
		'Y-Ý--¾--'
		'y-ý--ÿ--'
		'Z------´'
		'z------¸'
	).! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 11/7/2013 19:04'!
generateKeyboardEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| buttons modifiers type keyValue pressType stamp |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [stamp _ Time localMillisecondClock].
	(evtBuf sixth <= 0 or: [
		(keyValue _ (Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth)) isNil ])
			ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].

	"start: Combining diacritical marks (i.e. accents in the Linux VM)"
	(Sensor peekEvent) ifNotNil: [ :nxt |
		(nxt fourth = EventSensor eventKeyDown and:[ nxt third >255 ])
			ifTrue: [
				keyValue _ ((Character value: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
				Sensor nextEvent; nextEvent; nextEvent
				]].
	"end: Combining diacritical marks (i.e. accents in the Linux VM)"

	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue].
	pressType = EventSensor eventKeyUp ifTrue: [type _ #keyUp].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [		"Control key pressed"
			keyValue < 27 ifTrue: [
				
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [		"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ 	"If Equal, Ctrl-alphabetic in Windows => do translate"
							lastKeyDownValue < 47 ]) ifTrue: [		"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifFalse: [ keyValue + 96 ]	"shift not pressed: conver to lowercase letter"
							ifTrue: [ keyValue + 64 ]].	"shift pressed: conver to uppercase letter"
					]
				].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [
				modifiers _ modifiers bitOr: 8 ]
			]].
	buttons _ modifiers bitShift: 3.
	^KeyboardEvent new 
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !

