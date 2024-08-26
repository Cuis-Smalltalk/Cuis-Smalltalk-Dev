'From Cuis7.1 [latest update: #6635] on 26 August 2024 at 4:40:35 pm'!

!InnerTextMorph methodsFor: 'events' stamp: 'jmv 6/10/2024 16:31:01'!
keyStroke: aKeyboardEvent

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ 
		self selectionChanged.  "Note new selection"
		self updateFromTextComposition.
		^self ].

	autoCompleter 
		ifNil: [ self processKeystrokeEvent: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeystrokeEvent: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].

	aKeyboardEvent wasHandled: true.
! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 8/26/2024 16:39:26'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |

	"Ignore keystrokes that could be keyboard shortcuts, even if not defined as such."
	((aKeyboardEvent keyValue between: 32 and: 126) and: [ aKeyboardEvent commandAltKeyPressed ])
		ifTrue: [ ^true ].

	"Convert strings like '\+161 ' (š) to unicode"
	aKeyboardEvent isSpace ifTrue: [ | string |
		string := self privateCurrentString.
		self pointIndex - 1 to: (self pointIndex - 7 max: 1) by: -1 do: [ :index |
		(string at: index) = $\ ifTrue: [ | key |
			key := string copyFrom: index + 1 to: self pointIndex - 1.
			(key size > 2 and: [ (key at: 1) = $+ ])
				ifTrue: [ | stream number value |
					stream := ReadStream on: key asUppercase from: 2 to: key size.
					[ number := Integer readFrom: stream  base: 16 ] on: Error do: [ ^ false ].
					value := Character codePoint: number.
					self
						selectFrom: index to: self pointIndex - 1;
						replaceSelectionWith: (UnicodeString with: value).
						^ false ] ] ] ].
	character := aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i := self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement := character caseOf: {
				[ `Character codePoint: 16r300` ] -> [ '`' ].
				[ `Character codePoint: 16r301` ] -> [ '''' ].
				[ `Character codePoint: 16r302` ] -> [ '^' ].
				[ `Character codePoint: 16r303` ] -> [ '~' ].
				[ `Character codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !

