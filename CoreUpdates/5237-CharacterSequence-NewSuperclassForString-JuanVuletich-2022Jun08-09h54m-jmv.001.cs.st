'From Cuis 6.0 [latest update: #5234] on 8 June 2022 at 9:55:57 am'!

!String methodsFor: 'enumerating' stamp: 'jmv 6/8/2022 09:46:53'!
collect: aBlock
	"Refer to the comment in Collection >> #collect:."
	| array resultClass |
	array _ self collectInArray: aBlock.
	resultClass _ String.
	array do: [ :element |
		element isCharacter ifFalse: [ ^array ].
		(element is: #Character) ifFalse: [
			resultClass _ Utf8String ]].
	^resultClass newFrom: array! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ArrayedCollection removeSubclass: String.
String superclass: CharacterSequence.
CharacterSequence addSubclass: String.
String class superclass: CharacterSequence class.!

