'From Cuis7.9 [latest update: #8398] on 25 November 2026 at 3:20:02 pm'!

!AdditionalMethodState methodsFor: 'accessing' stamp: 'jmv 25/Nov/2026, 5:41:38 pm (UTC)'!
pragmas
	"Answer the raw messages comprising my pragmas.
	Skip method properties."
	
	| propertyOrPragma "<Association|Message>" |
	^Array streamContents: [ :strm |
		1 to: self basicSize do: [:i |
			(propertyOrPragma := self basicAt: i) isPragma ifTrue: [
				strm nextPut: propertyOrPragma ]]
	] estimatedSize: self basicSize! !

!AdditionalMethodState methodsFor: 'converting' stamp: 'jmv 25/Nov/2026, 5:40:42 pm (UTC)'!
propertiesAsArray
	"Answer an array with methodProperties (instances of Association).
	Include only the method properties. Skip Pragmas."

	| element "<Association|Pragma>" |
	^Array streamContents: [ :strm |
		1 to: self basicSize do: [:i |
			(element := self basicAt: i) isPragma ifFalse: [
				strm nextPut: element ]]
	] estimatedSize: self basicSize! !

