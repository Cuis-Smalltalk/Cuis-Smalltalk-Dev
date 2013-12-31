'From Cuis 4.0 of 21 April 2012 [latest update: #4220] on 11 May 2012 at 3:42:03 pm'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 5/11/2012 15:40'!
endPC
	"Answer the index of the last bytecode."
	| size flagByte |
	"Can't create a zero-sized CompiledMethod so no need to use last for the errorEmptyCollection check.
	 We can reuse size."
	size := self size.
	flagByte := self at: size.
	flagByte = 0 ifTrue: [
		"If last byte = 0, may be either 0, 0, 0, 0 or just 0"
		size-1 to: size-3 by: -1 do: [ :i |
			i < self initialPC ifTrue: [ ^ i ].
			(self at: i) = 0 ifFalse: [ ^ i ]].
		^size - 4].
	flagByte < 252 ifTrue: [
		"Magic sources (temp names encoded in last few bytes)"
		^flagByte <= 127
			ifTrue: [size - flagByte - 1]
			ifFalse: [size - (flagByte - 128 * 128) - (self at: size - 1) - 2]].
	"Normal 4-byte source pointer"
	^size - 4! !

