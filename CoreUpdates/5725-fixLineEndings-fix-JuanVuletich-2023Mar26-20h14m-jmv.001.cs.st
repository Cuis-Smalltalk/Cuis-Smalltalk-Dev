'From Cuis 6.0 [latest update: #5724] on 26 March 2023 at 8:36:05 pm'!

!String class methodsFor: 'services' stamp: 'jmv 3/26/2023 20:34:15'!
string: aString withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr := Character cr.
	lf := Character lf.
	crlf := CharacterSet new.
	crlf add: cr; add: lf.
	(aString is: #ByteArray)
		ifTrue: [
			cr := cr asciiValue.
			lf := lf asciiValue ].

	inPos := 1.
	outPos := 1.
	lineEndingSize := lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString := aString class new: aString size * lineEndingSize.

	[
		lineEndPos := aString indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aString startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos := newOutPos + 1 ].
			outPos := newOutPos.
			((aString at: lineEndPos) = cr and: [ lineEndPos < aString size and: [ (aString at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (aString size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aString startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !

