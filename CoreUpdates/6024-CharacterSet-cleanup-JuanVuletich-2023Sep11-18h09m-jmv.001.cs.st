'From Cuis 6.0 [latest update: #6022] on 11 September 2023 at 6:11:26 pm'!

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 17:54:36'!
basicCharSetNonSeparators
	"CR and LF--characters that terminate a line"
	| answer |
	answer := CharacterSet new.
	0 to: 255 do: [ :i | | char |
		char := Character numericValue: i.
		char isSeparator ifFalse: [
			answer add: char ]].
	^answer! !


!Character methodsFor: 'testing' stamp: 'jmv 9/11/2023 17:54:15'!
isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."
	"Also see #separators"

	^ #(32 9 10 13 12) statePointsTo: self numericValue! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 9/11/2023 17:54:07'!
separators
	"Also see #isSeparator"
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character numericValue: v]

	
! !


!String class methodsFor: 'services' stamp: 'jmv 9/11/2023 18:04:12'!
string: aString withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr := Character cr.
	lf := Character lf.
	crlf := `CharacterSet new
			add: Character cr; add: Character lf; yourself`.
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

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 17:55:05'!
charSetNonSeparators
	"CR and LF--characters that terminate a line"
	^`String basicCharSetNonSeparators`.! !

!String class methodsFor: 'static state' stamp: 'jmv 9/11/2023 17:51:30'!
charSetSeparators
	"CR and LF--characters that terminate a line"
	^`CharacterSet new addAll: Character separators; yourself`.! !

!methodRemoval: CharacterSet class #nonSeparators stamp: 'jmv 9/11/2023 18:10:02'!
CharacterSet class removeSelector: #nonSeparators!
!methodRemoval: CharacterSet class #separators stamp: 'jmv 9/11/2023 18:11:09'!
CharacterSet class removeSelector: #separators!
!methodRemoval: CharacterSet class #allCharacters stamp: 'jmv 9/11/2023 18:10:46'!
CharacterSet class removeSelector: #allCharacters!
!methodRemoval: CharacterSet class #ascii stamp: 'jmv 9/11/2023 18:10:04'!
CharacterSet class removeSelector: #ascii!
!methodRemoval: CharacterSet class #nonAscii stamp: 'jmv 9/11/2023 18:10:06'!
CharacterSet class removeSelector: #nonAscii!
!methodRemoval: CharacterSet #complement stamp: 'jmv 9/11/2023 18:10:44'!
CharacterSet removeSelector: #complement!
