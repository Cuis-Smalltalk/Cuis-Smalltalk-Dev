'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 2:37:55 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:06'!
lineCount
	"Answer the number of lines represented by the receiver, where every cr adds one line.  5/10/96 sw"


	| i |
	i _ 0.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i _ i + 1 ].
	^i

"
'Fred
the
Bear' lineCount
"! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:05'!
lineNumber: anIndex
	"Answer a string containing the characters in the given line number. "
	| i |
	i _ 1.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i = anIndex ifTrue: [
			^ self copyFrom: start to: endWithoutDelimiters ].
		i _ i + 1 ].
	^nil
	
"
'Fred
the
Bear' lineNumber: 3
".! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:08'!
linesDo: aBlock
	"execute aBlock with each line in this string.  The terminating CR's are not included in what is passed to aBlock"
	
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]
		! !


!String methodsFor: 'accessing' stamp: 'jmv 6/7/2022 22:27:42'!
string: aString lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[
		end _ aString indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters _ end _ aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters _ end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf codePoint`
			and: [(aString basicAt: end) = `Character cr codePoint` ]])
				ifTrue: [ end _ end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ] repeat! !

!String methodsFor: 'converting' stamp: 'jmv 6/7/2022 22:24:58'!
string: aString withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr _ Character cr.
	lf _ Character lf.
	crlf _ CharacterSet new.
	crlf add: cr; add: lf.

	inPos _ 1.
	outPos _ 1.
	lineEndingSize _ lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString _ aString class new: aString size * lineEndingSize.

	[
		lineEndPos _ aString indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos _ outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aString startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos _ newOutPos + 1 ].
			outPos _ newOutPos.
			((aString at: lineEndPos) = cr and: [ lineEndPos < aString size and: [ (aString at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos _ lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos _ lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos _ outPos + (aString size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aString startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/8/2022 14:37:30'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	'' string: bytes lineIndicesDo: [ :startByte :endByteWithoutDelimiters :endByte |
		| start end endWithoutDelimiters |
		start _ self codePointIndexAt: startByte.
		end _ self codePointIndexAt: endByte.
		endWithoutDelimiters _ endByteWithoutDelimiters = endByte
			ifTrue: [end]
			ifFalse: [self codePointIndexAt: endByteWithoutDelimiters].
		aBlock
			value: start
			value: endWithoutDelimiters
			value: end
		]! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/7/2022 22:07:54'!
copyWith: anUnicodeCodePoint

	^Utf8String withBytes:
		bytes, (Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint).! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/8/2022 13:44:41'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	^  Utf8String withBytes:  ('' string: bytes withLineEndings: lineEndingString).! !


!String methodsFor: 'accessing' stamp: 'jmv 6/8/2022 13:47:52'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	self string: self lineIndicesDo: aBlock! !

!String methodsFor: 'converting' stamp: 'jmv 6/7/2022 22:21:30'!
substrings: aString
	"Answer an array of the substrings that compose the argument."
	| end beginning |
	end _ 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning _ aString indexOfAnyOf: CSNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end _ aString indexOfAnyOf: CSSeparators startingAt: beginning ifAbsent: [ aString size + 1 ].
			end _ end - 1.
			result nextPut: (aString copyFrom: beginning to: end).
		]
	].! !

!String methodsFor: 'converting' stamp: 'jmv 6/8/2022 13:46:18'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	^ self string: self withLineEndings: lineEndingString! !

!methodRemoval: String #lineNumber: stamp: 'jmv 6/8/2022 13:42:05'!
String removeSelector: #lineNumber:!
!methodRemoval: String #lineCount stamp: 'jmv 6/8/2022 13:42:10'!
String removeSelector: #lineCount!
!methodRemoval: String #linesDo: stamp: 'jmv 6/8/2022 13:36:01'!
String removeSelector: #linesDo:!
