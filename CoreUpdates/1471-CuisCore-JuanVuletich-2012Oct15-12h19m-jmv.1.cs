'From Cuis 4.0 of 21 April 2012 [latest update: #1469] on 15 October 2012 at 12:20:37 pm'!

!StringTest methodsFor: 'testing' stamp: 'jmv 10/14/2012 21:34'!
testLineSeparators
	"
	Test that #newLineCharacter is considered a line separator and not a line terminator.
	This means that the last line never ends with a #newLineCharacter (although it might be empty!!)
	StringTest new testLineSeparators
	"
	'
' lineIndicesDo: [ :start :endWithoutDelimiters :end |
		{ start . endWithoutDelimiters. end } print
		].! !


!String methodsFor: 'accessing' stamp: 'jmv 10/15/2012 11:23'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[
		end _ self indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters _ end _ self size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^self ].

		"Account for CrLf sequence"
		endWithoutDelimiters _ end - 1.
		(end < self size
			and: [(self at: end + 1) = Character lfCharacter
			and: [(self at: end) = Character crCharacter ]])
				ifTrue: [ end _ end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ] repeat! !

!methodRemoval: Editor #lines!
Editor removeSelector: #lines!
