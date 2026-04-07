'From Cuis7.7 [latest update: #7857] on 7 April 2026 at 2:52:52 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 7/Apr/2026 14:48:16'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."
	self subclassResponsibility! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 7/Apr/2026 14:48:29'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."
	self subclassResponsibility! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 7/Apr/2026 14:51:45'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	self subclassResponsibility! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 7/Apr/2026 14:49:32'!
, aStringOrText
	^ aStringOrText appendToString: self! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 7/Apr/2026 14:50:36'!
appendToString: aString
	self subclassResponsibility! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 7/Apr/2026 14:52:38'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	self subclassResponsibility! !

!methodRemoval: UnicodeString #, stamp: 'jmv 7/Apr/2026 14:49:42'!
UnicodeString removeSelector: #,!
!methodRemoval: String #, stamp: 'jmv 7/Apr/2026 14:49:44'!
String removeSelector: #,!
