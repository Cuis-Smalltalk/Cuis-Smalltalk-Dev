'From Cuis 6.0 [latest update: #5503] on 21 October 2022 at 10:53:44 am'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/21/2022 10:52:57'!
assignmentArrowString
	^ UnicodeContentsHandling = #useUtf8Editors
		ifTrue: [ "Some examples to try.
			2190 - 	Leftwards Arrow
			21D0 - 	Leftwards Double Arrow
			21E0	 - Leftwards Dashed Arrow
			21E6 - Leftwards White Arrow
			21FD	 - Leftwards Open-Headed Arrow
			27F5 - LONG LEFTWARDS ARROW
			2B05	 - LEFTWARDS BLACK ARROW	"
			`(UnicodeCodePoint codePoint: 16r2190) asString` ]
		ifFalse: [ `(Character numericValue: 28) asString` ]! !

