'From Cuis 6.0 [latest update: #5532] on 26 October 2022 at 4:49:13 pm'!

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/26/2022 16:48:00'!
arrowStringForAssignment
	"Some examples to try.
		2190 - 	Leftwards Arrow
		21D0 - 	Leftwards Double Arrow
		21E0	 - Leftwards Dashed Arrow
		21E6 - Leftwards White Arrow
		21FD	 - Leftwards Open-Headed Arrow
		27F5 - Long Leftwards Arrow
		2B05	 - Leftwards Black Arrow	"
	^ `(UnicodeCodePoint codePoint: 16r2190) asString`! !

!TextModel class methodsFor: 'handling of Unicode' stamp: 'jmv 10/26/2022 16:48:04'!
arrowStringForReturn
	"Some examples to try.
		2191 - 	Upwards Arrow
		21D1 - 	Upwards Double Arrow
		21E1	 - Upwards Dashed Arrow
		21E7 - Upwards White Arrow
		2B06	 - Upwards Black Arrow	"
	^ `(UnicodeCodePoint codePoint: 16r2191) asString`! !

