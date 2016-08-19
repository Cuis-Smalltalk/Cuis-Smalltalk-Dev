'From Cuis 4.2 of 25 July 2013 [latest update: #2866] on 28 July 2016 at 4:33:22 pm'!
!classDefinition: #NumberParser category: #'Kernel-Numbers'!
Object subclass: #NumberParser
	instanceVariableNames: 'sourceStream base neg integerPart fractionPart exponent scale nDigits lastNonZero requestor failBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!NumberParser commentStamp: 'nice 3/15/2010 00:16' prior: 0!
NumberParser is an abstract class for parsing and building numbers from string/stream.
It offers a framework with utility methods and exception handling.

Number syntax is not defined and should be subclassResponsibility.

Instance variables:
sourceStream <Stream> the stream of characters from which the number is read
base <Integer> the radix in which to interpret digits
neg <Boolean> true in case of minus sign
integerPart <Integer> the integer part of the number
fractionPart <Integer> the fraction part of the number if any
exponent <Integer> the exponent used in scientific notation if any
scale <Integer> the scale used in case of ScaledDecimal number if any
nDigits <Integer> number of digits read to form an Integer
lasNonZero <Integer> position of last non zero digit, starting at 1 from left, 0 if all digits are zero
requestor <TextEditor | nil> can be used to insert an error message in the requestor
failBlock <BlockClosure> Block to execute whenever an error occurs.
	The fail block can have 0, 1 or 2 arguments (errorString and source position)
!

!classDefinition: #SqNumberParser category: #'Kernel-Numbers'!
NumberParser subclass: #SqNumberParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!SqNumberParser commentStamp: 'nice 2/13/2010 00:36' prior: 0!
SqNumberParser is a NumberParser specialized in reading Number with Squeak syntax.

Squeak syntax follows general Smalltalk-80 conventions for integer and floats, extended with scaled decimals.
Noticeable differences with st-80 and other Smalltalks are:
- allow both 2r-10 and -2r10 and even -2r-10
- allow floating point with radix 2r10.011
- do not allow single s without following digits as ScaledDecimal
- handle special case of Float (NaN Infinity and -0.0 as negative zero)!

!classDefinition: #ZipReadStream category: #'Squeak Compatibility'!
ReadStream subclass: #ZipReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #ZipWriteStream category: #'Squeak Compatibility'!
WriteStream subclass: #ZipWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #NumberParserError category: #'Squeak Compatibility'!
Error subclass: #NumberParserError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #ScaledDecimal category: #'Squeak Compatibility'!
Object subclass: #ScaledDecimal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #SyntaxMorph category: #'Squeak Compatibility'!
Object subclass: #SyntaxMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #SystemNavigation category: #'Squeak Compatibility'!
Object subclass: #SystemNavigation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #TextConstants category: #'Squeak Compatibility'!
Object subclass: #TextConstants
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #TextStyle category: #'Squeak Compatibility'!
Object subclass: #TextStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #ToolSet category: #'Squeak Compatibility'!
Object subclass: #ToolSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!
!classDefinition: #UIManager category: #'Squeak Compatibility'!
Object subclass: #UIManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Squeak Compatibility'!

!NumberParser methodsFor: 'accessing' stamp: 'nice 2/12/2010 23:56'!
allowPlusSign
	"return a boolean indicating if plus sign is allowed or not"

	^self subclassResponsibility! !

!NumberParser methodsFor: 'accessing' stamp: 'nice 2/12/2010 23:56'!
allowPlusSignInExponent
	"return a boolean indicating if plus sign is allowed or not in exponent"

	^self allowPlusSign! !

!NumberParser methodsFor: 'accessing' stamp: 'nice 2/12/2010 23:55'!
exponentLetters
	"answer the list of possible exponents for Numbers."

	^self subclassResponsibility! !

!NumberParser methodsFor: 'accessing' stamp: 'nice 5/1/2006 01:58'!
failBlock: aBlockOrNil
	failBlock := aBlockOrNil! !

!NumberParser methodsFor: 'accessing' stamp: 'nice 5/1/2006 01:59'!
requestor: anObjectOrNil
	requestor := anObjectOrNil! !

!NumberParser methodsFor: 'error' stamp: 'cmm 10/11/2012 22:30'!
error: aString
	NumberParserError signal: aString! !

!NumberParser methodsFor: 'error' stamp: 'nice 2/25/2010 02:39'!
expected: aString 
	| errorString |
	errorString := aString , ' expected'.
	requestor isNil
		ifFalse: [requestor
				notify: errorString
				at: sourceStream position + 1
				in: sourceStream].
	failBlock ifNotNil: [^failBlock cull: errorString cull: sourceStream position + 1].
	self error: 'Reading a number failed: ' , errorString! !

!NumberParser methodsFor: 'parsing-private' stamp: 'nice 7/26/2009 00:22'!
makeFloatFromMantissa: m exponent: k base: aRadix 
	"Convert infinite precision arithmetic into Floating point.
	This alogrithm rely on correct IEEE rounding mode
	being implemented in Integer>>asFloat and Fraction>>asFloat"

	^(k positive
		ifTrue: [m * (aRadix raisedToInteger: k)]
		ifFalse: [Fraction numerator: m denominator: (aRadix raisedToInteger: k negated)]) asFloat! !

!NumberParser methodsFor: 'parsing-private' stamp: 'nice 7/26/2009 00:23'!
makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart
	"at this point integerPart fractionPart and scale have been read out (in inst var).
	Form a ScaledDecimal.
	Care of eliminating trailing zeroes from the fractionPart"
	
	| decimalMultiplier decimalFraction |
	decimalMultiplier := base raisedToInteger: numberOfNonZeroFractionDigits.
	decimalFraction := integerPart * decimalMultiplier + (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)) / decimalMultiplier.
	^ ScaledDecimal
		newFromNumber: (neg
			ifTrue: [decimalFraction negated]
			ifFalse: [decimalFraction])
		scale: scale! !

!NumberParser methodsFor: 'parsing-private' stamp: 'nice 2/12/2010 23:57'!
peekSignIsMinus
	"Peek an optional sign from sourceStream.
	Answer true if it is minus sign"

	| isMinus |
	isMinus := sourceStream peekFor: $-.
	isMinus ifFalse: [self allowPlusSign ifTrue: [sourceStream peekFor: $+]].
	^isMinus! !

!NumberParser methodsFor: 'parsing-private' stamp: 'nice 2/13/2010 15:53'!
readExponent
	"read the exponent if any (stored in instVar).
	Answer true if found, answer false if none.
	If exponent letter is not followed by a digit,
	this is not considered as an error.
	Exponent are always read in base 10."
	
	| eneg epos |
	exponent := 0.
	sourceStream atEnd ifTrue: [^ false].
	(self exponentLetters includes: sourceStream peek)
		ifFalse: [^ false].
	sourceStream next.
	eneg := sourceStream peekFor: $-.
	epos := eneg not and: [self allowPlusSignInExponent and: [sourceStream peekFor: $+]].
	exponent := self nextUnsignedIntegerOrNilBase: 10.
	exponent ifNil: ["Oops, there was no digit after the exponent letter.Ungobble the letter"
		exponent := 0.
		sourceStream
						skip: ((eneg or: [epos])
								ifTrue: [-2]
								ifFalse: [-1]).
					^ false].
	eneg ifTrue: [exponent := exponent negated].
	^true! !

!NumberParser methodsFor: 'parsing-large int' stamp: 'nice 3/15/2010 00:14'!
nextElementaryLargeIntegerBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading if end of digits or if a LargeInteger is formed.
	Count the number of digits and the position of lastNonZero digit and store them in instVar."

	| value digit char |
	value := 0.
	nDigits := 0.
	lastNonZero := 0.
	[value isLarge or: [(char := sourceStream next) == nil
		or: [digit := char digitValue.
			(0 > digit or: [digit >= aRadix])
				and: [sourceStream skip: -1.
					true]]]]
		whileFalse: [
			nDigits := nDigits + 1.
			0 = digit
				ifFalse: [lastNonZero := nDigits].
			value := value * aRadix + digit].
	^value! !

!NumberParser methodsFor: 'parsing-large int' stamp: 'nice 1/14/2014 03:27'!
nextLargeIntegerBase: aRadix nPackets: nPackets 
	"Form a Large integer with incoming digits from sourceStream.
	Return this integer, or zero if no digits found.
	Stop reading when no more digits or when nPackets elementary LargeInteger have been encountered.
	Count the number of digits and the lastNonZero digit and store them in instVar"
	
	| high nDigitsHigh lastNonZeroHigh low nDigitsLow halfPackets |
	halfPackets := nPackets bitShift: -1.
	halfPackets = 0 ifTrue: [^self nextElementaryLargeIntegerBase: aRadix].
	high := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	high isLarge ifFalse: [^high].
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	low := self nextLargeIntegerBase: aRadix nPackets: halfPackets.
	nDigitsLow := nDigits.
	nDigits := nDigitsHigh + nDigitsLow.
	lastNonZero := lastNonZero = 0
		ifTrue: [lastNonZeroHigh]
		ifFalse: [lastNonZero + nDigitsHigh].
	^high * (aRadix raisedToInteger: nDigitsLow) + low! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 5/20/2012 15:00'!
nextInteger
	"Read an Integer from sourceStream, asnwser that Integer.
	This is a generic version dealing with an optional sign and a simple sequence of decimal digits.
	Subclass might define extended syntax."
	
	base := 10.
	^self nextIntegerBase: base ifFail: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: base - 1))]! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 4/2/2010 20:25'!
nextIntegerBase: aRadix
	"Form an integer with following digits.
	Fail if no digit found"
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerBase: aRadix.
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 8/27/2010 20:51'!
nextIntegerBase: aRadix ifFail: aBlock
	"Form an integer with optional sign and following digits from sourceStream."
	
	| isNeg value |
	isNeg := self peekSignIsMinus.
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^aBlock value].
	^isNeg
		ifTrue: [value negated]
		ifFalse: [value]! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 2/12/2010 23:59'!
nextNumber
	"read next number from sourceStream contents"

	^self subclassResponsibility! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 5/20/2012 15:00'!
nextUnsignedInteger
	"Read an Integer from sourceStream, asnwser that Integer.
	This is a generic version dealing with a simple sequence of decimal digits.
	Subclass might define extended syntax."
	
	base := 10.
	^self nextUnsignedIntegerBase: base ifFail: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: base - 1))]! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 10/16/2008 01:05'!
nextUnsignedIntegerBase: aRadix 
	"Form an unsigned integer with incoming digits from sourceStream.
	Fail if no digit found.
	Count the number of digits and the lastNonZero digit and store int in instVar "
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^self expected: ('a digit between 0 and ' copyWith: (Character digitValue: aRadix - 1))].
	^value! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 10/16/2008 01:05'!
nextUnsignedIntegerBase: aRadix ifFail: errorBlock
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or execute errorBlock if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| value |
	value := self nextUnsignedIntegerOrNilBase: aRadix.
	value ifNil: [^errorBlock value].
	^value! !

!NumberParser methodsFor: 'parsing-public' stamp: 'nice 7/26/2009 00:21'!
nextUnsignedIntegerOrNilBase: aRadix
	"Form an unsigned integer with incoming digits from sourceStream.
	Answer this integer, or nil if no digit found.
	Count the number of digits and the position of lastNonZero digit and store them in instVar"
	
	| nPackets high nDigitsHigh lastNonZeroHigh low |
	"read no more digits than one elementary LargeInteger"
	high :=  self nextElementaryLargeIntegerBase: aRadix.
	nDigits = 0 ifTrue: [^nil].
	
	"Not enough digits to form a LargeInteger, stop iteration"
	high isLarge ifFalse: [^high].

	"We now have to engage arithmetic with LargeInteger
	Decompose the integer in a high and low packets of growing size:"
	nPackets := 1.
	nDigitsHigh := nDigits.
	lastNonZeroHigh := lastNonZero.
	[
	low := self nextLargeIntegerBase: aRadix nPackets: nPackets .
	high := high * (aRadix raisedToInteger: nDigits) + low.
	lastNonZero = 0 ifFalse: [lastNonZeroHigh := lastNonZero + nDigitsHigh].
	nDigitsHigh := nDigitsHigh + nDigits.
	low isLarge]
		whileTrue: [nPackets := nPackets * 2].

	nDigits := nDigitsHigh.
	lastNonZero := lastNonZeroHigh.
	^high! !

!NumberParser methodsFor: 'initialize-release' stamp: 'damiencassou 5/30/2008 10:56'!
on: aStringOrStream 
	sourceStream := aStringOrStream isString 
		ifTrue: [ aStringOrStream readStream ]
		ifFalse: [ aStringOrStream ].
	base := 10.
	neg := false.
	integerPart := fractionPart := exponent := scale := 0.
	requestor := failBlock := nil! !


!NumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 00:45'!
on: aStringOrStream
	^self new on: aStringOrStream! !

!NumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 02:02'!
parse: aStringOrStream 
	^(self new)
		on: aStringOrStream;
		nextNumber! !

!NumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 02:02'!
parse: aStringOrStream onError: failBlock 
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber! !


!SqNumberParser methodsFor: 'accessing' stamp: 'nice 2/13/2010 00:00'!
allowPlusSign
	"return a boolean indicating if plus sign is allowed or not"

	^false! !

!SqNumberParser methodsFor: 'accessing' stamp: 'nice 4/27/2006 22:57'!
exponentLetters
	"answer the list of possible exponents for Numbers.
	Note: this parser will not honour precision attached to the exponent.
	different exponent do not lead to different precisions.
	only IEEE 754 floating point numbers will be created"
	
	^'edq'! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'eem 8/6/2014 10:33'!
makeIntegerOrScaledInteger
	"At this point, there is no digit, nor fractionPart.
	 Maybe it can be a scaled decimal with fraction omitted...
	 Maybe it can be a Float with a negative exponent"
	
	neg ifTrue:
		[integerPart := integerPart negated].
	self readExponent ifTrue:
		["Check that the result is an integer, otherwise answer a Float.  Fractions are /not/ valid literals."
		 (exponent >= 0 or: [(integerPart * (base raisedToInteger: exponent)) isInteger]) ifFalse:
			[base := base asFloat].
		^integerPart * (base raisedToInteger: exponent)].
	(self readScaleWithDefaultNumberOfDigits: 0) ifTrue:
		[^integerPart asScaledDecimal: scale].
	^ integerPart! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'nice 10/7/2009 01:40'!
makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart
	"at this point integerPart fractionPart and scale have been read out (in inst var).
	Form a ScaledDecimal.
	Care of eliminating trailing zeroes from the fractionPart"
	
	| decimalMultiplier decimalFraction |
	decimalMultiplier := base raisedToInteger: numberOfNonZeroFractionDigits.
	decimalFraction := integerPart * decimalMultiplier + (fractionPart // (base raisedTo: numberOfTrailingZeroInFractionPart)) / decimalMultiplier.
	neg
		ifTrue: [decimalFraction := decimalFraction negated].
	^decimalFraction asScaledDecimal: scale! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'nice 5/22/2012 22:32'!
readFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart
	"at this stage, sign integerPart and a decimal point have been read.
	try and form a number with a fractionPart"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart mantissa value |
	fractionPart := self nextUnsignedIntegerOrNilBase: base.
	fractionPart ifNil: ["No fractionPart found, ungobble the decimal point and return the integerPart"
					sourceStream skip: -1.
					^ neg
						ifTrue: [integerPart negated]
						ifFalse: [integerPart]].
	numberOfNonZeroFractionDigits := lastNonZero.
	numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
	self readExponent.
	fractionPart isZero
		ifTrue: [mantissa := integerPart
						// (base raisedToInteger: numberOfTrailingZeroInIntegerPart).
			exponent := exponent + numberOfTrailingZeroInIntegerPart]
		ifFalse: [mantissa := integerPart
						* (base raisedToInteger: numberOfNonZeroFractionDigits) + (fractionPart // (base raisedToInteger: numberOfTrailingZeroInFractionPart)).
			exponent := exponent - numberOfNonZeroFractionDigits].

	value := exponent positive
		ifTrue: [mantissa * (base raisedToInteger: exponent)]
		ifFalse: [mantissa / (base raisedToInteger: exponent negated)].
	^ neg
		ifTrue: [value negated]
		ifFalse: [value]! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'nice 5/3/2014 22:33:10.493'!
readNamedFloatOrFail
	"This method is used when there is no digit encountered:
	It try and read a named Float NaN or Infinity.
	Negative sign for -Infinity has been read before sending this method, and is indicated in the neg inst.var.
	Fail if no named Float is found"
		
	neg ifFalse: [(sourceStream nextMatchAll: 'NaN')
			ifTrue: [^ Float nan]].
	(sourceStream nextMatchAll: 'Infinity')
		ifTrue: [^ neg
			ifTrue: [Float negativeInfinity]
			ifFalse: [Float infinity]].
	^self expected: 'a digit between 0 and ' , (String with: (Character digitValue: base - 1))! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'nice 3/15/2014 15:45'!
readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart
	"at this stage, sign integerPart and a decimal point have been read.
	try and form a number with a fractionPart"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart mantissa value |
	fractionPart := self nextUnsignedIntegerOrNilBase: base.
	fractionPart ifNil: ["No fractionPart found,ungobble the decimal point and return the integerPart"
					sourceStream skip: -1.
					^ neg
						ifTrue: [integerPart negated]
						ifFalse: [integerPart]].
	numberOfNonZeroFractionDigits := lastNonZero.
	numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
	self readExponent
		ifFalse: [(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifTrue: [^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits
					andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart]].

	fractionPart isZero
		ifTrue: [mantissa := integerPart
						// (base raisedToInteger: numberOfTrailingZeroInIntegerPart).
			exponent := exponent + numberOfTrailingZeroInIntegerPart]
		ifFalse: [mantissa := integerPart
						* (base raisedToInteger: numberOfNonZeroFractionDigits) + (fractionPart // (base raisedToInteger: numberOfTrailingZeroInFractionPart)).
			exponent := exponent - numberOfNonZeroFractionDigits].

	value := self makeFloatFromMantissa: mantissa exponent: exponent base: base.
	^ neg
		ifTrue: [value isZero
				ifTrue: [Float negativeZero]
				ifFalse: [value negated]]
		ifFalse: [value]! !

!SqNumberParser methodsFor: 'parsing-private' stamp: 'nice 3/15/2014 15:43'!
readScaleWithDefaultNumberOfDigits: anInteger
	"Read the scale if any and store it into scale instance Variable.
	Answer true if found, answer false if none.
	The scale is specified by letter s, optionnally followed by a positive integer in base 10.
	If no integer is specified, that means using as many digits as provided after the fraction separator, as provided by parameter anInteger.
	A letter s followed by another letter is not considered as a scale specification, because it could be part of a message."

	scale := 0.
	sourceStream atEnd
		ifTrue: [ ^ false ].
	(sourceStream peekFor: $s)
		ifFalse: [ ^ false ].
	scale := self nextUnsignedIntegerOrNilBase: 10.
	scale
		ifNil: [ 
			scale := anInteger.
			(sourceStream peek ifNil: [ false ] ifNotNil: [ :nextChar | nextChar isLetter ])
				ifTrue: [ 
					sourceStream skip: -1.	"ungobble the s"
					^ false ]
				ifFalse: [ ^ true ] ].
	^ true! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 5/22/2012 22:48'!
nextFraction
	| numerator denominator numberOfTrailingZeroInIntegerPart |
	base := 10.
	neg := self peekSignIsMinus.
	(integerPart := self nextUnsignedIntegerOrNilBase: base)
		ifNil: ["No integerPart, raise an error"
			^ self expected: 'a digit'].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			self peekSignIsMinus
				ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	(sourceStream peekFor: $.)
		ifTrue:
			[^self readFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart].
	numerator := neg
		ifTrue: [integerPart negated]
		ifFalse: [integerPart].
	self readExponent ifTrue: [numerator := numerator * (base raisedToInteger: exponent)].
	(sourceStream peekFor: $/) ifFalse: [^numerator].
	base := 10.
	(denominator := self nextUnsignedIntegerOrNilBase: base)
		ifNil:
			[sourceStream skip: -1. "Not a valid denominator, ungobble / and return numerator"
			^numerator].
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := denominator) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			denominator := self nextUnsignedIntegerBase: base].
	self readExponent ifTrue: [denominator := denominator * (base raisedToInteger: exponent)].
	^numerator / denominator! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 5/20/2012 16:09'!
nextFractionBase: aRadix
	| numerator denominator |
	base := aRadix.
	neg := self peekSignIsMinus.
	(integerPart := self nextUnsignedIntegerOrNilBase: base)
		ifNil: [^self expected: 'a digit between 0 and ' , (Character digitValue: aRadix)].
	numerator := neg
		ifTrue: [integerPart negated]
		ifFalse: [integerPart].
	self readExponent ifTrue: [numerator := numerator * (base raisedToInteger: exponent)].
	(sourceStream peekFor: $/) ifFalse: [^numerator].
	(denominator := self nextUnsignedIntegerOrNilBase: base)
		ifNil:
			[sourceStream skip: -1. "Not a valid denominator, ungobble / and return numerator"
			^numerator].
	self readExponent ifTrue: [denominator := denominator * (base raisedToInteger: exponent)].
	^numerator / denominator! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 5/20/2012 14:55'!
nextInteger
	"Read an Integer from sourceStream, asnwser that Integer.
	In Smalltalk syntax, a radix can be specified, and an exponent too."
	
	| numberOfTrailingZeroInIntegerPart |
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			self peekSignIsMinus
				ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	^ self makeIntegerOrScaledInteger! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 2/24/2010 02:51'!
nextNumber
	"main method for reading a number.
	This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			self peekSignIsMinus
				ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerBase: base.
			numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero].
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledInteger]! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 10/16/2008 00:50'!
nextNumberBase: b
	"Method for reading a number without radix prefix.
	This one can read Float Integer and ScaledDecimal"
	
	| numberOfTrailingZeroInIntegerPart |
	base := b.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [
		"This is not a regular number beginning with a digit
		It is time to check for exceptional condition NaN and Infinity"
		^self readNamedFloatOrFail].
	numberOfTrailingZeroInIntegerPart := nDigits - lastNonZero.
	^ (sourceStream peekFor: $.)
		ifTrue: [self readNumberWithFractionPartNumberOfTrailingZeroInIntegerPart: numberOfTrailingZeroInIntegerPart]
		ifFalse: [self makeIntegerOrScaledInteger]! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 3/15/2014 15:46'!
nextScaledDecimal
	"Main method for reading a (scaled) decimal number.
	Good Gracious, do not accept a decimal in another base than 10!!
	In other words, do not accept radix notation like 2r1.1, even not 10r5.3
	Do not accept exponent notation neither, like 1.0e-3"
	
	| numberOfNonZeroFractionDigits numberOfTrailingZeroInFractionPart |
	base := 10.
	neg := sourceStream peekFor: $-.
	integerPart := self nextUnsignedIntegerBase: base.
	(sourceStream peekFor: $.)
		ifTrue: [fractionPart := self nextUnsignedIntegerOrNilBase: base.
			fractionPart ifNil: ["Oops, the decimal point seems not part of this number"
							sourceStream skip: -1.
							^ neg
								ifTrue: [integerPart negated asScaledDecimal: 0]
								ifFalse: [integerPart asScaledDecimal: 0]].
			numberOfNonZeroFractionDigits := lastNonZero.
			numberOfTrailingZeroInFractionPart := nDigits - lastNonZero.
			(self readScaleWithDefaultNumberOfDigits: nDigits)
				ifFalse: ["No scale were provided. use number of digits after decimal point as scale"
					scale := nDigits].
			^self makeScaledDecimalWithNumberOfNonZeroFractionDigits: numberOfNonZeroFractionDigits andNumberOfTrailingZeroInFractionPart: numberOfTrailingZeroInFractionPart].
	self readScaleWithDefaultNumberOfDigits: 0.
	neg	ifTrue: [integerPart := integerPart negated].
	^integerPart asScaledDecimal: scale! !

!SqNumberParser methodsFor: 'parsing-public' stamp: 'nice 5/20/2012 15:01'!
nextUnsignedInteger
	"Read an unsigned Integer from sourceStream, asnwser that Integer.
	In Smalltalk syntax, a radix can be specified, and an exponent too."
	
	base := 10.
	neg := false.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: [
					sourceStream skip: -1.
					^ self expected: 'an integer greater than 1 as valid radix'].
			integerPart := self nextUnsignedIntegerBase: base].
	^ self makeIntegerOrScaledInteger! !


!SqNumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 00:45'!
on: aStringOrStream
	^self new on: aStringOrStream! !

!SqNumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 02:02'!
parse: aStringOrStream 
	^(self new)
		on: aStringOrStream;
		nextNumber! !

!SqNumberParser class methodsFor: 'instance creation' stamp: 'nice 5/1/2006 02:02'!
parse: aStringOrStream onError: failBlock 
	^(self new)
		on: aStringOrStream;
		failBlock: failBlock;
		nextNumber! !


!ZipWriteStream methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 23:35'!
encodedStream
^ self! !


!ScaledDecimal class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:27'!
newFromNumber: decimalFraction scale: scale
^ decimalFraction! !


!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:33'!
addTextRow: nothing! !

!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:55'!
messageNode: aNode receiver: receiver selector: selector keywords: key arguments: args ! !

!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:33'!
parseNode: nothing! !

!SyntaxMorph methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:33'!
translateColor: nothing! !


!SystemNavigation class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:20'!
default
^ Smalltalk! !


!TextConstants class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:22'!
at: stuff
stuff == #DefaultTab ifTrue: [^4]! !


!TextStyle class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:21'!
default! !

!TextStyle class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:21'!
defaultFont
^ StrikeFont default ! !


!ToolSet class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:28'!
debugSyntaxError: anError
self halt! !


!UIManager class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:18'!
chooseFrom: labels lines: lines title: caption
(PopUpMenu labelArray: labels lines: lines) startUpWithCaption: caption! !

!UIManager class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:18'!
default! !

!UIManager class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:20'!
informUser: aString during: aBlock
Utilities informUser: aString during: aBlock! !

!UIManager class methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:19'!
request: queryString initialAnswer: defaultAnswer 
^ FillInTheBlankMorph
		request: queryString
		initialAnswer: defaultAnswer
! !

