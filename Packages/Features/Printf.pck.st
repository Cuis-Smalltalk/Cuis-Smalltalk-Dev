'From Cuis 6.0 [latest update: #5299] on 18 June 2022 at 11:53:03 am'!
'Description '!
!provides: 'Printf' 1 40!
SystemOrganization addCategory: 'Printf'!


!classDefinition: #PrintfFormatStringTest category: 'Printf'!
TestCase subclass: #PrintfFormatStringTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfFormatStringTest class' category: 'Printf'!
PrintfFormatStringTest class
	instanceVariableNames: ''!

!classDefinition: #Printf category: 'Printf'!
Object subclass: #Printf
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'Printf class' category: 'Printf'!
Printf class
	instanceVariableNames: ''!

!classDefinition: #PrintfFormatDescriptor category: 'Printf'!
Object subclass: #PrintfFormatDescriptor
	instanceVariableNames: 'flush width precision'
	classVariableNames: 'Flags Operators'
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfFormatDescriptor class' category: 'Printf'!
PrintfFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfCharacterFormatDescriptor category: 'Printf'!
PrintfFormatDescriptor subclass: #PrintfCharacterFormatDescriptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfCharacterFormatDescriptor class' category: 'Printf'!
PrintfCharacterFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfNumberFormatDescriptor category: 'Printf'!
PrintfFormatDescriptor subclass: #PrintfNumberFormatDescriptor
	instanceVariableNames: 'operator padding radix space thousandSeparate'
	classVariableNames: 'Base Cased DecimalSeparator Radix ThousandSeparator UserDecimalSeparator UserThousandSeparator'
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfNumberFormatDescriptor class' category: 'Printf'!
PrintfNumberFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfFloatFormatDescriptor category: 'Printf'!
PrintfNumberFormatDescriptor subclass: #PrintfFloatFormatDescriptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfFloatFormatDescriptor class' category: 'Printf'!
PrintfFloatFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfNumberHolderDescriptor category: 'Printf'!
PrintfFormatDescriptor subclass: #PrintfNumberHolderDescriptor
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfNumberHolderDescriptor class' category: 'Printf'!
PrintfNumberHolderDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfStringFormatDescriptor category: 'Printf'!
PrintfFormatDescriptor subclass: #PrintfStringFormatDescriptor
	instanceVariableNames: 'widthDescriptor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfStringFormatDescriptor class' category: 'Printf'!
PrintfStringFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintStringFormatDescriptor category: 'Printf'!
PrintfStringFormatDescriptor subclass: #PrintStringFormatDescriptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintStringFormatDescriptor class' category: 'Printf'!
PrintStringFormatDescriptor class
	instanceVariableNames: ''!

!classDefinition: #PrintfFormatString category: 'Printf'!
Object subclass: #PrintfFormatString
	instanceVariableNames: 'format string outputStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Printf'!
!classDefinition: 'PrintfFormatString class' category: 'Printf'!
PrintfFormatString class
	instanceVariableNames: ''!


!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 1/15/2022 17:22:03'!
test100_ThousandSeparator
       ". small numbers are ignored "
	self assert: (('%_d' printf: {123}) = '123' ) .
	self assert: (('%_d' printf: {1234}) = '1_234' ) .
	self assert: (('%_d' printf: {12345678}) = '12_345_678' ) .
	". very large numbers must be ok "
	self assert: (('%_d' printf: {123456789123456789}) = '123_456_789_123_456_789' ) .   
	
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 1/15/2022 17:24:52'!
test101_LargeFloat
       ". this test was added to remember there is an ugly bug here "
	self assert: ('%.2f' printf: {1.1}) = '1.10' .  
	self assert: ('%.2f' printf: {12.1}) = '12.10' .  
	self assert: ('%.2f' printf: {123.1}) = '123.10' . 
       self assert: ('%.2f' printf: {1234.1}) = '1234.10' .	  
	self assert: ('%.2f' printf: {12345.1}) = '12345.10' . 
	self assert: ('%.2f' printf: {123456.1}) = '123456.10' .  
	". from here on it is broken " 
	self assert: ('%.2f' printf: {1234567.1}) = '1234567.10'.     ". gives wrongly 1.23"
	self assert: ('%.2f' printf: {12345678.1}) = '12345678.10'.  ". gives wrongly 1.23"

	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 1/15/2022 22:26:12'!
test102_FloatsWithThousandSeparator
       ". Testing the print of floating points with thousand separator "
	self assert: (('%_0.2f' printf: {1234}) = '1_234.00' ) .
	self assert: (('%_0.2f' printf: {1234.1}) = '1_234.10' ) .
	self assert: (('%_0.2f' printf: {1234.12}) = '1_234.12' ) .
	self assert: (('%_0.2f' printf: {1234.123}) = '1_234.12' ) .
	". digit limiting is by trucation "
	self assert: (('%_0.2f' printf: {1234.123}) = '1_234.12' ) .
	self assert: (('%_0.2f' printf: {1234.127}) = '1_234.12' ) .! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 1/16/2022 00:24:24'!
test103_UserDefinedThousandAndDecimals
       ". If the user set nothing number are formatted like this "
	self assert: (('%_0.2f' printf: {1234.567}) = '1_234.56'  ). 
	". the underscore may not be liked but it can be userful to re-parse the number,
	  Perl, Ruby, and recently Python recognize '123_123.12' numbers literals 
	. here we format the number in the US standard way.
	"
	PrintfNumberFormatDescriptor setUserThousandSeparatorTo: $, .
	self assert: (('%_0.2f' printf: {1234.567}) = '1,234.56'  ).
	". now let's print the number in the way it is considered 'normal' in Italy, for example " 
	PrintfNumberFormatDescriptor setUserThousandSeparatorTo: $. .
	PrintfNumberFormatDescriptor setUserDecimalSeparatorTo: $, .
	self assert: (('%_0.2f' printf: {1234.567}) = '1.234,56'  ).
	". and let's put back the default configuaration, since the other test expect that " 
	PrintfNumberFormatDescriptor setUserThousandSeparatorTo: nil .
	PrintfNumberFormatDescriptor setUserDecimalSeparatorTo: nil .
	self assert: (('%_0.2f' printf: {1234.567}) = '1_234.56'  ). 
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/30/2021 18:39:08'!
testFprintf
	| stream expected |
	stream := String new writeStream.
	expected := 'Here is a long string "hello world" and the same shortened "hello w       " with left flush.\The new line has a number "42.0"\and a 0-padded limited precision one "0000042.12345".' withNewLines.
	self assert: expected size equals: (stream f: 'Here is a %s string "%s" and the same shortened "%-14.7s" with left flush.\nThe new line has a number "%e"
and a 0-padded limited precision one "%013.5e".'
		printf: #('long' 'hello world' 'hello world' 42.0 42.1234567)).
	self assert: expected equals: stream contents
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:47:49'!
testManyArguments

	self assert: 'Here is a long string "hello world" and the same shortened "hello w       " with left flush.
The new line has a number "42.0" and a 0-padded limited precision one "0000042.12345".' equals: ('Here is a %s string "%s" and the same shortened "%-14.7s" with left flush.\nThe new line has a number "%e" and a 0-padded limited precision one "%013.5e".'
		printf: #('long' 'hello world' 'hello world' 42.0 42.1234567))
		! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:00'!
testOneExponent

	self assert: 'Here is a number "42.0".' equals: ('Here is a number "%e".' printf: 42.0).
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:09'!
testOneExponentAndOneInteger

	self assert: 'Here is a float "42.0" and an integer "42".' equals: ('Here is a float "%e" and an integer "%d".' printf: #(42.0 42)).
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:18'!
testOneExponentWithLength

	self assert: 'Here is a string "0000042.12345".' equals: ('Here is a string "%013.5e".' printf: 42.1234567).
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:26'!
testOneInteger

	self assert: 'Here is a number "42".' equals: ('Here is a number "%d".' printf: 42).
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:37'!
testOneIntegerWithLength

	self assert: 'Here is a string "0000042".' equals: ('Here is a string "%07.7d".' printf: 42).
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:47'!
testOnePrintPointer

	self assert: 'Here is a pointer "0x12345678".' equals: ('Here is a pointer "%p".' printf: 16r12345678)
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:48:57'!
testOnePrintString

	self assert: 'Here is a point "3@4".' equals: ('Here is a point "%P".' printf: 3@4)
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:49:08'!
testOneString

	self assert: 'Here is a string "hello world".' equals: ('Here is a string "%s".' printf: 'hello world')
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:49:19'!
testOneStringWithLength

	self assert:  'Here is a string "hello world" and another shortened "hello w       ".' equals: ('Here is a string "%s" and another shortened "%-14.7s".' printf: #('hello world' 'hello world'))
	! !

!PrintfFormatStringTest methodsFor: 'as yet unclassified' stamp: 'NM 8/16/2021 14:49:28'!
testReturnValue

	self assert: 6+8 equals: ((PrintfFormatString new setFormat: 'hello %ld') printf: {12345678})
	! !

!Printf methodsFor: 'documentation' stamp: 'NM 1/16/2022 00:42:53'!
changeLog
	^ nil 
"

==== [NM] on 16-jan-2022 =========================
. Added new test, started numbering of new tests. Easier to reference them, Easier to 
follow them when studying a new package. PLEASE, keep numbering the test progressively
if you add more. 

. added support for prettyprintinf float numbers, that is, with thousand separator and decimal
separator. These can be also set by the user, making the work locale independent. 
Please see tests 100, 102, 103.

. discovered bug, exposed in test 101, still to be corrected. it relates to the printing of large numbers. 

. added the method 'hackMe' , to contain some minimal documentation about how to add stuff
to this package. 

=======================================
"! !

!Printf methodsFor: 'documentation' stamp: 'NM 1/16/2022 00:59:35'!
hackIt
	^ nil 
"

====== [NM] on  16-Jan-2022 =============================

. This package is not so easy to understand, when i started i underestimated the complexity.

. Suppose you are adding a new function character 'z', the first thing you need to get working is
 the third line, the others are examples. 
PrintfFormatDescriptor scanFrom: (ReadStream on: 'd'). 
PrintfFormatDescriptor scanFrom: (ReadStream on: '-s'). 
PrintfFormatDescriptor scanFrom: (ReadStream on: 'z'). 

. The recognized charactes (%d, %s, %f ... ) and flags are defined in: 
PrintfFormatDescriptor class >> initialize 

. Who does the number printinting are, for example:
PrintfNumberFormatDescription >> applyOperator: anObject. 
  [ the anObject here is for example: 123 ]
PrintfFloatFormatDescription >> applyOperator: anObject.
[ the anObject here is for example: 123.123 ] 



=====================================================

"! !

!Printf methodsFor: 'documentation' stamp: 'NM 8/30/2021 18:56:05'!
history
	^ nil 
"
This package was ported from Squeak to Cuis on 20 Aug 2021 by Dr. Nicola Mingotti [NM]. 

It was taken from the Squeak release: Printf-eem.12.mcz.

At the moment of porting the package has 12 tests. 7 Pass, 5 give errors. 
The same outcome apply in Squeak as in Cuis. 

On date 30-Aug-2021 [NM] has made a few changes. Now all tests are green.
To make 2 test pass [NM] has to convert '\n' to NewLines instead of CR as it 
was set in 2 tests. 

Extracted from: http://www.squeaksource.com/Printf.html 
------
Creator: 	Dominic Letz
Admin: 	Eliot Miranda, Michael Rueger, Dominic Letz
------	
		
	
"! !

!Printf methodsFor: 'documentation' stamp: 'NM 8/20/2021 14:44:53'!
readme
	^ nil 
"

. 'printf' is the name an extremely popular function for string formatting in the language C. 
. It has been ported to several languages
. in Printf you will find implemented several, but not all, the features of the original 'printf',

=================================================
======== Examples ==================================
==================================================

-] substitute an integer in string
'foo %d bar' printf: {123}.                 =>  'foo 123 bar' 

-] substitute a float in string with a fixed number of decimals
'foo %.5f bar' printf: {123.12}.         =>  'foo 123.12000 bar' 

----- ADD MORE !!

==================================================
======= Features of C printf still unavailable in Printf ============
===================================================



----- TODO 


"! !

!PrintfFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:09:40'!
printOn: aStream
	aStream nextPut: $%.
	flush == #leftFlush ifTrue: [aStream nextPut: $-]
	! !

!PrintfFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:09:52'!
printWidthOn: aStream
	width ~= 0 ifTrue: [width printOn: aStream].
	precision ifNotNil: [aStream nextPut: $.. precision printOn: aStream]
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 1/15/2022 19:54:31'!
doThousandSeparate
	|out| 
	out _ (PrintfNumberFormatDescriptor newFrom: self) .
	out thousandSeparate: true.
	^ out 
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:07:52'!
leftFlush
	flush := #leftFlush
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:10:05'!
radix
	^ (PrintfNumberFormatDescriptor newFrom: self) radix
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:11:42'!
rightFlush
	flush := #rightFlush
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:12:08'!
space
	^ (PrintfNumberFormatDescriptor newFrom: self) space
	! !

!PrintfFormatDescriptor methodsFor: 'scanning' stamp: 'NM 1/15/2022 18:43:55'!
zero
	|out| 
	(self class = PrintfNumberFormatDescriptor) 
	ifTrue: [ out _ self zero ] 
	ifFalse: [ out _ (PrintfNumberFormatDescriptor newFrom: self) zero ].
	^ out 
	" ^ (PrintfNumberFormatDescriptor newFrom: self) zero "
	! !

!PrintfFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:07:00'!
applyOperator: object
	self subclassResponsibility! !

!PrintfFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:10:16'!
render: object
	| string |
	string := self applyOperator: object.
	self stringLength ~= 0
		ifTrue: [ string := string copyFrom: 1 to: (self stringLength min: string size) ].
	width == 0
		ifTrue: [ ^ string ].
	^ (String new: width withAll: self padding)
		copyReplaceFrom: (self startIndexOfCopyReplaceWithStringSize: string size)
		to: (self stopIndexOfCopyReplaceWithStringSize: string size)
		with: string
		! !

!PrintfFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:12:20'!
startIndexOfCopyReplaceWithStringSize: anInteger

	|start|
	flush == #leftFlush ifTrue: [start := 1].
	flush == #rightFlush ifTrue: [start := width - anInteger + 1]. 
	^(start max: 1)

! !

!PrintfFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:12:37'!
stopIndexOfCopyReplaceWithStringSize: anInteger

	| stop |
	flush == #leftFlush ifTrue: [stop := anInteger].
	flush == #rightFlush ifTrue: [stop := width]. 
	^stop min: width
	
! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:07:15'!
flush
	^ flush! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 1/15/2022 22:33:18'!
operator: char
	| myself |
	myself := (Smalltalk at: (Operators at: char)) newFrom: self.
	myself setOperator: char.
	". case of the floating point with thousand separator, not good here, just a start " 
	((self class = PrintfNumberFormatDescriptor) and:
	 (myself class = PrintfFloatFormatDescriptor )) ifTrue: [
		"Transcript show: 'DBG --- Transforming a float with thousand sep ' . "
		" myself thousandSeparator: (self thousandSeparator) "
		].
	^ myself
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:09:04'!
padding
	^ Character space
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:09:16'!
precision
	^ precision
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:09:27'!
precision: anInteger
	precision := anInteger
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:11:57'!
setOperator: char
! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/30/2021 17:38:20'!
stringLength
	^precision ifNil: [0]
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:12:59'!
width
	^ width
	! !

!PrintfFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:13:09'!
width: anInteger
	width := anInteger
	! !

!PrintfFormatDescriptor methodsFor: 'initialize-release' stamp: 'NM 8/16/2021 15:27:06'!
initialize
	flush := #rightFlush.
	width := 0.
	! !

!PrintfFormatDescriptor class methodsFor: 'instance creation' stamp: 'NM 8/16/2021 14:19:22'!
new
	^ super new initialize
	! !

!PrintfFormatDescriptor class methodsFor: 'instance creation' stamp: 'NM 8/16/2021 14:19:33'!
newFrom: desc
	| myself |
	myself := self new.
	myself perform: desc flush.
	myself width: desc width.
	myself precision: desc precision.
	^ myself
	! !

!PrintfFormatDescriptor class methodsFor: 'instance creation' stamp: 'NM 8/16/2021 14:19:45'!
scanFrom: stream
	| descriptor numberHolderDescriptor |
	descriptor := self new.
	[Flags includesKey: stream peek]
		whileTrue: [descriptor := descriptor perform: (Flags at: stream next)].
	stream peek isDigit ifTrue: [descriptor width: (Integer readFrom: stream)].
	stream peek == $. ifTrue:
		[stream next.
		 stream peek == $*
			ifTrue: [numberHolderDescriptor := PrintfNumberHolderDescriptor new. stream next]
			ifFalse: [descriptor precision: (Integer readFrom: stream)]].
	stream peek == $l ifTrue: [stream next].
	descriptor := descriptor operator: stream next.
	numberHolderDescriptor ifNotNil:
		[descriptor widthDescriptor: numberHolderDescriptor.
		 ^{numberHolderDescriptor. descriptor}].
	^descriptor
	
	! !

!PrintfFormatDescriptor class methodsFor: 'class initialization' stamp: 'NM 1/15/2022 19:53:06'!
initialize
	"PrintfFormatDescriptor initialize"
	Operators := Dictionary new.
	Operators at: $P put: #PrintStringFormatDescriptor.
	Operators at: $c put: #PrintfCharacterFormatDescriptor.
	Operators at: $s put: #PrintfStringFormatDescriptor.
	#($d $o $u $x $X $p)
		do: [:operator | Operators at: operator put: #PrintfNumberFormatDescriptor].
	#($e $E $f $g $G $a $A)
		do: [:operator | Operators at: operator put: #PrintfFloatFormatDescriptor].

	Flags := Dictionary new.
	Flags at: $- put: #leftFlush.
	Flags at: $+ put: #rightFlush.
	Flags at: $  put: #space.
	Flags at: $# put: #radix.
	Flags at: $0 put: #zero.
	Flags at: $_ put: #doThousandSeparate
	
	
	
	! !

!PrintfCharacterFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:23:11'!
printOn: aStream
	super printOn: aStream.
	self printWidthOn: aStream.
	aStream nextPut: $c
	! !

!PrintfCharacterFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:23:00'!
applyOperator: object
	^ String with: object asCharacter
	! !

!PrintfNumberFormatDescriptor methodsFor: 'printing' stamp: 'NM 1/15/2022 15:26:57'!
printOn: aStream
	Transcript log: '---> called NumFormDescriptor >> printOn: '.
	super printOn: aStream.
	thousandSeparate ifTrue: [ aStream nextPut: $_ ]. 
	padding == $0 ifTrue: [aStream nextPut: $0].
	radix ifTrue: [aStream nextPut: $#].
	space ifTrue: [aStream nextPut: $ ].
	self printWidthOn: aStream.
	aStream nextPut: operator
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:31:04'!
base
	^ Base at: operator
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:31:14'!
padding
	^ padding
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:31:24'!
radixString
	^ Radix at: operator
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:31:35'!
setOperator: char
	operator := char
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:31:50'!
setPadding: paddingChar
	padding := paddingChar
	! !

!PrintfNumberFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:32:00'!
stringLength
	^precision isNil
		ifTrue: [SmallInteger maxVal]
		ifFalse: [precision]
		! !

!PrintfNumberFormatDescriptor methodsFor: 'rendering' stamp: 'NM 1/16/2022 00:02:07'!
addThousandSeparator: numberString
	". receives in input an integer numberString e.g. '12345'
	. returns in output a thousand separated string e.g. '12_345' 
	. '_' is the value lf 'thousandSeparator' 
	. if 'base' is not is not 10 returns just 'numberString', since a separation by 3 digits blocks
	 would not mean what 'thousand' says; in that case also print a warning in the Transcript'. 
	"
	(self base ~= 10) ifTrue:  [ 
		Transcript log: 'Error. Printf. thousand separation is not defined for non base 10 numbers.' . 
		^ numberString . 
		].
	". if we arrived here the base is 10 " 
	^ self formatWithUserPreferences: ( 
		(numberString asNumber printStringWithCommas) 
			copyReplaceAll: ',' with: (self thousandSeparator asString) ) 
 	
	! !

!PrintfNumberFormatDescriptor methodsFor: 'rendering' stamp: 'NM 1/15/2022 16:28:30'!
applyOperator: object 
	"Character and Number are the only valid classes"

	| number string |
	object ifNil: [^'-'].
	number := object asInteger.
	
	string := number printStringBase: self base.
	". apply thousand separation, if reasonable "
	thousandSeparate ifTrue: [ string _ self addThousandSeparator: string ] . 
	(radix or: [operator == $p]) ifTrue: [string := self radixString , string].
	(Cased includes: operator) ifTrue:
		[string := operator isLowercase
					ifTrue: [string asLowercase]
					ifFalse: [string asUppercase]].
	(space and: [operator == $d and: [number < 0]]) ifTrue:
		[string := ' ' , string].
	^ (width ~= 0 and: [string size > self stringLength])
		ifTrue: [String new: width withAll: $*]
		ifFalse: [string]
		! !

!PrintfNumberFormatDescriptor methodsFor: 'rendering' stamp: 'NM 1/16/2022 00:14:50'!
formatWithUserPreferences: numString
	". numString is a string representing a number of the kind: 123_456.123.
	. according to the values in the class variables UserDecimalSeparator and UserThousandSeparator
	 a string like the following is returned, for example: 123.456,123, which would be appropriate 
	 for a user in Italy.  
	" 
	|userDec userThou tmp|
	userDec _ self class getUserDecimalSeparator . 
	userThou _ self class getUserThousandSeparator .
	". if there aren't user preferences set, then quit and use the default form string. "  
	((userDec isNil) and: (userThou isNil)) ifTrue: [ ^ numString ].
	". if any of the user preferences is undefined set it to the default valued " 
	(userDec isNil) ifTrue: [userDec _ self class getDecimalSeparator ].
	(userThou isNil) ifTrue: [userThou _ self class getThousandSeparator  ] . 
	". do the change and return the new formatted string " 
	tmp _ numString copyReplaceAll: '_' with: 'THO' . 
	tmp _ tmp copyReplaceAll: '.' with: 'DEC'. 
	tmp _ tmp copyReplaceAll: 'DEC' with: (userDec asString) . 
	tmp _ tmp copyReplaceAll: 'THO' with: (userThou asString) . 
	^ tmp 	! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:34:07'!
radix
	radix := true
	! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:34:16'!
space
	space := true
	! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 1/15/2022 19:55:07'!
thousandSeparate
	^ thousandSeparate  ! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 1/15/2022 19:55:15'!
thousandSeparate: aBoolean
	thousandSeparate _ aBoolean .
! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 1/15/2022 22:51:02'!
thousandSeparator
	^ self class getThousandSeparator 
	! !

!PrintfNumberFormatDescriptor methodsFor: 'scanning' stamp: 'NM 8/16/2021 14:34:26'!
zero
	padding := $0
	! !

!PrintfNumberFormatDescriptor methodsFor: 'initialize-release' stamp: 'NM 1/15/2022 18:47:47'!
initialize
	super initialize.
	padding := $ .
	radix := false.
	space := false.
	thousandSeparate := false .
	" . TODO temporary value, we should by default set $, if decimal separator is $. and viceversa. "
	thousandSeparator := $_ .
	". to permit print in the debugger " 
	operator _ $? . ! !

!PrintfNumberFormatDescriptor class methodsFor: 'instance creation' stamp: 'NM 8/16/2021 14:35:16'!
newFrom: desc
	desc class == self ifTrue: [^ desc].
	^ (super newFrom: desc) setPadding: desc padding
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/15/2022 22:50:07'!
getDecimalSeparator
	^ DecimalSeparator  
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/15/2022 22:50:26'!
getThousandSeparator
	^ ThousandSeparator  
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/15/2022 23:18:19'!
getUserDecimalSeparator
	^ UserDecimalSeparator  
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/15/2022 23:18:27'!
getUserThousandSeparator
	^ UserThousandSeparator  
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/16/2022 00:06:50'!
setUserDecimalSeparatorTo: aCharacter
	UserDecimalSeparator _ aCharacter . ! !

!PrintfNumberFormatDescriptor class methodsFor: 'settings' stamp: 'NM 1/16/2022 00:06:56'!
setUserThousandSeparatorTo: aCharacter
	UserThousandSeparator _ aCharacter . 
	! !

!PrintfNumberFormatDescriptor class methodsFor: 'class initialization' stamp: 'NM 1/15/2022 23:17:57'!
initialize
	"PrintfNumberFormatDescriptor initialize"
	Base := Dictionary newFromPairs: #(	$d 10
											$o 8
											$p 16
											$u 10
											$x 16
											$X 16).

	Radix := Dictionary newFromPairs: #(	$d ''
											$o '0'
											$p '0x'
											$u ''
											$x '0x'
											$X '0X').

	Cased := 'AaEeFfGgXx' . 
	
	". default values, don't modify these, test code use them.  "
	DecimalSeparator _ $.  .
	ThousandSeparator _ $_ . 
	". user modifiable values, for example to print numerics in agreement with some locale
	 . if these values are 'nil' they are ignored. If only one is 'nil' the other is taken from the default values above. 
	 " 
	UserDecimalSeparator _ nil. 
	UserThousandSeparator _ nil. 
	
	! !

!PrintfFloatFormatDescriptor methodsFor: 'rendering' stamp: 'NM 1/15/2022 23:52:47'!
addThousandSeparator: numberString
	". receives in input an integer numberString e.g. '12345.1233123'
	. returns in output a thousand separated string e.g. '12_345.1233123' 
	. '_' is the value lf 'thousandSeparator' 
	. if 'base' is not is not 10 returns just 'numberString', since a separation by 3 digits blocks
	 would not mean what 'thousand' says; in that case also print a warning in the Transcript'. 
	"
	|integerPart decimalPart tokens out |
	". TODO. CHECK. we suppose the base is 10 for floats " 
	"(self base ~= 10) ifTrue:  [ 
		Transcript log: 'Error. Printf. thousand separation is not defined for non base 10 numbers.' . 
		^ numberString . 
		]."
	". if we arrived here the base is 10 " 
	tokens _ numberString findTokens: '.' . 
	integerPart _ tokens first.
	decimalPart _ tokens second. 
	". build the default thousand separated numeric string "
	out _ (integerPart asNumber printStringWithCommas :: 
		copyReplaceAll: ',' with: (self thousandSeparator asString) ), '.' , decimalPart .
	". apply user preferences regarding who are thousand and decimal separators "
	^ self formatWithUserPreferences: out . 	
	! !

!PrintfFloatFormatDescriptor methodsFor: 'rendering' stamp: 'NM 6/18/2022 11:52:41'!
applyOperator: object 
	"Number is the only valid class"
	| string out |
	string := self zeroPaddedStringOfBase: (('aA' includes: operator)
												ifTrue: [16]
												ifFalse: [10])
				forFloat: object asNumber .
	string := string copyFrom: 1 to: ((string indexOf: $.) + (precision = 0
						ifTrue: [-1]
						ifFalse: [self precision]) min: string size).
	(Cased includes: operator) ifTrue:
		[string := operator isLowercase
					ifTrue: [string asLowercase]
					ifFalse: [string asUppercase]].
	(space and: [object asFloat >= 0]) ifTrue:
		[string := ' ' , string].
	out _ (width ~= 0 and: [string size > width])
			ifTrue: [String new: width withAll: $*]
			ifFalse: [string].
	" . add thousand separators, if needed " 
	"Transcript show: 'DBG --- applyOperator for FLoat numbers '. "
	thousandSeparate 
		ifTrue: [
			"Transcript show: 'DBG --- thousand sep is ON'. "
			out _ self addThousandSeparator: out. 
			] 
		ifFalse: [
			"Transcript show: 'DBG --- thousand sep is OFF' "
			] . 
	^ out 		
			
		! !

!PrintfFloatFormatDescriptor methodsFor: 'rendering' stamp: 'NM 1/15/2022 23:10:12'!
initializeThousandSeparatorFrom: descriptor 
	(descriptor class = PrintfNumberFormatDescriptor) ifTrue: [
		thousandSeparate  _ (descriptor thousandSeparate ).
		].
	^ self ! !

!PrintfFloatFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:37:28'!
zeroPaddedStringOfBase: base forFloat: aFloat 
	| stream |
	stream := String new writeStream.
	aFloat printOn: stream base: base.
	stream next: self precision-1 put: $0.
	^stream contents
	! !

!PrintfFloatFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:36:25'!
digits
	^ width == 0 ifTrue: [7] ifFalse: [width]
	! !

!PrintfFloatFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:36:39'!
precision
	^ precision isNil ifTrue: [1] ifFalse: [precision]
	! !

!PrintfFloatFormatDescriptor methodsFor: 'private' stamp: 'NM 8/16/2021 14:36:49'!
stringLength
	^ width
	! !

!PrintfFloatFormatDescriptor class methodsFor: 'as yet unclassified' stamp: 'NM 1/15/2022 21:49:42'!
newFrom: descriptor
	descriptor class == self ifTrue: [^ descriptor].
	^ ((super newFrom: descriptor) setPadding: descriptor padding) initializeThousandSeparatorFrom: descriptor 

	! !

!PrintfNumberHolderDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:38:35'!
applyOperator: ignored
! !

!PrintfNumberHolderDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:38:48'!
render: object
	value := object.
	^''
	! !

!PrintfNumberHolderDescriptor methodsFor: 'accessing' stamp: 'NM 8/16/2021 14:38:22'!
value
	^value
	! !

!PrintfStringFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:40:01'!
printOn: aStream
	super printOn: aStream.
	self printWidthOn: aStream.
	aStream nextPut: $s
	! !

!PrintfStringFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:40:11'!
printWidthOn: aStream
	widthDescriptor ifNotNil: [aStream nextPutAll: '.*']
	! !

!PrintfStringFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:40:24'!
applyOperator: object
	^ object
	! !

!PrintfStringFormatDescriptor methodsFor: 'accessing' stamp: 'NM 8/16/2021 14:39:34'!
stringLength
	^widthDescriptor ifNotNil: [widthDescriptor value] ifNil: [super stringLength]
	! !

!PrintfStringFormatDescriptor methodsFor: 'accessing' stamp: 'NM 8/16/2021 14:39:46'!
widthDescriptor: aPrintfNumberFormatDescriptor

	widthDescriptor := aPrintfNumberFormatDescriptor
	! !

!PrintStringFormatDescriptor methodsFor: 'printing' stamp: 'NM 8/16/2021 14:41:41'!
printOn: aStream
	aStream nextPut: $%.
	flush == #leftFlush ifTrue: [aStream nextPut: $-].
	self printWidthOn: aStream.
	aStream nextPut: $p
	! !

!PrintStringFormatDescriptor methodsFor: 'rendering' stamp: 'NM 8/16/2021 14:41:55'!
applyOperator: object
	^ object printLeanString
	! !

!PrintfFormatString methodsFor: 'initialize-release' stamp: 'NM 8/30/2021 18:32:22'!
setFormat: aString
	| formatStream |
	"copy actual formatstrings to format"
	"copy any nonformating text to string"
	outputStream := (String new: 20) writeStream.
	formatStream := ((aString copyReplaceAll: '\n' with: (String with: Character newLineCharacter))
		copyReplaceAll: '\t'
		with: (String with: Character tab)) readStream.
	self collectFormatDescriptorsAndStrings: formatStream
	! !

!PrintfFormatString methodsFor: 'private' stamp: 'NM 8/16/2021 14:45:45'!
collectFormatDescriptorsAndStrings: formatStream
	format := ReadWriteStream on: (Array new: 10).
	string := ReadWriteStream on: (Array new: 10).
	[| descriptor |
	 "copy actual formatstrings to format"
	 string nextPut: (self scanStringFrom: formatStream).
	 formatStream atEnd ifTrue:
		[self reset.
		 ^self].
	 descriptor := PrintfFormatDescriptor scanFrom: formatStream.
	 "copy any nonformating text to string"
	 descriptor isArray
		ifTrue: [format nextPutAll: descriptor]
		ifFalse: [format nextPut: descriptor]] repeat
	! !

!PrintfFormatString methodsFor: 'private' stamp: 'NM 8/16/2021 14:46:03'!
process: arguments
 	arguments asArgumentArrayForFormatString do: 
		[:object | 
		"put any text from the format string into outputStream"
		outputStream nextPutAll: string next. 
		 "get next FormatDescriptor from format and render object as specified"
		format atEnd ifFalse: [outputStream nextPutAll: (format next render: object)]].
        "any remainder is string, if so append to outputStream"
        string atEnd ifFalse: [outputStream nextPutAll: string next]
! !

!PrintfFormatString methodsFor: 'private' stamp: 'NM 8/16/2021 14:46:16'!
reset
        format reset.
        string reset.
        outputStream reset
! !

!PrintfFormatString methodsFor: 'private' stamp: 'NM 8/16/2021 14:46:29'!
scanStringFrom: aStream 
	| newString |
	newString := (String new: 40) writeStream.
	[aStream atEnd]
		whileFalse: 
			[| next | 
			next := aStream next.
			next == $% ifTrue: [^newString contents].
			next == $\
				ifTrue: 
					[next := aStream next.
					next == $n ifTrue: [next := Character cr].
					next == $t ifTrue: [next := Character tab]].
			newString nextPut: next].
	^newString contents
	
! !

!PrintfFormatString methodsFor: 'printf' stamp: 'NM 8/16/2021 14:44:02'!
<< object 
	"Render object according to next format descriptor in format. 
	Append it to string"
	"Format description 
	syntax: %{flags}{width}{precision}{long}<operator> 
	
	flags 
		-		left flush 
		+		right flush 
		space	non-negative number are preceeded by a blank 
		#		display integer with a radix indicator (0=octal, 0x=hex, float have .) 
		0		0 is used as left padding character for numbers 
	width		minimum field width (rest is padded) 
	.precision	maximum field width or trailing digits 
	long		ignored 
	operator 
		c		display object as character 
		d		display as integer 
		e,E		float in scientific notation 
		f		display as float 
		g,G		display as f or e,E using least amount of space 
		o		display as octal value 
		s		display as string 
		u		display as unsigned 
		x,X		display as hex value 
	"

	outputStream nextPutAll: string next.
	outputStream nextPutAll: (format next render: object).
	format atEnd ifTrue: 
		[format reset.
		outputStream nextPutAll: string next.
		string reset].
	^self string
	! !

!PrintfFormatString methodsFor: 'printf' stamp: 'NM 8/16/2021 14:44:37'!
f: aStream printf: arguments
	"inst var string holds all text contained in the formatstring. %f blabla %d"
	| position savedStream |
	"inst var format is a stream of FormatDescriptors"
	self reset.
	savedStream := outputStream.
	outputStream := aStream.
	position := aStream position.
	[self process: arguments]
		ensure: [outputStream := savedStream].
	aStream flush.
	^aStream position - position
	! !

!PrintfFormatString methodsFor: 'printf' stamp: 'NM 8/16/2021 14:44:53'!
printf: arguments
	"inst var string holds all text contained in the formatstring. %f blabla %d"

	"inst var format is a stream of FormatDescriptors"
	self reset.
	self process: arguments.
	^outputStream position
	! !

!PrintfFormatString methodsFor: 'printf' stamp: 'NM 8/16/2021 14:45:09'!
string
	^outputStream contents
	
	! !

!PrintfFormatString class methodsFor: 'examples' stamp: 'NM 8/16/2021 14:47:00'!
examples
	self inform: ('Here is a string "%s".' printf: 'hello world').
	self inform: ('Here is a string "%s" and another shortened "%-14.7s".'
		printf: #('hello world' 'hello world')).

	self inform: ('Here is a number "%d".' printf: 42).
	self inform: ('Here is a string "%07.7d".' printf: 42).

	self inform: ('Here is a number "%e".' printf: 42.0).
	self inform: ('Here is a float "%e" and an integer "%d".' printf: #(42.0 42)).
	self inform: ('Here is a string "%013.5e".' printf: 42.1234567).

	self inform: ('Here is a %s string "%s" and the same shortened "%-14.7s" with left flush.\nThe new line has a number "%e" and a 0-padded limited precision one "%013.5e".'
		printf: ((Array with: 'long' with: 'hello world' with: 'hello world' with: 42.0) copyWith: 42.1234567)).
		! !

!Object methodsFor: '*Printf' stamp: 'NM 8/16/2021 15:08:36'!
asArgumentArrayForFormatString

	^{self}
	! !

!Object methodsFor: '*Printf' stamp: 'NM 8/16/2021 15:08:48'!
printLeanString
	^self printStringLimitedTo: 256
	! !

!Collection methodsFor: '*Printf' stamp: 'NM 8/16/2021 15:06:19'!
asArgumentArrayForFormatString

	^self
	! !

!String methodsFor: '*Printf' stamp: 'NM 8/16/2021 14:54:46'!
asArgumentArrayForFormatString

	^{self}
	! !

!String methodsFor: '*Printf' stamp: 'NM 8/16/2021 14:55:02'!
printf
	^ (self indexOf: $%) ~= 0
		ifTrue: [ self ]
		ifFalse: [ (PrintfFormatString new setFormat: self) printf: #() ]
		! !

!String methodsFor: '*Printf' stamp: 'NM 8/16/2021 14:55:17'!
printf: arguments
	"Answer the string that results from applying the receiver
	 as a printf-style format string to the arguments.  For example
		'Hello %d world!!' printf: {Date today year}"
	^(PrintfFormatString new setFormat: self)
		printf: arguments;
		string
		! !

!WriteStream methodsFor: '*Printf' stamp: 'NM 8/16/2021 15:10:20'!
f: printfString printf: arguments
	"Use the printf convention to output on the receiver."
	^(PrintfFormatString new setFormat: printfString)
		f: self
		printf: arguments
		! !
PrintfFormatDescriptor initialize!
PrintfNumberFormatDescriptor initialize!
