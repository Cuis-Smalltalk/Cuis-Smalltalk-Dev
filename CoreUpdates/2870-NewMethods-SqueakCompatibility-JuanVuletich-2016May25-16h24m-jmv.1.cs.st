'From Cuis 4.2 of 25 July 2013 [latest update: #2769] on 25 May 2016 at 4:25:14.375525 pm'!

!Object methodsFor: 'testing' stamp: 'yo 8/28/2002 13:41'!
isCharacter

	^ false.
! !

!Object methodsFor: 'testing' stamp: 'md 11/21/2003 12:14'!
isCompiledMethod

	^ false! !

!Object methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!
isStream
	"Return true if the receiver responds to the stream protocol"
	^false
! !

!Object methodsFor: 'testing'!
isText
	^ false! !

!Object methodsFor: 'Squeak Compatibility' stamp: 'jmv 5/24/2016 22:59'!
deprecated: string! !


!Behavior methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/24/2016 23:56'!
allowUnderscoreAssignments
	"Return true if the receiver and its subclasses should be allowed to use underscore as assignment operator. Returning nil here means that the compiler should use the system-wide default preference. Also see #allowUnderscoreSelectors.

	Combinations: If both preferences are false, underscore is illegal. If both preferences are true, underscore assignment needs the be surrounded by spaces and a single underscore cannot be used as selector."
	^nil! !


!BlockClosure methodsFor: 'printing' stamp: 'eem 7/18/2014 09:24'!
printSourceOn: aStream 
	self decompile
		ifNil: [ aStream nextPutAll: '--source missing--' ]
		ifNotNil: [ : blockNode | blockNode printOn: aStream indent: 0 ]! !

!BlockClosure methodsFor: 'printing' stamp: 'cmm 10/9/2013 14:43'!
sourceString
	^ String streamContents: [ : stream | self printSourceOn: stream ]! !

!BlockClosure methodsFor: 'displaying' stamp: 'jmv 5/22/2016 22:52'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:bar |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			bar value: x.]].

['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:bar |
	1 to: 100 do: [:x | bar value: x.
			(Delay forMilliseconds: 100) wait]].] fork
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:bar |
	1 to: 30 do: [:x | bar value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:bar |
	1 to: 30 do: [:x | bar value: nil.
			(Delay forMilliseconds: 200) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !


!Character methodsFor: 'testing' stamp: 'yo 8/28/2002 13:42'!
isCharacter

	^ true.
! !

!Character methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/23/2016 22:29'!
charCode
^ self value = 26 ifTrue: [16r10FFFF] ifFalse: [self value ]! !


!Collection methodsFor: 'enumerating' stamp: 'nice 12/9/2009 15:37'!
findFirstInByteString: aByteString startingAt: start
	"Find the index of first character starting at start in aByteString that is included in the receiver.
	Default is to use a naive algorithm.
	Subclasses might want to implement a more efficient scheme"

	start to: aByteString size do:
		[:index |
		(self includes: (aByteString at: index)) ifTrue: [^ index]].
	^ 0! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 5/24/2016 23:01'!
findFirstInByteString: aByteString startingAt: startIndex
	"Double dispatching: since we know this is a ByteString, we can use a superfast primitive using a ByteArray map with 0 slots for byte characters not included and 1 for byte characters included in the receiver."
	^String
		findFirstInString: aByteString
		inSet: self byteArrayMap
		startingAt: startIndex! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'nice 12/9/2009 14:54'!
indexOfAnyOf: aCollection startingAt: start
	"Answer the index of the first occurence of any element included in aCollection after start within the receiver.
	If the receiver does not contain anElement, answer zero, which is an invalid index."

	^self indexOfAnyOf: aCollection startingAt: start ifAbsent: [0]! !


!String methodsFor: 'accessing' stamp: 'nice 12/9/2009 15:39'!
indexOfAnyOf: aCollection startingAt: start
	"Use double dispatching for speed"
	^aCollection findFirstInByteString: self startingAt: start! !

!String methodsFor: 'formatting' stamp: 'yo 11/3/2004 19:24'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character cr ] ifFalse: [ c ]].! !

!String methodsFor: '*Squeak compatibility' stamp: 'jmv 5/23/2016 21:59'!
asOctetString
! !

!String methodsFor: '*Squeak compatibility' stamp: 'jmv 5/24/2016 22:30'!
convertFromEncoding: stuff! !

!String methodsFor: '*Squeak compatibility' stamp: 'jmv 5/24/2016 22:31'!
convertToEncoding: stuff! !


!Symbol methodsFor: 'accessing' stamp: 'nice 12/9/2009 15:40'!
indexOfAnyOf: aCollection startingAt: start
	"Use double dispatching for speed"
	^aCollection findFirstInByteString: self startingAt: start! !


!Text methodsFor: 'comparing'!
isText
	^ true! !


!SystemDictionary methodsFor: 'dictionary access' stamp: 'jmv 5/24/2016 22:48'!
undeclared
^Undeclared! !

!SystemDictionary methodsFor: 'Squeak Compatibility' stamp: 'jmv 5/22/2016 22:46'!
allClassesAndTraits
	^ self allClasses! !

!SystemDictionary methodsFor: 'Squeak Compatibility' stamp: 'jmv 5/24/2016 23:02'!
valueOf: aSymbol ifAbsent: aBlock
	^ (self at: aSymbol ifAbsent: [^ aBlock value]) value! !


!Integer methodsFor: 'printing-numerative' stamp: 'laza 3/29/2004 13:35'!
radix: base 
	^ self printStringBase: base! !


!LargePositiveInteger methodsFor: 'testing' stamp: 'nice 8/31/2008 00:07'!
isLarge
	^true! !


!SmallInteger methodsFor: 'testing' stamp: 'nice 8/31/2008 00:07'!
isLarge
	^false! !


!MethodReference methodsFor: 'accessing' stamp: 'sr 6/4/2004 01:55'!
sourceString
	^ (self actualClass sourceCodeAt: self methodSymbol) asString! !


!SHTextStyler methodsFor: 'styling' stamp: 'jmv 5/24/2016 22:57'!
styledTextFor: aText
	"Answer a copy of aText that is both formatted and styled"	
	
	formattedText := aText.
	self privateStyle.
	^formattedText! !


!Stream methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!
isStream
	"Return true if the receiver responds to the stream protocol"
	^true! !


!DummyStream methodsFor: 'as yet unclassified' stamp: 'jmv 5/24/2016 22:47'!
cr
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."! !


!WriteStream methodsFor: 'character writing' stamp: 'jmv 5/24/2016 22:55'!
crtab
self crtab: 1! !

!WriteStream methodsFor: 'character writing' stamp: 'jmv 5/24/2016 22:23'!
crtab: anInteger
	self newLineTab: anInteger! !


!FileStream class methodsFor: 'instance creation' stamp: 'jmv 5/24/2016 22:46'!
newFileNamed: fileName 
	^ fileName asFileEntry privateWriteStream! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 5/24/2016 22:47'!
cr
	"WriteStream protocol.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry! !

