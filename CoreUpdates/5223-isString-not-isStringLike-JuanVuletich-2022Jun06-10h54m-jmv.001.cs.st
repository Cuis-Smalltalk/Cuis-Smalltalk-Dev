'From Cuis 6.0 [latest update: #5229] on 6 June 2022 at 11:03:28 am'!

!Utf32String methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:55:00'!
isString
	^true! !


!Utf8String methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:55:03'!
isString
	^true! !


!Object methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:54:35'!
isString
	"Overridden to return true in String, Utf8String and Utf32String."
	^ false! !


!Object class methodsFor: 'instance creation' stamp: 'jmv 9/2/2013 09:01'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream is: #Stream) or: [ aStream isString ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object! !


!Number class methodsFor: 'instance creation' stamp: 'jmv 6/6/2022 10:59:12'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream _ stringOrStream isString
		ifTrue: [stringOrStream readStream]
		ifFalse: [stringOrStream].
	(aStream nextMatchAll: 'NaN') ifTrue: [^ Float nan].
	sign _ (aStream peekFor: $-) ifTrue: [-1] ifFalse: [aStream peekFor: $+. 1].
	(aStream nextMatchAll: 'Infinity') ifTrue: [^ Float infinity * sign].
	base _ 10.
	aStream peek = $.
		ifTrue: [ value _ 0 ]
		ifFalse: [
			value _ Integer readFrom: aStream base: base.
			(aStream peekFor: $r)
				ifTrue: [
					"<base>r<integer>"
					(base _ value) < 2 ifTrue: [
						base = 1 ifTrue: [ ^Integer readBaseOneFrom: aStream ].
						^self error: 'Invalid radix'].
					(aStream peekFor: $-) ifTrue: [sign _ sign negated].
					value _ Integer readFrom: aStream base: base].
			(aStream peekFor: $x)
				ifTrue: [
					"0x<integer>" "Hexadecimal"
					base _ 16.
					(aStream peekFor: $-) ifTrue: [sign _ sign negated].
					value _ Integer readFrom: aStream base: base]].
	^ self readRemainderOf: value from: aStream base: base withSign: sign.! !


!String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 10:59:35'!
= aString 
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isByteString ifFalse: [
		^aString = self ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2.! !

!String methodsFor: 'testing' stamp: 'jmv 6/6/2022 10:54:40'!
isString
	^ true! !


!StdIOWriteStream methodsFor: 'streaming' stamp: 'jmv 6/6/2022 11:00:00'!
nextPutAll: aStringOrUTF8Bytes
	"Write the given String (possibly including NCRs) or ByteArray (with utf-8 contents)
		 StdIOWriteStream stdout nextPutAll: '--- &#16r2713; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: '--- &#10003; ==='; flush.
		 StdIOWriteStream stdout nextPutAll: #[226 156 147]; flush.
		StdIOWriteStream stdout nextPutAll: '¿El Ñandú toma agüita?', String newLineString; flush.
		StdIOWriteStream stdout nextPutAll: ('¿El Ñandú toma agüita?', String newLineString) asUtf8 ; flush.
	See at the end of this method for a larger example with NCRs for arbitrary Unicode
	"
	| utf8Bytes |
	utf8Bytes _ aStringOrUTF8Bytes isString
		ifTrue: [ aStringOrUTF8Bytes asUtf8Bytes ]
		ifFalse: [ aStringOrUTF8Bytes ].
	self primWrite: fileID from: utf8Bytes startingAt: 1 count: utf8Bytes size.
	^aStringOrUTF8Bytes
"
StdIOWriteStream stdout nextPutAll: '
&#913;&#945; 	Alpha 	
&#914;&#946; 	Beta 	
&#915;&#947; 	Gamma 	
†&#948; 	Delta 	
&#917;„ 	Epsilon 	
&#918;… 	Zeta 	
&#919;&#951; 	Eta 	
&#920;&#952; 	Theta 	
&#921;&#953; 	Iota 	
&#922;&#954; 	Kappa 	
&#923;&#955; 	Lambda 	
&#924;&#956; 	Mu 	
&#925;&#957; 	Nu
&#926;&#958; 	Xi
&#927;&#959; 	Omicron
&#928;ƒ 	Pi
&#929;&#961; 	Rho
&#931;&#963;&#962; 	Sigma
&#932;&#964; 	Tau
&#933;&#965; 	Upsilon
&#934;&#966; 	Phi
&#935;&#967; 	Chi
&#936;&#968; 	Psi
‡&#969; 	Omega
&# 937;&# 969; 	Not a NCR, just regular ASCII chars!!
'; flush
"! !


!Utf32String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:00:10'!
= aString
	"Answer whether the receiver sorts equally as anUtf32String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Expensive. Assume it is extremely unlikely."
	aString isUtf32String ifFalse: [
		^self asUtf8String = aString ].

	^ codePoints = aString words! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 6/6/2022 11:00:28'!
appendToString: aString

	aString isString ifFalse: [
'pufff' print.
	self halt ].
	^Utf8String withBytes: aString asUtf8Bytes, bytes! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:00:14'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	aString isByteString ifTrue: [
		"If aString contains NCRs wit will never equal self. If this ever changes, don't forget about #hash,
		and String >> #="
		^bytes = (aString asUtf8Bytes: false) ].

	^ bytes = aString asUtf8Bytes.! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 6/6/2022 11:00:47'!
retrieveStringOrText: stringClass
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |
	
	answer _ self retrieveObject: stringClass.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: (stringClass with: $*) attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ stringClass new ] ifNotNil: [ answer printString ]! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 6/6/2022 11:00:55'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents _ anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'private' stamp: 'jmv 1/10/2014 10:25'!
stringOrIdFor: anObject
	"
	Clipboard default stringOrIdFor: Smalltalk
	"
	anObject isString ifTrue: [ ^anObject ].
	(anObject is: #Text) ifTrue: [ ^anObject string ].
	^self idFor: anObject! !


!CodeFile methodsFor: 'private' stamp: 'jmv 6/6/2022 11:02:06'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) isString ]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) isString ]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass theMetaClass classComment: tokens last].
! !

!CodeFile methodsFor: 'private' stamp: 'jmv 6/6/2022 11:02:26'!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [tokens first isString]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 6/6/2022 11:02:40'!
possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) isString ]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.! !

!methodRemoval: Utf8String #isStringLike stamp: 'jmv 6/6/2022 11:02:59'!
Utf8String removeSelector: #isStringLike!
!methodRemoval: Utf32String #isStringLike stamp: 'jmv 6/6/2022 11:02:58'!
Utf32String removeSelector: #isStringLike!
!methodRemoval: String #isStringLike stamp: 'jmv 6/6/2022 11:02:56'!
String removeSelector: #isStringLike!
!methodRemoval: Object #isStringLike stamp: 'jmv 6/6/2022 11:02:53'!
Object removeSelector: #isStringLike!
