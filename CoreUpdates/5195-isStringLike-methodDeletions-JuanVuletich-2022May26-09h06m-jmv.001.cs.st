'From Cuis 6.0 [latest update: #5194] on 26 May 2022 at 9:18:01 am'!

!Object class methodsFor: 'instance creation' stamp: 'jmv 5/22/2022 20:36:48'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream is: #Stream) or: [ aStream isStringLike ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object! !


!Number class methodsFor: 'instance creation' stamp: 'jmv 5/22/2022 20:20:38'!
readFrom: stringOrStream 
	"Answer a number as described on aStream.  The number may
	include a leading radix specification, as in 16rFADE"
	| value base aStream sign |
	aStream _ stringOrStream isStringLike
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


!CodeFile methodsFor: 'private' stamp: 'jmv 5/23/2022 13:30:51'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) isStringLike ]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) isStringLike]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass theMetaClass classComment: tokens last].
! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/23/2022 13:31:05'!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first isStringLike]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 5/23/2022 13:32:06'!
possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) isStringLike]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.! !

!methodRemoval: ReadWriteStream #fileNameEndsWith: stamp: 'jmv 5/26/2022 09:06:31'!
ReadWriteStream removeSelector: #fileNameEndsWith:!
!methodRemoval: PositionableStream #backUpTo: stamp: 'jmv 5/26/2022 09:06:31'!
PositionableStream removeSelector: #backUpTo:!
!methodRemoval: PositionableStream #isCharacters stamp: 'jmv 5/26/2022 09:06:31'!
PositionableStream removeSelector: #isCharacters!
!methodRemoval: PositionableStream #nextDelimited: stamp: 'jmv 5/26/2022 09:06:31'!
PositionableStream removeSelector: #nextDelimited:!
