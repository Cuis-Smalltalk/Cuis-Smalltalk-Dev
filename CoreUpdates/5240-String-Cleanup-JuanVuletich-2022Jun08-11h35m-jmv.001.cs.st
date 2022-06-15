'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 11:38:36 am'!

!String methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !

!String methodsFor: 'testing' stamp: 'jmv 6/8/2022 08:41:33'!
isEmpty
	^self size = 0! !

!methodRemoval: CharacterSequence #size stamp: 'jmv 6/8/2022 11:35:22'!
CharacterSequence removeSelector: #size!
!methodRemoval: CharacterSequence #isEmpty stamp: 'jmv 6/8/2022 11:36:58'!
CharacterSequence removeSelector: #isEmpty!

!CharacterSequence reorganize!
('adding' add:)
('copying' appendToText:)
('comparing' commonPartWith:startAt:stopAt:applying:)
('converting' asByteString asPlainString asString asText asUtf8Bytes contractTo:)
('enumerating' collect: collectInArray:)
('testing' isString)
('user interface' edit editLabel:)
!

