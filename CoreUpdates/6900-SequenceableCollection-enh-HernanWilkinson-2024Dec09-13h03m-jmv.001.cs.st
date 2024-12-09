'From Cuis7.3 [latest update: #6899] on 9 December 2024 at 1:04:11 pm'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 10/29/2024 16:34:13'!
with: otherCollection allSatisfy: twoArgConditionBlock  
	
	self with: otherCollection do: [ :left :right | (twoArgConditionBlock value: left value: right) ifFalse: [ ^false ]].
	^true! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 10/30/2024 08:11:29'!
with: otherCollection anySatisfy: twoArgConditionBlock 
	
	self with: otherCollection do: [ :left :right | (twoArgConditionBlock value: left value: right) ifTrue: [ ^true ]].
	^false! !


!SequenceableCollection class methodsFor: 'stream creation' stamp: 'HAW 10/28/2024 13:41:05'!
writeStream
	
	^WriteStream on: self new! !

