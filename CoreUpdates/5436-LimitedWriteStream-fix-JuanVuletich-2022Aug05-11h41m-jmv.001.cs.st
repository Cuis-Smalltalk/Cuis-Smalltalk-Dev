'From Cuis 6.0 [latest update: #5435] on 5 August 2022 at 11:46:50 am'!

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 8/5/2022 11:41:43'!
asStreamResult
	"See senders.
	Also see #thatCanHoldCodePoints"
	^self! !


!String methodsFor: 'converting' stamp: 'jmv 8/5/2022 11:41:59'!
thatCanHoldCodePoints
	"See senders.
	Also see #asStreamResult"
	^ArrayOfCharactersAndCodePoints withAll: self! !


!ArrayOfCharactersAndCodePoints methodsFor: 'converting' stamp: 'jmv 8/5/2022 11:41:49'!
asStreamResult
	"See senders.
	Also see #thatCanHoldCodePoints"
	^Utf8String newFrom: self! !


!Text methodsFor: 'converting' stamp: 'jmv 8/5/2022 11:41:52'!
asStreamResult
	"See senders.
	Also see #thatCanHoldCodePoints"
	self setString: string asStreamResult setRuns: runs.
	^self! !

!Text methodsFor: 'converting' stamp: 'jmv 8/5/2022 11:46:29'!
thatCanHoldCodePoints
	"See senders.
	Also see #asStreamResult	"
	string _ string thatCanHoldCodePoints! !


!LimitedWriteStream methodsFor: 'as yet unclassified' stamp: 'jmv 8/5/2022 11:45:40'!
nextPutAll: aCollection

	position + aCollection size > limit ifTrue: [
		super nextPutAll: (aCollection copyFrom: 1 to: (limit - position max: 0)).
		limitBlock value.
		^self
	].
	super nextPutAll: aCollection.! !


!LimitedWriteStream reorganize!
('accessing' nextPut: nextPutAll: pastEndPut: setLimit:limitBlock:)
!
