'From Cuis 4.2 of 25 July 2013 [latest update: #2093] on 12 December 2014 at 5:13:02.730883 pm'!

!ShortRunArray class methodsFor: 'instance creation' stamp: 'jmv 12/12/2014 17:12'!
newFrom: aCollection
	"Compress aCollection into a ShortRunArray"
	| lastValue lastRun runs values valueCount |
	aCollection isEmpty ifTrue: [ ^self runs:#() values: #() ].
	runs _ WriteStream on: (WordArray new: 100).
	values _ WriteStream on: (ShortIntegerArray new: 100).
	lastValue _ aCollection first.
	lastRun _ 0.
	valueCount _ 0.
	aCollection do: [ :item |
		(item = lastValue and:[lastRun < 16r8000]) ifTrue:[
			lastRun _ lastRun + 1.
		] ifFalse:[
			runs nextPut: lastRun.
			values nextPut: lastValue.
			lastRun _ 1.
			lastValue _ item.
			valueCount _ valueCount + 1.
		].
	].
	runs nextPut: lastRun.
	values nextPut: lastValue.
	valueCount _ valueCount + 1.
	valueCount odd ifTrue: [
		values nextPut: 0 ].
	^ self runs: runs contents values: values contents! !

