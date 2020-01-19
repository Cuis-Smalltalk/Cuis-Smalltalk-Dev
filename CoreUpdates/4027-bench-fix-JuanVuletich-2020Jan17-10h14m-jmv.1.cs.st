'From Cuis 5.0 [latest update: #4026] on 17 January 2020 at 10:23:32 am'!

!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 1/17/2020 10:22:55'!
bench
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description.
	[ Float pi printString ] bench print.
	[ 80000 factorial printString ] bench print.
	"

	| secondsPerRun startTime endTime count run |
	count _ 0.
	run _ true.
	[ (Delay forSeconds: 5) wait. run _ false ] forkAt: Processor timingPriority - 1.
	startTime _ Time localMillisecondClock.
	[ run ] whileTrue: [ self value. count _ count + 1 ].
	endTime _ Time localMillisecondClock.
	secondsPerRun _ (endTime - startTime) / (count * 1000).
	secondsPerRun >= 1
		ifTrue: [
			secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				^String streamContents: [ :strm |
					value printOn: strm fractionDigits: 2.
					strm
						space;
						nextPutAll: unitPrefixSymbol;
						nextPutAll: ' seconds per run']]
			]
		ifFalse: [
			1.0 / secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				^String streamContents: [ :strm |
					value printOn: strm fractionDigits: 2.
					strm
						space;
						nextPutAll: unitPrefixSymbol;
						nextPutAll: ' runs per second' ]]
			]! !

