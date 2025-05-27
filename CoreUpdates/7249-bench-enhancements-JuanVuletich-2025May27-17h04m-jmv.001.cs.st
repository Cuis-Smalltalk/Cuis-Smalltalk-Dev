'From Cuis7.3 [latest update: #7248] on 27 May 2025 at 5:05:13 pm'!

!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 5/27/2025 17:04:56'!
bench
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description.
	[ Float pi printString ] bench print.
	[ 80000 factorial printString ] bench print.
	"

	| secondsPerRun startTime endTime count run elapsedTime gcStart gcTime |
	count := 0.
	run := true.
	Smalltalk garbageCollect.
	[ (Delay forSeconds: 5) wait. run := false ] forkAt: Processor timingPriority - 1.
	startTime := Time localMillisecondClock.
	gcStart := (Smalltalk vmParameterAt: 8) + (Smalltalk vmParameterAt: 10).
	[ run ] whileTrue: [ self value. count := count + 1 ].
	endTime := Time localMillisecondClock.
	elapsedTime := endTime - startTime.
	gcTime := (Smalltalk vmParameterAt: 8) + (Smalltalk vmParameterAt: 10) - gcStart.
	secondsPerRun := (endTime - startTime) / (count * 1000).
	^String streamContents: [ :strm |
		secondsPerRun < 1 ifTrue: [
			1.0 / secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				value printOn: strm fractionDigits: 2.
				strm
					space;
					nextPutAll: unitPrefixSymbol;
					nextPutAll: ' runs per second. ' ]].
		secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixSymbol;
				nextPutAll: ' seconds per run. '].
		gcTime / elapsedTime * 100 printOn: strm fractionDigits: 2.
		strm
			space;
			nextPutAll: ' % GC time.' ]! !

