'From Cuis 4.2 of 25 July 2013 [latest update: #2411] on 10 July 2015 at 11:32:23.941572 pm'!

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 7/10/2015 23:32'!
report
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :textStream |
		self
			reportTextOn: textStream
			linesOn: (DummyStream on: nil)
			talliesOn: (DummyStream on: nil) ]! !

