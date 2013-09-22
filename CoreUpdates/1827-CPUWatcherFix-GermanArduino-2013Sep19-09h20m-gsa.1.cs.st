'From Cuis 4.2 of 25 July 2013 [latest update: #1824] on 19 September 2013 at 10:22:00.142285 am'!

!CPUWatcher commentStamp: 'gsa 9/19/2013 09:24' prior: 0!
CPUWatcher implements a simple runaway process monitoring tool
that will suspend a process that is taking up too much of Squeak's
time and allow user interaction. By default it watches for a Process that
is taking more than 80% of the time; this threshold can be changed.

CPUWatcher can also be used to show cpu percentages for each process 
from within the ProcessBrowser.

	CPUWatcher startMonitoring.	"process period 20 seconds, sample rate 100 msec, threshold 80%"
	CPUWatcher startMonitoringPeriod: 10 rate: 20 threshold: 0.8 suspendPorcine: true
	CPUWatcher current threshold: 0.5.	"change from 80% to 50%"
	CPUWatcher stopMonitoring.
!


!CPUWatcher class methodsFor: 'as yet unclassified' stamp: 'gsa 9/19/2013 09:22'!
startMonitoring
	"CPUWatcher startMonitoring"

	^self startMonitoringPeriod: 20 rate: 100 threshold: 0.8 suspendPorcine: true.! !

