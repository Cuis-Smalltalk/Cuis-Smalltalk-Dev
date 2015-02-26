'From Cuis 4.2 of 25 July 2013 [latest update: #2180] on 7 February 2015 at 10:49:38.107551 pm'!
!classDefinition: #AndreasSystemProfiler category: #'Tools-Profiling'!
Object subclass: #AndreasSystemProfiler
	instanceVariableNames: 'semaphore ticks profilerProcess tallyRoot vmStats totalTally totalTime startTime ticksPerMSec totalTicks observedProcess'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/7/2015 22:46'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop method leaf |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Time primHighResClock.
	[ true ]
		whileTrue: [ 
			tallyStart := tallyStop.
			tallyStart := Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyStop := Time primHighResClock.
			tallyTicks := tallyStop - tallyStart.
			totalTicks := totalTicks + tallyTicks.
			process := Smalltalk profileSample.
			method := Smalltalk profilePrimitive.
			totalTally := totalTally + 1.
			process
				ifNotNil: [ 
					leaf := tallyRoot
						tally: (process suspendedContext ifNil: [ thisContext ])
						inProcess: process
						by: tallyTicks.
					"por que cadorna hace falta esto????
					pero seguro que si hace falta...
					en que casos el method no es process suspendedContext method?"
					"mhhh en
						AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
						ni siquiera es el sender, y aparece cualquier banana. es preciso ver la doc
						
						
						p. ej
  |  2.9% (7 ms) (Number>>#raisedToInteger:)
  |    2.2% (5 ms) (Float>>#timesTwoPower: )

raisedToInteger: NO envia timesTwoPower:

					"
					"method ifNotNil: [ leaf tallyMethod: method by: tallyTicks ] "
					"Comentando esto, esa banana desaparece.... entender....."
					] ]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/7/2015 22:30'!
spyOn: aBlock
	"Profile system activity during execution of aBlock."
	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method nesting: 1.
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/7/2015 22:30'!
addToLeafReportDict: leafDict
	"Leaves Dict has no more than one entry for each method,
	even if this method appeared in many leves in the tree."
	| leafNode |
	leafNode _ leafDict
		at: method
		ifAbsentPut: [
			(QSystemTally new class: class method: method nesting: 0)
				process: process ].
	leafNode addTallies: tally! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 22:44'!
tally: context inProcess: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender leaf |

	"Add to this node if appropriate"
	"No es preciso subir mas
	Matchear proceso? clase?. Proceso no importa!! Clase SIIIIII"
	(context method == method and: [(self blockNestingCountOf: context) = blockNesting ]) ifTrue: [
		self addTallies: count.
		^self ].

	"No sender? Add new branch to the tree."
	sender _ context sender.
	"No entiendo cuando podria ocurrir...
	Creo que cuando ya no puedo subir mas en la recursion.
	Creo que normalmente solo pasa una vez, las siguientes las encuentra
	en la condicion de arriba."
	"no entiendo bien quien soy en este caso..."
	sender ifNil: [
		self addTallies: count.
		^self tallyLeafChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary)"
	"recursivamente intento encontrar o agregar el sender primero,
	y despues me agrego a mi"
	leaf _ self tally: sender inProcess: aProcess by: count.
	^leaf tallyLeafChild: context inProcess: aProcess by: count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/7/2015 22:32'!
tallyMethod: aMethod by: count
	"Called explicitly and needs to decrement receiver's tally count"
	"hace falta??????
	Se parece demasiado a #tallyLeafChild:inProcess:by:
	precisaria el proceso?????
	"
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !

!methodRemoval: QSystemTally #class:method:!
QSystemTally removeSelector: #class:method:!
