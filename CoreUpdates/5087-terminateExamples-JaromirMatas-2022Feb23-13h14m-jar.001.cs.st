'From Cuis 6.0 [latest update: #5086] on 23 February 2022 at 1:15:14 pm'!

!Process class methodsFor: 'documentation' stamp: 'jar 2/3/2022 12:23:58'!
terminateExamples
"
This is the contents of the examples posted by Jaromir to exercise his worn on process termination and exceptions.
It was posted to the Cuis mail list at https://lists.cuis.st/mailman/archives/cuis-dev/2021-May/003171.html
(or maybe, it is a later version of edition of it if this comment is not up to date!!)

Thanks Jaromir for this great contribution!!

Workspace new
		contents: Process terminateExamples;
		openLabel: 'Jaromir Matas''s Process - terminate examples Cuis'.
"
^


'"
The following examples illustrate the behavior and semantics of the new #terminate.
"
"
Part 1: terminate a healthy process inside an unwind block
======================================
Show that all pending unwind blocks get executed regardless whether they have already started or not.
"
"
terminate a suspended process:
....................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess suspend. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate a runnable process:
.................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor yield. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate a blocked process:
...............................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Semaphore new wait] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Semaphore new wait. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

"
terminate the active process:
................................................
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess terminate. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString

"prints x1 x2 x3 true"


"
Part 2: terminate a process after encountering an unhandled error inside an unwind block
===========================================================
Show that all unhandled errors inside unwind blocks get caught and all pending unwind blocks get executed as expected.
"
"
Termination happens when you hit Abandon on the Debugger window.
Note: when you hit Proceed the computation will continue normally and will print x1 x2 x3 x4
"
"
one unhandled error scenario:
.................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test.
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"

"
multiple nested unhandled errors:
........................................................
"
[
	[
		[ ] ensure: [
			[self error: #outer_error] ensure: [
				self error: #inner_error.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] fork

"prints x1 x2 x3"

[
	[
		[self error: #outer_error] ensure: [
			[self error: #middle_error] ensure: [
				self error: #inner_error.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] fork
"prints x1 x2 x3"


[self error: #outer_error] ensure: [
    [self error: #middle_error] ensure: [
        [self error: #inner_error] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4
"prints x1 x2 x3"
"same result when wrapped in fork"


[] ensure: [
[self error: #outer_error] ensure: [
    [self error: #middle_error] ensure: [
        [self error: #inner_error] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4]
"prints x1 x2 x3 x4"
"same result when wrapped in fork"


"
Part 3: terminate a process after encountering an unhandled error AND a non-local return inside an unwind block
==========================================================================
Show the semantics of non-local returns inside unwind blocks combined with unhandled error during termination.
"
"
Termination happens when you hit Abandon on the Debugger window.
Note: when you hit Proceed the computation will continue normally and will print x1 x2 x3 x4"
"

"
non-local return inside inner-most halfway-thru unwind block:
......................................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3"

"
non-local return inside outer-most halfway-thru unwind block:
......................................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		^Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			Transcript show: #x1]. 
		^Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4


"prints x1 x2 x3"

"
one more level...:
............................
"
[
	[
		[ ] ensure: [
			[ ] ensure: [
				self error: #unwind_test. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		^Transcript show: #x3]
] ensure: [
	Transcript show: #x4].
Transcript show: #x5

"prints x1 x2 x3 x4 (even if wrapped in #fork)"

"
non-local return outside halfway-thru unwind blocks:
......................................................................................
"
[
	[ ] ensure: [
		[self error: #unwind_test] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	^Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: #unwind_test. 
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	^Transcript show: #x3].
Transcript show: #x4

"prints x1 x2 x3"


"
Part 4: COMPARISON with the behavior when encountering a handled error or a healthy non-local return inside an unwind block
===================================================================================
Show that ONLY pending unwind blocks that have not yet started get executed during an exception or non-local return unwind.
Why? Handled exceptions and non-local returns do not use #terminate and so do not follow the new terminate semantics and use the traditional simpler semantics instead, skipping unwind blocks halfway through their execution (see #resume[:through:] methods).
"

"
one single handled error scenario:
.......................................................
"
[
	[
		[ ] ensure: [
			[self error: #unwind_test] ensure: [
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x1 x3 x4 only, skips x2"

[
	[
		[ ] ensure: [
			[] ensure: [
				self error: #unwind_test.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x3 x4 only, skips x1 and x2"

"
multiple nested handled errors:
...................................................
"
[
	[
		[ ] ensure: [
			[self error: #unwind_test_outer] ensure: [
				self error: #unwind_test_inner.
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3]
] on: Error do: [].
Transcript show: #x4

"prints x3 x4 only, skips x1 and x2"

"
non-local return scenario:
.........................................
"
[
	[ ] ensure: [
		[^Transcript show: #x0] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x0 x1 x3 only, skips x2"


[
	[ ] ensure: [
		[] ensure: [
			^Transcript show: #x1]. 
		Transcript show: #x2]
] ensure: [
	Transcript show: #x3].
Transcript show: #x4

"prints x1 x3 only, skips x2"


"
Part 4: Crazies - explore new teminate behavior and semantics under more extreme circumstances:
==================================================================
Show that the system will not crash and execute all pending unwind blocks as expected.
"

"
Unwind after active process termination; run in the UI. CAUTION: save image first!!
All examples following this one can be run in the UI; forking them is for convenience.
"
[ ] ensure: [
	[Processor activeProcess terminate] ensure: [
		Transcript show: #x1]. 
	Transcript show: #x2
	]
"prints x1 x2 and suspends the UI; recover via Alt+."


"
Unwind after active process termination; run forked
"
[
	[ ] ensure: [
		[Processor activeProcess terminate] ensure: [
			Transcript show: #x1]. 
		Transcript show: #x2
		]
] forkAt: Processor activePriority + 1
"prints x1 x2"


"
Unwind after double active process termination:
"
| p |
p := [
	[Processor activeProcess terminate] 	ensure: [
		Processor activeProcess terminate. Transcript show: #x1 ]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 true"


"
Unwind after double active process termination (equivalent to previous):
"
| p ap |
p := [
	[ap := Processor activeProcess. ap terminate] 	ensure: [
		ap terminate. Transcript show: #x1]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 true"


"
Unwind after double active process termination inside the same unwind block:
"
| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: #x1. 
		Processor activeProcess terminate. Transcript show: #x2]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 x2 true"


"
Unwind after double active process termination inside nested unwind blocks:
"
| p |
p := [
	[
		[] ensure: [
			[Processor activeProcess terminate] ensure: [
				Processor activeProcess terminate. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume.
Transcript show: p isTerminated printString
"prints x1 x2 x3 true"


"
Unwind after active process termination combined with non-local return:
"
| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: #x1. 
		true ifTrue: [^Transcript show: #x2]]
] newProcess priority: Processor activePriority + 1.
"raise active priority to avoid preemption in Cuis"
p resume
"prints x1 x2 and raises BlockCannotReturn error as expected"


"
Unwind after triple nested active process termination - run forked:
"
[
	[Processor activeProcess terminate] ensure: [
	    [Processor activeProcess terminate] ensure: [
	        [Processor activeProcess terminate] ensure: [
	            Transcript show: #x1].
	        Transcript show: #x2].
	    Transcript show: #x3].
	Transcript show: #x4
] forkAt: Processor activePriority + 1
"prints x1 x2 x3"


"
Unwind after triple nested active process termination - run in the UI:
"
[Processor activeProcess terminate] ensure: [
    [Processor activeProcess terminate] ensure: [
        [Processor activeProcess terminate] ensure: [
            Transcript show: #x1].
        Transcript show: #x2].
    Transcript show: #x3].
Transcript show: #x4
"prints x1 x2 x3 and suspends the UI; recover via Alt+."


"
Unwind after active process suspension during termination inside nested unwind blocks:
"
| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Processor activeProcess suspend. 
				Transcript show: #x1]. 
			Transcript show: #x2]
	] ensure: [
		Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
p terminate.
"first terminate gets suspended inside the unwind block as expected"
p terminate.
"second terminate (or resume) then finishes the termination"
Transcript show: p isTerminated printString
"prints x1 x2 x3 true"


"
Unwind multiple nested active process suspension:
"
| p |
p := [
	[Processor activeProcess suspend] ensure: [
	    [Processor activeProcess suspend] ensure: [
	        [Processor activeProcess suspend] ensure: [
	            Transcript show: #x1].
	        Transcript show: #x2].
	    Transcript show: #x3].
	Transcript show: #x4
] newProcess.
p resume.
Processor yield.
"p stops at the first suspend"
p terminate.
"termiination stops at the second suspend"
p resume.
"after resuming, p stops at the next suspend"
p resume

"prints x1 x2 x3"


"
Part 5: Some additional unsorted examples
============================
Show that the system will not crash and execute all pending unwind blocks as expected.
"

"
Raise a ZeroDivide exception after 1 second.
"
| p |
p := [ [ (Delay forSeconds: 100) wait ] ensure: [ 1 / 0 ] ] newProcess resume.
(Delay forSeconds: 1) wait.
p terminate.


"
BlockCannotReturn error inside nested unwind block during termination:
(this is one of many similar test scenarios in ProcessTerminateUnwindNonLocalReturn class)
"
| p a |
a := Array new: 4 withAll: false.
p := [	[[
			[ ] ensure: [
				[Processor activeProcess suspend] ensure: [
					^a at: 1 put: true].
				a at: 2 put: true]
		] ensure: [
			a at: 3 put: true].
		a at: 4 put: true
	] ensure: [
		Transcript show: a]
] newProcess.
p resume.
Processor yield.
p terminate
"prints #(true false true false)"
"!!!!!! without fork crashes image"

"
Show the examples terminate peacefully after Abandoning the BlockCannotReturn notifications.
CAUTION: Proceeding BlockCannotReturn debugger notifications WILL crash your image !!!!!! 
Save the image first if you want to try.
"

[^2] fork

[[self error: #error] ensure: [^2]] fork

[[[] ensure: [^2]] ensure: [^42]] fork

[[] ensure: [[^2] fork]] fork

"
Guess the result of this test:
"
"Both statements need to be executed separately."
a := [true ifTrue: [^ 1]. 2]
"execute separately"
a value
"What would you say should be the right answer and why??"


"
This example will immediately crash your image !!!!!!
"
"Both statements need to be executed separately."
a := [true ifTrue: [^ 1] yourself]
"execute separately"
[a value] on: BlockCannotReturn do: [:ex | ex resume]


"
Note: The following examples require a fix in #doesNotUnderstand: preventing an infinite recursion of the MNU error.
"

"
Show both ZeroDivide and MessageNotUnderstood errors are raised and all unwind blocks executed after closing the debuggers.
"
[
	[
		[ ] ensure: [ "halfway through completion when suspended"
			[ ] ensure: [ "halfway through completion when suspended"
				Transcript show: (1 / 0) > 0]. "raise ZeroDivide AND MessageNotUnderstood errors"
			Transcript show: #x2]
	] ensure: [ "not started yet when suspended"
		Transcript show: #x3]
] fork
"prints Error: infinite recursion in doesNotUnderstand: x2 x3"


"
Show the following examples deal with MessageNotUnderstood error correctly and do nott crash or freeze the image or cycle through the MessageNotUnderstood error indefinitely. (Note: a fix in #doesNotUnderstand: preventing an infinite recursion of the MNU error needs to be applied.)
Accept gotcha when asked (it represents an unknown method) and Proceed or Abandon as you like...
"
[] ensure: [self gotcha. Transcript show: #done]

[] ensure: [[self gotcha] ensure: [self halt. Transcript show: #done]

[self error: #error] ensure: [self gotcha. Transcript show: #done]

[self error: #error] ensure: [[self gotcha] ensure: [Transcript show: #done]]

[[] ensure: [[self gotcha] ensure: [self halt. Transcript show: #done]]] fork
"This one used to freeze Cuis UI after Halt -> Proceed but recoverable via Alt+.
Latest terminate with the above mentioned #dnu fix executes as expected."


'! !

