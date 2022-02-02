'From Cuis 6.0 [latest update: #5057] on 2 February 2022 at 5:03:04 pm'!

!Process class methodsFor: 'documentation' stamp: 'jmv 2/2/2022 17:02:55'!
terminateExamples
"
This is the contents of the examples posted by Jaromir to exercise his worn on process termination and exceptions.
It was posted to the Cuis mail list at https://lists.cuis.st/mailman/archives/cuis-dev/2021-May/003171.html
(or maybe, it is a later version of edition of it if this comment is not up to date!!)

Thanks Jaromir for this great contribution!!

Workspace new
		contents: Process terminateExamples;
		openLabel: 'Jaromir Mata''s Process - terminate examples Cuis'.
"
^


'"Process - Cuis terminate examples
Some examples to illustrate the termination bugs and test the proposed rewrite of #terminate
=========================================="
"terminate suspended:"

| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess suspend. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"

".......................................
terminate runnable:"

| p |
p := [
	[
		[ ] ensure: [
			[Processor yield] ensure: [
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"

| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor yield. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"
".......................................
terminate blocked:"

| p s |
s := Semaphore new.
p := [
	[
		[ ] ensure: [
			[s wait] ensure: [
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"


| p s |
s := Semaphore new.
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				s wait. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
p terminate.
Transcript show: p isTerminated printString

"prints x1 x2 x3"

".......................................
terminate active:"

| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
"Two yields necessary: terminate active is a two-step procedure"
Processor yield. Processor yield.
Transcript show: p isTerminated printString

"prints x1 x2 x3"


| p |
p := [
	[
		[ ] ensure: [
			[ ] ensure: [
				Processor activeProcess terminate. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
"Two yields necessary: terminate active is a two-step procedure"
Processor yield. Processor yield.
Transcript show: p isTerminated printString

"prints x1 x2 x3"



"==========================================
unhandled error:
Termination happens when the user hits Abandon on the Debugger window."
"cf.: prints x1 x2 x3 x4 when hit Proceed"

[
	[ ] ensure: [
		[self error: ''unwind test''] ensure: [
			Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: ''unwind test''.
			Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x2 x3"

"........................
nested unhandled errors:"

| p |
p := [
	[
		[ ] ensure: [
			[self error: ''unwind test outer''] ensure: [
				self error: ''unwind test inner''.
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield.
"prints x1 x2 x3"


"triple nested errors:"

[self error: ''outer error''] ensure: [
    [self error: ''middle error''] ensure: [
        [self error: ''inner error''] ensure: [
            Transcript show: ''x1''].
        Transcript show: ''x2''].
    Transcript show: ''x3''].
Transcript show: ''x4''
"prints x1 x2 x3"
"same result when wrapped in fork"


[] ensure: [
[self error: ''outer error''] ensure: [
    [self error: ''middle error''] ensure: [
        [self error: ''inner error''] ensure: [
            Transcript show: ''x1''].
        Transcript show: ''x2''].
    Transcript show: ''x3''].
Transcript show: ''x4'']
"prints x1 x2 x3 x4"
"same result when wrapped in fork"



"=========================================
error and non-local return combined:"

"Termination happens when the user hits Abandon on the Debugger window."

"........................
non-local return inside inner-most halfway thru unwind block:"

[
	[ ] ensure: [
		[self error: ''unwind test''] ensure: [
			^Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: ''unwind test''. 
			^Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x3"

"........................
non-local return inside outer-most halfway thru unwind block:"

[
	[ ] ensure: [
		[self error: ''unwind test''] ensure: [
			Transcript show: ''x1'']. 
		^Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: ''unwind test''. 
			Transcript show: ''x1'']. 
		^Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''


"prints x1 x2 x3"


"one more level..."
[
	[
		[ ] ensure: [
			[ ] ensure: [
				self error: ''unwind test''. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		^Transcript show: ''x3'']
] ensure: [
	Transcript show: ''x4'']

"prints x1 x2 x3 x4 (even if wrapped in #fork)"



".............................
non-local return outside halfway thru unwind blocks:"

[
	[ ] ensure: [
		[self error: ''unwind test''] ensure: [
			Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	^Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x2 x3"


[
	[ ] ensure: [
		[ ] ensure: [
			self error: ''unwind test''. 
			Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	^Transcript show: ''x3''].
Transcript show: ''x4''


"prints x1 x2 x3"


"============================================================
For comparison only:"

"The tests presented here are not affected by the new #terminate.
(A) an unhandled error unwind in the preceding examples followed the new termination logic completing all unwind blocks halfway through their execution
(B) a handled error unwind follows the traditional ''direct'' unwind path using simpler semantics - it doesn''t complete unwind blocks halfway through their execution"

".........................................
handled error:"


[
	[
		[ ] ensure: [
			[self error: ''unwind test''] ensure: [
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3'']
] on: Error do: [].
Transcript show: ''x4''

"prints x1 x3 x4, skips x2"

[
	[
		[ ] ensure: [
			[] ensure: [
				self error: ''unwind test''.
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3'']
] on: Error do: [].
Transcript show: ''x4''

"prints x3 x4, skips x1 x2"

"............................................
nested handled errors:"

| p |
p := [
	[
		[
			[ ] ensure: [
				[self error: ''unwind test outer''] ensure: [
					self error: ''unwind test inner''.
					Transcript show: ''x1'']. 
				Transcript show: ''x2'']
		] ensure: [
			Transcript show: ''x3'']
	] on: Error do: [].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield. Processor yield.

"prints x3 x4, skips x1 x2"



"............................................
non-local return:"

"Similarly a simple non-local return execution follows a ''direct'' unwind path logic in #resume[:through:] using simpler semantics."

[
	[ ] ensure: [
		[^1] ensure: [
			Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x3, skips x2"


[
	[ ] ensure: [
		[] ensure: [
			^Transcript show: ''x1'']. 
		Transcript show: ''x2'']
] ensure: [
	Transcript show: ''x3''].
Transcript show: ''x4''

"prints x1 x3, skips x2"



"==========================================
Crazies:"

"These tests explore new #teminate behavior under more extreme circumstances.
Unwind after active process termination"

[ ] ensure: [
	[Processor activeProcess terminate] ensure: [Transcript show: ''x1'']. 
	Transcript show: ''x2''
	]
"prints x1 x2 and terminates UI - recoverable via Alt+. or cmd+."


"Unwind after active process suspension during termination:"

| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess suspend] ensure: [
				Processor activeProcess suspend. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3'']
] newProcess.
p resume.
Processor yield.
p terminate
"suspends UI and prints x1 x2 x3 after Alt+. recovery"


"Unwind after double active process termination:"

| p |
p := [
	[
		[ ] ensure: [
			[Processor activeProcess terminate] ensure: [
				Processor activeProcess terminate. 
				Transcript show: ''x1'']. 
			Transcript show: ''x2'']
	] ensure: [
		Transcript show: ''x3''].
	Transcript show: ''x4''
] newProcess.
p resume.
Processor yield. Processor yield. Processor yield. Processor yield.
Transcript show: p isTerminated printString
"lets UI live and prints x1 x2 x3"


| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: ''x1''. 
		Processor activeProcess terminate. Transcript show: ''x2'']. 
] newProcess.
p resume.
Processor yield. Processor yield. Processor yield. Processor yield.
Transcript show: p isTerminated printString
"lets current UI live and prints x1 x2"


| p ap |
p := [
	[ ap := Processor activeProcess. ap terminate. Transcript show: ''x1'' ] 
		ensure: [ ap terminate. Transcript show: ''x2'' ]. 
] newProcess.
p resume.
Processor yield. Processor yield. Processor yield.
Transcript show: p isTerminated printString
"lets current UI live and prints x2"


| p |
p := [
	[ Processor activeProcess terminate ] 
		ensure: [ Processor activeProcess terminate ]. 
] newProcess.
p resume.
Processor yield. Processor yield. Processor yield. 
Transcript show: p isTerminated printString
"answers true, no error"



"Unwind after active process termination combined with non-local return:"

| p |
p := [
	[] ensure: [ 
		Processor activeProcess terminate. Transcript show: ''x1''. 
		true ifTrue: [^2]. 
		Processor activeProcess terminate. Transcript show: ''x2'']
] newProcess.
p resume.
Processor yield. Processor yield. 
Transcript show: p isTerminated printString
"prints x1 and correctly raises BlockCannotReturn error"


"Unwind after BlockCannotReturn error:"

| p a |

a := Array new: 4 withAll: false.
p := [
         [
        	[ ] ensure: [
        			[Processor activeProcess suspend] ensure: [
            			^a at: 1 put: true].
        			a at: 2 put: true]
		] ensure: [a at: 3 put: true].
		a at: 4 put: true
    ] newProcess.
p resume.
Processor yield.
"make sure p is suspended and none of the unwind blocks has finished yet"
self assert: p isSuspended.
a noneSatisfy: [ :b | b ].
"now terminate the process and make sure all unwind blocks have finished"
p terminate.
self assert: p isTerminated.
self assert: a first & a third.
self assert: (a second | a fourth) not.
"--->   #(true false true false) ...OK"


"Triple nested active process terminate:"

[	x := ''''.
	[Processor activeProcess terminate] ensure: [
	    [Processor activeProcess terminate] ensure: [
	        [Processor activeProcess terminate] ensure: [
	            x:=x,''3''].
	        x:=x,''2''].
	    x:=x,''1''].
	x:=x,''0''
] fork
x
"---> ''321'' ...OK"

"Same as before but without fork:"

x := ''''.
[Processor activeProcess terminate] ensure: [
    [Processor activeProcess terminate] ensure: [
        [Processor activeProcess terminate] ensure: [
            x:=x,''3''].
        x:=x,''2''].
    x:=x,''1''].
x:=x,''0''
x
"terminates UI and answers ---> ''321'' after Alt+. recovery"
"(Squeak crashes irrecoverably)"


"Triple nested active process terminate:"

p := 
[	x := ''''.
	[] ensure: [
		[Processor activeProcess suspend] ensure: [
		    [Processor activeProcess suspend] ensure: [
		        [Processor activeProcess suspend] ensure: [
		            x:=x,''3''].
		        x:=x,''2''].
		    x:=x,''1''].
		x:=x,''0'']
] newProcess resume.
Processor yield.
p terminate
x   
"Cuis suspends UI repeatedly but answers ---> ''3210'' after repeated Alt+. recovery
Squeak answers ---> ''3210'' without suspending UI - why the difference?
Without fork Squeak suspends UI just once but Cuis 3 times, both answer x correctly"


"These behave as expected and won''t crash the image even after proceeding the BlockCannotReturn error:"

[^2] fork

[[self error: ''error''] ensure: [^2]] fork

"do-it:"
"Both statements need to be executed separately in a Workspace"
a := [true ifTrue: [^ 1] yourself]
[a value] on: BlockCannotReturn do: [:ex | ex resume]

"do-it:"
"Both statements need to be executed separately in a Workspace"
a := [true ifTrue: [^ 1]. 2]
a value

"These will deal with MessageNotUnderstood correctly and won''t crash the image or loop infinitely"

[] ensure: [self gotcha. Transcript show: ''0'']

[] ensure: [[self gotcha] ensure: [self halt. Transcript show: ''0'']]

[self error: ''error''] ensure: [[self gotcha] ensure: [Transcript show: ''0'']]

[self error: ''error''] ensure: [self gotcha. Transcript show: ''0'']

"This one freezes UI after Halt -> Proceed but recoverable via Alt+."
[[] ensure: [[self gotcha] ensure: [self halt. Transcript show: ''0'']]] fork.



"===============
Some additional unsorted examples:"

"This example should show both ZeroDivide and MessageNotUnderstood errors"

x1 := x2 := x3 := nil.
p:=[
		[
			[ ] ensure: [ "halfway through completion when suspended"
				[ ] ensure: [ "halfway through completion when suspended"
					Processor activeProcess suspend. 
					x1 := (2 / 0  "error!!") > 0]. 
				x2 := true]
		] ensure: [ "not started yet when suspended"
			x3 := true]
] newProcess resume.
Processor yield.
p terminate
{x1 . x2 . x3} --->  #(MessageNotUnderstood: ZeroDivide>>> true true) 


'! !

