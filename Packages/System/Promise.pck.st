'From Cuis7.1 [latest update: #6795] on 28 October 2024 at 9:45:45 pm'!
'Description '!
!provides: 'Promise' 1 1!
SystemOrganization addCategory: #Promise!


!classDefinition: #BrokenPromise category: #Promise!
Error subclass: #BrokenPromise
	instanceVariableNames: 'promise'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Promise'!
!classDefinition: 'BrokenPromise class' category: #Promise!
BrokenPromise class
	instanceVariableNames: ''!

!classDefinition: #NullMutex category: #Promise!
Object subclass: #NullMutex
	instanceVariableNames: 'semaphore owner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Promise'!
!classDefinition: 'NullMutex class' category: #Promise!
NullMutex class
	instanceVariableNames: ''!

!classDefinition: #Promise category: #Promise!
Object subclass: #Promise
	instanceVariableNames: 'value resolvers mutex state error rejecters'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Promise'!
!classDefinition: 'Promise class' category: #Promise!
Promise class
	instanceVariableNames: ''!


!BrokenPromise commentStamp: 'tonyg 2/17/2017 13:53' prior: 0!
I am signalled when, during a Promise>>wait, the promise is rejected.	promise:		the promise itself.!

!NullMutex commentStamp: 'fbs 5/17/2013 20:30' prior: 0!
A Mutex is a light-weight MUTual EXclusion object being used when two or more processes need to access a shared resource concurrently. A Mutex grants ownership to a single process and will suspend any other process trying to aquire the mutex while in use. Waiting processes are granted access to the mutex in the order the access was requested.This Mutex DOES NOT mutually exclude anything. It just implements the same protocol.!

!Promise commentStamp: 'tonyg 1/31/2018 23:34' prior: 0!
I represent the result of an asynchronous message.  Once the message is processed, I will be resolved to a value.  I am typically instantiated by invocations of #futureSend:at:args: (and not by #futureDo:atArgs:).See class-comment of FutureNode.I also implement the Promises/A+ Javascript specification. This allows you to chain my instances to perform arbitrarily complex asynchronous tasks with error handling baked in.A Promise may be in one of three possible states: #pending, #fulfilled or #rejected. A Promise may move from #pending -> #fulfilled (by way of the resolveWith: message), or from #pending -> #rejected (by way of rejectWith:). No other state changes may occur.Once #fulfilled or #rejected, a Promise's value must not change. In keeping with the major Javascript Promise implementations' interpretations of this, calls to resolveWith: or rejectWith: when a Promise is in #fulfilled or #rejected state are simply ignored - an error is not signalled. (See test cases PromiseTest testFirstResolutionWins, testCannotRejectFulfilledPromise and testCannotResolveaRejectedPromise.)!

!BrokenPromise methodsFor: 'as yet unclassified' stamp: 'tonyg 2/17/2017 13:52'!
defaultAction
	self messageText: 'Promise was rejected'.
	^super defaultAction! !

!BrokenPromise methodsFor: 'as yet unclassified' stamp: 'tonyg 2/17/2017 13:52'!
isResumable
	^ true! !

!BrokenPromise methodsFor: 'as yet unclassified' stamp: 'tonyg 2/17/2017 13:51'!
promise
	^ promise! !

!BrokenPromise methodsFor: 'as yet unclassified' stamp: 'tonyg 2/17/2017 13:51'!
promise: aPromise
	promise := aPromise! !

!NullMutex methodsFor: 'mutual exclusion' stamp: 'fbs 5/17/2013 20:29'!
critical: aBlock
	^ aBlock value.! !

!Promise methodsFor: 'accessing' stamp: 'fbs 5/17/2013 18:42'!
error
	^ error.! !

!Promise methodsFor: 'accessing' stamp: 'jcg 12/17/2009 01:03'!
value
	^value! !

!Promise methodsFor: 'initialize' stamp: 'fbs 5/26/2013 14:19'!
initialize
	state := #pending.
	resolvers := #().
	rejecters := #().
	mutex := Mutex new.! !

!Promise methodsFor: 'initialize' stamp: 'fbs 5/20/2013 20:37'!
initializeWithIfRejected: aBlock
	self initialize.
	rejecters := {aBlock}.! !

!Promise methodsFor: 'initialize' stamp: 'fbs 5/15/2013 17:37'!
initializeWithResolvedValue: anObject
	self initialize.
	self resolveWith: anObject.! !

!Promise methodsFor: 'monad' stamp: 'tonyg 1/28/2018 16:07'!
>> resolvedBlock
	"Like >>=, but discards the result of the promise."
	^ self then: [:v | resolvedBlock value]! !

!Promise methodsFor: 'monad' stamp: 'tonyg 1/28/2018 15:54'!
>>= resolvedBlock
	"Alias for `then:` allowing convenient chaining."
	^ self then: resolvedBlock! !

!Promise methodsFor: 'monad' stamp: 'tonyg 6/4/2020 19:45'!
ifRejected: errBlock
	^ self then: nil ifRejected: errBlock! !

!Promise methodsFor: 'monad' stamp: 'tonyg 6/4/2020 19:45'!
then: resolvedBlock
	^ self then: resolvedBlock ifRejected: nil! !

!Promise methodsFor: 'monad' stamp: 'tonyg 10/8/2020 18:05'!
then: resolvedBlock ifRejected: errBlock
	"Return a Promise that, if it resolves, runs the resolvedBlock. If resolution throws an Exception, it runs the errBlock."
	| p |
	p := self species new.
	resolvedBlock
		ifNil: [self whenResolved: [:v | p resolveWith: v]]
		ifNotNil: [
			self whenResolved: [:v |
				[p resolveWith: (resolvedBlock value: v)]
					on: Error do: [:e | p rejectWith: e]]].
	errBlock
		ifNil: [self whenRejected: [:e | p rejectWith: e]]
		ifNotNil: [
			self whenRejected: [:e |
				[p resolveWith: (errBlock value: e)]
					on: Error do: [:e2 | p rejectWith: e2]]].
	^ p.! !

!Promise methodsFor: 'printing' stamp: 'fbs 5/20/2013 20:42'!
printOn: aStream
	aStream nextPutAll: 'a Promise'.
	self isResolved ifTrue: [
		aStream
			nextPutAll: '(resolved: ';
			nextPutAll: value printString;
			nextPutAll: ')'].
	self isRejected ifTrue: [
		aStream
			nextPutAll: '(rejected: ';
			nextPutAll: error printString;
			nextPutAll: ')'].! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 2/13/2018 15:35'!
fulfillWith: aBlock
	self fulfillWith: aBlock passErrors: rejecters isEmpty! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 2/13/2018 16:41'!
fulfillWith: aBlock passErrors: aBoolean
	"Evaluate aBlock. If it signals an exception, reject this promise with the exception
	as the argument; if it returns a value [or another Promise], resolve this promise
	with the result.
	
	If aBoolean is true, and an exception is signaled, it is passed out to the caller.
	If aBoolean is false, signaled exceptions are considered handled after the promise
	has been rejected."
	[ self resolveWith: aBlock value ]
		on: Exception
		do: [ :ex |
			(ex isKindOf: Halt)
				ifTrue: [ex pass]
				ifFalse: [
					self rejectWith: ex.
					aBoolean ifTrue: [ ex pass ] ]]! !

!Promise methodsFor: 'resolving' stamp: 'pre 12/17/2018 17:42'!
reject

	self rejectWith: nil! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 2/11/2018 15:44'!
rejectWith: anObject
	"Reject this promise."
	mutex critical: [
		(state == #pending) ifTrue: [ | worklist |
			error := anObject.
			state := #rejected.
			worklist := rejecters.
			resolvers := #().
			rejecters := #().
			worklist do: [:r | self evaluateRejecter: r]]]! !

!Promise methodsFor: 'resolving' stamp: 'pre 12/17/2018 17:41'!
resolve
	
	self resolveWith: nil! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 1/24/2020 15:26'!
resolveWith: arg
	"Resolve this promise. If arg is itself a Promise, make this promise depend upon it,
	as detailed in the Promises/A+ spec:
		https://promisesaplus.com/#the-promise-resolution-procedure"

	arg isPromise
		ifTrue: [
			arg whenResolved: [:v | self resolveWith: v].
			arg whenRejected: [:e | self rejectWith: e]]
		ifFalse: [
			mutex critical: [
				(state == #pending) ifTrue: [ | worklist |
					value := arg.
					state := #fulfilled.
					worklist := resolvers.
					resolvers := #().
					rejecters := #().
					worklist do: [:r | self evaluateResolver: r]]]]! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 2/11/2018 15:48'!
whenRejected: aBlock
	"Evaluate aBlock when I am rejected"
	aBlock numArgs <= 1 ifFalse: [self error: 'Must be 0- or 1-argument block'].
	mutex critical: [
		(state == #pending)
			ifTrue: [rejecters := rejecters copyWith: aBlock]
			ifFalse: [self isRejected ifTrue: [self evaluateRejecter: aBlock]]].
	^ nil! !

!Promise methodsFor: 'resolving' stamp: 'tonyg 2/11/2018 15:50'!
whenResolved: aBlock
	"Evaluate aBlock when I am resolved"
	aBlock numArgs <= 1 ifFalse:[self error: 'Must be 0- or 1-argument block'].
	mutex critical: [
		(state == #pending)
			ifTrue: [resolvers := resolvers copyWith: aBlock]
			ifFalse: [self isResolved ifTrue: [self evaluateResolver: aBlock]]].
	^ nil! !

!Promise methodsFor: 'testing' stamp: 'tonyg 2/1/2018 09:57'!
isPromise
	^ true! !

!Promise methodsFor: 'testing' stamp: 'fbs 5/20/2013 09:32'!
isRejected
	^ state == #rejected.! !

!Promise methodsFor: 'testing' stamp: 'fbs 5/17/2013 18:33'!
isResolved
	^ state == #fulfilled.! !

!Promise methodsFor: 'waiting' stamp: 'tonyg 2/17/2017 13:55'!
wait
	"Wait unconditionally for this promise to become fulfilled or rejected."
	| sema |
	sema := Semaphore new.
	self whenResolved:[sema signal].
	self whenRejected:[sema signal].
	sema wait.
	^ self isResolved
		ifTrue: [ value ]
		ifFalse: [ BrokenPromise new promise: self; signal ]! !

!Promise methodsFor: 'waiting' stamp: 'tonyg 2/9/2018 11:49'!
waitTimeoutMSecs: msecs
	"Wait for at most the given number of milliseconds for this promise to settle.
	Answer true if it is resolved, false otherwise. False can therefore mean EITHER 'timeout' OR 'rejected'."
	| sema delay |
	sema := Semaphore new.
	self whenResolved: [sema signal].
	self whenRejected: [sema signal].
	delay := Delay timeoutSemaphore: sema afterMSecs: msecs.
	[sema wait] ensure: [delay unschedule].
	^ self isResolved.! !

!Promise methodsFor: 'waiting' stamp: 'jcg 12/17/2009 01:11'!
waitTimeoutSeconds: seconds
	"Wait for at most the given number of seconds for this promise to resolve. Answer true if it is resolved, false otherwise."
	^self waitTimeoutMSecs: seconds*1000! !

!Promise methodsFor: 'private' stamp: 'jmv 10/28/2024 21:16:55'!
evaluateRejecter: rejecterBlock
	^ rejecterBlock valueWithPossibleArgument: error.! !

!Promise methodsFor: 'private' stamp: 'jmv 10/28/2024 21:17:34'!
evaluateResolver: resolverBlock
	^ resolverBlock valueWithPossibleArgument: value.! !

!Promise class methodsFor: 'instance creation' stamp: 'tonyg 10/8/2020 18:05'!
ifRejected: aBlock
	^ self basicNew initializeWithIfRejected: aBlock.! !

!Promise class methodsFor: 'instance creation' stamp: 'tonyg 10/8/2020 18:05'!
unit: anObject
	"Return a resolved Promise. #new is the other half of Promise's unit function; #new returns an unresolved Promise."
	^ self basicNew initializeWithResolvedValue: anObject.! !

!Object methodsFor: '*Promise' stamp: 'JMM 10/23/2024 11:11:29'!
isPromise
	^ false! !
