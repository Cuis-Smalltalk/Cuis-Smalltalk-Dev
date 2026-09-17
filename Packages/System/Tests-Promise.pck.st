'From Cuis7.9 [latest update: #8200] on 17 September 2026 at 2:26:09 pm'!
'Description '!
!provides: 'Tests-Promise' 1 2!
!requires: 'Promise' 1 0 nil!
SystemOrganization addCategory: #'Tests-Promise'!


!classDefinition: #KeyNotFound category: #'Tests-Promise'!
Error subclass: #KeyNotFound
	instanceVariableNames: 'key'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Promise'!
!classDefinition: 'KeyNotFound class' category: #'Tests-Promise'!
KeyNotFound class
	instanceVariableNames: ''!

!classDefinition: #PromiseTest category: #'Tests-Promise'!
TestCase subclass: #PromiseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Promise'!
!classDefinition: 'PromiseTest class' category: #'Tests-Promise'!
PromiseTest class
	instanceVariableNames: ''!


!KeyNotFound methodsFor: 'accessing' stamp: 'nice 20/Nov/2013, 2:21:00 am (UTC)'!
description
	"Return a textual description of the exception."
	^self messageText! !

!KeyNotFound methodsFor: 'accessing' stamp: 'ar 20/Nov/2007, 5:49:00 pm (UTC)'!
key
	"The key which wasn't found"
	^key! !

!KeyNotFound methodsFor: 'accessing' stamp: 'ar 20/Nov/2007, 5:49:00 pm (UTC)'!
key: anObject
	"The key which wasn't found"
	key := anObject! !

!KeyNotFound methodsFor: 'accessing' stamp: 'ct 13/Apr/2021, 1:01:00 am (UTC)'!
messageText

	^ messageText ifNil: ['Key not found: {1}' translated format: {self key}]! !

!KeyNotFound class methodsFor: 'instance creation' stamp: 'ar 20/Nov/2007, 5:49:00 pm (UTC)'!
key: anObject
	^self new key: anObject! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 4/Jun/2020, 7:32:00 pm (UTC)'!
testAnErrorInOnRejectedRejectsPromise
	"https://promisesaplus.com section 2.2.7.2"
	| p q error |
	p := Promise new.
	q := p ifRejected: [:e | (error := KeyNotFound new) signal].
	p rejectWith: 1.
	self assert: p isRejected description: 'Original Promise not rejected'.
	self assert: q isRejected description: 'Broken Promise not rejected'.
	self assert: p error = 1.
	self assert: q error == error.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 4/Jun/2020, 7:28:00 pm (UTC)'!
testAnErrorInThenRejectsPromise
	"https://promisesaplus.com section 2.2.7.2"
	| p q error |
	p := Promise new.
	q := p then: [:v | (error := KeyNotFound new) signal].
	p resolveWith: 1.
	self deny: p isRejected description: 'Original Promise rejected'.
	self assert: q isRejected description: 'Broken Promise not rejected'.
	self assert: p value = 1.
	self assert: q error == error.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 17/May/2013, 9:26:00 pm (UTC)'!
testCanRejectPromise
	| p |
	p := Promise new.
	p rejectWith: Error new.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 29/Jan/2018, 5:32:00 pm (UTC)'!
testCannotRejectFulfilledPromise
	| p |
	p := Promise unit: 1.
	p rejectWith: Error new.
	self assert: p isResolved.
	self assert: 1 equals: p value.
! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 29/Jan/2018, 5:34:00 pm (UTC)'!
testCannotResolveaRejectedPromise
	| p e |
	p := Promise new.
	e := Error new.
	p rejectWith: e.
	p resolveWith: 1.
	self assert: p isRejected.
	self assert: p error == e.
! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 28/Jan/2018, 7:23:00 pm (UTC)'!
testCollapsesChainsOfPromises
	"The monadic bind operator has signature (m a -> (a -> m b) -> m b): that is, in our setting,
	the block given to `then:` is expected to return a *Promise* of a value, not a value directly.
	It is convenient to accept non-promise values and automatically lift them into the monad,
	but we must also ensure we treat the case where a `then:`-block yields a Promise correctly."
	| p q r |
	p := Promise new.
	q := p then: [:v | Promise unit: v * 2].
	r := q then: [:v | Promise unit: v + 1].
	p resolveWith: 4.
	self assert: 4 * 2 equals: q value.
	self assert: (4 * 2 + 1) equals: r value.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 1/Feb/2018, 2:33:00 am (UTC)'!
testFirstResolutionWins
	| p |
	p := Promise new.
	p resolveWith: 1.
	p resolveWith: 2.
	self assert: p isResolved.
	self assert: p value == 1.
! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 4/Jun/2020, 10:45:00 pm (UTC)'!
testNilErrBlockPropagation
	"https://promisesaplus.com section 2.2.7.4"
	| p q |
	p := Promise new.
	q := p then: [:v | self error: 'Shouldn''t call resolvedBlock'] ifRejected: nil.
	p rejectWith: 1.
	self assert: p isRejected.
	self assert: q isRejected.
	self assert: p error equals: 1.
	self assert: q error equals: 1.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 4/Jun/2020, 10:44:00 pm (UTC)'!
testNilResolvedBlockPropagation
	"https://promisesaplus.com section 2.2.7.3"
	| p q |
	p := Promise new.
	q := p then: nil ifRejected: [:e | self error: 'Shouldn''t call errBlock'].
	p resolveWith: 1.
	self assert: p isResolved.
	self assert: q isResolved.
	self assert: p value equals: 1.
	self assert: q value equals: 1.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 20/May/2013, 11:36:00 pm (UTC)'!
testRejectWithInvokesErrorHandlers
	| p error returnedError |
	returnedError := nil.
	error := KeyNotFound new.
	p := Promise ifRejected: [:e | returnedError := e].
	p rejectWith: error.
	self assert: returnedError notNil description: 'Error block did not run.'.
	self assert: error equals: returnedError description: 'Error not passed into block'.
	self assert: error equals: p error description: 'Promise didn''t store error'.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 23/May/2013, 8:35:00 pm (UTC)'!
testThenPermitsChainingOfPromises
	| p q r |
	p := Promise new.
	q := p then: [:v | v * 2].
	r := q then: [:v | v + 1].
	p resolveWith: 4.
	self assert: 4 * 2 equals: q value.
	self assert: (4 * 2 + 1) equals: r value.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 15/May/2013, 8:23:00 pm (UTC)'!
testThenReturnsaPromise
	| p |
	p := Promise new then: [:v | v * 2].
	self assert: Promise equals: p class.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 15/May/2013, 8:12:00 pm (UTC)'!
testUnitReturnsaPromise
	| p |
	p := Promise unit: 1.
	self assert: Promise equals: p class.
	self assert: p isResolved.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 17/Feb/2017, 5:04:00 pm (UTC)'!
testWaitForRejection
	| p |
	p := Promise new.
	[ (Delay forMilliseconds: 1) wait. p rejectWith: Error new ] fork.
	self should: [ p wait ] raise: BrokenPromise.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 17/Feb/2017, 5:01:00 pm (UTC)'!
testWaitForResolution
	| p |
	p := Promise new.
	[ (Delay forMilliseconds: 1) wait. p resolveWith: #ok ] fork.
	self assert: [ p wait = #ok ]! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 17/Feb/2017, 5:04:00 pm (UTC)'!
testWaitRejectionYieldsCorrectBrokenPromise
	| p |
	p := Promise new.
	[ (Delay forMilliseconds: 1) wait. p rejectWith: Error new ] fork.
	[ p wait ] on: BrokenPromise do: [ :bp | ^ self assert: [ bp promise == p ] ].
	self fail: 'Should not reach this point'! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'fbs 26/May/2013, 5:13:00 pm (UTC)'!
testifRejectedDoesNotRunBlockIfPromiseResolves
	| p q error |
	error := nil.
	p := Promise new.
	q := p ifRejected: [:e | error := e].
	p resolveWith: 1.
	self deny: q isRejected.
	self assert: nil equals: error.! !

!PromiseTest methodsFor: 'tests - monad' stamp: 'tonyg 4/Jun/2020, 7:33:00 pm (UTC)'!
testifRejectedRunsBlockIfPromiseFails
	"https://promisesaplus.com section 2.2.7.1"
	| p q error |
	error := nil.
	p := Promise new.
	q := p ifRejected: [:e | error := e "N.B. returns a value, does not signal an Exception"].
	p rejectWith: KeyNotFound new.
	self assert: q isResolved.
	self assert: KeyNotFound equals: error class.
	self assert: q value == error.! !

!PromiseTest methodsFor: 'tests' stamp: 'jcg 5/Apr/2010, 3:36:00 am (UTC)'!
testChainedResolvers
	| promise1 promise2 result |
	promise1 := Promise new.
	promise2 := Promise new.
	promise1 whenResolved: [:bool | promise2 resolveWith: bool not].
	promise2 whenResolved: [:bool | result := bool].
	promise1 resolveWith: false.
	self should: [result].! !

!PromiseTest methodsFor: 'tests' stamp: 'tonyg 13/Feb/2018, 2:30:00 pm (UTC)'!
testFulfillWithError
	| p |
	p := Promise new.
	p fulfillWith: [ 1 / 0 ] passErrors: false.
	self assert: p isRejected.
	self assert: ZeroDivide equals: p error class.! !

!PromiseTest methodsFor: 'tests' stamp: 'tonyg 13/Feb/2018, 7:40:00 pm (UTC)'!
testFulfillWithHaltAndResult
	| p |
	p := Promise new.
	[
		p fulfillWith: [ self halt. 3 + 4 ]
	] on: Halt do: [:ex | ex resume].
	self assert: p isResolved.
	self assert: 7 equals: p value.! !

!PromiseTest methodsFor: 'tests' stamp: 'tonyg 13/Feb/2018, 2:24:00 pm (UTC)'!
testFulfillWithResult
	| p |
	p := Promise new.
	p fulfillWith: [ 3 + 4 ].
	self assert: p isResolved.
	self assert: 7 equals: p value.! !

!PromiseTest methodsFor: 'tests' stamp: 'jcg 5/Apr/2010, 3:31:00 am (UTC)'!
testMultipleResolvers
	| promise sum |
	sum := 0.
	promise := Promise new.
	5 timesRepeat: [
		promise whenResolved: [:val | sum := sum + val].
	].
	promise resolveWith: 5.
	self should: [sum = 25].
	! !

!PromiseTest methodsFor: 'tests' stamp: 'fbs 23/May/2013, 11:28:00 am (UTC)'!
testSingleResolver
	| promise sum |
	sum := 0.
	promise := Promise new.
	promise whenResolved: [:val | sum := sum + val].
	promise resolveWith: 5.
	self assert: 5 equals: sum.
	! !

!PromiseTest methodsFor: 'tests' stamp: 'tonyg 9/Feb/2018, 2:45:00 pm (UTC)'!
testTimeout
	| promise |
	promise := Promise new.
	self shouldnt: [promise waitTimeoutMSecs: 1].
	self shouldnt: [promise isResolved].
	self shouldnt: [promise isRejected].
	promise resolveWith: 45.
	self should: [promise waitTimeoutMSecs: 1].
	self should: [promise isResolved].
	self shouldnt: [promise isRejected].! !

!PromiseTest methodsFor: 'tests' stamp: 'tonyg 9/Feb/2018, 2:47:00 pm (UTC)'!
testTimeoutRejected
	| promise |
	promise := Promise new.
	self shouldnt: [promise waitTimeoutMSecs: 1].
	self shouldnt: [promise isResolved].
	self shouldnt: [promise isRejected].
	promise rejectWith: 45.
	self shouldnt: [promise waitTimeoutMSecs: 1].
	self shouldnt: [promise isResolved].
	self should: [promise isRejected].! !

!PromiseTest methodsFor: 'private' stamp: 'jmv 16/Sep/2026, 9:35:13 pm (UTC)'!
waitUntil: aBlock orCycleCount: anInteger
	"This is a gross hack that depends on running the tests in Morphic.
	We simply repeatedly do a cycle of the interaction loop, which happens
	to also be the way that the queue of pending futures gets serviced."
	| counter |
	counter := 0.
	[
		aBlock value ifTrue: [^ true].
		self runningWorld doOneMinimalCycleNow.
		counter := counter + 1.
		counter >= anInteger ifTrue: [^ false].
	] repeat! !
